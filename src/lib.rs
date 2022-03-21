extern crate core;

use std::fmt;
use std::fmt::Formatter;

use crate::operators::{ConditionalOperator, IncompatibleOperation};

pub mod operators;
pub mod parser;
mod tests;

pub struct VariableNotFound(String);

impl fmt::Display for VariableNotFound {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "Variable not found {}", self.0)
    }
}

pub type ArithmeticResult = f64;
pub type VariableTranslationResult<'a> = std::result::Result<VariableType<'a>, VariableNotFound>;
pub type EvaluationResult = std::result::Result<OperandEvaluationType, OperandEvaluationError>;

pub enum OperandEvaluationError {
    VariableNotFound(VariableNotFound),
    IncompatibleOperation(IncompatibleOperation),
}

impl fmt::Display for OperandEvaluationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OperandEvaluationError::VariableNotFound(x) => x.fmt(f),
            OperandEvaluationError::IncompatibleOperation(x) => x.fmt(f)
        }
    }
}

pub struct MathSentence(ArithmeticResult);

impl MathSentence {
    fn eval<F>(&self, _variables: F) -> EvaluationResult where F: Fn(&str) -> VariableTranslationResult {
        Ok(OperandEvaluationType::Arithmetic(self.0))
    }
    fn soft_clone(&self) -> MathSentence {
        MathSentence(100f64)
    }
}

impl PartialEq for MathSentence {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl ToString for MathSentence {
    fn to_string(&self) -> String {
        String::from("{Math Expression Template}")
    }
}

pub enum OperandEvaluationType {
    Logical(bool),
    Arithmetic(ArithmeticResult),
}

impl PartialEq for OperandEvaluationType {
    fn eq(&self, other: &Self) -> bool {
        match self {
            OperandEvaluationType::Logical(x) => {
                if let OperandEvaluationType::Logical(y) = other { x == y } else { false }
            }
            OperandEvaluationType::Arithmetic(x) => {
                if let OperandEvaluationType::Arithmetic(y) = other { x == y } else { false }
            }
        }
    }
}

impl ToString for OperandEvaluationType {
    fn to_string(&self) -> String {
        String::from(match self {
            OperandEvaluationType::Logical(x) => x.to_string(),
            OperandEvaluationType::Arithmetic(x) => x.to_string()
        })
    }
}

// There's no fucking enum inheritance.
pub enum VariableType<'a> {
    Text(&'a str),
    Logical(bool),
    Arithmetic(ArithmeticResult),
}

pub enum Operand {
    Variable(String),
    Arithmetic(MathSentence),
    UnaryOperation(&'static ConditionalOperator, Box<Self>),
    BiOperation(Box<Self>, &'static ConditionalOperator, Box<Self>),
}

impl fmt::Debug for Operand {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Variable(x) => x.fmt(f),
            Operand::Arithmetic(x) => write!(f, "{}", x.to_string()),
            Operand::BiOperation(lhs, op, rhs) => {
                <Operand as fmt::Debug>::fmt(lhs, f)?;
                write!(f, "{}", op.to_string())?;
                <Operand as fmt::Debug>::fmt(rhs, f)
            }
            Operand::UnaryOperation(op, rhs) => {
                write!(f, "{}", op.to_string())?;
                <Operand as fmt::Debug>::fmt(rhs, f)
            }
        }
    }
}

impl Operand {
    pub fn soft_clone(&self) -> Operand {
        match self {
            Operand::Variable(x) => Operand::Variable(x.clone()),
            Operand::Arithmetic(x) => Operand::Arithmetic(x.soft_clone()),
            Operand::BiOperation(lhs, op, rhs) =>
                Operand::BiOperation(Box::new(lhs.soft_clone()), op, Box::new(rhs.soft_clone())),
            Operand::UnaryOperation(op, rhs) =>
                Operand::UnaryOperation(op, Box::new(rhs.soft_clone()))
        }
    }
}

impl PartialEq for Operand {
    fn eq(&self, other: &Self) -> bool {
        use Operand::*;

        if let (Variable(x), Variable(y)) = (self, other) { return x == y; }
        if let (Arithmetic(x), Arithmetic(y)) = (self, other) { return x == y; }
        if let (
            UnaryOperation(op, rhs),
            UnaryOperation(op_other, rhs_other)
        ) = (self, other) { return op == op_other && rhs == rhs_other; }
        if let (
            BiOperation(lhs, op, rhs),
            BiOperation(lhs_other, op_other, rhs_other)
        ) = (self, other) { return lhs == lhs_other && op == op_other && rhs == rhs_other; }


        return false;
    }
}

impl Operand {
    pub fn eval<F>(&self, variables: &F) -> EvaluationResult
        where F: Fn(&str) -> VariableTranslationResult {
        match self {
            Operand::Variable(x) => handle_variable(variables(x)),
            Operand::Arithmetic(x) => x.eval(variables),
            //Operand::UnaryOperation(op, rhs) => op.handle(rhs.eval()),
            Operand::BiOperation(lhs, op, rhs) => {
                let lhs = match lhs.eval(variables) {
                    Ok(x) => x,
                    Err(x) => return Err(x)
                };

                let rhs = match rhs.eval(variables) {
                    Ok(x) => x,
                    Err(x) => return Err(x)
                };

                match op.handle(lhs, rhs) {
                    Ok(x) => Ok(OperandEvaluationType::Logical(x)),
                    Err(x) => Err(OperandEvaluationError::IncompatibleOperation(x))
                }
            }
            Operand::UnaryOperation(op, rhs) => {
                let rhs = match rhs.eval(variables) {
                    Ok(x) => x,
                    Err(x) => return Err(x)
                };

                // Note: lhs should never be used.
                match op.handle(OperandEvaluationType::Logical(false), rhs) {
                    Ok(x) => Ok(OperandEvaluationType::Logical(x)),
                    Err(x) => Err(OperandEvaluationError::IncompatibleOperation(x))
                }
            }
        }
    }
}

impl ToString for Operand {
    fn to_string(&self) -> String {
        match self {
            Operand::Variable(x) => x.clone(),
            Operand::Arithmetic(x) => x.to_string(),
            Operand::BiOperation(lhs, op, rhs) =>
                String::from(format!("({} {} {})", lhs.to_string(), op.sym(), rhs.to_string())),
            Operand::UnaryOperation(op, rhs) =>
                String::from(format!("{}{}", op.sym(), rhs.to_string())),
        }
    }
}

pub fn parse_variable(result: &VariableType) -> OperandEvaluationType {
    match result {
        VariableType::Logical(x) => OperandEvaluationType::Logical(*x),
        VariableType::Arithmetic(x) => OperandEvaluationType::Arithmetic(*x),
        VariableType::Text(x) => {
            match *x {
                "true" => OperandEvaluationType::Logical(true),
                "false" => OperandEvaluationType::Logical(false),
                num => OperandEvaluationType::Arithmetic(
                    num.parse::<ArithmeticResult>().expect(
                        format!("Unknown boolean/numeric value '{}'", num).as_str()))
            }
        }
    }
}

fn handle_variable(result: VariableTranslationResult) -> EvaluationResult {
    match result {
        Ok(x) => Ok(parse_variable(&x)),
        Err(e) => Err(OperandEvaluationError::VariableNotFound(e))
    }
}
