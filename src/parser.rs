use std::collections::linked_list::LinkedList;
use std::fmt;

use crate::{ArithmeticResult, MathSentence, Operand, with};
use crate::operators::{self, ConditionalOperator};

#[derive(Debug)]
pub struct LogicalError<'a> {
    expression: &'a str,
    index: usize,
    message: String,
    target: &'a str,
}

type CompilerResult<'a, T> = std::result::Result<T, LogicalError<'a>>;

impl fmt::Display for LogicalError<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "{} at offset {} in expression \n\"{}\"\n{}{}",
               self.message, self.index, self.expression,
               " ".repeat(self.index + 1), "^".repeat(self.target.len()))
    }
}

type TokenTracker = Option<usize>;

pub struct LogicalCompiler<'a> {
    expression: &'a String,
    end: usize,
    offset: usize,
    last_operand_index: TokenTracker,
    last_op_index: TokenTracker,
    last_last_op_index: TokenTracker,
    operands: LinkedList<Operand>,
    operators: LinkedList<&'static ConditionalOperator>,
}

impl<'a> LogicalCompiler<'a> {
    pub fn compile(expression: &String) -> LogicalCompiler {
        LogicalCompiler {
            expression,
            offset: 0,
            end: expression.len(),
            last_operand_index: None,
            last_op_index: None,
            last_last_op_index: None,
            operators: LinkedList::new(),
            operands: LinkedList::new(),
        }
    }

    #[inline(always)]
    fn err<T>(&self, index: usize, message: String, target: &'a str) -> CompilerResult<'a, T> {
        Err(LogicalError {
            expression: self.expression,
            index,
            message,
            target,
        })
    }

    #[inline]
    fn get_operator(&self, last_logical: usize) -> CompilerResult<'a, &'static ConditionalOperator> {
        let name = &self.expression[last_logical..self.offset];
        let op = operators::get_operator(name);

        match op {
            None => {
                let hint: &str = match name {
                    _ if name.starts_with("!!") => " (hint: Redundant multiple negation operators are not allowed)",
                    _ if name.starts_with("=>") => " (hint: Did you mean '>=' operator?)",
                    _ if name.starts_with("=<") => " (hint: Did you mean '<=' operator?)",
                    _ => ""
                };

                self.err(
                    last_logical,
                    format!("Unrecognized operator '{}'{}", name, hint),
                    name,
                )
            }
            Some(x) => Ok(x)
        }
    }

    #[inline]
    #[must_use]
    fn get_sub_expr(&self, start: usize) -> Option<usize> {
        let mut params: i16 = 1;
        let mut index: usize = start;
        let mut chars: std::str::Chars = self.expression[start..].chars();

        while index < self.end {
            let chr = chars.next().unwrap();
            if chr == '(' { params += 1 } else if chr == ')' {
                params -= 1;
                if params == 0 { return Some(index); };
            }
            index += 1;
        }

        None
    }
}

#[allow(dead_code)]
fn debug_list<T>(intro: &str, list: &LinkedList<T>, mapper: fn(&T) -> String) {
    // if true { return }

    // Formatter type is discrete right now, let's stick to strings.
    let mut formatter = String::with_capacity(100);

    list.iter().enumerate().for_each(|(index, item)| {
        formatter.push_str(mapper(item).as_str());
        if index + 1 != list.len() { formatter.push_str(", ") }
    });

    println!("{} ({}) [{}]", intro, list.len(), formatter);
}

// Until Try trait is stabilized.
macro_rules! validate_operation {
    ($var:ident) => {
        if let Err(e) = $var { return Err(e) };
        let $var = $var.unwrap();
    }
}

// Handlers
impl<'a> LogicalCompiler<'a> {
    fn handle_operand(&mut self, finalize: bool) -> CompilerResult<'a, ()> {
        if self.last_operand_index.is_none() { return Ok(()); }

        let full = &self.expression[self.last_operand_index.unwrap()..self.offset];
        let (ignore_from_index, is_variable) = operand_properties(full);
        let full: &str = if ignore_from_index != 0 { &full[..ignore_from_index] } else { full };

        let operand = if is_variable {
            match full {
                "true" | "false" | "null" | "nil" => {
                    return self.err(self.last_operand_index.unwrap(),
                                    format!("Cannot use reserved logical boolean value '{}'", full),
                                    full);
                }
                _ => Operand::Variable(String::from(full))
            }
        } else {
            // Temporary
            Operand::Arithmetic(MathSentence(full.parse::<ArithmeticResult>().expect(format!("Invalid constant number '{}'", full).as_str())))
        };

        self.finalize_operand(operand, finalize)
    }

    fn finalize_operand(&mut self, operand: Operand, finalize: bool) -> CompilerResult<'a, ()> {
        let mut final_operand = operand;

        if let Some(last_op) = self.operators.last() {
            if last_op.is_unary() {
                final_operand = Operand::UnaryOperation(last_op, Box::new(final_operand));
                self.operators.pop_last();
            }
        }

        self.operands.add_last(final_operand);
        if finalize || (self.operators.len() == 2 && self.operands.len() == 3) {
            self.handle_operations(finalize)
        } else {
            Ok(())
        }
    }

    #[allow(dead_code)]
    fn debug(&self, desc: &str) {
        println!("---------------- {} ---------------", desc);
        println!("Index: {} - {} | last op? {} | last operand? {}", self.offset, self.end,
                 self.last_op_index.map_or(0, |item| item),
                 self.last_operand_index.map_or(0, |item| item)
        );
        debug_list("Operators:", &self.operators, |item| item.sym().to_string());
        debug_list("Operands:", &self.operands, |item| item.to_string());
        println!("====================================");
    }

    fn handle_operations(&mut self, finalize: bool) -> CompilerResult<'a, ()> {
        if self.operands.len() < 2 { return Ok(()); }

        if self.operators.len() == 2 {
            with! { self.operands;
                let first_operand = pop_first() unwrap();
                let sec_operand = pop_first() unwrap();
                let thr_operand = pop_first()
            }

            with! { self.operators;
                let first_op = first() unwrap();
                let sec_op = last() unwrap()
            }

            if thr_operand.is_none() {
                return self.err(self.last_operand_index.unwrap(),
                                String::from("Right hand side empty"),
                                sec_op.sym());
            }
            let thr_operand = thr_operand.unwrap();

            // 0 < x < 10
            if first_op.is_comparator() && sec_op.is_comparator() {
                let first_operation = self.create_operation(first_operand, first_op, sec_operand.soft_clone());
                validate_operation!(first_operation);

                let second_operation = self.create_operation(sec_operand, sec_op, thr_operand);
                validate_operation!(second_operation);

                let intermediate_operation = self.create_operation(first_operation, &operators::AND, second_operation);
                validate_operation!(intermediate_operation);

                self.operands.add_last(intermediate_operation);
                self.operators.clear();
                return Ok(());
            }

            if first_op.has_precedence_over(sec_op) {
                let operation: CompilerResult<Operand> = self.create_operation(first_operand, first_op, sec_operand);
                validate_operation!(operation);

                with! { self.operands
                    => add_last(operation)
                    => add_last(thr_operand)
                }
                self.operators.pop_first();
            } else {
                let operation: CompilerResult<Operand> = self.create_operation(sec_operand, sec_op, thr_operand);
                validate_operation!(operation);

                with! { self.operands
                    => add_first(first_operand)
                    => add_last(operation)
                }
                self.operators.pop_last();
            }

            // We want to handle the final operation since this is the end of expression.
            if !finalize { return Ok(()); }
        }

        // Everything else is at the end of expression, so we only have one operator to handle.
        let first = self.operands.pop_first().unwrap();
        let op = self.operators.pop_last().unwrap();
        let sec = self.operands.pop_first().unwrap();
        let operation = self.create_operation(first, op, sec);
        validate_operation!(operation);
        self.operands.add_last(operation);
        Ok(())
    }

    fn create_operation(&self, lhs: Operand, op: &'static ConditionalOperator, rhs: Operand) -> CompilerResult<'a, Operand> {
        if !op.accepts_operand_of_type(&lhs) {
            self.err(self.last_last_op_index.unwrap(),
                     format!("Left hand side of '{}' operator must be {} expression", op.sym(), op.main_accepted_operand_name()),
                     op.sym())
        } else if !op.accepts_operand_of_type(&rhs) {
            self.err(self.last_last_op_index.unwrap(),
                     format!("Right hand side of '{}' operator must be {} expression", op.sym(), op.main_accepted_operand_name()),
                     op.sym())
        } else {
            Ok(Operand::BiOperation(Box::new(lhs), op, Box::new(rhs)))
        }
    }

    fn handle_op(&mut self) -> CompilerResult<'a, ()> {
        if self.last_op_index.is_none() { return Ok(()); }
        let current_op = match self.get_operator(self.last_op_index.unwrap()) {
            Ok(x) => x,
            Err(x) => return Err(x)
        };

        if self.operands.is_empty() && !current_op.is_unary() { // (|| test)
            return self.err(self.last_last_op_index.unwrap(),
                            format!("Blank operand on left hand side of '{}' operator", current_op.sym()),
                            current_op.sym());
        }

        if let Some(last_op) = self.operators.first() { // Blank operand (right && && left) or (right && )

            // The index is not current, but whatever
            if !current_op.is_unary() && self.operands.len() < 2 {
                return self.err(self.last_last_op_index.unwrap() - last_op.symbol_size(),
                                format!("Blank operand on right side of '{}' binary operator.", last_op.sym()),
                                last_op.sym());
            }
            if last_op.is_unary() {
                return self.err(self.last_operand_index.unwrap(),
                                format!("Unary operator '{}' was followed by another operator '{}'", last_op.sym(), current_op.sym()),
                                current_op.sym());
            }
        }

        self.operators.add_last(current_op);
        Ok(())
    }

    fn check_blanks(&self) -> CompilerResult<'a, ()> {
        if self.operands.is_empty() { return self.err(0, String::from("Blank expression"), ""); }
        if let Some(op) = self.operators.first() {
            return self.err(self.last_last_op_index.unwrap(),
                            format!("Blank operand on right hand side of '{}' binary operator", op.sym()),
                            op.sym());
        }

        Ok(())
    }
}

// Parser
impl<'a> LogicalCompiler<'a> {
    pub fn parse(&mut self) -> CompilerResult<'a, Operand> {
        let chars: Vec<char> = self.expression.chars().collect();

        loop {
            let ch = chars[self.offset];

            if is_logical_char(ch) {
                if self.last_operand_index.is_some() {
                    self.handle_operand(false)?;
                    self.last_operand_index = None;
                }

                if ch != ' ' && self.last_op_index.is_none() {
                    self.last_op_index = Some(self.offset);
                    self.last_last_op_index = self.last_op_index;
                }
            } else {
                self.handle_op()?;

                if ch != ' ' && self.last_operand_index.is_none() {
                    if ch == '(' {
                        let sub_expr_start = self.offset + 1;
                        let sub_expr_end = self.get_sub_expr(sub_expr_start);
                        if sub_expr_end.is_none() {
                            return self.err(self.offset, String::from("Unclosed parentheses"), "(");
                        }
                        let sub_expr_end = sub_expr_end.unwrap();

                        self.last_operand_index = Some(self.offset);
                        self.offset += 1;

                        let operand = LogicalCompiler::<'a> {
                            expression: self.expression,
                            end: sub_expr_end,
                            offset: sub_expr_start,
                            last_operand_index: None,
                            last_op_index: None,
                            last_last_op_index: None,
                            operands: Default::default(),
                            operators: Default::default(),
                        }.parse()?;
                        self.finalize_operand(operand, false)?;

                        self.offset = sub_expr_end + 1;
                        self.last_operand_index = None;
                    } else {
                        self.last_operand_index = Some(self.offset);
                    }
                }

                self.last_op_index = None;
            }

            self.offset += 1;
            if self.offset >= self.end { break; }
        }


        if self.last_operand_index.is_some() { self.handle_operand(true) } else { self.handle_operations(true) }?;
        self.handle_op()?;
        self.check_blanks()?;

        Ok(self.operands.pop_last().unwrap())
    }
}

/// This bad boy is only called in one location
/// It's always called too, so the best candidate for inline.
#[inline(always)]
fn operand_properties(expr: &str) -> (usize, bool) {
    let mut variable = true;
    let mut started_spaces: usize = 0;

    for (i, ch) in expr.chars().enumerate() {
        if (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || ch == '_' {
            if started_spaces != 0 { variable = false; }
        } else if ch == ' ' {
            if started_spaces == 0 { started_spaces = i }
        } else {
            variable = false;
            started_spaces = 0;
        }
    }

    return (started_spaces, variable);
}

#[inline(always)]
const fn is_logical_char(chr: char) -> bool {
    match chr {
        '<' | '>' | '!' | '=' | '&' | '|' => true,
        _ => false
    }
}

/// Because I can't fucking understand the front and back naming convention.
/// Figuring out which element is the first and which is the last is harder than learning about lifetimes.
/// Cant be more ambiguous than that
trait SimplifiedLinkedList<T> {
    fn pop_first(&mut self) -> Option<T>;
    fn pop_last(&mut self) -> Option<T>;

    fn first(&self) -> Option<&T>;
    fn last(&self) -> Option<&T>;

    fn add_first(&mut self, item: T);
    fn add_last(&mut self, item: T);
}

// I need this code to remind myself
// let test = LinkedList::from([1, 2, 3, 4]);
// println!("front: {} | back: {}", test.front().unwrap(), test.back().unwrap());
// output: front: 1 | back: 4
impl<T> SimplifiedLinkedList<T> for LinkedList<T> {
    fn pop_first(&mut self) -> Option<T> { self.pop_front() }
    fn pop_last(&mut self) -> Option<T> { self.pop_back() }
    fn first(&self) -> Option<&T> { self.front() }
    fn last(&self) -> Option<&T> { self.back() }
    fn add_first(&mut self, item: T) { self.push_front(item) }
    fn add_last(&mut self, item: T) { self.push_back(item) }
}