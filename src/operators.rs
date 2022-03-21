use std::fmt;
use std::fmt::Formatter;

use AcceptedOperand::*;

use crate::{Operand, OperandEvaluationType};

// Warning: Macro flare of void ahead. If you experience any seizures, aching, muscle twitching, vomiting, itching, dizziness, neck stiffness,
// dysuria, amaurosis fugax, nystagmus, exophthalmos, dalrymple while reading, stop reading/the code IMMEDIATELY
// and consult your personal healthcare professional.

enum AcceptedOperand { ARITHMETIC, LOGICAL, COMPARATOR }

impl ToString for AcceptedOperand {
    fn to_string(&self) -> String {
        String::from(match self {
            ARITHMETIC => "Arithmetic",
            LOGICAL => "Logical",
            COMPARATOR => panic!("Called disallowed accepted operand string operation")
        })
    }
}

impl PartialEq for AcceptedOperand {
    fn eq(&self, other: &Self) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

pub struct ConditionalOperator {
    symbol: &'static str,
    unary: bool,
    priority: u8,
    accepted_operands: [Option<AcceptedOperand>; 2],
    handler: fn(OperandEvaluationType, OperandEvaluationType) -> std::result::Result<bool, IncompatibleOperation>,
}

impl PartialEq for ConditionalOperator {
    fn eq(&self, other: &Self) -> bool {
        self.symbol == other.symbol
    }
}
impl ToString for ConditionalOperator {
    fn to_string(&self) -> String {
        String::from(self.symbol)
    }
}

impl ConditionalOperator {
    pub const fn symbol_size(&self) -> usize { self.symbol.len() }

    /// This check should be performed by the left operand since it checks for equality, thus
    /// left-to-right associativity
    pub const fn has_precedence_over(&self, other: &Self) -> bool
    { self.priority <= other.priority }

    pub fn handle<'i>(&self, lhs: OperandEvaluationType, rhs: OperandEvaluationType)
                      -> std::result::Result<bool, IncompatibleOperation> {
        (self.handler)(lhs, rhs)
    }

    pub const fn is_unary(&self) -> bool { self.unary }

    pub fn is_comparator(&self) -> bool {
        self.accepted_operands.contains(&Option::Some(COMPARATOR))
    }

    pub fn main_accepted_operand_name(&self) -> String {
        self.accepted_operands[0].as_ref().unwrap().to_string()
    }

    pub fn accepts_operand_of_type(&self, operand: &Operand) -> bool {
        match operand {
            Operand::Variable(_) => true,
            Operand::Arithmetic(_) => self.accepted_operands.contains(&Option::Some(ARITHMETIC)),
            Operand::BiOperation(_, _, _) | Operand::UnaryOperation(_, _) => self.accepted_operands.contains(&Option::Some(LOGICAL)),
        }
    }

    pub const fn sym(&self) -> &str { self.symbol }
}

macro_rules! _get_operator_standalone {
    ($sym:ident <- $( $op:ident )|+ ) =>
    {
        $( const $op: &str = self::$op.symbol; )+
        match $sym {
            $( $op => &self::$op, )+
            _ => &self::AND
        }
    }
}
macro_rules! get_operator_from_association {
    ($sym:ident <- $( $op:ident )|+ ) =>
    {
        match $sym {
            $( OperatorSymbolContainer::$op => Some(&self::$op), )+
            _ => None
        }
    }
}

/// Creates a constant HashMap-ish structure.
macro_rules! construct_operator_slots {
    ($container_name:ident $($name:ident)|+) => {
        pub struct $container_name {}
        impl $container_name {
            $( #[allow(unused)] pub const $name: &'static str = $name.symbol; )+
        }
    }
}

macro_rules! operator_delegation_proxy {
    // Must use curly braces for internal macro to avoid trailing semi-colon errors.
    ($mac:ident $($initials:tt)*) => {$mac!{ $($initials)* AND | OR | NOT | GT | GE | LT | LE | EQ | NQ }};
}

operator_delegation_proxy! { construct_operator_slots OperatorSymbolContainer }

pub fn print_operators() {
    macro_rules! print_operators {
        ($( $op:ident )|+) => {
            println!(
                stringify![$($op:{}  -   )+], $( OperatorSymbolContainer::$op, )+
            )
        }
    }
    operator_delegation_proxy!(print_operators);
}

/// I'm not gonna use phf
pub fn get_operator(symbol: &str) -> Option<&'static ConditionalOperator> {
    operator_delegation_proxy![ get_operator_from_association symbol <- ]
}

pub enum IncompatibleOperation {
    Lhs(&'static ConditionalOperator, OperandEvaluationType),
    Rhs(&'static ConditionalOperator, OperandEvaluationType),
}

impl fmt::Display for IncompatibleOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let op: &ConditionalOperator;
        let operand: &OperandEvaluationType;
        let side: &str;

        match self {
            IncompatibleOperation::Lhs(x, y) => {
                op = x;
                operand = y;
                side = "Left";
            }
            IncompatibleOperation::Rhs(x, y) => {
                op = x;
                operand = y;
                side = "Right";
            }
        }

        write!(f, "{} hand side of '{}' operator should be {} instead got {}", side, op.symbol,
               op.accepted_operands[0].as_ref().expect("").to_string(), operand.to_string())
    }
}

macro_rules! optional {
    () => { Option::None };
    ($entity:ident) => { Option::Some($entity) }
}
macro_rules! logical {
    () => { false };
    ($entity:ident) => { true }
}
macro_rules! def_op {
    ($scope:vis $id:ident $priority:literal $(is $unary:ident)? [$mainAcceptedOperand:ident $(| $secondOperand:ident)?]
    -> lhs $operator:tt rhs)
    => { def_op![$scope $id $operator $priority [$mainAcceptedOperand $(| $secondOperand)?] -> |lhs, rhs| {
        let casted_lhs = operand![$id lhs as $mainAcceptedOperand];
        let casted_rhs = operand![$id rhs as $mainAcceptedOperand];
        casted_lhs $operator casted_rhs
    } ]; };
    ($scope:vis $id:ident $operator:tt $priority:literal $(is $unary:ident)? [$acceptedOperands:ident $(| $secondOperand:ident)?]
    -> |$first:ident, $second:ident| $handler:block)
    => { pub const $id: ConditionalOperator = ConditionalOperator {
            symbol: stringify![$operator], unary: logical!($($unary)?), priority: $priority,
            accepted_operands: [Option::Some($acceptedOperands), optional!($($secondOperand)?)],
            handler: |$first, $second| Ok($handler)
        };
    };
}
macro_rules! map_operand_type {
    (reverse ARITHMETIC) => { map_operand_type![LOGICAL] };
    (reverse LOGICAL)    => { map_operand_type![ARITHMETIC] };

    (ARITHMETIC $($param:ident)?) => { OperandEvaluationType::Arithmetic(.., $($param)?) };
    (LOGICAL    $($param:ident)?) => { OperandEvaluationType::Logical   (.., $($param)?) };
}
macro_rules! detect_side {
    (lhs) => { IncompatibleOperation::Lhs };
    (rhs) => { IncompatibleOperation::Rhs };
}
macro_rules! operand {
    ($op:ident $var:ident as $type:ident) => {
        match $var {
            map_operand_type![$type x] => x,
            map_operand_type![reverse $type] => return Err(detect_side![$var] (&$op, $var))
        }
    }
}
#[macro_export] macro_rules! with {
    ($var:expr $(; let $inner_var:ident = $($meth:ident $($invoc:tt)?)*)+) => {
        $(
            let $inner_var = $var $(.$meth$($invoc)?)*;
        )+
    };
    ($var:expr $(=> $($meth:ident $($invoc:tt)?)*)+) => {
        $(
            $var $(.$meth$($invoc)?)*;
        )+
    }
}

/// updating!(let toast =>
///     a += 5;
///     b *= 40
/// );
macro_rules! _updating {
    (let $name:ident => $($field:ident $op:tt $val:expr);+) => {
        // Makes immutable as a shorthand.
        updating![$name => $($field $op $val);+];
        let $name = $name;
    };
    ($container:ident => $($field:ident $op:tt $val:expr);+) => {
        $( $container.$field $op $val; )+
    }
}

def_op! { NOT ! 1 is unary [LOGICAL] -> |_lhs, rhs| { !operand![NOT rhs as LOGICAL] } }

def_op! { GT 2 [ARITHMETIC | COMPARATOR] -> lhs > rhs  }
def_op! { GE 2 [ARITHMETIC | COMPARATOR] -> lhs >= rhs  }
def_op! { LT 2 [ARITHMETIC | COMPARATOR] -> lhs < rhs  }
def_op! { LE 2 [ARITHMETIC | COMPARATOR] -> lhs <= rhs  }

def_op! { AND 3 [LOGICAL] -> lhs && rhs }

def_op! { EQ 4 [ARITHMETIC | LOGICAL] -> lhs == rhs }
def_op! { NQ 4 [ARITHMETIC | LOGICAL] -> lhs != rhs }

def_op! { OR 5 [LOGICAL] -> lhs || rhs }