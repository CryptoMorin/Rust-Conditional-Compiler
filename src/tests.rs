#[cfg(test)]
#[allow(unused_must_use)]
#[allow(unused_imports)]
mod test {
    use crate::{Operand, parser::LogicalCompiler};
    use crate::Operand::*;
    use crate::operators::{AND, GE, GT, LE, LT, NOT, OR};

    macro_rules! var {
        ($name:ident) => { crate::Operand::Variable(String::from(stringify![$name])) }
    }

    macro_rules! box_operand {
        ($operand:ident) => {
            Box::new(var!($operand))
        };
        ($operand:literal) => {
            Box::new(crate::Operand::Arithmetic(crate::MathSentence($operand as f64)))
        }
    }

    macro_rules! bi {
        ($first:tt $op:tt $second:tt) => {
            BiOperation(box_operand![$first], &crate::operators::get_operator(stringify![$op]).unwrap(), box_operand![$second])
        }
    }

    /// -> Result<Operand, LogicalError<'static>>
    macro_rules! def_test {
        ($name:ident $(#[$attrs:meta])* $input:literal $assertion:block) => {
            #[test]
            $(#[$attrs])*
            fn $name() {
                let result = match LogicalCompiler::compile(&String::from($input)).parse() {
                    Ok(e) => e,
                    Err(e) => panic!("{}", e)
                };
                let exact = $assertion;
                assert_eq![result, exact, "Failed to match \n{}\n{}\n{}",
                  result.to_string(), show_diff_index(result.to_string(), exact.to_string()), exact.to_string()];
            }
        }
    }

    macro_rules! def_should_panic {
        ($(#[$attrs:meta])* $name:ident $input:literal) => {
            #[test]
            #[should_panic]
            $(#[$attrs])*
            fn $name() {
                match LogicalCompiler::compile(&String::from($input)).parse() {
                    Ok(x) => eprintln!("Expected {} to fail. Succeeded with: {}", stringify!($name), x.to_string()),
                    Err(e) => panic!("{}", e)
                };
            }
        }
    }

    fn bi(lhs: crate::Operand, op: &'static crate::ConditionalOperator, rhs: crate::Operand) -> crate::Operand {
        BiOperation(Box::new(lhs), &op, Box::new(rhs))
    }

    fn unary(op: &'static crate::ConditionalOperator, rhs: crate::Operand) -> crate::Operand {
        UnaryOperation(&op, Box::new(rhs))
    }

    def_test! { simple_and "first && second" { bi![first && second] } }
    def_test! { precedence "first || second && third" { bi(var!(first), &OR, bi![second && third]) } }
    def_test! { long_precedence "first && second || third && fourth && sixth || seventh" {
        let first_and = bi![first && second];
        let thr_four = bi![third && fourth];
        let thr_four_six = bi(thr_four, &AND, var!(sixth));
        let first_or = bi(first_and, &OR, thr_four_six);

        bi(first_or, &OR, var!(seventh))
        }
    }

    def_test! { simple_negate "!test" { unary(&NOT, var!(test)) } }
    def_test! { multi_logic "!first || !second && !third" {
        let second_half = bi(unary(&NOT, var!(second)), &AND, unary(&NOT, var!(third)));
        bi(unary(&NOT, var!(first)), &OR, second_half)
    } }

    def_test! { simple_arithmetic_comp "5 <= x" { bi![5 <= x] } }
    def_test! { simple_short_math_arithmetic "0 <= x < 10" { bi(bi![0 <= x], &AND, bi![x < 10]) } }

    def_test! { simple_sub_expr "(first || second) && third" { bi(bi![first || second], &AND, var!(third)) } }
    def_test! { multi_sub_expr "(5 <= x < 10) && ((first || firstPrime) && (second && third)) && fourth" {
        let interval = bi(bi![5 <= x], &AND, bi![x < 10]);
        let first_fp = bi![first || firstPrime];
        let sec_th = bi![second && third];
        let middle = bi(first_fp, &AND, sec_th);
        bi(bi(interval, &AND, middle), &AND, var!(fourth))
    } }

    // Tests if spaces are ignored correctly.
    // Tests unary operators for whole sub expressions.
    // Tests if bi operators allow unary operators as their rhs.
    def_test! { final_boss
        "!   test    ||   testB   &&   (   5  <=  x    <   10   ) &&    \
        (   (   !first || !firstPrime    ) && !(   second && !(third   )  )   ) &&   fourth   " {
            // (!testB && interval)
            let testb_interval = {
                let interval = bi(bi![5 <= x], &AND, bi![x < 10]); // (5 <= x < 10)
                bi(var!(testB), &AND, interval)
            };

            let f_fp_sec_th = {
                let first_fp = bi(unary(&NOT, var!(first)), &OR, unary(&NOT, var!(firstPrime))); // (!first || !firstPrime)
                let sec_th = unary(&NOT, bi(var!(second), &AND, unary(&NOT, var!(third)))); // !(second && !third)
                bi(first_fp, &AND, sec_th)
            };

            let middle = bi(testb_interval, &AND, f_fp_sec_th);
            let rhs = bi(middle, &AND, var!(fourth));

            bi(unary(&NOT, var!(test)), &OR, rhs)
    }}


    // ------------------------------------- Should Panic ------------------------------------- //

    def_should_panic! { missing_lhs " && second" }
    def_should_panic! { missing_rhs "test || " }
    def_should_panic! { invalid_op "first %=> second" }
    def_should_panic! { adjacent_ops "test || && yes" }

    fn show_diff_index(first: String, second: String) -> String {
        let index = diff_index(first.to_string(), second.to_string());
        let mut spaces = " ".repeat(index);
        spaces.push('^');
        spaces
    }

    fn diff_index(first: String, second: String) -> usize {
        let mut first_chars = first.chars();
        let mut second_chars = second.chars();
        let longest = std::cmp::max(first.len(), second.len());

        (0..longest)
            .map(|_| (first_chars.next(), second_chars.next()))
            .enumerate()
            .find_map(|(index, (first, second))| {
                if let (Some(x), Some(y)) = (first, second) {
                    if x != y { Some(index) } else { None }
                } else { Some(index) }
            }).unwrap()
    }
}