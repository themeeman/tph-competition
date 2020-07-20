///
/// TPH Code Bowling Fibonacci Number Printer Submission
/// by themeeman  
/// 
/// This is a program that computes the first 100 fibonacci numbers by means of algbraic manipulation of the golden ratio
/// constant, called phi.
/// 
/// If we call a given fibonacci number F(n), where F(0) = 0 and F(1) = 1, phi is the value of the ratio F(n+1) / F(n), as 
/// n apporaches infinity. It also has some interesting properties as a number.
/// 
/// The identity used in this program is phi = 1 + 1/phi. Multiplying both sides by phi gives us phi^2 = phi + 1, which can be 
/// solved to give the value of phi, (1 + sqrt(5)) / 2.
/// 
/// The actual process used in this program goes like this:
/// 
/// Start with phi^2 = phi + 1
/// Multiply both sides by phi, to give phi^3 = phi(1 + phi) = phi^2 + phi 
/// Use the identity from before to substitute in for phi^2, giving (1 + phi) + phi
/// Simplify the sum to give phi^3 = 2*phi + 1
/// Multiply both sides by phi again and repeat to infinity.
/// 
/// What does this actually achieve though? Well, when we write down our first results, the pattern becomes obvious:
/// 
/// phi^2 = phi + 1
/// phi^3 = 2*phi + 1
/// phi^4 = 3*phi + 2
/// phi^5 = 5*phi + 3
/// phi^6 = 8*phi + 5
/// phi^7 = 13*phi + 8
/// etc
/// 
/// As you can see, the fibonacci numbers are being generated in the coefficents of each expression. In general,
/// 
/// phi^n = F(n)phi + F(n-1)
/// 
/// for all n > 0.
/// 
/// This is the fact that I exploit in this program.
/// 

use std::fmt;

/// 
/// The expression tree that is used. It does not support full mathematical expressions, as - and / are omitted since
/// we are only working with natural numbers. Only integer exponents are supported, not arbitrary expressions.
///
#[derive(Debug, Clone, PartialEq, Eq)]
enum Expr {
    Add(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Pow(Box<Expr>, i128),
    Const(i128),
    Phi
}

///
/// The code for recursively printing out expressions. It is not an integral part of the solution, but demonstrates the steps 
/// used.
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expr::*;
        match self {
            Add(lhs, rhs) => write!(f, "{} + {}", **lhs, **rhs),
            Mul(lhs, rhs) => match (&**lhs, &**rhs) {
                e @ (Add(_, _), Add(_, _)) => write!(f, "({}) * ({})", e.0, e.1),
                e @ (Add(_, _), _) => write!(f, "({}) * {}", e.0, e.1),
                e @ (_, Add(_, _)) => write!(f, "{} * ({})", e.0, e.1),
                e => write!(f, "{} * {}", e.0, e.1),
            }
            Pow(base, ex) => match &**base {
                e @ Const(_) | e @ Phi => write!(f, "{}^{}", e, *ex),
                e => write!(f, "({})^{}", e, *ex),
            }
            Const(x) => write!(f, "{}", *x),
            Phi => write!(f, "φ"),
        }
    }
}

///
/// Represents a pair of expressions that are in the form x = y, forming an equation.
/// 
#[derive(Debug, Clone, PartialEq, Eq)]
struct Equation(Expr, Expr);

impl fmt::Display for Equation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = {}", self.0, self.1)
    }
}

///
/// Multiplies two expressions together and performs some simplification, including collecting turning Phi into exponents.
/// 
/// Some branches are left unimplemented because I know for sure that they won't come up, and I'm too lazy to write them.
/// 
fn multiply(lhs: &Expr, rhs: &Expr) -> Expr {
    use Expr::*;
    match (lhs, rhs) {
        (x, Const(1)) | (Const(1), x) => x.clone(),
        (Phi, Phi) => Pow(Box::new(Phi), 2),
        (Phi, Pow(base, ex)) | (Pow(base, ex), Phi) => match **base {
            Phi => Pow(base.clone(), ex + 1),
            _ => Mul(Box::new(lhs.clone()), Box::new(rhs.clone())),
        },
        (Phi, Const(x)) | (Const(x), Phi) => Mul(Box::new(Const(*x)), Box::new(Phi)),
        (Const(x), Const(y)) => Const(x * y),
        (Phi, Add(rlhs, rrhs)) | (Add(rlhs, rrhs), Phi) 
            => Add(Box::new(multiply(rlhs, &Phi)), Box::new(multiply(rrhs, &Phi))),
        (Const(x), Add(rlhs, rrhs)) | (Add(rlhs, rrhs), Const(x)) 
            => Add(Box::new(multiply(rlhs, &Const(*x))), Box::new(multiply(rrhs, &Const(*x)))),
        (Phi, Mul(rlhs, rrhs)) | (Mul(rlhs, rrhs), Phi) 
            => match (&**rlhs, &**rrhs) {
                (Const(x), Phi) | (Phi, Const(x)) => Mul(Box::new(Const(*x)), Box::new(Pow(Box::new(Phi), 2))),
                _ => unimplemented!(),
            }
        _ => unimplemented!(),
    }
}

///
/// Applies the multiply function to both sides of an equation.
/// 
fn multiply_equation(eq: &Equation, ex: &Expr) -> Equation {
    Equation(multiply(&eq.0, ex), multiply(&eq.1, ex))
}

/// 
/// Recursive algorithm that substitues replace all instances of the LHS of a given equation with the RHS, inside an expression.
/// This is a general algorithm, even though the only use of it in the program is to replace phi^2 with phi+1
/// 
/// It is recursive and horribly inefficent, and could be rewritten iteratively to be much faster, but it runs fast enough.
/// 
fn substitute(ex: &Expr, axiom: &Equation) -> Expr {
    use Expr::*;
    if ex.clone() == axiom.0 {
        return axiom.1.clone();
    }
    match ex {
        Add(lhs, rhs) => Add(Box::new(substitute(&**lhs, axiom)), Box::new(substitute(&**rhs, axiom))),
        Mul(lhs, rhs) => Mul(Box::new(substitute(&**lhs, axiom)), Box::new(substitute(&**rhs, axiom))),
        Pow(base, ex) => Pow(Box::new(substitute(&**base, axiom)), *ex),
        Const(x) => Const(*x),
        Phi => Phi,
    }
}

/// 
/// A simplification function that collects terms into single terms, removing duplicates.
/// 
/// I say that, except it's a lie. All this does is collect constant terms and phi terms not raised to any power, since those
/// are the only ones it is called on, which makes the algorithm easier to write. Sue me.
/// 
/// Written iteratively because I love myself.
/// 
fn simplify_sum(ex: &Expr) -> Expr {
    use Expr::*;
    let mut todo = vec![ex.clone()];
    let mut coeffs = (0i128, 0i128);
    while !todo.is_empty() {
        let current = todo.pop().unwrap();
        match current {
            Add(lhs, rhs) => { 
                todo.push(*lhs.clone());
                todo.push(*rhs.clone());
            },
            Mul(lhs, rhs) => match (&*lhs, &*rhs) {
                (Const(x), Phi) | (Phi, Const(x)) => coeffs.0 += x,
                (Const(x), Add(xlhs, xrhs)) | (Add(xlhs, xrhs), Const(x)) => todo.push(multiply(&Add(xlhs.clone(), xrhs.clone()), &Const(*x))),
                _ => unimplemented!(),
            },
            Const(x) => coeffs.1 += x,
            Phi => coeffs.0 += 1,
            _ => unimplemented!(),
        };
    }
    Add(Box::new(multiply(&Const(coeffs.0), &Phi)), Box::new(Const(coeffs.1)))
}

fn main() {
    use Expr::*;
    const PRINT_STEPS: bool = false; // Change to true to print out each step, proving the program is actually doing something.
    // Trivial case F(0) = 0. Uses the identity phi = phi. Pretty trivial.
    if PRINT_STEPS {
        println!("φ = φ");
    }
    println!("{}", 0);
    // A representation of our basic identity, phi^2 = phi + 1
    let axiom = Equation(Pow(Box::new(Phi), 2), Add(Box::new(Phi), Box::new(Const(1))));
    // Trivial case F(1) = 1.
    if PRINT_STEPS {
        println!("{}", axiom);
    }
    println!("{}", 1);
    let mut eq = axiom.clone();
    // Prints F(2) up to F(99)
    for _ in 2..=99 {
        // Multiply both sides of the equation by phi
        eq = multiply_equation(&eq, &Phi);
        if PRINT_STEPS {
            println!("{}", eq);
        }
        // Substitute phi^2 for phi + 1
        eq.1 = substitute(&eq.1, &axiom);
        if PRINT_STEPS {
            println!("{}", eq);
        }
        // Simplify the result
        eq.1 = simplify_sum(&eq.1);
        if PRINT_STEPS {
            println!("{}", eq);
        }
        let val = match &eq.1 {
            Add(_, rhs) => match **rhs {
                Const(x) => x,
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };
        println!("{}", val);
    }
}
