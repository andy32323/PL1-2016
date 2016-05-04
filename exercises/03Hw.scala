// HOMEWORK ASSIGNMENT
// ===================
//
// SUBMISSION INSTRUCTIONS
// - Submit your solution to this exercise until Tuesday, 10.5., 23:59h
//   via email to paolo.giarrusso@uni-tuebingen.de.
// - Use the same email for questions.
//
// - Work in groups of 1 or 2 students.
// - Send the email CC to the other student in your team!
// - Put "pl1-hw03" in subject, please

// TASKS
// =====
// 1. Add a case class LazyFun and extend the interpreter so that
//    LazyFun creates function with call-by-need evaluation order.
//
// About task 1: This is similar to how Scala has functions with
// and without the => annotation for call-by-name.
//
// 2. Implement in your object language a function if that wraps
//    conditionals and uses LazyFun.

// Here is some code from lecture notes for environment-based interpretation.

sealed abstract class Exp
case class Num(n: Int) extends Exp
case class Id(name: Symbol) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
implicit def num2exp(n: Int) = Num(n)
implicit def id2exp(s: Symbol) = Id(s)

case class Fun(param: Symbol, body: Exp) extends Exp
case class App (funExpr: Exp, argExpr: Exp) extends Exp

def wth(x: Symbol, xdef: Exp, body: Exp) : Exp = App(Fun(x,body),xdef)

val test = App( Fun('x,Add('x,5)), 7)
val test2 = wth('x, 5, App(Fun('f, App('f,3)), Fun('y,Add('x,'y))))

sealed abstract class Value
type Env = Map[Symbol, Value]
case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value

def evalWithEnv(e: Exp, env: Env) : Value = e match {
  case Num(n: Int) => NumV(n)
  case Id(x) => env(x)
  case Add(l,r) => {
    (evalWithEnv(l,env), evalWithEnv(r,env)) match {
      case (NumV(v1),NumV(v2)) => NumV(v1+v2)
      case _ => sys.error("can only add numbers")
    }
  }
  case f@Fun(param,body) => ClosureV(f, env)
  case App(f,a) => evalWithEnv(f,env) match {
    // Use environment stored in closure to realize proper lexical scoping!
    case ClosureV(f,closureEnv) => evalWithEnv(f.body, closureEnv + (f.param -> evalWithEnv(a,env)))
    case _ => sys.error("can only apply functions")
  }
}

assert( evalWithEnv(test, Map.empty) == NumV(12))
assert( evalWithEnv(test2,Map.empty) == NumV(8))

