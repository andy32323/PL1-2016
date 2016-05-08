// HOMEWORK ASSIGNMENT
// ===================
//
// SUBMISSION INSTRUCTIONS
// - Submit your solution to this exercise until Tuesday, 10.5., 23:59h
//   via email to paolo.giarrusso@uni-tuebingen.de.
// - Use the same email for questions.
//
// - Work in groups of 1 or 2 students.
// - Send the email CC to the other student in your team, so I can answer all
//   directly!
// - Put "pl1-hw03" in subject, please


object Hw03 {
  // TASKS
  // =====
  // 1. Add a case class LazyFun and extend FAE's environment-based interpreter
  //    so that LazyFun creates function with call-by-need evaluation order.
  //    I'll call this language LFAE.
  //
  //    About task 1: This is similar to how Scala has functions with
  //    and without the => annotation for call-by-name.
  //
  //    You can reuse ideas from the implementation of call-by-need in
  //    08-lcfae.scala; make sure to still handle binding correctly.
  //    Create testcases or adapt them from past examples.
  //
  //    To verify and understand your solution of task 1, here are a few smaller tasks:
  //
  // 2. Implement in your object language LFAE a function if that wraps conditionals
  //    and uses LazyFun, similarly to ifWrapper from the exercise notes. Also
  //    translate badIfWrapper.
  //
  // 3. Your translation of ifWrapper in task 2 uses LazyFun (thus call-by-need
  //    evaluation) while the original version (in Scala) uses call-by-name.
  //    Recall the relation between call-by-name and by-need.
  //
  //    Will the LFAE version of ifWrapper behave differently from the Scala
  //    version of ifWrapper? Explain why.
  //
  // 4. Test task 2 by translating testIfWrapper/testBadIfWrapper from the exercise notes.
  //    Try verifying they have the correct behavior with your interpreter.

  // Here is some code from lecture notes for environment-based interpretation,
  // together with an implementation of booleans.
  sealed abstract class Exp
  case class Num(n: Int) extends Exp
  case class Id(name: Symbol) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp
  implicit def num2exp(n: Int) = Num(n)
  implicit def id2exp(s: Symbol) = Id(s)

  case class Fun(param: Symbol, body: Exp) extends Exp
  case class App (funExpr: Exp, argExpr: Exp) extends Exp

  case class Bool(b: Boolean) extends Exp
  implicit def bool2exp(b: Boolean) = Bool(b)
  case class If(test: Exp, thenB: Exp, elseB: Exp) extends Exp
  case class Eq(lhs: Exp, rhs: Exp) extends Exp
  def wth(x: Symbol, xdef: Exp, body: Exp) : Exp = App(Fun(x,body),xdef)
  //syntactic sugar
  def not(e: Exp): Exp = If(e, false, true)
  def or(l: Exp, r: Exp) = If(l, true, r)
  def and(l: Exp, r: Exp) =
    If(l, r, false)

  val test = App( Fun('x,Add('x,5)), 7)
  val test2 = wth('x, 5, App(Fun('f, App('f,3)), Fun('y,Add('x,'y))))

  def freeVars(e: Exp): Set[Symbol] = e match {
    case Id(x)          => Set(x)
    case Add(l, r)      => freeVars(l) ++ freeVars(r)
    case Fun(x, body)   => freeVars(body) - x
    case App(f, a)      => freeVars(f) ++ freeVars(a)
    case Num(n)         => Set.empty
    case Bool(b)        => Set.empty
    case If(test, t, e) => freeVars(test) ++ freeVars(t) ++ freeVars(e)
    case Eq(l, r)       => freeVars(l) ++ freeVars(r)
  }

  sealed abstract class Value
  type Env = Map[Symbol, Value]
  case class NumV(n: Int) extends Value
  case class BoolV(b: Boolean) extends Value
  case class ClosureV(f: Fun, env: Env) extends Value

  def evalWithEnv(e: Exp, env: Env) : Value = e match {
    case Num(n) => NumV(n)
    case Bool(b) => BoolV(b)
    case Id(x) => env(x)
    case Add(l,r) => {
      (evalWithEnv(l,env), evalWithEnv(r,env)) match {
        case (NumV(v1),NumV(v2)) => NumV(v1+v2)
        case _ => sys.error("can only add numbers")
      }
    }
    case f@Fun(param,body) =>
      ClosureV(f, env filterKeys freeVars(f))
    case App(f,a) => evalWithEnv(f,env) match {
      // Use environment stored in closure to realize proper lexical scoping!
      case ClosureV(f,closureEnv) => evalWithEnv(f.body, closureEnv + (f.param -> evalWithEnv(a,env)))
      case _ => sys.error("can only apply functions")
    }
    case If(test, t, e) =>
      evalWithEnv(test, env) match {
        case BoolV(b) =>
          if (b)
            evalWithEnv(t, env)
          else
            evalWithEnv(e, env)
        case _ =>
          sys.error("can only branch on booleans")
      }
    case Eq(l, r) =>
      (evalWithEnv(l, env), evalWithEnv(r, env)) match {
        case (NumV(v1), NumV(v2)) => BoolV(v1 == v2)
        case (BoolV(x), BoolV(y)) => BoolV(x == y)
        case _                    => sys.error("can only compare numbers or booleans")
      }
  }

  assert(evalWithEnv(test, Map.empty) == NumV(12))
  assert(evalWithEnv(test2, Map.empty) == NumV(8))
}

// Import the content of the above object. Useful when loading this file into
// Scala's REPL with :load.
import Hw03._
