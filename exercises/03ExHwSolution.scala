object Hw03Simple {
  sealed abstract class Exp
  case class Num(n: Int) extends Exp
  case class Id(name: Symbol) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp
  implicit def num2exp(n: Int) = Num(n)
  implicit def id2exp(s: Symbol) = Id(s)

  case class Fun(param: Symbol, body: Exp) extends Exp
  case class LazyFun(param: Symbol, body: Exp) extends Exp
  case class App(funExpr: Exp, argExpr: Exp) extends Exp

  case class Bool(b: Boolean) extends Exp
  implicit def bool2exp(b: Boolean) = Bool(b)
  case class If(test: Exp, thenB: Exp, elseB: Exp) extends Exp
  case class Eq(lhs: Exp, rhs: Exp) extends Exp
  def wth(x: Symbol, xdef: Exp, body: Exp): Exp = App(Fun(x, body), xdef)
  //syntactic sugar
  def not(e: Exp): Exp = If(e, false, true)
  def or(l: Exp, r: Exp) = If(l, true, r)
  def and(l: Exp, r: Exp) =
    If(l, r, false)

  def freeVars(e: Exp): Set[Symbol] = e match {
    case Id(x)            => Set(x)
    case Add(l, r)        => freeVars(l) ++ freeVars(r)
    case Fun(x, body)     => freeVars(body) - x
    case LazyFun(x, body) => freeVars(body) - x
    case App(f, a)        => freeVars(f) ++ freeVars(a)
    case Num(n)           => Set.empty
    //PG: Task1: Extend freeVars
    case Bool(b)          => Set.empty
    case If(test, t, e)   => freeVars(test) ++ freeVars(t) ++ freeVars(e)
    case Eq(l, r)         => freeVars(l) ++ freeVars(r)
  }

  //Common values
  sealed abstract class Value
  case class NumV(n: Int) extends Value
  case class BoolV(b: Boolean) extends Value
  case class ClosureV(f: Either[Fun, LazyFun], env: Env) extends Value

  //Recall what where the environments here:
  type EnvCBV = Map[Symbol, Value]
  type EnvCBN = Map[Symbol, Thunk]
  type Env = Map[Symbol, EnvValue]

  sealed abstract class EnvValue //if you prefer, you can also use
  //Either[Value, Thunk]
  case class EnvNonThunk(t: Value) extends EnvValue
  case class EnvThunk(t: Thunk) extends EnvValue

  case class Thunk(e: Exp, env: Env) {
    var cache: Value = null
  }

  def delay(e: Exp, env: Env): Thunk = Thunk(e, env)
  private def forceEval(e: Exp, env: Env): Value = {
    println("Forcing evaluation of expression: " + e)
    eval(e, env)
  }

  def force(t: Thunk): Value = {
    if (t.cache == null)
      t.cache = forceEval(t.e, t.env)
    else
      println("Reusing cached value " + t.cache + " for expression " + t.e)
    t.cache
  }

  def forceIfNeeded(v: EnvValue): Value = v match {
    case EnvThunk(t)     => force(t)
    case EnvNonThunk(nt) => nt
  }

  def eval(e: Exp, env: Env): Value = e match {
    case Num(n)  => NumV(n)
    case Bool(b) => BoolV(b)
    case Id(x)   => forceIfNeeded(env(x)) // force evaluation of thunk if identifier is evaluated
    case Add(l, r) =>
      (eval(l, env), eval(r, env)) match {
        case (NumV(v1), NumV(v2)) => NumV(v1 + v2)
        case _                    => sys.error("can only add numbers")
      }
    case f @ Fun(param, body) =>
      ClosureV(Left(f), env filterKeys freeVars(f))
    case f @ LazyFun(param, body) =>
      ClosureV(Right(f), env filterKeys freeVars(f))
    case App(f, a) => eval(f, env) match {
      // Use environment stored in closure to realize proper lexical scoping!
      case ClosureV(f, closureEnv) =>
        val (funParam, funBody, argValue) = f match {
          case Left(Fun(param, body)) =>
            (param, body, EnvNonThunk(eval(a, env)))
          case Right(LazyFun(param, body)) =>
            (param, body, EnvThunk(delay(a, env)))
        }
        eval(funBody, closureEnv + (funParam -> argValue))
      case _ => sys.error("can only apply functions")
    }
    case If(test, t, e) =>
      eval(test, env) match {
        case BoolV(b) =>
          if (b)
            eval(t, env)
          else
            eval(e, env)
        case _ =>
          sys.error("can only branch on booleans")
      }
    case Eq(l, r) =>
      (eval(l, env), eval(r, env)) match {
        case (NumV(v1), NumV(v2)) => BoolV(v1 == v2)
        case (BoolV(x), BoolV(y)) => BoolV(x == y)
        case _                    => sys.error("can only compare numbers or booleans")
      }
  }

  //
  // 2. Implement in your object language LFAE a function if that wraps conditionals
  //    and uses LazyFun, similarly to ifWrapper from the exercise notes. Also
  //    translate badIfWrapper.

  // 3. Your translation of ifWrapper in task 2 uses LazyFun (thus call-by-need
  //    evaluation) while the original version (in Scala) uses call-by-name.
  //    Recall the relation between call-by-name and by-need.
  //
  //    Will the LFAE version of ifWrapper behave differently from the Scala
  //    version of ifWrapper? Explain why.
  //
  // 4. Test task 2 by translating testIfWrapper/testBadIfWrapper from the exercise notes.
  //    Try verifying they have the correct behavior with your interpreter.

  //Solutions:
  //Relevant part of exercise notes, for reference, from https://github.com/ps-tuebingen/PL1-2016/blob/ce52479116a0877be8fae288e3730a050911a4da/exercises/02Ex.scala#L7-L49

  object InScala {
    def ifWrapper(x: Boolean, thenB: => Int, elseB: => Int) =
      if (x)
        thenB
      else
        elseB
    def badIfWrapper(x: Boolean, thenB: Int, elseB: Int) =
      if (x)
        thenB
      else
        elseB
    //scalaOmega is only semantically equivalent to omega: Scala-level evaluation
    //of scalaOmega loops, just like FAE-level evaluation of omega does.
    //Translating omega directly does not work, because omega is forbidden
    //by Scala's type system.
    def scalaOmega(): Nothing = scalaOmega()
    def testIfWrapper() = ifWrapper(true, 1, scalaOmega())
    def testBadIfWrapper() = badIfWrapper(true, 1, scalaOmega())
  }

  ////////////////////////////////
  //PG: Solution to task 2:
  ////////////////////////////////
  //we want a function in LFAE, that is, an object-level function, not Scala-level function. So:
  val ifWrapper = Fun('c, LazyFun('t, LazyFun('e, If('c, 't, 'e))))
  val badIfWrapper = Fun('c, Fun('t, Fun('e, If('c, 't, 'e))))

  ////////////////////////////////
  //PG: Solution to task 3:
  ////////////////////////////////
  /*
   * Call-by-name and call-by-need differ when
   * the same function argument is evaluated more than once in a call.
   *
   * But parameters 't and 'e are both evaluated only at most once by
   * 'ifWrapper; when that happens, the result of evaluation will be cached, but
   * this cache is never reused.
   *
   * Hence, in this case call-by-name and call-by-need have no big difference.
   */

  ////////////////////////////////
  //PG: Solution to task 4:
  ////////////////////////////////
  val omega = App(Fun('x, App('x, 'x)), Fun('x, App('x, 'x)))
  val testIfWrapper =
    wth('ifWrapper, ifWrapper,
      App(App(App('ifWrapper, true), 1), omega))
  val testBadIfWrapper =
    wth('badIfWrapper, badIfWrapper,
      App(App(App('badIfWrapper, true), 1), omega))
}

import Hw03Simple._

//Further examples of translation between object-language (FAE) and meta-language (Scala).

object GroupTask {
  //A. Translate these Scala snippets to FAE:
  //A.1
  def identity(x: Int) = x
  identity(1)

  //A.2:
  ((x: Int) => x)(1)

  //This translates A.2, not A.1:
  App(Fun('x, 'x), 1)

  //This evaluates (at the meta-level) to the same FAE-level expression:
  {
    val identity = Fun('x, 'x)
    App(identity, 1)
  }

  //This is a translation of A.1: the Scala-level binding of identity is
  //translated to a FAE-level binding of 'identity.
  wth('identity, Fun('x, 'x),
    App('identity, 1))

  //Group task: Translate this program:
  def addWrapper(a: Int, b: Int) = a + b
  addWrapper(1, 2)
  // to our object language
  //val translated: Exp =
  //result:
  val translatedProgram: Exp =
    wth('addWrapper, Fun('a, Fun('b, Add('a, 'b))),
      App(App('addWrapper, 1), 2))
  //...
  //translate this object-language code to Scala
  val testFAE = App(Fun('x, Add('x, 5)), 7)
  //Solution:
  val testScala = ((x: Int) => x + 5)(7)

  //Another task.
  //How many occurrences of Num(2) appear in the FAE expression resulting from
  //this Scala snippet?
  def double(x: Exp) = Add(x, x)
  double(double(double(2)))
  //Also, generalize to N nested calls to "double"; above, N = 3.
  //Solution: 2^N.

  //How many occurrences of Num(2) appear in this FAE expression? What about
  //generalizing to N nested calls to 'double?
  wth('double, Fun('x, Add('x, 'x)),
    App('double, App('double, App('double, 2))))

  //Answer: Num(2) appears only 1 for arbitrary N.

  //Let us look at translations of sum and its use:

  def sum(n: Int): Int =
    if (n == 0)
      0
    else
      n + sum(n - 1)
  sum(10)
  //
  /*
   * Incorrect translation, does not terminate, because it tries to build an
   * infinite expression. At the Scala-level, If is a normal call-by-value
   * constructor, so its arguments are evaluated eagerly; hence
   * factorial(Num(0)) will call factorial(Add(Num(0), Num(-1))), which will
   * call factorial(Add(Add(Num(0), Num(-1)), Num(-1))), and so on forever (or
   * until stopped by stack overflow).
   */
  //
  def factorial(x: Exp): Exp =
    If(Eq(x, 0), 0, Add(x, factorial(Add(x, -1))))
  //factorial(10) //commented out, it would loop.
  //A proper translation requires using Letrec, defined in 09-rcfae.scala. Remember from the lecture:
  //val sum = Letrec('sum, Fun('n, If0('n, 0, Add('n, App('sum, Add('n, -1))))), App('sum, 10))
}
