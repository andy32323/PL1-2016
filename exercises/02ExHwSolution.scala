package mine

object TestIf {
  // Class task: Write a function that wraps if and has the correct evaluation
  // behavior.
  //Solution:
  def ifWrapper(x: Boolean, thenB: => Int, elseB: => Int) =
    if (x)
      thenB
    else
      elseB

  /* Note the that thenB and elseB are by-name parameters, as indicated by =>
   * in the type.
   *
   * The corresponding arguments are only evaluated if and where they are used,
   * not when ifWrapper is called. This behavior matches the behavior of
   * functions under call-by-name, but instead of choosing between
   * call-by-value and call-by-name for the whole language, we can make a
   * different choice for each parameter.
   *
   * Contrast with badIfWrapper - the only difference is that thenB and elseB
   * are *NOT* by-name parameters.
   */
  def badIfWrapper(x: Boolean, thenB: Int, elseB: Int) =
    if (x)
      thenB
    else
      elseB

  /*
   * Class task:
   * - To test ifWrapper vs badIfWrapper, can we construct an example where
   * evaluating both arguments would lead to the wrong behavior in Scala?
   * Can we encode the same in FAE?
   */

  //To test ifWrapper vs badIfWrapper, we use it to write factorial.
  def badFactorial(x: Int): Int =
    ifWrapper(x != 0, x * badFactorial(x - 1), 1)
  def factorial(x: Int): Int =
    ifWrapper(x != 0, x * factorial(x - 1), 1)
  //Now, factorial(3) will terminate, but badFactorial(3) will loop.

  //Another test-case is the following:
  //scala version of FAE's omega
  def scalaOmega(): Nothing = scalaOmega()
  def testIfWrapper() = ifWrapper(true, 1, scalaOmega())
  def testBadIfWrapper() = badIfWrapper(true, 1, scalaOmega())
}

object Ex02 {
  //What is the correct design for an If expression? Think of Scala if expressions.
  //Can we encode And, Or, Not?

  //- Not calling eval on result
  //- Making booleans a separate syntactic class
  //- Evaluating branches too soon.
  /**
    * Higher-Order Functions
    * ======================
    * F1-WAE, the language with first-order functions, lets us abstract over patterns that involve numbers. But what if we want to abstract over patterns that involve functions, such as the "list fold" pattern, whose instantiations include summing or multiplying a list of integers?
    *
    * To enable this kind of abstraction, we need to make functions "first-class", which means that they become values that can be passed to or returned from functions or stored in data structures. Languages with first-class functions enable so-called "higher-order functions", which are functions that accept or return a (possibly again higher-order) function.
    *
    * We will see that this extension will make our language both simpler and much more powerful. This seeming contradiction is famously addressed by the first sentence of the Scheme language specification:
    *
    * "Programming languages should be designed not by piling feature on top of feature, but by removing the weaknesses and restrictions that make additional features appear necessary."
    *
    * The simplicity is due to the fact that this language is so expressive that many other language features can be "encoded", i.e., they do not need to be added to the language but can be expressed with the existing features.
    *
    * This language, which we call "FAE", is basically the so-called "lambda calculus", a minimal but powerful programming language that has been highly influential in the design and theory of programming languages.
    *
    * FAE is the language of arithmetic expressions, AE, plus only two additional language constructs: Function abstraction and function application.
    */

  sealed abstract class Exp
  case class Num(n: Int) extends Exp
  case class Id(name: Symbol) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp
  implicit def num2exp(n: Int) = Num(n)
  implicit def id2exp(s: Symbol) = Id(s)

  //PG: extend syntax for Task 1
  case class Bool(b: Boolean) extends Exp
  implicit def boolean2exp(b: Boolean) = Bool(b)
  case class If(cond: Exp, thenBranch: Exp, elseBranch: Exp) extends Exp
  // This was not explicitly requested, but makes conditionals more useful.
  case class Eq(lhs: Exp, rhs: Exp) extends Exp

  /**
    * Both function definitions and applications are expressions.
    */
  case class Fun(param: Symbol, body: Exp) extends Exp
  case class App(funExpr: Exp, argExpr: Exp) extends Exp

  /**
    * Due to the lambda calculus, the concrete syntax for function abstraction is often written with a lambda, such as ``lambda x. x+3``, thus also called lambda abstraction. The Scala syntax for lambda terms is ``(x) => x+3``, the Haskell syntax is ``\x -> x+3``.
    *
    * The concrete syntax for function application is often either  juxtaposition ``f a`` or using brackets ``f(a)``. Haskell and the lambda calculus use the former, Scala uses the latter.
    *
    * The ``with`` construct is not needed anymore since it can be encoded using ``App`` and ``Fun``. For instance, ``with x = 7 in x+3`` can be encoded (using Scala syntax) as ``((x) => x+3)(7)``
    *
    * We make this idea explicit by giving a constructive translation. Such translations are also often called "desugaring".
    */

  // "with" would be a better name for this function, but it is reserved in Scala
  def wth(x: Symbol, xdef: Exp, body: Exp): Exp = App(Fun(x, body), xdef)
  def not(e: Exp) = If(e, false, true)

  /**
    * ensureBool(e) produces an expression e2 such that:
    * - if e evaluates to a boolean, e2 is equivalent to e
    * - otherwise, triggers an evaluation error.
    */
  def ensureBool(e: Exp) = If(e, true, false)

  def and(l: Exp, r: Exp) = If(l, ensureBool(r), false)
  def or(l: Exp, r: Exp) = If(l, true, ensureBool(r))

  def freshName(names: Set[Symbol], default: Symbol): Symbol = {
    var last: Int = 0
    var freshName = default
    while (names contains freshName) { freshName = Symbol(default.name + last.toString); last += 1; }
    freshName
  }

  assert(freshName(Set('y, 'z), 'x) == 'x)
  assert(freshName(Set('x2, 'x0, 'x4, 'x, 'x1), 'x) == 'x3)

  def freeVars(e: Exp): Set[Symbol] = e match {
    case Id(x)          => Set(x)
    case Add(l, r)      => freeVars(l) ++ freeVars(r)
    case Fun(x, body)   => freeVars(body) - x
    case App(f, a)      => freeVars(f) ++ freeVars(a)
    case Num(n)         => Set.empty
    //PG: Task1: Extend freeVars
    case Bool(b)        => Set.empty
    case If(test, t, e) => freeVars(test) ++ freeVars(t) ++ freeVars(e)
    case Eq(l, r)       => freeVars(l) ++ freeVars(r)
  }
  assert(freeVars(Fun('x, Add('x, 'y))) == Set('y))

  //PG: Task1: Extend substitution
  def subst(e1: Exp, x: Symbol, e2: Exp): Exp = e1 match {
    case Num(n)    => e1
    case Add(l, r) => Add(subst(l, x, e2), subst(r, x, e2))
    case Id(y)     => if (x == y) e2 else Id(y)
    case App(f, a) => App(subst(f, x, e2), subst(a, x, e2))
    case Fun(param, body) =>
      if (param == x) e1 else {
        val fvs = freeVars(body) ++ freeVars(e2) + x
        val newvar = freshName(fvs, param)
        Fun(newvar, subst(subst(body, param, Id(newvar)), x, e2))
      }
    //PG: Task1: Extend substitution
    case Bool(b)        => e1
    case If(test, t, e) => If(subst(test, x, e2), subst(t, x, e2), subst(e, x, e2))
    case Eq(l, r)       => Eq(subst(l, x, e2), subst(r, x, e2))
  }

  assert(subst(Add(5, 'x), 'x, 7) == Add(5, 7))
  assert(subst(Add(5, 'x), 'y, 7) == Add(5, 'x))
  assert(subst(Fun('x, Add('x, 'y)), 'x, 7) == Fun('x, Add('x, 'y)))
  // test capture-avoiding substitution
  assert(subst(Fun('x, Add('x, 'y)), 'y, Add('x, 5)) == Fun('x0, Add(Id('x0), Add(Id('x), Num(5)))))
  assert(subst(Fun('x, Add(Id('x0), Id('y))), 'y, Add(Id('x), 5)) == Fun('x1, Add(Id('x0), Add(Id('x), Num(5)))))

  /**
    * OK, equipped with this new version of substitution we can now define the interpreter for this language.
    *
    * But how do we evaluate a function abstraction? Obviously we cannot return a number.
    *
    * We realize that functions are also values! Hence we have to broaden the return type of our evaluator to also allow functions as values.
    *
    * For simplicity, we use "Exp" as our return type since it allows us to return both numbers and functions. Later we will become more sophisticated.
    *
    * This means that a new class of errors can occur: A subexpression evaluates to a number where a function is expected, or vice versa. Such errors are called _type errors_, and we will talk about them in much more detail later. For now, it means that we need to analyze (typically by pattern matching) the result of recursive invocations of eval to check whether the result has the right type.
    *
    * The remainder of the interpreter is unsurprising.
    */

  def eval(e: Exp): Exp = e match {
    case Id(v) => sys.error("unbound identifier: " + v.name)
    case Add(l, r) => (eval(l), eval(r)) match {
      case (Num(x), Num(y)) => Num(x + y)
      case _                => sys.error("can only add numbers")
    }
    case App(f, a) => eval(f) match {
      case Fun(x, body) => eval(subst(body, x, eval(a))) // call-by-value
      //case Fun(x, body) => eval(subst(body, x, a)) // call-by-name
      case _            => sys.error("can only apply functions")
    }
    //PG: extend evaluation
    case If(cond, t, e) =>
      eval(cond) match {
        case Bool(evalCond) =>
          if (evalCond)
            eval(t)
          else
            eval(e)
        case _ =>
          sys.error("condition should be a boolean")
      }
    case Eq(l, r) =>
      (eval(l), eval(r)) match {
        case (Num(x), Num(y))   => Bool(x == y)
        case (Bool(x), Bool(y)) => Bool(x == y)
        case _                  => sys.error("can only compare numbers or booleans")
      }
    case _ => e // numbers and functions (PG: and booleans) evaluate to themselves
  }

  /**
    * Let's test.
    */

  val test = App(Fun('x, Add('x, 5)), 7)

  assert(eval(test) == Num(12))

  /**
    * FAE is a computationally (Turing)-complete language. For instance, we can define  a non-terminating program. This program is commonly called Omega
    */

  val omega = App(Fun('x, App('x, 'x)), Fun('x, App('x, 'x)))

  // try eval(omega) to crash the interpreter ;-)

  /**
    * However, consider the following example.
    */
  val test2 = wth('x, 5, App(Fun('f, App('f, 3)), Fun('y, Add('x, 'y))))

  /**
    * It works fine in the substitution-based interpreter.
    */
  assert(eval(test2) == Num(8))

  /**
    * but ``evalWithEnv0(test2,Map.empty)`` yields an "identifier not found: 'x" error.
    *
    * The problem is that we have forgotten the deferred substitutions to be performed in the body of the function.
    *
    * What can we do to fix this problem?
    *
    * We could try to replace the second line in the "App" case by
    * case Fun(x,body) => evalWithEnv0(body, env + (x -> evalWithEnv0(a,env)))
    * but this would again introduce dynamic scoping.
    *
    * Hence, when we evaluate a function, we do not only have to store the function, but also the environment active when the function was created. This pair of function and environment is called a _closure_. The environment stored in the closure is used when the function is eventually applied.
    *
    * Hint: If you cannot answer what a closure is and how it is used in the  interpreter, you will be toast in the exam!
    *
    * Since closures are not expressible in the language syntax, we now come to the point where we need a separate category of _values_. The values in FAE can be either numbers or closures.
    */

  sealed abstract class Value
  type Env = Map[Symbol, Value]
  case class NumV(n: Int) extends Value
  case class ClosureV(f: Fun, env: Env) extends Value
  case class BoolV(b: Boolean) extends Value
  /**
    * The evaluator becomes :
    */

  def evalWithEnv(e: Exp, env: Env): Value =
    e match {
      case Num(n: Int) => NumV(n)
      case Id(x)       => env(x)
      case Add(l, r) => {
        (evalWithEnv(l, env), evalWithEnv(r, env)) match {
          case (NumV(v1), NumV(v2)) => NumV(v1 + v2)
          case _                    => sys.error("can only add numbers")
        }
      }
      case f @ Fun(param, body) =>
        //PG: Task 2: instead of
        //ClosureV(f, env)
        //use:
        ClosureV(f, env filterKeys (freeVars(f).contains(_)))
        //PG:
        //- Warning: using freeVars(body) instead of freeVars(f) would store one
        //  useless binding — useless because we're going to replace it.
        // - Scala experts might want to write this as:
        //
        //       ClosureV(f, env filterKeys freeVars(f))
        //
        //   since Set.apply is equivalent to Set.contains.
      case App(f, a) => evalWithEnv(f, env) match {
        // Use environment stored in closure to realize proper lexical scoping!
        case ClosureV(f, closureEnv) => evalWithEnv(f.body, closureEnv + (f.param -> evalWithEnv(a, env)))
        case _                       => sys.error("can only apply functions")
      }
      //PG: task 1
      case Bool(b) =>
        BoolV(b)
      case Eq(l, r) =>
        (evalWithEnv(l, env), evalWithEnv(r, env)) match {
          case (NumV(v1), NumV(v2))   => BoolV(v1 == v2)
          case (BoolV(v1), BoolV(v2)) => BoolV(v1 == v2)
          case _                      => sys.error("can only compare numbers or booleans")
        }
      case If(cond, t, e) =>
        evalWithEnv(cond, env) match {
          case BoolV(evalCond) =>
            if (evalCond)
              evalWithEnv(t, env)
            else
              evalWithEnv(e, env)
          case _ =>
            sys.error("condition should be a boolean")
        }

    }

  //Convenience to test evaluation.
  def test(str: String, testExpr: => Boolean) {
    println("\n" + str)
    assert(testExpr)
  }

  test("evalWithEnv(test, Map.empty) == NumV(12)", evalWithEnv(test, Map.empty) == NumV(12))
  test("evalWithEnv(test2, Map.empty) == NumV(8)", evalWithEnv(test2, Map.empty) == NumV(8))

  //Shows the difference between static and dynamic scoping in FAE.
  val testDiff =
    wth('x, 40,
      wth('f, Fun('y, Add('x, 'y)),
        wth('z, App('f, 2),
          Add('z,
            wth('x, 1,
              App('f, 2))))))
  //test("evalWithEnvDynScope(testDiff, Map.empty) == Num(45)", evalWithEnvDynScope(testDiff, Map.empty) == Num(45))
  test("evalWithEnv(testDiff, Map.empty) == NumV(84)", evalWithEnv(testDiff, Map.empty) == NumV(84))
  test("eval(testDiff, Map.empty) == NumV(84)", eval(testDiff) == Num(84))

}
