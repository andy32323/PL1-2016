import scala.language.higherKinds
import scala.language.implicitConversions

/**
  * Deadline: Tuesday 28 at 14:00.
  *
  * Tasks are marked below by TASK.
  */

object Hw08 {
  /**
    * Here's the abstract syntax for first-class continuations.
    */
  sealed abstract class Exp
  case class Num(n: Int) extends Exp
  case class Id(name: Symbol) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp
  case class Fun(param: Symbol, body: Exp) extends Exp
  implicit def num2exp(n: Int) = Num(n)
  implicit def id2exp(s: Symbol) = Id(s)
  case class App(funExpr: Exp, argExpr: Exp) extends Exp
  case class Letcc(param: Symbol, body: Exp) extends Exp
  def wth(x: Symbol, xDef: Exp, body: Exp): Exp =
    App(Fun(x, body), xDef)
  /**
    * TASK 1:
    * Programs using let/cc can be transformed into
    * FAE programs by using the CPS transformation.
    * For instance, Racket's (let/cc k (k 3)), that is
    */

  val ex1: Exp = Letcc('k, App('k, 3))

  /**
    * transforms to (in Scala notation):
    * k => k(3)
    * that is
    */
  val ex1Cps: Exp = Fun('k, App('k, 3))
  /**
    * Transform examples below:
    * 2. (+ 1 (let/cc k (k 3))
    * 3. (+ (let/cc k (k 3) 2)
    * 4. (let/cc k 3)
    * 5. (+ 3
    *       (let/cc k
    *         (let ([f
    *                (lambda (x) (+ 1 x))])
    *           (k (+
    *               (f 2)
    *               (f 5))))))
    */
  val ex2 = Add(1, Letcc('k, App('k, 3)))
  val ex3 = Add(Letcc('k, App('k, 3)), 2)
  val ex4: Exp = Letcc('k, 3)
  val ex5 =
    Add(3,
      Letcc('k,
        wth('f, Fun('x, Add(1, 'x)),
          App('k,
            Add(
              App('f, 2),
              App('f, 5))))))
}

object Hw08Monads {
  // A common interface for all monads:
  trait Monad {
    type M[_]
    def unit[A](a: A): M[A]
    def bind[A, B](p: M[A], f: A => M[B]): M[B]

    implicit class monadicSyntax[A](p: M[A]) {
      def flatMap[B](f: A => M[B]) = bind(p, f)
      def map[B](f: A => B) = flatMap(x => unit(f(x)))
    }
  }

  /**
    * TASK 2:
    * - should prog1A and prog1B be equivalent or not, for the suggested
    *   definition of f and g?
    * - should prog1A and prog1B be equivalent or not, for arbitrary definitions
    *   of f and g?
    * - How does this relate to the monad laws? Which laws in particular are
    *   needed to relate prog1A and prog1B?
    * - Same questions for prog2A and prog2B.
    */
  trait MonadicEx1 extends Monad {
    def f: M[Int] = ??? //For instance unit(1)
    def g(x: Int): M[Int] = ??? //For instance unit(2)
    def prog1A =
      for {
        x <- f
        y <- g(x)
      } yield x + y

    def prog1B =
      for {
        x <- f
        y <- for {
          z <- g(x)
        } yield z
      } yield x + y

    def prog2A =
      for {
        y <- for {
          x <- f
          z <- g(x)
        } yield z
      } yield y + y
    def prog2B =
      for {
        x <- f
        y <- for {
          z <- g(x)
        } yield z
      } yield y + y
  }

  /**
    * TASK 3: complete the interpreters below. Part of the answer is in the
    * lecture notes, but this is a simpler example, which does not involve
    * continuations; try to understand it in more detail.
    */
  //A monadic interpreter interface
  trait MonadicInterpreterInterface extends Monad {
    sealed abstract class Exp
    sealed abstract class Value

    def eval(e: Exp): M[Value]
  }

  //Minimal monadic interpreter, for addition and numbers.
  trait MonadicAddInterpreter extends Monad with MonadicInterpreterInterface {
    //Syntax:
    case class Num(n: Int) extends Exp
    case class Add(lhs: Exp, rhs: Exp) extends Exp
    case class Mul(lhs: Exp, rhs: Exp) extends Exp
    implicit def num2exp(n: Int) = Num(n)

    //Semantics
    case class NumV(n: Int) extends Value
    implicit def num2Val(n: Int) = NumV(n)

    def eval(e: Exp): M[Value] = e match {
      case Num(n: Int) =>
        unit(NumV(n))
      //To convert code to monadic style, we must express the order of operations.
      case Add(l, r) => for {
        lv <- eval(l)
        rv <- eval(r)
      } yield (lv, rv) match {
        case (NumV(v1), NumV(v2)) => NumV(v1 + v2)
        case _                    => sys.error("can only add numbers")
      }
      case Mul(l, r) => for {
        lv <- eval(l)
        rv <- eval(r)
      } yield (lv, rv) match {
        case (NumV(v1), NumV(v2)) => NumV(v1 * v2)
        case _                    => sys.error("can only add numbers")
      }
    }

    val testAdd1 = Mul(Add(2, 3), 5) //Should eval to 25
    val testAdd2 = Add(Mul(2, 5), 4) //Should eval to 14
  }

  /**
    * The identity monad implements the Monad interface but doesn't
    * provide any additional features:
    */
  trait IdentityMonad extends Monad {
    type M[X] = X
    def unit[A](a: A): M[A] = a
    def bind[A, B](p: M[A], f: A => M[B]): M[B] = f(p)
  }

  //Combine MonadicAddInterpreter with monad implementation:
  object MonadicAddInterpreterIdentityMonad extends MonadicAddInterpreter with IdentityMonad

  //Declare interface for Reader monad:
  trait Reader extends Monad {
    type R
    def ask: M[R]
    def local[A](f: R => R, p: M[A]): M[A]
  }

  //A monadic interpreter interface, requiring a reader monad for the environment.
  trait MonadicReaderInterpreterInterface extends MonadicInterpreterInterface with Reader {
    type Env = Map[Symbol, Value]
    type R = Env
  }

  //Monadic interpreter for AE, supporting identifiers.
  //Note: the environment passing is encapsulated through the reader monad; existing branches do not have to be modified.
  trait MonadicAEInterpreter extends MonadicAddInterpreter with MonadicReaderInterpreterInterface {
    //Extend the syntax:
    case class Id(name: Symbol) extends Exp
    implicit def id2exp(s: Symbol) = Id(s)

    override def eval(e: Exp): M[Value] = e match {
      case Id(x) => for {
        //The interpreter is threaded through the reader monad.
        env <- ask
      } yield ???
      //PG: What goes there?

      //For other branches, reuse implementation from superclass.
      case _ =>
        super.eval(e)
    }

    //Testcases.
    val testAE1: Exp = 'x
    val testAE2: Exp = 'y
    val testAE3 = Mul(Add('x, 1), 'y)
  }

  //Monadic interpreter for FAE.
  trait MonadicFAEInterpreter extends MonadicAEInterpreter {
    //Extend the syntax:
    case class Fun(param: Symbol, body: Exp) extends Exp
    case class App(funExpr: Exp, argExpr: Exp) extends Exp
    def wth(x: Symbol, xDef: Exp, body: Exp): Exp =
      App(Fun(x, body), xDef)

    case class ClosureV(f: Fun, env: R) extends Value

    override def eval(e: Exp): M[Value] = e match {
      //PG: finish the implementation
      case f @ Fun(param, body) =>
        ???
      case App(f, a) =>
        ???
      case _ =>
        super.eval(e)
    }

    val testFAE1 = Fun('x, 'x)
    val testFAE2 = Add(Add(App(Fun('x, 'x), 42), 'x), 'y)
    val testFAE3 =
      //Test lexical scoping
      App(wth('x, 10, Fun('y, Add('x, 'y))),
        wth('x, 20, 30))
  }

  //Now, provide an implementation of the Reader Monad.
  trait ReaderMonad extends Monad with Reader {
    type M[X] = R => X
    def unit[A](a: A): M[A] =
      r => a
    def bind[A, B](p: M[A], f: A => M[B]): M[B] =
      r => f(p(r))(r)
    def ask: M[R] =
      r => r
    def local[A](f: R => R, p: M[A]): M[A] =
      r => p(f(r))
  }

  //Helper trait to define RunEval.
  trait ReaderMonadRunEval extends ReaderMonad {
    this: MonadicReaderInterpreterInterface =>

    def runEval(e: Exp)(env: Env): Value =
      eval(e)(env)
  }

  //Combine MonadicAEInterpreter with monad implementation and testcases
  object MonadicAEInterpreterReaderMonad extends MonadicAEInterpreter with ReaderMonad with ReaderMonadRunEval {
    val testEnv: Env = Map('x -> 1, 'y -> 2)
    val evalTest1 = runEval(testAE1)(testEnv)
    val evalTest2 = runEval(testAE2)(testEnv)
    val evalTest3 = runEval(testAE3)(testEnv)

    assert(evalTest1 == NumV(1))
    assert(evalTest2 == NumV(2))
    assert(evalTest3 == NumV(4))
  }
  //Combine MonadicFAEInterpreter with monad implementation and testcases
  object MonadicFAEInterpreterReaderMonad extends MonadicFAEInterpreter with ReaderMonad with ReaderMonadRunEval {
    val testEnv: Env = Map('x -> 1, 'y -> 2)
    val evalTest1 = runEval(testFAE1)(testEnv)
    val evalTest2 = runEval(testFAE2)(testEnv)
    val evalTest3 = runEval(testFAE3)(testEnv)

    assert(evalTest1 == ClosureV(testFAE1, testEnv))
    assert(evalTest2 == NumV(45))
    assert(evalTest3 == NumV(40))
  }
}
//Run assertions by loading containing objects:
//mine.Hw08Monads.MonadicAEInterpreterReaderMonad
//mine.Hw08Monads.MonadicFAEInterpreterReaderMonad
