object ExHw07Solution {
  // Direct Style          Continuation-Passing Style
  //
  //  42                   k => k(42)
  //
  //  f(5)                 k => f_k(5, k)
  //
  //  f(g(5))              k => g_k(5, a => f_k(a, k))
  //
  //  if (f(5))            k => f_k(5, a => if (a)
  //    g(1)                                  g_k(1, k)
  //  else                                  else
  //    27                                    k(27))

  //Let's look at the first example in more detail.
  def theAnswer(k: Int => Nothing): Nothing = {
    k(42)
  }

  def fails: Nothing =
    throw new Exception("Failure")
  //sys.exit(1)

  //Task 1.
  def f: Int => Int = x => x
  def t1 = f(f(3) + f(4))

  def fCps: Int => (Int => Nothing) => Nothing =
    x =>
      k =>
        k(x)

  def t1Cps: (Int => Nothing) => Nothing =
    k =>
      fCps(3) { n1 =>
        fCps(4) { n2 =>
          fCps(n1 + n2)(k)
        }
      }

  def t2 = (f(3), f(4))

  def t2Cps(k: (Int, Int) => Nothing): Nothing =
    fCps(3) { f3 =>
      fCps(4) { f4 =>
        k(f3, f4)
      }
    }

  //Task 2
  //version in direct style (that is, not CPS):
  def all(f: Int => Boolean, list: List[Int]): Boolean =
    if (list.isEmpty) {
      true
    } else {
      f(list.head) && all(f, list.tail)
      // fHead => fHead && all(f, list.tail)
    }

  val even = (number: Int) => number % 2 == 0

  assert(all(even, List(2, 4, 6, 8, 10)))
  assert(!(all(even, List(2, 4, 7, 8, 10))))

  //Type signature we have used:
  //def allCps(f: Int => (Boolean => Nothing) => Nothing, list: List[Int])(k: Boolean => Nothing): Nothing =
  //Generalization:
  def allCps[T](f: Int => (Boolean => T) => T, list: List[Int])(k: Boolean => T): T =
    if (list.isEmpty)
      k(true)
    else
      f(list.head) { fHead =>
        allCps(f, list.tail) {
          allFTail =>
            k(fHead && allFTail)
        }
      }

  def evenCps[T] = (number: Int) => (k: Boolean => T) => k(number % 2 == 0)
  allCps(evenCps, List(2, 4, 6, 8, 10)) { res =>
    assert(res)
  }
  allCps(evenCps, List(2, 4, 7, 8, 10)) { res =>
    assert(!res)
  }

  object Hw07MAE {
    sealed abstract class Exp
    case class Num(n: Int) extends Exp
    case class Id(name: Symbol) extends Exp
    case class Add(lhs: Exp, rhs: Exp) extends Exp
    case class Mul(lhs: Exp, rhs: Exp) extends Exp
    implicit def num2exp(n: Int) = Num(n)
    implicit def id2exp(s: Symbol) = Id(s)

    type Value = Int
    type Env = Map[Symbol, Value]

    def evalCps[T](e: Exp, env: Env)(k: Int => T): T =
      e match {
        case Num(n) => k(n)
        case Id(x)  => k(env(x))
        case Add(l, r) =>
          //eval(l, env) + eval(r, env)
          evalCps(l, env)(a =>
            evalCps(r, env)(b =>
              k(a + b)))
        case Mul(l, r) =>
          evalCps(l, env)(a => evalCps(r, env)(b => k(a * b)))
      }

    def pickCont[T](kZero: => T, kNonZero: Int => T)(n: Int): T =
      if (n == 0)
        kZero
      else
        kNonZero(n)

    //For this example:
    Add(
      Mul(Mul(Mul(Mul(0, Add(2, 3)), Add(2, 4)), Add(2, 4)), Add(2, 4)),
      5)
    //we keep using this kZero down the chain of Mul
    //kZero = a => a + 5

    //def evalCpsOptim[T](e: Exp, env: Env)(kZero: => T, kNonZero: Int => T): T = {
    def evalCpsOptim(e: Exp, env: Env)(kZero: => Nothing, kNonZero: Int => Nothing): Nothing = {
      println(s"Evaluating $e")
      e match {
        case Num(n) => pickCont(kZero, kNonZero)(n)
        case Id(x)  => pickCont(kZero, kNonZero)(env(x))
        case Add(l, r) =>
          evalCpsOptim(l, env)(
            evalCpsOptim(r, env)(kZero, b => kNonZero(b)),
            a => evalCpsOptim(r, env)(kNonZero(a), { b =>
              pickCont(kZero, kNonZero)(a + b)
            }))
        case Mul(l, r) => evalCpsOptim(l, env)(
          kZero,
          a => evalCpsOptim(r, env)(kZero, b => kNonZero(a * b))
        )
      }
    }

    type Env_K[T] = Map[Symbol, (Value => T) => T]

    def evalCpsEnv[T](e: Exp, env: Env_K[T])(k: Int => T): T =
      e match {
        case Num(n)    => k(n)
        case Id(x)     => env(x)(k)
        case Add(l, r) => evalCpsEnv(l, env)(a => evalCpsEnv(r, env)(b => k(a + b)))
        case Mul(l, r) => evalCpsEnv(l, env)(a => evalCpsEnv(r, env)(b => k(a * b)))
      }
  }
}
