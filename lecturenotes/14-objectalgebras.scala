/* Object algebras: A practical way of using Church Encodings */

/* 
We Church-encode expressions by means of an "object algebra".
This means that an expression is represented by its own "fold" function,
i.e., how to traverse that expression bottom-up. Another way
of phrasing object algebras is to say that each expression is
encoded as its own "visit" function.
*/ 
trait Exp[T] {
  implicit def id(name: Symbol) : T
  def fun(param: Symbol, body: T): T
  def app(funExpr: T, argExpr: T) :T
  implicit def num(n: Int) : T
  def add(e1: T, e2: T) : T
  def wth(x: Symbol, xdef: T, body: T) : T = app(fun(x,body),xdef)
}

/* The structure of expression forces compositional interpretations. Hence
we use the compositional interpretation using meta-level closures to represent
closures. */
sealed abstract class Value
type Env = Map[Symbol, Value]
case class ClosureV(f: Value => Value) extends Value
case class NumV(n: Int) extends Value

/* An interpretation of expressions is now an implementation of the Exp interface */
trait eval extends Exp[Env => Value] {
  def id(name: Symbol) = env => env(name)
  def fun(param: Symbol, body: Env => Value) = env => ClosureV(v => body(env + (param -> v)))
  def app(funExpr: Env => Value, argExpr: Env => Value) = env => funExpr(env) match {
    case ClosureV(f) => f(argExpr(env))
    case _ => sys.error("can only apply functions")
  }
  def num(n: Int) = env => NumV(n)
  def add(e1: Env => Value, e2: Env => Value) = env => (e1(env),e2(env)) match {
    case (NumV(n1), NumV(n2)) => NumV(n1+n2)
    case _ => sys.error("can only add numbers")
  }
}
object eval extends eval

/* An example program becomes a function that is parametric in the choosen interpretation */
def test[T](semantics : Exp[T]) = {
  import semantics._
  
  app(app(fun('x,fun('y,add('x,'y))),5),3)
}

/* We evaluate the program by folding the eval visitor over it. */
val testres = test(eval)(Map.empty)

/* The object algebra encoding of expressions is quite extensible. For instance, we can
add another case to the expression datatype by extending the interface. */

trait ExpWithMult[T] extends Exp[T] {
  def mult(e1 : T, e2: T) : T
}

trait evalWithMult extends eval with ExpWithMult[Env=>Value] {
  def mult(e1: Env => Value, e2: Env => Value) = env => (e1(env),e2(env)) match {
    case (NumV(n1), NumV(n2)) => NumV(n1*n2)
    case _ => sys.error("can only multiply numbers")
  }
}
object evalWithMult extends evalWithMult

def testMult[T](semantics : ExpWithMult[T]) = {
  import semantics._
  
  app(app(fun('x,fun('y,mult('x,'y))),5),3)
}

val testresMult = testMult(evalWithMult)(Map.empty)

/* Note that there is no danger of confusing the language variants. For instance,
an attempt to pass testMult to eval will be a static type error. */

/* We can also go one step further and combine object algebras with 
typed higher-order abstract syntax, using higher-kinded type members. 

Don't panic if you don't understand what is going on here. 
*/
trait ExpT {
  type Rep[_]
  def fun[S,T](f : Rep[S] => Rep[T]): Rep[S=>T]
  def app[S,T](funExpr: Rep[S=>T], argExpr: Rep[S]) :Rep[T]
  implicit def num(n: Int) : Rep[Int]
  def add(e1: Rep[Int], e2: Rep[Int]) : Rep[Int]
}

/* Note that, in contrast to eval, no dynamic checks (match ...) are needed
in the interpreter. This is because the ExpT datatype guarantees well-typedness
of expressions. */
object evalT extends ExpT {
  type Rep[X] = X
  def fun[S,T](f: S=>T) =f
  def app[S,T](f: S=>T, a: S) = f(a)
  def num(n: Int) = n
  def add(e1: Int, e2: Int) = e1+e2
}

object prettyprintT extends ExpT {
  var counter = 0
  type Rep[X] = String
  def fun[S,T](f: String=>String) = {
    val varname = "x" + counter.toString
    counter += 1
    "(" + varname + " => " +  f("x"+counter.toString) + ")"
  }
  def app[S,T](f: String, a: String) = f + "(" + a + ")"
  def num(n: Int) = n.toString
  def add(e1: String, e2: String) = "("+e1+"+"+e2+")"

}
def test2(semantics: ExpT) = {
  import semantics._
  app(app(fun((x:Rep[Int])=>fun((y:Rep[Int])=>add(x,y))),5),3)
}

val testres2 = test2(evalT)
val testres3 = test2(prettyprintT)

/* An attempt to construct an ill-formed object program will be detected by
the Scala type checker. For instance, the following program which adds 
a number and a function is rejected by the Scala type checker: 

def testilltyped(semantics: ExpT) = {
  import semantics._
  add(5,fun((x: Rep[Int]) => x))
}
*/
