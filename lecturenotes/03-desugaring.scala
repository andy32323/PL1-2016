/**
Desugaring
==========

In the exercises you have already seen the basic structure of an interpreter by means of an interpreter for a language of arithmetic Exps: 
*/

object AE {
  // Abstract Syntax Tree
  sealed trait Exp // we use "sealed" to get completeness checks for pattern matching
  case class Num(n: Int) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp

  // Example
  val ex = Add(Num(1), Add(Num(5), Num(3)))

  // Interpreter
  def eval(e: Exp): Int =
    e match {
      case Num(n) => n
      case Add(lhs, rhs) =>
        eval(lhs) + eval(rhs)
    }
}

/** 
In this lecture we want to study the technique of desugaring as a means to structure programming languages and decompose a language into a core language and syntactic sugar.
 
For illustration, consider the following proposed extensions to the language:
  1. Mult
  2. Sub
  3. Unary Negation 

Extension number 1 is a good example for a core language extension. We have no way of expressing mult in terms of the existing constructs (if we had some looping construct we could express mult as repeated Add but we do not have loops).
 
Hence we add this language construct to the (core) language:
*/

object MAE {
  // Abstract Syntax Tree
  sealed trait Exp
  case class Num(n: Int) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp
  case class Mult(lhs: Exp, rhs: Exp) extends Exp
  // Example
  val ex = Add(Num(1), Mult(Num(5), Num(3)))

  // Interpreter
  def eval(e: Exp): Int =
    e match {
      case Num(n) => n
      case Add(lhs, rhs) => eval(lhs) + eval(rhs)
      case Mult(lhs, rhs) =>  eval(lhs) * eval(rhs)
    }
}

/**
Let us now consider extension #2, sub. One way to support sub is to add it to the core language, just like mult: 
*/

object SMAE {
  // Abstract Syntax Tree
  sealed trait Exp
  case class Num(n: Int) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp
  case class Mult(lhs: Exp, rhs: Exp) extends Exp
  case class Sub(lhs: Exp, rhs: Exp) extends Exp
  // Example
  val ex = Sub(Num(1), Mult(Num(5), Num(3)))

  // Interpreter
  def eval(e: Exp): Int =
    e match {
      case Num(n) => n
      case Add(lhs, rhs) => eval(lhs) + eval(rhs)
      case Mult(lhs, rhs) =>  eval(lhs) * eval(rhs)
      case Sub(lhs, rhs) =>  eval(lhs) - eval(rhs)
    }
}

/**
However, another way of adding sub is to treat it as syntactic sugar using the fact that ``a - b = a + (-1 * b)``

One way of expressing the desugaring is as a syntax transformation: 
*/

def desugarSMAE2MAE(e: SMAE.Exp) : MAE.Exp = e match {
  case SMAE.Num(n) => MAE.Num(n)
  case SMAE.Add(lhs, rhs) => MAE.Add(desugarSMAE2MAE(lhs), desugarSMAE2MAE(rhs))
  case SMAE.Mult(lhs, rhs) => MAE.Mult(desugarSMAE2MAE(lhs), desugarSMAE2MAE(rhs)) 
  case SMAE.Sub(lhs, rhs) => 
    MAE.Add(desugarSMAE2MAE(lhs), 
                 MAE.Mult(MAE.Num(-1),desugarSMAE2MAE(rhs)))
}

/**
With this desugaring in place, we do not need an interpreter for SMAE anymore; rather we can reuse the MAE interpreter: 
*/

val res = MAE.eval(desugarSMAE2MAE(SMAE.ex))

/** 
If we had written other algorithms on MAE, or had proven properties of MAE, they'd be applicable to SMAE, too. Hence desugaring is a way of reusing code, proofs, ... . It is important, though, that the desugared language feature is gone after desugaring. For instance, a pretty printer would print the desugared code. A debugger would use the desugared code. This can be an important downside to desugaring. There are ways to avoid or mitigate these shortcomings, but they require Addal work.

There is a second way of realizing desugaring which does not require the definition of a copy of the AST classes. We can desugar earlier, namely during the construction of the AST: */

object SMAE2 {
  // Abstract Syntax Tree
  sealed trait Exp
  case class Num(n: Int) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp
  case class Mult(lhs: Exp, rhs: Exp) extends Exp
  def sub(e1: Exp, e2: Exp) : Exp =
    Add(e1, Mult(Num(-1), e2))
  
  // Compared to SMAE, we only have to change upper case Sub by lower case sub
  // when constructing examples.
  val ex = sub(Num(1), Mult(Num(5), Num(3)))

  // Interpreter - no case for sub needed
  def eval(e: Exp): Int =
    e match {
      case Num(n) => n
      case Add(lhs, rhs) => eval(lhs) + eval(rhs)
      case Mult(lhs, rhs) =>  eval(lhs) * eval(rhs)
    }
}

/** 
Let us now consider the third extension, unary minus. Here we have three choices:
 i) Add unary minus to the core language
 ii) Treat unary minus as syntactic sugar for the core language using  ``-x = (-1)*x``
 iii) Treat unary minus as syntactic sugar on top of the syntactic sugar using ``-x = 0 - x``.

We will use the third option to illustrate that one can build layers of syntactic sugar:
 */

object USMAE {
  // Abstract Syntax Tree
  sealed trait Exp
  case class Num(n: Int) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp
  case class Mult(lhs: Exp, rhs: Exp) extends Exp
  def sub(e1: Exp, e2: Exp) : Exp =
    Add(e1, Mult(Num(-1), e2))
  def unaryminus(e: Exp) = sub(Num(0), e)
  
  // Compared to SMAE, we only have to change upper case Sub by lower case sub
  // when constructing examples.
  val ex = sub(unaryminus(Num(1)), Mult(Num(5), Num(3)))

  // Interpreter - no case for sub needed
  def eval(e: Exp): Int =
    e match {
      case Num(n) => n
      case Add(lhs, rhs) => eval(lhs) + eval(rhs)
      case Mult(lhs, rhs) =>  eval(lhs) * eval(rhs)
    }
}
