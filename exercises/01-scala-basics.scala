/*
 * Group exercises
 */

/* Consider again the lecture on desugaring (03-desugaring.scala). In this
 * exercise, we'll implement a language extension (unary minus) in a different
 * way.
 *
 * Take again this arithmetic language from the lecture, with addition and
 * multiplication, and subtraction implemented as syntactic sugar. */
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

 The lecture used option iii). Try out two other options.
 */

/**
 * Arithmetic Expressions with Addition, Multiplication and Subtraction, and Unary minus (option i):
 * i) Add unary minus to the core language
 */
object UiSMAE {
}

/**
 * Arithmetic Expressions with Addition, Multiplication and Subtraction, and Unary Minus (option ii):
 * ii) Treat unary minus as syntactic sugar for the core language using  ``-x = (-1)*x``
 */
object UiiSMAE {
}

// HOMEWORK
//
// Email homework as Scala source file to:
//
//   paolo.giarrusso@uni-tuebingen.de
//
// Work in groups of 1 or 2 students.
// 
// Put "pl1-hw01" in subject, please
//
// 0. write in the email:
//      - your names
//      - your student ids ("Matrikelnummer")
//      - your study programme ("Studiengang")
//      - how long have you been studying ("Fachsemester")
// 1. install Scala
// 2. experiment with past lecture notes; try loading some of them into the Scala REPL, and try out them on examples, until you are satisfied with your understanding of them.
// 3. implement eval (from AE in 02-scala-basics.scala) without pattern matching
// 4. to better understand visitors, try translating countVisitor and printVisitor from their definition in 04-ae.scala to one using pattern matching
// 5. try implementing composition of expressions: write a program compose such that,
// given an expression e1 containing variable v1 (and no other one), and an expression e2 containing variable v2 (and no other one), with v2 != v1,
// compose(e1, v1, e2, v2) gives an expression e3 that computes expression e1 with v1 replaced by the result of expression e2.
//
// Send question by email to paolo.giarrusso@uni-tuebingen.de
