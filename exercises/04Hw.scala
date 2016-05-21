object Hw04 {
  // HOMEWORK ASSIGNMENT
  // ===================
  //
  // SUBMISSION INSTRUCTIONS
  // - Submit your solution to this exercise until *MONDAY*, 23.5., 23:59h
  //   via email to paolo.giarrusso@uni-tuebingen.de.
  // - For questions, use the forum, or the same email.
  //
  // - Work in groups of 1 or 2 students.
  // - Send the email CC to the other student in your team, so I can answer all
  //   directly. No need to repeat names/Matrikelnr. in email. But please let
  //   me know in the email if groups change!
  // - Put "pl1-hw04" in subject, please

  // TASK 0
  // ======
  // 0. Please register to our forum and send me our username.
  //
  //    https://forum-ps.informatik.uni-tuebingen.de/
  //
  // so that you can ask questions on the assignment (and lecture) here (by
  // answers to posts):
  //
  //    https://forum-ps.informatik.uni-tuebingen.de/c/pl1
  //
  // I will also post clarifications there.

  object Task1 {
    //Syntax of BCFAE from the lecture. Tasks follow below.
    sealed abstract class Exp
    case class Num(n: Int) extends Exp
    case class Id(name: Symbol) extends Exp
    case class Add(lhs: Exp, rhs: Exp) extends Exp
    case class Mul(lhs: Exp, rhs: Exp) extends Exp
    case class If0(cond: Exp, thenExp: Exp, elseExp: Exp) extends Exp
    implicit def num2exp(n: Int) = Num(n)
    implicit def id2exp(s: Symbol) = Id(s)
    case class Fun(param: Symbol, body: Exp) extends Exp
    case class App(funExpr: Exp, argExpr: Exp) extends Exp
    def wth(x: Symbol, xdef: Exp, body: Exp): Exp = App(Fun(x, body), xdef)

    case class NewBox(e: Exp) extends Exp // create a new box
    case class SetBox(b: Exp, e: Exp) extends Exp // assign to a box
    case class OpenBox(b: Exp) extends Exp // read value in a box
    case class Seq(e1: Exp, e2: Exp) extends Exp // sequencing of expression

    // TASK 1
    // ======
    // 1a. Consider the following FAE program snippets, faeProgram1 and faeProgram2, defined in terms of bodyOfFun:

    val faeProgram1 =
      wth('fun, Fun('x, Add('x, 0)),
        Add(App('fun, 0), App('fun, 0)))
    val faeProgram2 =
      wth('fun, Fun('x, Add('x, 0)),
        wth('x, App('fun, 0),
          Add('x, 'x)))
    //are they equivalent? Do they stay equivalent if we replace the body of function 'fun by
    //another arbitrary FAE expression?

    //1b. Now, let us consider task 1a for BCFAE programs bcfaeProgram1 and bcfaeProgram2:
    val bcfaeProgram1 =
      wth('counter, NewBox(0),
        wth('fun, Fun('x, Add('x, 0)),
          Add(App('fun, 0), App('fun, 0))))
    val bcfaeProgram2 =
      wth('counter, NewBox(0),
        wth('fun, Fun('x, Add('x, 0)),
          wth('x, App('fun, 0),
            Add('x, 'x))))
    //are they equivalent? Do they stay equivalent if we replace the body of function 'fun by
    //another arbitrary FAE expression? And if we replace the body of function 'fun by another
    //arbitrary BCFAE expression?

    //- Hint: this question is about the relation between mutation and purity.
    //If we call the same function twice with the same argument, are we going to
    //always get the same result, or not?

    //- Hint 2: to try this out, feel free to add here the code of the appropriate
    //interpreter; or you can copy the above definitions together with the
    //interpreters in the lecture notes.
  }

  // 2. Add to the FAE language assignment to arbitrary variables (and not just
  // to boxes), similarly to Java or C. The resulting language need not have
  // separate support for boxes (it's your choice), but it will need to support
  // sequencing. To this end, during execution, you will need to ensure that the
  // environment binds each variable to a box, rather than directly to a value.
  //
  // Hints:
  // - To implement this, it is probably useful to start from the BCFAE
  //   interpreter in 10-bcfae.scala. Make sure to review it first!
  //
  // - the syntax of assignment will be either:
  //     case class Assign(lhs: Symbol, rhs: Exp) extends Exp
  // or corresponding syntactic sugar (implemented, for instance, in terms of SetBox):
  //     def Assign(lhs: Symbol, rhs: Exp) extends Exp = ...
  //
  // - once you define the syntax, try writing example programs, in both this
  //   language and Scala, and writing down (in comments and possibly in testcases)
  //   what they should evaluate to.
  // - as mentioned, all variables will need to be boxes; this can be done either
  //   by changing the interpreter, or by redefining the constructs that bind
  //   variables.

  // insert your solution here. Note that definitions in Task1 are not available
  //here, so that you can change them if needed.
}
import Hw04._
