/**
  * Homework 01
  * ============
  *
  * Submit your solution to this exercise until Tuesday, 26.4., 23:59h
  * via email to paolo.giarrusso@uni-tuebingen.de.
  *
  * Work in groups of 1 or 2 students.
  *
  * Put "pl1-hw01" in subject, please
  *
  * Write in the email:
  * - your names
  * - your student ids ("Matrikelnummer")
  * - your study programme ("Studiengang")
  * - how long have you been studying ("Fachsemester")
  */

//Add a package declaration:
package masterSol
//and wrap a script into an object:
object Hw01Task1 {
  //to allow compiling it inside Eclipse/IntelliJ IDEA/whatnot.
  /**
    * Consider the language of arithmetic expressions with "with",
    * as illustrated by the following abstract syntax:
    */
  sealed abstract class Exp
  case class Num(n: Int) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp
  case class Mul(lhs: Exp, rhs: Exp) extends Exp
  case class Id(x: Symbol) extends Exp
  case class With(x: Symbol, xDef: Exp, body: Exp) extends Exp

  /**
    * We use implicits again to make example programs less verbose.
    */
  implicit def num2exp(n: Int) = Num(n)
  implicit def sym2exp(x: Symbol) = Id(x)

  /**
    * Your task is to extend the language with the following new binding constructs:
    */

  case class Let(defs: List[(Symbol, Exp)], body: Exp) extends Exp
  case class LetStar(defs: List[(Symbol, Exp)], body: Exp) extends Exp

  /**
    * The purpose of the Let construct is to bind a list of identifiers in such a way
    * that the scope of the bound variables is only in the body, but not any of the
    * right hand sides of definitions. In particular, there is no shadowing between the definitions.
    * For instance, the following test case should evaluate to 7 and not to 11:
    */

  val test1 =
    With('x, 1,
      Let(List('x -> 5, 'y -> Add('x, 1)), Add('x, 'y)))

  /**
    * The LetStar construct is similar to let, but the scope of a definition contains all
    * right hand sides of definitions that follow the current one.
    * The following test case should hence evaluate to 11.
    *
    * Note: The names "Let" and "LetStar" have been choosen in analogy to the
    * "let" and "let*" binding constructs in Scheme and Racket.
    */

  val test2 =
    With('x, 1,
      LetStar(List('x -> 5, 'y -> Add('x, 1)), Add('x, 'y)))

  /**
    * Tasks:
    *
    * 1) Implement the missing part of the eval and subst function
    * to support Let and LetStar
    *
    * 2) There is some redundancy in the binding constructs of this
    * language. Try to eliminate at least one (or even two) of the
    * language constructs With,Let,LetStar by defining the eliminated
    * binding constructs as syntactic sugar.
    *
    * 3) Bonus exercise (not mandatory): Implement a variant
    * "Letr" of "Let" in which the scope of a definition includes
    * all right-hand sides of definitions in the same block, including
    * earlier definitions. For instance,
    * Letr(List('x -> 'y, 'y -> 1),Add('x,'y))
    * should evaluate to 2.
    * On
    * Letr(List('x -> 'y, 'y -> 'x),Add('x,'y))
    * the interpreter should loop or terminate with an error message.
    */
  /**
    * Later clarification: subst replaces free occurrences of i in e with v.
    * Nothing else must be changed; in particular, desugaring expressions
    * inside subst is not admissible.
    */
  def subst(e: Exp, i: Symbol, v: Num): Exp = e match {
    case Num(n)    => e
    case Id(x)     => if (x == i) v else e
    case Add(l, r) => Add(subst(l, i, v), subst(r, i, v))
    case Mul(l, r) => Mul(subst(l, i, v), subst(r, i, v))
    case With(x, xDef, body) =>
      With(x,
        subst(xDef, i, v),
        substIfNotShadowed(Set(x), body, i, v))
    case Let(defs, body) =>
      val replBody = substBody(defs, body, i, v)
      val replDefs = defs map {
        //This is an anonymous function that decomposes the argument pair into x and xDef.
        case (x, xDef) => (x, subst(xDef, i, v))
      }
      Let(replDefs, replBody)
    case LetStar(defs, body) =>
      val replBody = substBody(defs, body, i, v)
      val replDefs = substDefs(defs, i, v)
      LetStar(replDefs, replBody)
  }

  def substIfNotShadowed(shadowed: Set[Symbol], body: Exp, i: Symbol, v: Num) = {
    if (shadowed contains i)
      body
    else
      subst(body, i, v)
  }

  def substBody(defs: List[(Symbol, Exp)], body: Exp, i: Symbol, v: Num) =
    substIfNotShadowed(defs.map(_._1).toSet, body, i, v)

  /**
    * Substitute variable i by number v into a list of definitions defs. The
    * definitions come from a LetStar construct, that is, each definition is in
    * scope in the subsequent ones. As this is used inside substitution, we
    * cannot evaluate the substitutions here.
    */
  def substDefs(defs: List[(Symbol, Exp)], i: Symbol, v: Num): List[(Symbol, Exp)] =
    defs match {
      case (x, xDef) :: tail =>
        (x, subst(xDef, i, v)) :: (
          if (x == i)
            tail
          else
            substDefs(tail, i, v))
      case Nil => Nil
    }

  //
  /**
    * Group exercise (in class):
    * implement a method
    *    def freeVars(e: Exp): Set[Symbol]
    * to compute free variables of an expression.
    */
  //Examples:
  //expression => its free variables.
  //Add('y, 'z) => Set('y, 'z)
  //With('y, 0, 'y) => Set()
  //With('y, 'z, 'w) => Set('z, 'w)
  //With('y, 'z, 'y) => Set('z)
  //With('y, Add(Add('y, 'z), 'w), 'y) => Set('z, 'w, 'y)
  /*
	 * Below, some examples at the Scala REPL of using Set — these are useful here:
	 */
  //scala> val emptySet = Set()
  //emptySet: scala.collection.immutable.Set[Nothing] = Set()
  //
  //scala> val setOf1 = Set(1)
  //setOf1: scala.collection.immutable.Set[Int] = Set(1)
  //
  //scala> val setOfNums = Set(1, 2, 3, 4, 5, 543)
  //setOfNums: scala.collection.immutable.Set[Int] = Set(5, 1, 2, 3, 4, 543)
  //
  //scala> Set('a, 'b)
  //res1: scala.collection.immutable.Set[Symbol] = Set('a, 'b)
  //
  //scala> Set('a) ++ Set('b)
  //res2: scala.collection.immutable.Set[Symbol] = Set('a, 'b)
  //
  //scala> Set('a) + 'b
  //res3: scala.collection.immutable.Set[Symbol] = Set('a, 'b)
  //
  //scala> Set('a, 'b, 'c) - 'a
  //res4: scala.collection.immutable.Set[Symbol] = Set('b, 'c)
  //
  //scala> Set('a, 'b, 'c) -- Set('b, 'c)
  //res5: scala.collection.immutable.Set[Symbol] = Set('a)
  //
  //scala> Set('a, 'b, 'c) - 'a
  //res6: scala.collection.immutable.Set[Symbol] = Set('b, 'c)
  //
  def freeVars(e: Exp): Set[Symbol] = e match {
    case Num(n) => Set()
    case Id(x)  => Set(x)
    case Add(l, r) =>
      freeVars(l) ++ freeVars(r)
    case Mul(l, r) =>
      freeVars(l) ++ freeVars(r)
    case With(x, xdef, body) =>
      //freeVars(body) ++ freeVars(xdef) - x //wrong
      (freeVars(body) - x) ++ freeVars(xdef) //correct
    case Let(defs, body) =>
      val bodyFreeVars = freeVars(body)

      /*
       * Alternative equivalent ways of writing this:
        for ((x, xDef) <- defs)
          yield x
      ==
        for (pair <- defs)
          yield pair._1
      ==
        defs.map(pair => pair._1)
        Feel free to use what you find easier.
       */
      val varsBoundByDefs = for {
        (x, xDef) <- defs
      } yield x

      val defsFreeVars = for {
        (x, xDef) <- defs
        freeVar <- freeVars(xDef)
      } yield freeVar

      bodyFreeVars -- varsBoundByDefs ++ defsFreeVars

    //Added after the exercise session.
    case LetStar(defs, body) =>
      freeVarsLetStar(defs, body)
  }

  def freeVarsLetStar(defs: List[(Symbol, Exp)], body: Exp): Set[Symbol] =
    defs match {
      case Nil => freeVars(body)
      //With(x, xDef, desugar... (moreDefs, body))
      case (x, xDef) :: moreDefs =>
        freeVars(xDef) ++
          (freeVarsLetStar(moreDefs, body) - x)
    }

  def eval(e: Exp): Int = e match {
    case Num(n)              => n
    case Id(x)               => sys.error("unbound variable: " + x.name)
    case Add(l, r)           => eval(l) + eval(r)
    case Mul(l, r)           => eval(l) * eval(r)
    case With(x, xDef, body) => eval(subst(body, x, Num(eval(xDef))))

    //Implement evaluation for task 1.
    case Let((x, xDef) :: defs, body) =>
      //For Let, we substitute the first definition only in the body.
      eval(Let(defs, subst(body, x, Num(eval(xDef)))))
    case Let(Nil, body) => eval(body)
    case LetStar((x, xDef) :: defs, body) =>
      //For LetStar, we substitute the first definition in the body and in the other definitions.
      eval(subst(LetStar(defs, body), x, Num(eval(xDef))))
    case LetStar(Nil, body) => eval(body)
  }
}

/**
  * Task 2.
  * We desugar With and LetStar into Let.
  * This object then implements both task 1 and freeVars for the remaining
  * constructs. The implementations are mostly the same (with fewer cases);
  * for subst, I refactored the code a bit to simplify it (in particular, I
  * inlined a few functions that are now used only once).
  */
object Hw01Task2 {
  sealed abstract class Exp
  case class Num(n: Int) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp
  case class Mul(lhs: Exp, rhs: Exp) extends Exp
  case class Id(x: Symbol) extends Exp

  /**
    * We use implicits again to make example programs less verbose.
    */
  implicit def num2exp(n: Int) = Num(n)
  implicit def sym2exp(x: Symbol) = Id(x)

  /**
    * Your task is to extend the language with the following new binding constructs:
    */

  case class Let(defs: List[(Symbol, Exp)], body: Exp) extends Exp

  //Instead of defining With and LetStar as case classes, so that they are also
  //available as constructors, define functions with their names, so that we can
  //write the same examples (test1 and test2) whether we use a desugaring or
  //not. Such functions are called "smart constructors" — they seem like
  //syntax constructors, but as an implementation detail they do something else.
  /*
   * XXX: normally function names, by convention, start with lowercase names.
   * So it'd be better to have, in an interface:
  def with_(x: Symbol, xdef: Exp, body: Exp): Exp
  def let(defs: List[(Symbol, Exp)], body: Exp): Exp
  def letStar(defs: List[(Symbol, Exp)], body: Exp): Exp
   * and have their implementation use either case classes or a desugaring.
   * The examples
   */
  def With(x: Symbol, xdef: Exp, body: Exp): Exp = Let(List((x -> xdef)), body)
  def LetStar(defs: List[(Symbol, Exp)], body: Exp): Exp =
    defs match {
      case Nil =>
        body
      case (x, xDef) :: rest =>
        Let(List(x -> xDef), LetStar(rest, body))
    }
  //LetStar follows a common pattern for processing lists. Instead of writing it
  //out each time, one can reuse a definition of this pattern, called foldRight,
  //and write the equivalent code:
  /*
    defs.foldRight(body) {
      case ((x, xDef), curr) =>
        Let(List(x -> xDef), curr)
    }
  	*/

  /**
    * The purpose of the Let construct is to bind a list of identifiers in such a way
    * that the scope of the bound variables is only in the body, but not any of the
    * right hand sides of definitions. In particular, there is no shadowing between the definitions.
    * For instance, the following test case should evaluate to 7 and not to 11:
    */

  val test1 =
    With('x, 1,
      Let(List('x -> 5, 'y -> Add('x, 1)), Add('x, 'y)))

  /**
    * The LetStar construct is similar to let, but the scope of a definition contains all
    * right hand sides of definitions that follow the current one.
    * The following test case should hence evaluate to 11.
    *
    * Note: The names "Let" and "LetStar" have been choosen in analogy to the
    * "let" and "let*" binding constructs in Scheme and Racket.
    */

  val test2 =
    With('x, 1,
      LetStar(List('x -> 5, 'y -> Add('x, 1)), Add('x, 'y)))

  /**
    * Tasks:
    *
    * 1) Implement the missing part of the eval and subst function
    * to support Let and LetStar
    *
    * 2) There is some redundancy in the binding constructs of this
    * language. Try to eliminate at least one (or even two) of the
    * language constructs With,Let,LetStar by defining the eliminated
    * binding constructs as syntactic sugar.
    *
    * 3) Bonus exercise (not mandatory): Implement a variant
    * "Letr" of "Let" in which the scope of a definition includes
    * all right-hand sides of definitions in the same block, including
    * earlier definitions. For instance,
    * Letr(List('x -> 'y, 'y -> 1),Add('x,'y))
    * should evaluate to 2.
    * On
    * Letr(List('x -> 'y, 'y -> 'x),Add('x,'y))
    * the interpreter should loop or terminate with an error message.
    */
  /**
    * Clarification: subst replaces free occurrences of i in e with v.
    * Nothing else must be changed; in particular, desugaring expressions
    * inside subst is not admissible.
    */
  def subst(e: Exp, i: Symbol, v: Num): Exp = e match {
    case Num(n)    => e
    case Id(x)     => if (x == i) v else e
    case Add(l, r) => Add(subst(l, i, v), subst(r, i, v))
    case Mul(l, r) => Mul(subst(l, i, v), subst(r, i, v))
    case Let(defs, body) =>
      val replBody =
        if (defs.map(_._1).toSet contains i)
          body
        else
          subst(body, i, v)
      val replDefs = defs map {
        //This is an anonymous function that decomposes the argument pair into x and xDef.
        case (x, xDef) => (x, subst(xDef, i, v))
      }
      Let(replDefs, replBody)
  }

  /**
    * Group exercise (in class):
    * implement a method
    *    def freeVars(e: Exp): Set[Symbol]
    * to compute free variables of an expression.
    */
  def freeVars(e: Exp): Set[Symbol] = e match {
    case Num(n) => Set()
    case Id(x)  => Set(x)
    case Add(l, r) =>
      freeVars(l) ++ freeVars(r)
    case Mul(l, r) =>
      freeVars(l) ++ freeVars(r)
    case Let(defs, body) =>
      val bodyFreeVars = freeVars(body)

      val varsBoundByDefs = for {
        (x, xDef) <- defs
      } yield x

      val defsFreeVars = for {
        (x, xDef) <- defs
        freeVar <- freeVars(xDef)
      } yield freeVar

      bodyFreeVars -- varsBoundByDefs ++ defsFreeVars
  }

  def eval(e: Exp): Int = e match {
    case Num(n)    => n
    case Id(x)     => sys.error("unbound variable: " + x.name)
    case Add(l, r) => eval(l) + eval(r)
    case Mul(l, r) => eval(l) * eval(r)

    //Implement evaluation for task 1.
    case Let((x, xDef) :: defs, body) =>
      eval(Let(defs, subst(body, x, Num(eval(xDef)))))
    case Let(Nil, body) => eval(body)
  }
}
