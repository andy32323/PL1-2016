sealed trait Exp
case class UnitCircle() extends Exp
case class UnitSquare() extends Exp
case class Union(e1: Exp, e2: Exp) extends Exp
case class Complement(e: Exp) extends Exp
case class Scale(x: Double, y: Double, e: Exp) extends Exp
case class Move(x: Double, y: Double, e: Exp) extends Exp
def intersect(e1: Exp, e2: Exp) : Exp = Complement(Union(Complement(e1),Complement(e2)))

val test = Complement(Scale(2,2,UnitCircle()))

def interp(e: Exp) : (Double,Double) => Boolean = e match {
  case UnitCircle() => (x,y) => x*x + y*y <= 1
  case UnitSquare() => (x,y) => (-1 <= x) && (x <= 1) && (-1 <= y) && (y <= 1)
  case Union(e1,e2) => (x,y) => interp(e1)(x,y) || interp(e2)(x,y)
//  case Intersect(e1,e2) => (x,y) => interp(e1)(x,y) && interp(e2)(x,y)
  case Complement(e1) => (x,y) => ! interp(e1)(x,y)
  case Scale(a,b,e1) => (x,y) => interp(e1)(x/a,y/b)
  case Move(a,b,e1) => (x,y) => interp(e1)(x-a, y-b)
}

case class ExpVisitor[T](c : T, s: T, union: (T,T) => T, compl: T => T,
                  scale: (Double,Double,T) => T, move: (Double,Double,T) => T)
                  
def visit[T](e: Exp, v: ExpVisitor[T]) : T = e match {
  case UnitCircle() => v.c
  case UnitSquare() => v.s
  case Union(e1,e2) => v.union(visit(e1,v), visit(e2,v))
  case Complement(e1) => v.compl(visit(e1,v))
  case Scale(a,b,e1) => v.scale(a,b,visit(e1,v))
  case Move(a,b,e1) => v.move(a,b,visit(e1,v))
} 

val interpv = ExpVisitor[(Double,Double) => Boolean](
  (x,y) => x*x + y*y <= 1,
  (x,y) => (-1 <= x) && (x <= 1) && (-1 <= y) && (y <= 1),
  (e1,e2) => (x,y) => e1(x,y) || e2(x,y),
  e => (x,y) => !e(x,y),
  (a,b,e) => (x,y) => e(x/a,y/b),
  (a,b,e) => (x,y) => e(x-a, y-b)
)  

def eval2(e: Exp) = visit(e,interpv)              