import com.sun.xml.internal.bind.v2.runtime.unmarshaller.XsiNilLoader

import scala.math.Numeric.BigDecimalAsIfIntegral.mkOrderingOps


sealed trait List[+A] //+A means A is covariant to List, meaning if B sub A -> List[B] sub List[A]
//trait -> abstract interface
// | case implementations
case object Nil extends List[Nothing] //Nothing is a subtype of all types
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List{

  def apply[A](a: A*): List[A] = { //A* variadic parameter, function accepts zero or more arguments.
    if (a.isEmpty) Nil else Cons(a.head, apply(a.tail: _*)) //_* is the way to pass a variadic parameter recursively
  }

  def tail[A](ls: List[A]): List[A] = {
    ls match {
      case Nil => sys.error("Tail on an empty list")
      case Cons(x, t) => t
    }
  }
  /*************************************************/
//3.4
  def drop[A](ls: List[A], n: Int): List[A] = {
    ls match {
        case Nil => Nil
        case Cons(x, Nil) => { if(n == 1) Nil else sys.error("n greater than size of list")}
        case Cons(x, t) => { if (n == 1) t else drop(t, n - 1) }
    }
  }

  def head[A](ls: List[A]): A = {
      ls match {
        case Nil => ???
        case Cons(x, _) => x
      }

  }
  /*************************************************/
//3.3
  def setHead[A](a: A, ls: List[A]): List[A] ={
    ls match {
        case Nil => sys.error("Set head on an empty list")
        case Cons(_, t) => Cons(a, t)
    }
  }

  /*************************************************/
  //3.5
  def dropWhile[A](lst: List[A])(p: A => Boolean): List[A] = {
      lst match {
        case Cons(h, xs) => if (p(h)) dropWhile(xs)(p) else lst
        case _ => lst
      }
  }
  /*************************************************/
  //3.9
  /** First some review of the generalization of foldRight */

    def sum(lst: List[Int]): Int = {
      lst match {
          case Nil => 0
          case Cons(h, t) => h + sum(t)
      }
    }

    def product(lst: List[Double]): Double = {
        lst match {
            case Nil => 1
            case Cons(h, t) => h * product(t)
        }
    }

  def foldRight[A, B](lst: List[A], neutral: B)(f: (A, B) => B): B = {
    lst match {
        case Nil => neutral
        case Cons(h, t) => f(h, foldRight(t, neutral)(f))
    }
  }


  def length[A](lst: List[A]): Int = {
    foldRight(lst, 0)((elem, x) => x + 1)
  }

  //3.10
   @annotation.tailrec
  def foldLeft[A,B](lst: List[A], n: B)(f: (B,A) => B): B = {
    lst match {
        case Nil => n
        case Cons(h,t) => foldLeft(t, f(n, h))(f)
    }
  }

  //3.11

  /* FoldLeft for sum

    List = (1,2,3)

    sum = foldLeft(lst,0)(x,y)=>(x+y)

    foldLeft((1,2,3), 0)(f) = foldLeft((2,3), 1 + 0)(f) = foldLeft((3), 2 + 1 + 0)(f) = foldLeft(Nil, 3 + 2 + 1 + 0)(f) = 3 + 2 + 1 + 0


    FoldRight for sum

    sum = foldRight(lst, 0)((x ,y) => x + y)
    foldRight((1, 2, 3), 0)(f) = 1 + foldRight((2, 3), 0)(f) = 1 + 2 + foldRight((3), 0)(f) = 1 + 2 + 3 + foldRight(Nil, 0)(f) = 1 + 2 + 3 + 0


  */
  def sumLeft(lst: List[Int]): Int = {
    foldLeft(lst, 0)((x,y) => x + y)
  }

  def productLeft(lst:List[Int]): Int = {
    foldLeft(lst,1)((x,y) => x * y)
  }

  def lenLeft[A](lst: List[A]): Int = {
    foldLeft(lst, 0)((y,_) => y + 1)
  }

  //3.12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))

  /*
  * lst = List(1,2,3)
  *
  * reverseLeft(1,2,3) = foldLeft((1,2,3), new List[A])(g) = foldLeft((2,3), Cons(1, new List[A]))(g) = foldLeft((3), Cons(2, Cons(1, new List[A]))(g) = ...
  *
  *
  *
  * */

  //3.13
  /*
    def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  foldLeft(List(1,2,3), 0)(_ + _)

   */

  //3.14

  /*
  Evaluation of left = List(1,2,3) and right = List(4,5,6)

    def foldRight[A, B](lst: List[A], neutral: B)(f: (A, B) => B): B = {
    lst match {
        case Nil => neutral
        case Cons(h, t) => f(h, foldRight(t, neutral)(f))
    }
  }

  Cons(1, foldRight((2,3), (4,5,6))((x,y) => Cons(x,y)) =
  Cons(1, Cons(2, foldRight((3), (4,5,6))(f) =
  Cons(1, Cons(2, Cons(3, foldRight(Nil, (4,5,6))(f) =
  Cons(1, Cons(2, Cons(3, (4,5,6)))

   */
  def append[A](left: List[A], right: List[A]): List[A] = {
    foldRight(left, right)((h, n) => Cons(h, n))
  }


  //3.15
  def concatenate[A](lsts: List[List[A]]): List[A] = {
//    lsts match {
//      case Nil => Nil
//      case Cons(h,t) => append(h, concatenate(t))
//    }

    //we reuse the recursion pattern from foldRight
    foldRight(lsts, Nil: List[A])(append)
  }

  //3.16
  def increment(lst: List[Int]): List[Int] = {
    foldRight(lst, Nil: List[Int])((x, y) => Cons((x + 1), y))
  }

  //3.17

  def stringify(lst: List[Double]): List[String] = {
    foldRight(lst, Nil: List[String])((h,t) => (Cons(h.toString, t)))
  }

  //3.18

  def map[A,B](lst: List[A])(f: A => B): List[B] = {
    foldRight(lst, Nil: List[B])((h,t) => Cons(f(h), t))
  }


  //3.19
  def filter[A](lst: List[A])(f: A => Boolean): List[A] =
    foldRight(lst, Nil: List[A])((h,t) => if(f(h)) Cons(h,t) else t)

  //3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = ???






}


val y = List(1,2,3,4,5)
val x = List.tail(y)
List.head(y)
List.setHead(9, y)
List.drop(y, 4)
//List.dropWhile(y, (x: Int) => x < 3)
List.dropWhile(y)(x => x < 5)

List.foldRight(y, 0)((x,y) => x+y)
List.sumLeft(y)
List.productLeft(y)

List.lenLeft(y)

List.append(y, x)

val a = List(1,2,3)
val b = List(4,5,6)
val c = List(7,8,9)

val lol = List(a,b,c)

List.concatenate(lol)

List.increment(c)

val d = List(1.0, 2.0, 3.0)

List.stringify(d)
List.map(y)(x => x + 1)
List.filter(y)(a => a <= 3)

