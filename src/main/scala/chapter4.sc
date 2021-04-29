import scala.::

//4.1
trait Option[+A]{ //Option is a list of at most one parameter
  def map[B] (f: A => B): Option[B] = //apply f if the Option is not none
    this match {
      case Some(a) => Some(f(a))
      case None => None
    }

  def flatMap[B] (f: A => Option[B]): Option[B] =  //apply f, which may fail, to the Option if not None
    this map f getOrElse(None)

  def flatMap_2[B](f: A => Option[B]): Option[B] =
    this match{
      case Some(a) => f(a)
      case _ => None
    }

  def getOrElse[B >: A] (default: => B): B =  // return a default value which will not be evaluated unless needed
    this match {
      case Some(a) => a
      case None => default
    }

  def orElse[B >: A] (ob: => Option[B]): Option[B] =  //return an ob value which is an option value in itself
    this match {
      case Some(a) => Some(a)
      case None => ob
    }

  //An elegant way to implement orElse from the previously defined functions
  def orElse_1[B >:A] (ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse(ob)

  def filter (f: A => Boolean): Option[A] =  //convert Some to None if the value does not satisfy f
    this match {
      case Some(a) if f(a) => Some(a)
      case _ => None
    }


  //4.3
  def map2 [A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap( aa => (b map (bb => f(aa, bb))))
  //Map2 allows to lift a binary function using the flatmap -> map pattern
  //Patterns notes: flatMap applies a function that returns an option as a first step
  // in case of the binary function, it actually returns not an Option, but a function that takes in Option[B] as
  // a first parameter and returns  f(a: A, b: B): C, which is translated to Some[C] due to the application of map, hence:
  // a flatMap (g): Option[B] where g(a): A => Option[B]. But in this binary form, g(a): A => (Option[B] => Option[C]), thus the
  // complete substitution results in Option[A] => Option[B] => Option[C]


  //4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match{
      case h :: t =>
    }

}
case class Some[+A] (get: A) extends Option[A]
case object None extends Option[Nothing]

object chapter4 {
  def mean(xs: Seq[Double]): Option[Double] = {
    xs match {
      case e if e.isEmpty => None
      case _ => Some(xs.sum/xs.length)
    }
  }

  //4.2
  def variance(xs: Seq[Double]): Option[Double] = {  //when might variance not be defined?
    mean(xs) flatMap( m => mean(xs map ( e => math.pow(e - m, 2))))


  }
}