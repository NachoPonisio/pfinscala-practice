sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  //3.25
  def size[A](tr: Tree[A]): Int = {
    tr match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  //3.26

  def maximum(tr: Tree[Int]): Int = {
     def _max(tree: Tree[Int], acc: Int = 0): Int =
       tree match {
          case Leaf(v) => v
          case Branch(l,r) => _max(l) max _max(r)
       }

    _max(tr)
  }

  /*
  Example for val sampleTree = Branch(Branch(Leaf(1), Leaf(4)), Leaf(70))
  _max(Branch(Leaf(1), Leaf(4))) max _max(Leaf(70) =
  (_max(Leaf(1)) max _max(Leaf(4))) max 70 =
  (1 max 4) max 70
  4 max 70
  70
   */

 //3.27
  def depth[A](tr: Tree[A]): Int =
    tr match {
        case Leaf(_) => 0
        case Branch(l,r) => 1 + (depth(l) max depth(r))
    }

}

val testTree: Tree[Int] =  Branch(
                              Branch(
                                  Branch(
                                      Leaf(1),
                                      Leaf(1)
                                  )
                                  , Leaf(1)
                              ),Leaf(4)
                            )

val testTree2: Tree[Int] =  Branch(
  Branch(
    Branch(
      Leaf(1),
      Leaf(1)
    )
    , Branch(
        Leaf(1),
        Leaf(45)
    )
  ),Leaf(1)
)


Tree.size(testTree)

Tree.maximum(testTree2)
Tree.maximum(testTree)
Tree.size(testTree2)