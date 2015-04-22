package quickcheck

import common._

import math.min
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == min(a, b)
  }

  property("delete_min") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("sorted") = forAll { th: H => 
    val min_list = getMinList(h = th)
    min_list._1 == min_list._1.sorted && isEmpty(min_list._2)
  }

  property("min_meld") = forAll { (ha: H, hb: H) =>
    val mina = findMin(ha)
    val minb = findMin(hb)
    findMin(meld(ha, hb)) == min(mina, minb)
  }

  property("no_change") = forAll { l: List[Int] => 
    val min_list = getMinList(h = createHeap(l))
    l.sorted == min_list._1.sorted
  }

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    m <- oneOf(const(empty), genHeap)
  } yield insert(x, m)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  lazy val genList: Gen[List[Int]] = for {
    x <- arbitrary[Int]
    m <- oneOf(const(List()), genList)
  } yield m :+ x
  implicit lazy val arbList: Arbitrary[List[Int]] = Arbitrary(genList)

  def getMinList(l: List[A] = List(), h: H) : (List[A], H) = {
    h match {
      case hh::ht => getMinList(l :+ findMin(h), deleteMin(h))
      case _ => (l, h)
    }
  }

  def createHeap(l: List[A]) : H = l match {
    case Nil => throw new NoSuchElementException("Cannot create heap from empty list")
    case l::Nil => insert(l, empty)
    case l::ls => insert(l, createHeap(ls))
  }
}
