package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(x, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("Min after a and b inserted to empty is min(a, b)") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == math.min(a, b)
  }

  property("Insert into empty and deleteMin results in empty heap") = forAll { (a: Int) =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2)
  }

  property("Recursively finding and deleting mins in a heap should return sorted elements") = forAll { (h: H) =>
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        val h2 = deleteMin(h)
        isEmpty(h2) || (m <= findMin(h2) && isSorted(h2))
      }

    isSorted(h)
  }

  property("Finding min of two melded results is min of h1 or min of h2") = forAll { (h1: H, h2: H) =>
    val h3 = meld(h1, h2)
    val mins = List(findMin(h1), findMin(h2))
    mins.contains(findMin(h3))
  }

  property("Finding a minimum of heap melded with empty heap should return minimum of the heap") =
    forAll { (h: H) =>
      val m = findMin(h)
      val h1 = meld(h, empty)
      val h2 = meld(empty, h)
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      (m == m1) && (m == m2)
    }

  property("Finding min of two melded identical heaps should return a minimum of one or the other") =
    forAll { (h: H) =>
      val m = findMin(h)
      val h1 = meld(h, h)
      val m1 = findMin(h1)
      m == m1
    }

  property("Two heaps should be equal if recursively removing min elements until empty") =
    forAll { (h1: H, h2: H) =>
      def heapEqual(h1: H, h2: H): Boolean =
        if (isEmpty(h1) && isEmpty(h2)) true
        else {
          val m1 = findMin(h1)
          val m2 = findMin(h2)
          m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
        }

      heapEqual(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
    }
}
