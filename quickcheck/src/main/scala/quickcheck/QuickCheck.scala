package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const[H](QuickCheckHeap.this.empty),
    for {
      i <- Arbitrary.arbitrary[Int]
      h <- genHeap
    } yield insert(i, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int) =>
    deleteMin(insert(a, empty)) == empty
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    deleteMin(insert(m, h)) == h
  }

  property("gen3") = forAll { (h: H, a1: A, a2: A) =>
    deleteMin(insert(a2, deleteMin(insert(a1, h)))) == h
  }

  property("gen4") = forAll { (h: H, a1: A, a2: A) =>
    insert(a2, deleteMin(insert(a1, h))) == insert(a2, h)
  }

  property("gen5") = forAll { (h: H, a1: A, a2: A) =>
    insert(a2, insert(a1, h)) == insert(a1, insert(a2, h))
  }

  property("gen5") = forAll { (h: H) =>
    insert(1, insert(2, h)) == insert(2, insert(1, h))
  }

  property("empty gen1") = forAll { (h: H) =>
    isEmpty(h) ==>
      throws(classOf[NoSuchElementException])(findMin(h))
  }

  property("empty gen2") = forAll { (h: H) =>
    isEmpty(h) ==>
      throws(classOf[NoSuchElementException])(deleteMin(h))
  }

  property("empty gen3") = forAll { (h: H, a: A) =>
    isEmpty(h) ==>
      throws(classOf[NoSuchElementException])(deleteMin(deleteMin(insert(a, h))))
  }

  property("meld1") = forAll { (h1: H, h2: H, a1: A, a2: A) =>

    val b1 = findMin(insert(a1, h1))
    val b2 = findMin(insert(a2, h2))

    findMin(meld(insert(a1, h1), insert(a2, h2))) == Math.min(b1, b2)
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    findMin(meld(insert(Integer.MIN_VALUE, h1), h2)) == Integer.MIN_VALUE
  }

}
