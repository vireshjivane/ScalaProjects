package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._
import scala.collection.JavaConverters._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    for {
      key <- arbitrary[A]
      value <- frequency((1, Gen.const(empty)), (5, genHeap))
    } yield insert(key, value)
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("getMinVal") = {
    forAll { (p: A, v: A) =>
      findMin(insert(p, insert(v, empty))) == min(p, v)
    }
  }

  property("checkEmpty") = {
    forAll { (v: A) =>
      isEmpty(deleteMin(insert(v, empty)))
    }
  }

  property("sorted") = {
    forAll { (heapOne: H) =>
      def isSorted(heapOne: H): Boolean =
        isEmpty(heapOne) match {
          case true => true
          case false => {
            if (isEmpty(deleteMin(heapOne)) || (findMin(heapOne) <= findMin(deleteMin(heapOne)) && isSorted(deleteMin(heapOne)))) true else false
          }
        }
      isSorted(heapOne)
    }
  }

  property("meldHeap") = {
    forAll { (heapOne: H, heapTwo: H) =>
      def heapEqual(heapOne: H, heapTwo: H): Boolean = {
        isEmpty(heapOne) && isEmpty(heapTwo) match {
          case true => true
          case false =>
            findMin(heapOne) == findMin(heapTwo) && heapEqual(deleteMin(heapOne), deleteMin(heapTwo))
        }
      }
      heapEqual(meld(heapOne, heapTwo), meld(deleteMin(heapOne), insert(findMin(heapOne), heapTwo)))
    }
  }

}