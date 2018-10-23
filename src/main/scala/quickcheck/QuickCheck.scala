package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import scala.util.Random

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      item <- arbitrary[Int]
      heap <- genHeap
    } yield insert(item, heap)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("meld") = forAll { (a: List[Int], b: Int, c: Int) =>
    val totalList = c :: b :: a
    val (xs, ys) = Random.shuffle(totalList).splitAt(Math.floor(totalList.length / 2).toInt)
    val firstHeap = xs.foldLeft(empty)((heap, x) => insert(x, heap))
    val secondHeap = ys.foldLeft(empty)((heap, x) => insert(x, heap))
    val meldedHeap = meld(firstHeap, secondHeap)
    val min = totalList.min
    findMin(meldedHeap) == min
    // handle if the lowest value is the same Int (i.e. 1 and 1), this filtering wont work
    val (before, _ :: withoutFirstMin) = totalList span (x => x != min)
    findMin(deleteMin(meldedHeap)) == (before ++ withoutFirstMin).min
  }
}
