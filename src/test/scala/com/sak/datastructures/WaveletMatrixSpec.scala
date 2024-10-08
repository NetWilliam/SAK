import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable.ArrayBuffer

class WaveletMatrixSpec extends AnyFlatSpec with Matchers {

  "WaveletMatrix" should "correctly initialize and access elements" in {
    val arr = ArrayBuffer(3L, 1L, 4L, 1L, 5L, 9L, 2L, 6L, 5L, 3L)
    val wm = new WaveletMatrix(arr)

    for (i <- arr.indices) {
      wm(i) should be (arr(i))
    }
  }

  it should "correctly find kth smallest element" in {
    val arr = ArrayBuffer(3L, 1L, 4L, 1L, 5L, 9L, 2L, 6L, 5L, 3L)
    val wm = new WaveletMatrix(arr)

    wm.kthSmallest(0, arr.length, 0) should be (1L)
    wm.kthSmallest(0, arr.length, 4) should be (3L)
    wm.kthSmallest(0, arr.length, 9) should be (9L)
  }

  it should "correctly find kth largest element" in {
    val arr = ArrayBuffer(3L, 1L, 4L, 1L, 5L, 9L, 2L, 6L, 5L, 3L)
    val wm = new WaveletMatrix(arr)

    wm.kthLargest(0, arr.length, 0) should be (9L)
    wm.kthLargest(0, arr.length, 4) should be (4L)
    wm.kthLargest(0, arr.length, 9) should be (1L)
  }

  it should "correctly calculate rank" in {
    val arr = ArrayBuffer(3L, 1L, 4L, 1L, 5L, 9L, 2L, 6L, 5L, 3L)
    val wm = new WaveletMatrix(arr)

    wm.rank(1L, arr.length) should be (2)
    wm.rank(3L, arr.length) should be (2)
    wm.rank(5L, arr.length) should be (2)
  }

  it should "correctly calculate range frequency" in {
    val arr = ArrayBuffer(3L, 1L, 4L, 1L, 5L, 9L, 2L, 6L, 5L, 3L)
    val wm = new WaveletMatrix(arr)

    wm.rangeFreq(0, arr.length, 1L, 5L) should be (6)
    wm.rangeFreq(0, arr.length, 3L, 7L) should be (6)
  }

  it should "correctly find prev and next values" in {
    val arr = ArrayBuffer(3L, 1L, 4L, 1L, 5L, 9L, 2L, 6L, 5L, 3L)
    val wm = new WaveletMatrix(arr)

    wm.prevValue(0, arr.length, 5L) should be (4L)
    wm.nextValue(0, arr.length, 5L) should be (5L)
  }

  it should "correctly calculate kth smallest and largest sum" in {
    val arr = ArrayBuffer(3L, 1L, 4L, 1L, 5L, 9L, 2L, 6L, 5L, 3L)
    val wm = new WaveletMatrix(arr)

    wm.kthSmallestSum(0, arr.length, 3) should be (4L) // 1 + 1 + 2
    wm.kthLargestSum(0, arr.length, 3) should be (20L) // 9 + 6 + 5
  }
}

