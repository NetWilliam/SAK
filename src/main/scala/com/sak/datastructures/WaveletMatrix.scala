import scala.collection.mutable.ArrayBuffer

object WaveletMatrix {
  val maxlog: Int = 63  // Since we're using Long, which is 64 bits
}

class WaveletMatrix(v: ArrayBuffer[Long]) {
  import WaveletMatrix._

  private val len: Int = v.length
  private val matrix: Array[BitVector] = Array.fill(maxlog)(new BitVector(len + 1))
  private val mid: Array[Int] = new Array[Int](maxlog)
  private val sum: Array[ArrayBuffer[Long]] = Array.fill(maxlog)(ArrayBuffer.fill(len + 1)(0L))

  // Constructor logic
  {
    var vec = v.clone()
    for (level <- maxlog - 1 to 0 by -1) {
      val (l, r) = (ArrayBuffer[Long](), ArrayBuffer[Long]())
      var left = 0
      var right = 0
      for (i <- 0 until len) {
        if ((vec(i) >> level & 1) != 0) {
          matrix(level).set(i)
          r += vec(i)
          right += 1
        } else {
          l += vec(i)
          left += 1
        }
      }
      mid(level) = left
      matrix(level).build()
      vec = l ++ r
      sum(level)(0) = 0L
      for (i <- 0 until len) {
        sum(level)(i + 1) = sum(level)(i) + vec(i)
      }
    }
  }

  private def succ(f: Boolean, l: Int, level: Int): Int = {
    matrix(level).rank(if (f) 1 else 0, l) + (if (f) mid(level) else 0)
  }
  def access(k: Int): Long = {
    require(0 <= k && k < len, "Index out of bounds")
    var res = 0L
    var kk = k
    for (level <- maxlog - 1 to 0 by -1) {
      val f = matrix(level)(kk) != 0
      if (f) res |= 1L << level
      kk = succ(f, kk, level)
    }
    res
  }

  def apply(k: Int): Long = access(k)

  def rank(x: Long, r: Int): Int = {
    require(0 <= r && r <= len, "Index out of bounds")
    var (l, rr) = (0, r)
    for (level <- maxlog - 1 to 0 by -1) {
      val bit = ((x >> level) & 1) != 0
      l = succ(bit, l, level)
      rr = succ(bit, rr, level)
    }
    rr - l
  }

  def kthSmallest(l: Int, r: Int, k: Int): Long = {
    require(0 <= l && l <= r && r <= len && 0 <= k && k < r - l, "Invalid range or k")
    var res = 0L
    var (ll, rr, kk) = (l, r, k)
    for (level <- maxlog - 1 to 0 by -1) {
      val cnt = matrix(level).rank(0, rr) - matrix(level).rank(0, ll)
      val f = cnt <= kk
      if (f) {
        res |= 1L << level
        kk -= cnt
      }
      ll = succ(f, ll, level)
      rr = succ(f, rr, level)
    }
    res
  }

  def kthLargest(l: Int, r: Int, k: Int): Long = kthSmallest(l, r, r - l - k - 1)
  def kthSmallestSum(l: Int, r: Int, k: Int): Long = {
    require(0 <= l && l <= r && r <= len && 0 <= k && k <= r - l, "Invalid range or k")
    var res = 0L
    var (ll, rr, kk) = (l, r, k)
    for (level <- maxlog - 1 to 0 by -1) {
      val l0 = matrix(level).rank(0, ll)
      val r0 = matrix(level).rank(0, rr)
      if (kk < r0 - l0) {
        ll = l0
        rr = r0
      } else {
        kk -= r0 - l0
        res += sum(level)(r0) - sum(level)(l0)
        ll += mid(level) - l0
        rr += mid(level) - r0
      }
    }
    if (kk > 0) res += sum(0)(ll + kk) - sum(0)(ll)
    res
  }

  def kthLargestSum(l: Int, r: Int, k: Int): Long = {
    require(0 <= l && l <= r && r <= len && 0 <= k && k <= r - l, "Invalid range or k")
    kthSmallestSum(l, r, r - l) - kthSmallestSum(l, r, r - l - k)
  }

  def rangeFreq(l: Int, r: Int, upper: Long): Int = {
    var res = 0
    var (ll, rr) = (l, r)
    for (level <- maxlog - 1 to 0 by -1) {
      val f = ((upper >> level) & 1) != 0
      if (f) res += matrix(level).rank(0, rr) - matrix(level).rank(0, ll)
      ll = succ(f, ll, level)
    rr = succ(f, rr, level)
    }
    res
  }

  def rangeFreq(l: Int, r: Int, lower: Long, upper: Long): Int = {
    require(0 <= l && l <= r && r <= len, "Invalid range")
    rangeFreq(l, r, upper) - rangeFreq(l, r, lower)
  }

  def prevValue(l: Int, r: Int, upper: Long): Long = {
    require(0 <= l && l <= r && r <= len, "Invalid range")
    val cnt = rangeFreq(l, r, upper)
    if (cnt == 0) -1L else kthSmallest(l, r, cnt - 1)
  }

  def nextValue(l: Int, r: Int, lower: Long): Long = {
    require(0 <= l && l <= r && r <= len, "Invalid range")
    val cnt = rangeFreq(l, r, lower)
    if (cnt == r - l) -1L else kthSmallest(l, r, cnt)
  }
}
