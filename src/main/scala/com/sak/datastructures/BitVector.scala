import scala.collection.mutable.ArrayBuffer

class BitVector(val len: Int) {
  private val w = 32
  private def rem(k: Int): Int = k & (w - 1)
  private val blocks: Int = (len + w - 1) / w
  private val bit: ArrayBuffer[Int] = ArrayBuffer.fill(blocks)(0)
  private val sum: ArrayBuffer[Int] = ArrayBuffer.fill(blocks)(0)

  def set(k: Int, b: Int = 1): Unit = {
    if (b != 0)
      bit(k / w) |= 1 << rem(k)
    else
      bit(k / w) &= ~(1 << rem(k))
  }

  def build(): Unit = {
    sum(0) = 0
    for (i <- 1 until blocks) {
      sum(i) = sum(i - 1) + Integer.bitCount(bit(i - 1))
    }
  }

  def apply(k: Int): Int = (bit(k / w) >> rem(k)) & 1

  def rank(k: Int): Int = {
    sum(k / w) + Integer.bitCount(bit(k / w) & ((1 << rem(k)) - 1))
  }

  def rank(value: Int, k: Int): Int = if (value != 0) rank(k) else k - rank(k)
}
