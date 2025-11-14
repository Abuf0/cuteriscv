package mainelib
import spinal.core._
import spinal.lib._   // IMasterSlave

object FFONE {
  case class Hit(has: Bool, idx: UInt, oh: Bits)

  /** 对 Bits 做优先编码，返回是否命中/索引/one-hot */
  def first(bits: Bits): Hit = {
    val oh  = OHMasking.first(bits)   // 只保留最低位的1
    val has = oh.orR                  // 是否存在为1
    val idx = OHToUInt(oh)            // 索引
    Hit(has, idx, oh)
  }

  /** 对 Seq[Bool] 做优先编码（内部自动转 Bits） */
  def first(bools: Seq[Bool]): Hit = first(B(bools))
}