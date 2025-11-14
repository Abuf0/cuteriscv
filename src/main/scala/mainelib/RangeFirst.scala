package mainelib
import spinal.core._
import spinal.lib._

object RangeFirstDyn {

  /** 在 vec 内的 [start..end]（可环形）区间里，找“第一个 true”
   * - circular=true 允许跨 0 点（环形）；false 为线性区间
   * - preferEndNearest=true 从 end 向回找；false 从 start 向前找
   *
   * @return (found, index)
   */
  def firstTrueInRangeDyn(
                           vec: Vec[Bool],
                           start: UInt,
                           end:   UInt,
                           circular: Boolean = true,
                           preferEndNearest: Boolean = true
                         ): (Bool, UInt) = {
    val N = vec.length
    require(N >= 1, "vec length must be >= 1")
    val w = log2Up(N)

    // 0..N-1 常量索引
    val idxTab = Vec.tabulate(N)(k => U(k, w bits))

    // 计算每个 idx 是否在区间内，并与 vec 结合为候选
    val startLEend = start <= end
    val inRange = Vec(Bool(), N)
    for (k <- 0 until N) {
      val idx = idxTab(k)
      val geS = idx >= start
      val leE = idx <= end
      val inLinear = geS && leE         // 非环
      val inWrap   = geS || leE         // 环：跨 0 点
      val allowed  = if(circular) (startLEend ? inLinear | inWrap) else inLinear
      inRange(k) := allowed && vec(k)
    }

    // 扫描顺序：从 end 向回，或从 start 向前（均按 N 模环）
    val order = Vec(UInt(w bits), N)
    if (preferEndNearest) {
      for (t <- 0 until N) order(t) := (end   - U(t, w bits)).resized
    } else {
      for (t <- 0 until N) order(t) := (start + U(t, w bits)).resized
    }

    // 依次取候选（动态索引只读，不产生多驱动）
    val cand = Vec(Bool(), N)
    for (t <- 0 until N) {
      val idx = order(t)
      cand(t) := inRange(idx)   // 动态读取 Vec 元素
    }

    // 前缀 OR：prefixSeen(t) = 在 t 之前是否见过 true
    val prefixSeen = Vec(Bool(), N)
    prefixSeen(0) := False
    for (t <- 1 until N) {
      prefixSeen(t) := prefixSeen(t-1) || cand(t-1)
    }

    // selected = one-hot 的“第一个命中”
    val selected = Vec(Bool(), N)
    for (t <- 0 until N) {
      selected(t) := cand(t) && !prefixSeen(t)
    }

    // found / index（用 one-hot 多路复用，绝无组合环/多驱动）
    val found = selected.asBits.orR
    val index = MuxOH(selected, order)   // spinal.lib 的 one-hot 多路复用

    (found, index)
  }
}
