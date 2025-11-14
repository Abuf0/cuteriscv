package mainelib
import spinal.core._
import spinal.lib._   // IMasterSlave

/** 通用流水段：
 * - 自动抓取任意 Bundle 作为负载（保持层级/字段不变）
 * - 输出同结构的 out
 * - 带 out_valid，同步走
 * - 支持 stall / flush；flush 可选恢复到初值
 *
 * 用法示例见文末
 */



//class Pipeline[T <: Bundle](val payloadType: HardType[T], val initOpt: Option[T] = None) extends Bundle {
//  val in = payloadType() // 任意 Bundle，结构自带
//  val in_valid = Bool()
//  val stall = Bool()
//  val flush = Bool()
//  val out = payloadType() // 同结构输出
//  val out_valid = Bool()
//
//  /** 在组件上下文中调用：生成寄存器与控制逻辑 */
//  def build(): Unit = {
//    // 数据寄存器（整包寄存 -> 各字段分别寄存）
//    val pack = initOpt match {
//      case Some(initV) => Reg(payloadType()) init (initV)
//      case None => Reg(payloadType())
//    }
//    // 有效位寄存
//    val valid = RegInit(False)
//
//    // flush > (!stall) 更新 > stall保持
//    when(flush) {
//      initOpt.foreach(pack := _)
//      valid := False
//    } .elsewhen (!stall) {
//      pack := in
//      valid := in_valid
//    } // stall=1 时保持
//
//    out := pack
//    out_valid := valid
//  }
//}

class Pipeline[T <: Bundle](val payloadType: HardType[T], val initOpt: Option[T] = None) extends Bundle {
  val in = payloadType()
  val in_valid = Bool()
  val stall = Bool()
  val flush = Bool()
  val out = payloadType()
  val out_valid = Bool()

  def build(): Unit = {
    // 定义复位值：优先使用 initOpt，否则生成全 0 实例
    val resetValue = initOpt.getOrElse {
      // 创建 payloadType 的实例并递归清零
      val zeroInst = payloadType()
      def zeroOut(data: Data): Unit = data match {
        case bundle: Bundle =>
          bundle.elements.foreach { case (_, elem) => zeroOut(elem) }
        case vec: Vec[_] =>
          vec.foreach(zeroOut)
        case uint: UInt =>
          uint := 0
        case sint: SInt =>
          sint := 0
        case bool: Bool =>
          bool := False
        // 其他类型：通过克隆类型并置零解决
        case other =>
          other := other.clone().asInstanceOf[Data].getZero
      }
      zeroOut(zeroInst)
      zeroInst
    }

    // 数据寄存器：用复位值初始化
    val pack = RegInit(resetValue)
    // 有效位寄存器：复位为false
    val valid = RegInit(False)

    // 控制逻辑：flush优先，其次更新，stall保持
    when(flush) {
      // flush时恢复为复位值，输出无效
      pack := resetValue
      valid := False
    } .elsewhen (!stall) {
      // 无stall时，更新数据和有效位
      pack := in
      valid := in_valid
    }  // stall时保持当前状态

    // 输出直接连接寄存器（复位时输出复位值）
    out := pack
    out_valid := valid
  }
}

object Pipeline {
  /** 语法糖：传入端口信号，内部连线并调用 build()，返回 (out, out_valid) */
  def apply[T <: Bundle](
                          payloadType: HardType[T],
                          initOpt    : Option[T]
                        )(
                          in      : T,
                          in_valid: Bool,
                          stall   : Bool,
                          flush   : Bool
                        ): (T, Bool) = {
    val p = new Pipeline(payloadType, initOpt)
    p.in       := in
    p.in_valid := in_valid
    p.stall    := stall
    p.flush    := flush
    p.build()                   // <—— 显式调用 build()
    //printPorts(p, "Pipeline Ports") // 如果需要打印出pipe payload
    (p.out, p.out_valid)
  }

  // 便捷重载：不传 init 的版本
  def apply[T <: Bundle](payloadType: HardType[T])(in: T, in_valid: Bool, stall: Bool, flush: Bool): (T, Bool) =
    apply(payloadType, None)(in, in_valid, stall, flush)

  /** 递归打印 Data（Bundle/Vec/标量）的层级与位宽 */
  private def dumpData(prefix: String, d: Data): Unit = {
    d match {
      case b: Bundle =>
        // Bundle.elements 是保持顺序的 Map[String, Data]
        b.elements.foreach { case (name, sub) => dumpData(s"$prefix.$name", sub) }

      case v: Vec[_] =>
        v.zipWithIndex.foreach { case (e, i) => dumpData(s"$prefix[$i]", e) }

      case leaf =>
        // widthOf 在 elaboration 期可用；Bool=1，Bits/UInt/SInt=位宽
        val w = widthOf(leaf)
        SpinalInfo(f"${prefix}%-40s : ${w}%d bits")
    }
  }

  /** Service 1：打印接口信息（展开期/编译期用） */
  def printPorts[T <: Bundle](p: Pipeline[T], title: String = "Pipeline Ports"): Unit = {
    SpinalInfo(s"========== $title ==========")
    dumpData("in",  p.in)
    SpinalInfo("------------------------")
    dumpData("out", p.out)
    SpinalInfo("------------------------")
    SpinalInfo(f"${"in_valid"}%-40s : 1 bit")
    SpinalInfo(f"${"out_valid"}%-40s : 1 bit")
    SpinalInfo(s"----")
    SpinalInfo(f"${"stall"}%-40s : 1 bit")
    SpinalInfo(f"${"flush"}%-40s : 1 bit")
    SpinalInfo(s"==============================")
  }
}

/*
class Top extends Component {
  class Payload extends Bundle {
    val pc  = UInt(32 bits)
    val rd  = UInt(5 bits)
    val imm = Bits(32 bits)
  }

  val io = new Bundle {
    val i  = in(new Payload)
    val iv = in Bool()
    val st = in Bool()
    val fl = in Bool()
    val o  = out(new Payload)
    val ov = out Bool()
  }

  val (o, ov) = Pipeline(HardType(new Payload))(io.i, io.iv, io.st, io.fl) // 内部已调用 build()
  io.o  := o
  io.ov := ov
}

*/

/*

class Top extends Component {
  // 1) 定义负载类型
  class Payload extends Bundle {
    val pc  = UInt(32 bits)
    val rd  = UInt(5 bits)
    val imm = Bits(32 bits)
  }

  // 2) 实例化 Pipeline Bundle（可选给一个初始化值）
  val init = {
    val x = new Payload
    x.pc := 0; x.rd := 0; x.imm := 0
    x
  }
  val pipe = new Pipeline(HardType(new Payload), Some(init))

  // 3) 声明到顶层 IO（示例）
  val io = new Bundle {
    val i   = in(cloneOf(pipe.in))
    val iv  = in Bool()
    val st  = in Bool()
    val fl  = in Bool()
    val o   = out(cloneOf(pipe.out))
    val ov  = out Bool()
  }

  // 4) 连接输入
  pipe.in       := io.i
  pipe.in_valid := io.iv
  pipe.stall    := io.st
  pipe.flush    := io.fl

  // 5) 生成流水段逻辑
  pipe.build()

  // 6) 导出输出
  io.o  := pipe.out
  io.ov := pipe.out_valid
}

*/

