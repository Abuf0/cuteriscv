package mainelib
import spinal.core._
import spinal.lib._

/**  **/
case class instr_realign(cfg : CoreConfig) extends Component {
  val io = new Bundle {
    val pc_fl = slave(Flow(UInt(cfg.InstAddrBus bits)))  // from if2-tm
    val instr_fl = slave(Flow(UInt(cfg.IMemDataWidth bits)))  // from mmu(valid = valid | err)
    val instr_err = in Bool() // from mmu
    val instr_realign_fl0 = Vec.fill(cfg.issueWidth)(Vec.fill(cfg.InstSlice)(master(Flow(UInt(cfg.InstBus bits)))))
    val pc_realign_fl0 = Vec.fill(cfg.issueWidth)(Vec.fill(cfg.InstSlice)(master(Flow(UInt(cfg.InstAddrBus bits)))))
    val instr_err_fl0 = Vec.fill(cfg.issueWidth)(Vec.fill(cfg.InstSlice)(out Bool()))
  }

  val pc_slice = Vec.fill(cfg.InstStep)(Flow(UInt(cfg.InstAddrBus bits)))
  val instr_slice = Vec.fill(cfg.InstStep)(Flow(UInt(cfg.InstBus bits)))
  val instr_err_slice = Vec.fill(cfg.InstStep)(Bool())

  if(cfg.rvc) {
    val cross_flag_pre = Bool()
    cross_flag_pre := False
    val cross_flag = RegNextWhen(cross_flag_pre, io.instr_fl.valid) init(False)
    val instr_buff = RegNext(io.instr_fl.payload(cfg.IMemDataWidth-1 downto cfg.IMemDataWidth-16)) init(0)
    val pc_buff = RegNext(io.pc_fl.payload) init(0)
    val instr_err_buff = RegNext(io.instr_err) init(False)

    for (i <- 0 until cfg.InstStep) {
      val ptr_base = i * 16
      val ptr_base_last = (i-1) * 16
      pc_slice(i).payload := 0
      pc_slice(i).valid := False
      instr_slice(i).payload := 0
      instr_slice(i).valid := False
      instr_err_slice(i) := False
      if(i==0){ // lowest half-word
        when(cross_flag){
          instr_slice(i).payload := io.instr_fl.payload(ptr_base+15 downto ptr_base)@@instr_buff
          instr_slice(i).valid := True
          pc_slice(i).payload := pc_buff
          pc_slice(i).valid := True
          instr_err_slice(i) := io.instr_err || instr_err_buff
        } .elsewhen(io.instr_fl.valid && (io.instr_fl.payload(ptr_base + 1 downto ptr_base) =/= U"2'b11")){  // C
          instr_slice(i).payload := io.instr_fl.payload(ptr_base+15 downto ptr_base).resized
          instr_slice(i).valid := True
          pc_slice(i).payload := io.pc_fl.payload + i*2
          pc_slice(i).valid := True
          instr_err_slice(i) := io.instr_err
        } .elsewhen(io.instr_fl.valid && (io.instr_fl.payload(ptr_base + 1 downto ptr_base) === U"2'b11")){ // 32b
          instr_slice(i).payload := io.instr_fl.payload(ptr_base+cfg.xlen-1 downto ptr_base).resized
          instr_slice(i).valid := True
          pc_slice(i).payload := io.pc_fl.payload + i*4
          pc_slice(i).valid := True
          instr_err_slice(i) := io.instr_err
        }
      } else if(i==cfg.InstStep-1){ // highest half-word
        when(instr_slice(i-1).valid && (io.instr_fl.payload(ptr_base_last + 1 downto ptr_base_last) === U"2'b11")) { // as 32b MSB
          instr_slice(i).payload := 0
          instr_slice(i).valid := False
          pc_slice(i).payload := 0
          pc_slice(i).valid := False
          instr_err_slice(i) := io.instr_err
        } .elsewhen(io.instr_fl.valid && (io.instr_fl.payload(ptr_base + 1 downto ptr_base) =/= U"2'b11")) { // C
          instr_slice(i).payload := io.instr_fl.payload(ptr_base + 15 downto ptr_base).resized
          instr_slice(i).valid := True
          pc_slice(i).payload := io.pc_fl.payload + i*2
          pc_slice(i).valid := True
          instr_err_slice(i) := io.instr_err
        }.elsewhen(io.instr_fl.valid && (io.instr_fl.payload(ptr_base + 1 downto ptr_base) === U"2'b11")) { // 32b LSB but cross instr
          instr_slice(i).payload := 0
          instr_slice(i).valid := False
          pc_slice(i).payload := 0
          pc_slice(i).valid := False
          instr_err_slice(i) := io.instr_err
          cross_flag_pre := True
        }
      } else {
        when(instr_slice(i-1).valid && (io.instr_fl.payload(ptr_base_last + 1 downto ptr_base_last) === U"2'b11")) { // as 32b MSB
          instr_slice(i).payload := 0
          instr_slice(i).valid := False
          pc_slice(i).payload := 0
          pc_slice(i).valid := False
          instr_err_slice(i) := io.instr_err
        } .elsewhen(io.instr_fl.valid && (io.instr_fl.payload(ptr_base + 1 downto ptr_base) =/= U"2'b11")) { // C
          instr_slice(i).payload := io.instr_fl.payload(ptr_base + 15 downto ptr_base).resized
          instr_slice(i).valid := True
          pc_slice(i).payload := io.pc_fl.payload + i*2
          pc_slice(i).valid := True
          instr_err_slice(i) := io.instr_err
        }.elsewhen(io.instr_fl.valid && (io.instr_fl.payload(ptr_base + 1 downto ptr_base) === U"2'b11")) { // 32b
          instr_slice(i).payload := io.instr_fl.payload(ptr_base+cfg.xlen-1 downto ptr_base).resized
          instr_slice(i).valid := True
          pc_slice(i).payload := io.pc_fl.payload + i*4
          pc_slice(i).valid := True
          instr_err_slice(i) := io.instr_err
        }
      }
    }
  } else if(!cfg.rvc){
    for (i <- 0 until cfg.InstStep) {
      val ptr_base = i * 16
      instr_slice(i).payload := 0
      instr_slice(i).valid := False
      pc_slice(i).payload := 0
      pc_slice(i).valid := False
      instr_err_slice(i) := io.instr_err
      when(io.instr_fl.valid & io.pc_fl.valid){
        instr_slice(i).payload := io.instr_fl.payload(ptr_base+cfg.xlen-1 downto ptr_base).resized
        instr_slice(i).valid := True
        pc_slice(i).payload := io.pc_fl.payload + i*cfg.xlen/cfg.ByteWidth
        pc_slice(i).valid := True
        instr_err_slice(i) := io.instr_err
      }
    }
  }

  val instr_num = UInt(log2Up(cfg.InstStep+1) bits)
  val instr_num_last = RegNextWhen(instr_num, io.instr_fl.valid) init(0)
  val idx = Vec(UInt(log2Up(cfg.InstStep+1) bits), cfg.InstStep+1)
  idx(0) := 0
  for(i <- 0 until cfg.InstStep){
    idx(i+1) := idx(i) + instr_slice(i).valid.asUInt
  }
  instr_num := idx(cfg.InstStep)

  val instr_compact = Vec.fill(cfg.InstStep)(Flow(UInt(cfg.InstBus bits)))
  val pc_compact = Vec.fill(cfg.InstStep)(Flow(UInt(cfg.InstAddrBus bits)))
  val instr_err_compact = Vec.fill(cfg.InstStep)(Bool())

  for (i <- 0 until cfg.InstStep) {
    instr_compact(i).valid := False
    instr_compact(i).payload := 0
    pc_compact(i).valid := False
    pc_compact(i).payload := 0
    instr_err_compact(i) := False
  }
  /** 窗口滑动，紧凑排列进queue **/
  for (i <- 0 until cfg.InstStep) {
    when(instr_slice(i).valid){
      val pos = idx(i).resize(log2Up(cfg.InstStep))
      val pos_offset = UInt(log2Up(cfg.InstStep + 1) bits)
      val pos_os = pos_offset.resize(log2Up(cfg.InstStep))
      when(pos + instr_num_last < cfg.InstStep){
        pos_offset := pos + instr_num_last
      } .otherwise {
        pos_offset := pos + instr_num_last - cfg.InstStep
      }
      instr_compact(pos_os) := instr_slice(i)
      pc_compact(pos_os) := pc_slice(i)
      instr_err_compact(i) := instr_err_slice(i)
    }
  }

  for (i <- 0 until cfg.issueWidth) {
    for (j <- 0 until cfg.InstSlice) {
      io.instr_realign_fl0(i)(j).valid := False
      io.instr_realign_fl0(i)(j).payload := 0
      io.pc_realign_fl0(i)(j).valid := False
      io.pc_realign_fl0(i)(j).payload := 0
      io.instr_err_fl0(i)(j) := False
      val instr_id = i*j
      when(instr_id < instr_num && io.instr_fl.valid){
        io.instr_realign_fl0(i)(j) := instr_compact(j*cfg.issueWidth+i)
        io.pc_realign_fl0(i)(j) := pc_compact(j*cfg.issueWidth+i)
        io.instr_err_fl0(i)(j) := instr_err_compact(j*cfg.issueWidth+i)
      }
    }
  }

}

