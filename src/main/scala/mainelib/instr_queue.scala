package mainelib
import spinal.core._
import spinal.lib._
/**  **/
case class instr_queue(cfg : CoreConfig) extends Component {
  val io = new Bundle {
    val instr_realign_fl0 = Vec.fill(cfg.issueWidth)(Vec.fill(cfg.InstSlice)(slave(Flow(UInt(cfg.InstBus bits)))))
    val pc_realign_fl0 = Vec.fill(cfg.issueWidth)(Vec.fill(cfg.InstSlice)(slave(Flow(UInt(cfg.InstAddrBus bits)))))
    val instr_err_fl0 = Vec.fill(cfg.issueWidth)(Vec.fill(cfg.InstSlice)(in Bool()))

    val predict_branch_if = slave(branch_predict_entry(cfg)) // from if2-tm
    val instr_2id_fl = Vec.fill(cfg.issueWidth)(master(Flow(UInt(cfg.InstBus bits)))) // to id
    val instr_2id_err = Vec.fill(cfg.issueWidth)(out Bool())
    val pc_2id_fl = Vec.fill(cfg.issueWidth)(master(Flow(UInt(cfg.InstAddrBus bits)))) // to id
    val predict_2id_if = master(branch_predict_entry(cfg)) // to id
    val in_flush = in Bool() // from ctrl
    val in_stall = in Bool() // from ctrl
    val queue_full_stall = out Bool() // to ctrl
    //val instr_queue_out_valid = out Bool()  // to pipe
    val queue_empty_stall = out Bool()
  }

  SpinalInfo(s"========== generate instr_queue ==========")
  SpinalInfo(f"issue: ${cfg.issueWidth}%d")
  SpinalInfo(f"InstSlice: ${cfg.InstSlice}%d")


  val fifo_full = Vec.fill(cfg.issueWidth)(Bool())
  val fifo_empty = Vec.fill(cfg.issueWidth)(Bool())
  val fifo_all_full = fifo_full.asBits.andR
  val fifo_all_empty = fifo_empty.asBits.andR
  io.queue_full_stall := fifo_all_full
  io.queue_empty_stall := fifo_all_empty
  //io.instr_queue_out_valid := io.instr_2id_fl.map(_.valid).asBits.orR

  val slice_ptr = Reg(UInt((log2Up(cfg.InstSlice) max 1) bits)) init(0)
  when(~io.in_stall && ~fifo_all_empty){
    when(slice_ptr === cfg.issueWidth-1){
      slice_ptr := 0
    } .otherwise{
      slice_ptr := slice_ptr + 1
    }
  }

  if(cfg.withbranch_predict) {
    /** 仅做了轻量化分支预测，默认只对LSB指令做分支预测， todo: 后续考虑支持混长/多发射 * */
    val stream_predict_in, stream_predict_out = Stream(branch_predict_entry(cfg))
    val predict_fifo = StreamFifo(branch_predict_entry(cfg),cfg.Instr_FIFO_DEPTH)
    stream_predict_in.valid := io.instr_realign_fl0(0)(0).valid && ~fifo_full(0)
    stream_predict_in.payload := io.predict_branch_if
    stream_predict_out.ready := ~fifo_empty(0) && ~io.in_stall
    predict_fifo.io.push << stream_predict_in
    predict_fifo.io.pop >> stream_predict_out
    predict_fifo.io.flush := io.in_flush
    io.predict_2id_if := stream_predict_out.payload
  } else {
    io.predict_2id_if.assignFromBits(B(0, io.predict_2id_if.asBits.getWidth bits))
  }


  for (i <- 0 until cfg.issueWidth) {
    val stream_instr_in, stream_instr_out = Vec.fill(cfg.InstSlice)(Stream(UInt(cfg.InstBus bits)))
    val stream_instr_err_in,stream_instr_err_out  = Vec.fill(cfg.InstSlice)(Stream(Bool()))
    //val instr_fifo = Vec.fill(cfg.InstSlice)(StreamFifo(UInt(cfg.InstBus bits),cfg.Instr_FIFO_DEPTH))
    val stream_pc_in, stream_pc_out = Vec.fill(cfg.InstSlice)(Stream(UInt(cfg.InstAddrBus bits)))
    val fifo_full_slice = Vec.fill(cfg.InstSlice)(Bool())
    val fifo_empty_slice = Vec.fill(cfg.InstSlice)(Bool())
    fifo_full(i) := fifo_full_slice.orR
    fifo_empty(i) := fifo_empty_slice.orR
    //val pc_fifo = Vec.fill(cfg.InstSlice)(StreamFifo(UInt(cfg.InstAddrBus bits),cfg.Instr_FIFO_DEPTH))
    for (j <- 0 until cfg.InstSlice) {
      val instr_fifo = StreamFifo(UInt(cfg.InstBus bits),cfg.Instr_FIFO_DEPTH)
      val pc_fifo = StreamFifo(UInt(cfg.InstAddrBus bits),cfg.Instr_FIFO_DEPTH)
      val instr_err_fifo = StreamFifo(Bool(),cfg.Instr_FIFO_DEPTH)
      fifo_full_slice(j) := instr_fifo.io.occupancy >= (cfg.Instr_FIFO_DEPTH-1)
      fifo_empty_slice(j) := instr_fifo.io.occupancy === 0
      stream_instr_in(j).valid := io.instr_realign_fl0(i)(j).valid && ~fifo_full(i)
      stream_instr_in(j).payload := io.instr_realign_fl0(i)(j).payload
      stream_instr_out(j).ready := ~fifo_empty(i) && ~io.in_stall
      stream_pc_in(j).valid := io.pc_realign_fl0(i)(j).valid && ~fifo_full(i)
      stream_pc_in(j).payload := io.pc_realign_fl0(i)(j).payload
      stream_pc_out(j).ready := ~fifo_empty(i) && ~io.in_stall
      stream_instr_err_in(j).valid := io.instr_realign_fl0(i)(j).valid && ~fifo_full(i)
      stream_instr_err_in(j).payload := io.instr_err_fl0(i)(j)
      stream_instr_err_out(j).ready := ~fifo_empty(i) && ~io.in_stall
      instr_fifo.io.push << stream_instr_in(j)
      instr_fifo.io.pop >> stream_instr_out(j)
      instr_fifo.io.flush := io.in_flush
      instr_err_fifo.io.push << stream_instr_err_in(j)
      instr_err_fifo.io.pop >> stream_instr_err_out(j)
      instr_err_fifo.io.flush := io.in_flush
      pc_fifo.io.push << stream_pc_in(j)
      pc_fifo.io.pop >> stream_pc_out(j)
      pc_fifo.io.flush := io.in_flush
    }
    if(cfg.InstSlice == 1) {  // 如果Vec的size=1，只能用(0)去索引
      io.instr_2id_fl(i).valid := stream_instr_out(0).valid
      io.instr_2id_fl(i).payload := stream_instr_out(0).payload
      io.pc_2id_fl(i).valid := stream_pc_out(0).valid
      io.pc_2id_fl(i).payload := stream_pc_out(0).payload
      io.instr_2id_err(i) := stream_instr_err_out(0).payload
    }
    else {
      io.instr_2id_fl(i).valid := stream_instr_out(slice_ptr).valid
      io.instr_2id_fl(i).payload := stream_instr_out(slice_ptr).payload
      io.pc_2id_fl(i).valid := stream_pc_out(slice_ptr).valid
      io.pc_2id_fl(i).payload := stream_pc_out(slice_ptr).payload
      io.instr_2id_err(i) := stream_instr_err_out(slice_ptr).payload
    }
  }

}
