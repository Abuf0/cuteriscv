package mainelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class div_unit(cfg : CoreConfig, id: Int, latency: Int = 1) extends Component {
  val io = new Bundle {
    val ex_in_entry = Vec.fill(cfg.issueWidth)(slave(issue_entry(cfg)))  // from issue, broadcast
    val ex_out_entry = master(exe_res_entry(cfg))  // to commit
    val unit_busy = out Bool()
    val flush_mispredict_in = slave(flush_mispredict_entry(cfg))

  }

  /** todo : multicycle pipeline **/
  val issue_id = UInt(log2Up(cfg.issueWidth) bits)
  val unit_hit = Bool()
  issue_id := 0
  unit_hit := False

  io.unit_busy := False  // todo
  io.ex_out_entry.issue_id := issue_id
  for (i <- 0 until cfg.issueWidth) {
    when(io.ex_in_entry(i).fu_id.asBits.asUInt === id){
      issue_id := i
      unit_hit := io.ex_in_entry(i).valid
    }
  }
  val rs1 = S(io.ex_in_entry(issue_id).rs1_scb_entry.reg_rdata)
  val rs2 = S(io.ex_in_entry(issue_id).rs2_scb_entry.reg_rdata)
  val pc = io.ex_in_entry(issue_id).pc
  val div_res = SInt(cfg.RegDataBus bits)
  val div_res_vld = Bool()


  val cnt = Reg(UInt(cfg.DivCycle_WIDTH+1 bits)) init(cfg.DivCycle_NUM)
  val add_sum = SInt(2*cfg.RegDataBus bits)
  val data_tmp = Reg(UInt(2*cfg.RegDataBus bits)) init(0)
  val hit = ~add_sum(2*cfg.RegDataBus-1)


  when(unit_hit){
    div_res_vld := (cnt === cfg.DivCycle_NUM) // todo multicycle
  } . otherwise{
    div_res_vld := False  // todo multicycle
  }

  val out_trans_id = io.ex_in_entry(issue_id).trans_id
  val in_trans_id = io.flush_mispredict_in.trans_id
  val after_mispredict = io.flush_mispredict_in.hit && ((in_trans_id(cfg.SCB_ID_WIDTH - 1) === out_trans_id(cfg.SCB_ID_WIDTH - 1) && in_trans_id(cfg.SCB_ID_WIDTH - 2 downto (0)) <= out_trans_id(cfg.SCB_ID_WIDTH-2 downto(0))) || (in_trans_id(cfg.SCB_ID_WIDTH - 1) =/= out_trans_id(cfg.SCB_ID_WIDTH - 1) && in_trans_id(cfg.SCB_ID_WIDTH - 2 downto (0)) > out_trans_id(cfg.SCB_ID_WIDTH-2 downto(0)) ))

  io.ex_out_entry.trans_id := io.ex_in_entry(issue_id).trans_id
  io.ex_out_entry.instr := io.ex_in_entry(issue_id).instr
  io.ex_out_entry.pc := io.ex_in_entry(issue_id).pc
  io.ex_out_entry.result := div_res
  io.ex_out_entry.result_vld := div_res_vld & ~after_mispredict

  io.ex_out_entry.reg_wif.preg_addr := io.ex_in_entry(issue_id).rd_scb_entry.reg_addr_rename
  io.ex_out_entry.reg_wif.areg_addr := io.ex_in_entry(issue_id).rd_scb_entry.reg_addr_real
  io.ex_out_entry.reg_wif.reg_wten := io.ex_in_entry(issue_id).rd_scb_entry.reg_wten
  io.ex_out_entry.reg_wif.reg_wdata := io.ex_out_entry.result.asUInt

  // todo with divisor=0 exception
  val div_qu = data_tmp(cfg.RegDataBus-1 downto 0).asSInt
  val div_rem = data_tmp(2*cfg.RegDataBus-1 downto cfg.RegDataBus).asSInt
  val dividend = SInt(cfg.RegDataBus bits)
  val divisor = SInt(cfg.RegDataBus bits)
  val din_vld = Bool()
  val dec_valid_d1 = Reg(Bool()) init(False)
  dec_valid_d1 := io.ex_in_entry(issue_id).valid && unit_hit
  din_vld := io.ex_in_entry(issue_id).valid && ~dec_valid_d1

  when(unit_hit) {
    switch(io.ex_in_entry(issue_id).instr) {
      is(DIV) {
        dividend := rs1
        divisor := rs2
        div_res := rs1/rs2//div_qu
      }
      is(DIVU) {  // todo
        dividend := rs1//.asUInt
        divisor := rs2//.asUInt
        div_res := (rs1.asUInt/rs2.asUInt).asSInt//div_qu
      }
      is(REM) {
        dividend := rs1
        divisor := rs2
        div_res := rs1%rs2//div_rem
      }
      is(REMU) {  // todo
        dividend := rs1//.asUInt
        divisor := rs2//.asUInt
        div_res := (rs1.asUInt%rs2.asUInt).asSInt//div_rem
      }
      default {
        dividend := rs1
        divisor := rs2
        div_res := 0
      }
    }
  } .otherwise{
    dividend := rs1
    divisor := rs2
    div_res := 0
  }

  when(din_vld){
    cnt := 0
  } .elsewhen(cnt >= cfg.DivCycle_NUM){
    cnt := cfg.DivCycle_NUM + 1
  } .otherwise{
    cnt := cnt + 1
  }

  when(din_vld){
    add_sum := (dividend |<< 1) - (divisor << cfg.RegDataBus)
  } .otherwise{
    add_sum := S(data_tmp |<< 1) - (divisor << cfg.RegDataBus)
  }

  when(din_vld){
    when(hit){
      data_tmp := add_sum(2*cfg.RegDataBus-1 downto 1).asUInt@@U"1'b1"
    } .otherwise{
      data_tmp := U(0,cfg.RegDataBus bits)@@dividend(cfg.RegDataBus-1 downto 1).asUInt@@U"1'b0"
    }
  } .elsewhen(cnt =/= 0){
    when(hit){
      data_tmp := add_sum(2*cfg.RegDataBus-1 downto 1).asUInt@@U"1'b1"
    } .otherwise{
      data_tmp := data_tmp(2*cfg.RegDataBus-2 downto 0)@@U"1'b0"
    }
  }


}
