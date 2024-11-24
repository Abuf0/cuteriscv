package cutelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class pc_gen() extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val mtvec = in UInt (InstAddrBus bits) // CSR设定初始pc地址 <--csr regfile
    val csr_epc1 = in UInt (InstAddrBus bits) // CSR中exception target地址？？ <--csr regfile
    val csr_epc2 = in UInt (InstAddrBus bits) // CSR中exception target地址？？ <--csr regfile
    val epc = in UInt (InstAddrBus bits) // commit时exception target地址 <--commit
    //val flush = out Bool()
    val flush = in Bool()
    val flush_mis_predict = in Bool()
    val flush_mis_predict_target_pc = in UInt(InstAddrBus bits)
    val instr_realign = slave(instr_entry(CoreConfig())) // ICache instr-->realign-->
    val pc_valid = out Bool()
    //val bht_taken = in Bool() // BHT预测是否进入分支 <--BHT
    val predict_bht_entry = slave(bht_predict_entry(CoreConfig()))  // from bht
    val predict_btb_entry = slave(btb_predict_entry(CoreConfig()))  // from btb
    //val btb_target = in UInt (InstAddrBus bits) // BTB预测的target地址 <--BTB
    val ras_target = in UInt (InstAddrBus bits) // call返回地址 <--RAS
    val pc = out UInt (InstAddrBus bits) // 当前pc --> ICache等
    val if_branch_predict = master(branch_predict_entry(CoreConfig())) // to instr_queue
    val ex_branch_predict = slave(branch_mispredict_entry(CoreConfig())) // from ex stage
    val bju_branch_predict = slave(branch_predict_entry(CoreConfig())) // from ex stage
    val resolved_bht_entry = master(bht_predict_entry(CoreConfig())) // ex stage to bht
    val resolved_btb_entry = master(btb_predict_entry(CoreConfig())) // ex stage to btb
    val icache_rdy = in Bool()  // from icache
    //val mispredict_entry = master(branch_mispredict_entry(CoreConfig())) // ex stage to ras
  }
  val pc_r = Reg(UInt(InstAddrBus bits)) init (io.mtvec)

  val is_jump = Bool()
  val jump_target = UInt(InstAddrBus bits)

  /*
  //val flush_r = Reg(Bool()) init(False)
  val flush_r = Bool()
  flush_r := io.ex_branch_predict.branch_cor || io.ex_branch_predict.ret_cor || io.ex_branch_predict.call_cor
  io.flush := flush_r
*/
  //io.mispredict_entry := io.ex_branch_predict

  is_jump := False

  when(io.ex_branch_predict.branch_cor){
    io.resolved_bht_entry.bht_valid := io.bju_branch_predict.branch_valid
    io.resolved_bht_entry.bht_taken := io.bju_branch_predict.branch_taken
    io.resolved_bht_entry.pc := io.bju_branch_predict.branch_target
    io.resolved_btb_entry.btb_valid := io.bju_branch_predict.branch_valid
    io.resolved_btb_entry.btb_taken := io.bju_branch_predict.branch_taken
    io.resolved_btb_entry.btb_target := io.bju_branch_predict.branch_target
    io.resolved_btb_entry.pc := io.bju_branch_predict.branch_target
  } .otherwise {
    io.resolved_bht_entry.bht_valid := False
    io.resolved_bht_entry.bht_taken := False
    io.resolved_bht_entry.pc := 0
    io.resolved_btb_entry.btb_valid := False
    io.resolved_btb_entry.btb_taken := False
    io.resolved_btb_entry.btb_target := 0
    io.resolved_btb_entry.pc := 0
  }

  when(io.flush === True) { // todo wrong
    //pc_r := io.ex_branch_predict.target_pc
    when(io.flush_mis_predict===True && io.flush===False){
      pc_r := io.mtvec
    } .otherwise{
      pc_r := io.flush_mis_predict_target_pc
    }
  }.elsewhen(io.icache_rdy === True) { // 包含了stall_push
    when(is_jump === True) {
      pc_r := jump_target
    }.elsewhen(io.predict_bht_entry.bht_valid === True && io.predict_bht_entry.bht_taken === True) {
      pc_r := io.predict_btb_entry.btb_target
    }.elsewhen(io.if_branch_predict.is_call === True) {
      pc_r := io.predict_btb_entry.btb_target
    }.elsewhen(io.if_branch_predict.is_ret === True) {
      pc_r := io.ras_target
    }.otherwise {
      pc_r := pc_r + InstLen
    }
  } .otherwise{}

  // RSICV中没有显式的call ret
  when(io.instr_realign.valid===True){
    io.if_branch_predict.is_branch := io.instr_realign.inst(6 downto 0) === U"1100011" // branch
    io.if_branch_predict.is_call := io.instr_realign.inst(6 downto 4) === U"110" && io.instr_realign.inst(2 downto 0) === U"111" && io.instr_realign.inst(7) === True// JAL or JALR, rd=x1/x5
    io.if_branch_predict.is_ret := io.instr_realign.inst(6 downto 0) === U"1100111" && io.instr_realign.inst(7) === False && io.instr_realign.inst(19 downto 18) === U"00" && io.instr_realign.inst(16 downto 15) === U"01"   // JALR, rd=0, rs=x1/x5
    //is_jump := io.if_branch_predict.is_call // TODO
    when(io.instr_realign.inst(6 downto 0) === U"1101111"){  // JAL
      jump_target := pc_r + U(InstBus-20 bits,default -> io.instr_realign.inst(31)) @@ io.instr_realign.inst(19 downto 12)@@io.instr_realign.inst(20 downto 20)@@io.instr_realign.inst(30 downto 21)@@U"0"  // 符号位扩展
      is_jump := True
    } .elsewhen(io.instr_realign.inst(6 downto 0) === U"1100111"){ // JALR
      jump_target := pc_r + U(InstBus-12 bits,default -> io.instr_realign.inst(31)) @@ io.instr_realign.inst(31 downto 20) // 符号位扩展
      is_jump := True
    } .otherwise{
      jump_target := io.mtvec
    }
  } .otherwise {
    io.if_branch_predict.is_branch := False
    io.if_branch_predict.is_call := False
    io.if_branch_predict.is_ret := False
    is_jump := False
    jump_target := io.mtvec
  }

  io.pc := pc_r
  io.pc_valid := io.instr_realign.valid

  io.if_branch_predict.pc := io.pc
  io.if_branch_predict.branch_valid := io.if_branch_predict.is_branch || io.if_branch_predict.is_call
  io.if_branch_predict.branch_taken := io.predict_bht_entry.bht_taken
  io.if_branch_predict.branch_target := io.predict_btb_entry.btb_target
}
