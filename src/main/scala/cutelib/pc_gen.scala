package cutelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._


case class pc_gen() extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val trap_entry = in UInt (InstAddrBus bits) // CSR设定初始pc地址 <--csr regfile
    val csr_epc1 = in UInt (InstAddrBus bits) // CSR中exception target地址？？ <--csr regfile
    val csr_epc2 = in UInt (InstAddrBus bits) // CSR中exception target地址？？ <--csr regfile
    val epc = in UInt (InstAddrBus bits) // commit时exception target地址 <--commit
    //val flush = out Bool()
    val flush = in Bool()
    val flush_mis_predict = in Bool()
    val flush_except = in Bool()
    val flush_mis_predict_target_pc = in UInt(InstAddrBus bits)
    val instr_realign = slave(instr_entry(CoreConfig())) // ICache instr-->realign-->
    val pc_valid = out Bool()
    //val bht_taken = in Bool() // BHT预测是否进入分支 <--BHT
    val predict_bht_entry = slave(bht_predict_entry(CoreConfig()))  // from bht
    val predict_btb_entry = slave(btb_predict_entry(CoreConfig()))  // from btb
    //val btb_target = in UInt (InstAddrBus bits) // BTB预测的target地址 <--BTB
    val ras_target = in UInt (InstAddrBus bits) // call返回地址 <--RAS
    val pc = out UInt (InstAddrBus bits) // 下一个 --> ICache等
    val pc_now = out UInt (InstAddrBus bits)
    val if_branch_predict = master(branch_predict_entry(CoreConfig())) // to instr_queue
    val ex_branch_predict = slave(branch_mispredict_entry(CoreConfig())) // from ex stage
    val bju_branch_predict = slave(branch_predict_entry(CoreConfig())) // from ex stage
    val resolved_bht_entry = master(bht_predict_entry(CoreConfig())) // ex stage to bht
    val resolved_btb_entry = master(btb_predict_entry(CoreConfig())) // ex stage to btb
    val icache_rdy = in Bool()  // from icache
    val call_push_target = out UInt(InstAddrBus bits)  // to RAS
    val outstanding_flag = out Bool()  // to instr realign
    //val scb_readop_wb_i = in Vec(UInt(RegDataBus bits),REG_NUM) // from regfile wb
    //val mispredict_entry = master(branch_mispredict_entry(CoreConfig())) // ex stage to ras
  }
  //val pc_r = Reg(UInt(InstAddrBus bits)) init (io.trap_entry)
  val pc_r = Reg(UInt(InstAddrBus bits)) init (0) // todo

  val is_jump = Bool()
  val jump_target = UInt(InstAddrBus bits)
  val call_push_target = UInt(InstAddrBus bits)
  call_push_target := 0

  io.call_push_target := call_push_target

  /*
  //val flush_r = Reg(Bool()) init(False)
  val flush_r = Bool()
  flush_r := io.ex_branch_predict.branch_cor || io.ex_branch_predict.ret_cor || io.ex_branch_predict.call_cor
  io.flush := flush_r
*/
  //io.mispredict_entry := io.ex_branch_predict

  is_jump := False

  val branch_cor_d1 = Reg(Bool()) init(False)
  branch_cor_d1 := io.ex_branch_predict.branch_cor
  val branch_cor_pos = io.ex_branch_predict.branch_cor && ~branch_cor_d1
  //val branch_cor_pos := io.ex_branch_predict.branch_cor

  when(branch_cor_pos){
    io.resolved_bht_entry.bht_valid := io.bju_branch_predict.branch_valid
    io.resolved_bht_entry.bht_taken := io.bju_branch_predict.branch_taken
    io.resolved_bht_entry.pc := io.bju_branch_predict.pc
    io.resolved_btb_entry.btb_valid := io.bju_branch_predict.branch_valid
    io.resolved_btb_entry.btb_taken := io.bju_branch_predict.branch_taken
    io.resolved_btb_entry.btb_target := io.bju_branch_predict.branch_target
    io.resolved_btb_entry.pc := io.bju_branch_predict.pc
  } .otherwise {
    io.resolved_bht_entry.bht_valid := False
    io.resolved_bht_entry.bht_taken := False
    io.resolved_bht_entry.pc := 0
    io.resolved_btb_entry.btb_valid := False
    io.resolved_btb_entry.btb_taken := False
    io.resolved_btb_entry.btb_target := 0
    io.resolved_btb_entry.pc := 0
  }

  val pc_now = UInt(InstAddrBus bits)
  // todo with outstanding transactions
  when(io.flush === True) { // todo wrong
    //pc_r := io.ex_branch_predict.target_pc
    when(io.flush_except === True){
      pc_r := io.trap_entry
    } .elsewhen(io.flush_mis_predict===True){
      pc_r := io.flush_mis_predict_target_pc
    } .otherwise{ } // todo
  }.elsewhen(/*io.icache_rdy === True*/True) { // 包含了stall_push
    when(is_jump === True && ~io.if_branch_predict.is_call && ~io.if_branch_predict.is_ret) {  // todo
      pc_r := jump_target
    }.elsewhen(io.if_branch_predict.is_branch && io.predict_bht_entry.bht_valid === True && io.predict_bht_entry.bht_taken === True) {
      pc_r := io.predict_btb_entry.btb_target
    }.elsewhen(io.if_branch_predict.is_call === True) {
      //pc_r := io.predict_btb_entry.btb_target
      call_push_target := pc_now + 4
      when(is_jump === True) {  // JAL call
        pc_r := jump_target
      } .otherwise{ // JALR call
        pc_r := io.predict_btb_entry.btb_target
      }
    }.elsewhen(io.if_branch_predict.is_ret === True) {
      pc_r := io.ras_target
    }.elsewhen(io.icache_rdy === True) {
      pc_r := pc_r + InstLen
    } .otherwise{ }
  } //.otherwise{}
  val outstanding_flag = Bool()
  val outstanding_flag_d1 = Reg(Bool()) init(False)
  val outstanding_pulse = Bool()
  val outstanding_pulse_d1 = Reg(Bool()) init(False)
  val pc_r_d1 = Reg(UInt(InstAddrBus bits)) init(0)
  outstanding_flag := io.flush || is_jump || io.if_branch_predict.is_call || io.if_branch_predict.is_ret || (io.if_branch_predict.is_branch && io.predict_bht_entry.bht_valid === True && io.predict_bht_entry.bht_taken === True)
  /*
  outstanding_flag_d1 := outstanding_flag
  outstanding_pulse := outstanding_flag && ~outstanding_flag_d1
  outstanding_pulse_d1 := outstanding_pulse
  io.outstanding_flag := outstanding_pulse_d1 // todo with outstanding numbers
  */
  val outstanding_pulse_tog = Reg(Bool()) init(False)
  when(outstanding_flag === True){
    outstanding_pulse_tog := True
  } .elsewhen(io.icache_rdy === True){
    outstanding_pulse_tog := False
  }
  io.outstanding_flag := outstanding_pulse_tog

  when(io.icache_rdy) {
    pc_r_d1 := pc_r
  } .otherwise{ }

//  pc_r_d1 := pc_r

  pc_now := pc_r_d1 // todo with outstanding numbers


  // RSICV中没有显式的call ret
  when(io.instr_realign.valid===True){
  //when(True/*io.icache_rdy === True*/){
    io.if_branch_predict.is_branch := io.instr_realign.inst(6 downto 0) === U"1100011" // branch
    io.if_branch_predict.is_call := io.instr_realign.inst(6 downto 4) === U"110" && io.instr_realign.inst(2 downto 0) === U"111" && (io.instr_realign.inst(11 downto 7) === U"00001" || io.instr_realign.inst(11 downto 7) === U"00101")// JAL or JALR, rd=x1/x5
    io.if_branch_predict.is_ret := io.instr_realign.inst(6 downto 0) === U"1100111" && io.instr_realign.inst(11 downto 7) === U"0" && io.instr_realign.inst(19 downto 18) === U"00" && io.instr_realign.inst(16 downto 15) === U"01"   // JALR, rd=0, rs=x1/x5
    io.if_branch_predict.is_jump := io.instr_realign.inst(6 downto 0) === U"1100111" //&& io.if_branch_predict.is_call === False && io.if_branch_predict.is_ret === False // JALR
    //is_jump := io.if_branch_predict.is_call // TODO
    when(io.instr_realign.inst(6 downto 0) === U"1101111"){  // JAL
      jump_target := pc_now + U(InstBus-20 bits,default -> io.instr_realign.inst(31)) @@ io.instr_realign.inst(19 downto 12)@@io.instr_realign.inst(20 downto 20)@@io.instr_realign.inst(30 downto 21)@@U"0"  // 符号位扩展
      is_jump := True
    } /*
    .elsewhen(io.instr_realign.inst(6 downto 0) === U"1100111"){ // JALR
      //jump_target := io.scb_readop_wb_i(io.instr_realign.inst(19 downto 15)) + U(InstBus-12 bits,default -> io.instr_realign.inst(31)) @@ io.instr_realign.inst(31 downto 20) // 符号位扩展
      jump_target :=
      is_jump := True
    } */
      .otherwise{
      jump_target := io.trap_entry
    }
  } .otherwise {
    io.if_branch_predict.is_branch := False
    io.if_branch_predict.is_call := False
    io.if_branch_predict.is_ret := False
    io.if_branch_predict.is_jump := False
    is_jump := False
    jump_target := io.trap_entry
  }

  io.pc := pc_r
  io.pc_valid := io.instr_realign.valid
  io.pc_now := pc_now

  // to instr_queue
  io.if_branch_predict.pc := pc_now
  io.if_branch_predict.branch_valid := io.if_branch_predict.is_branch || io.if_branch_predict.is_call || io.if_branch_predict.is_jump
  io.if_branch_predict.branch_taken := io.predict_bht_entry.bht_taken
  io.if_branch_predict.branch_target := io.predict_btb_entry.btb_target
}
