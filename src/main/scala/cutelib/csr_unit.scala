package cutelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class csr_unit() extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    //val dec_entry = slave(decorder_entry(CoreConfig())) // from issue stage
    val ex_operand_entry = slave(operand_entry(CoreConfig()))  // from issue
    //val csr_res = Bits(RegDataBus bits) // mul to commit
    val csr_ex_entry = master(csr_res_entry(CoreConfig()))  // to commit
  }
  // todo csr buffer
  val rs1 = io.ex_operand_entry.rs1_data
  val csr = io.ex_operand_entry.rs2_data
  val imm = io.ex_operand_entry.imm
  val pc = io.ex_operand_entry.pc
  val rd_res = UInt(RegDataBus bits)
  val csr_res = UInt(CSRDataBus bits)
  val csr_trans_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  val csr_instr = Reg(UInt(InstBus bits)) init(0)
  val csr_pc = Reg(UInt(InstAddrBus bits)) init(0)
  val rd_res_out = Reg(UInt(RegDataBus bits)) init(0)
  val csr_res_out = Reg(UInt(CSRDataBus bits)) init(0)

  when(io.ex_operand_entry.dec_valid){
    io.ex_operand_entry.busy := True
  } . otherwise{
    io.ex_operand_entry.busy := False
  }

  when(io.ex_operand_entry.dec_valid){
    csr_trans_id := io.ex_operand_entry.trans_id
    csr_instr := io.ex_operand_entry.instr
    csr_pc := io.ex_operand_entry.pc
    rd_res_out := rd_res
    csr_res_out := csr_res
  } . otherwise{  }

  io.csr_ex_entry.trans_id := csr_trans_id
  io.csr_ex_entry.instr := csr_instr
  io.csr_ex_entry.pc := csr_pc
  io.csr_ex_entry.result := rd_res_out
  io.csr_ex_entry.result_csr := csr_res_out


  // todo with other ALU instructions
  when(io.ex_operand_entry.busy === True) {
    switch(io.ex_operand_entry.instr) {
          is(CSRRW) {
            rd_res := csr
            csr_res := rs1
          }
          is(CSRRS) {
            rd_res := csr
            csr_res := rs1 | csr
          }
          is(CSRRC) {
            rd_res := csr
            csr_res := ~rs1 | csr
          }
          is(CSRRWI) {
            rd_res := csr
            csr_res := U(imm)
          }
          is(CSRRSI) {
            rd_res := csr
            csr_res := csr | U(imm)
          }
          is(CSRRCI) {
            rd_res := csr
            csr_res := csr | ~U(imm)
          }
          //...//
          default {
            rd_res := 0
            csr_res := 0
          }
        }


  } .otherwise{
    rd_res := 0
    csr_res := 0
  }


}

