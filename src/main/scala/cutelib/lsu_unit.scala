package cutelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class lsu_unit() extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    //val dec_entry = slave(decorder_entry(CoreConfig())) // from issue stage
    val ex_operand_entry = slave(operand_entry(CoreConfig()))  // from issue
    val lsu_ex_entry = master(lsu_res_entry(CoreConfig()))  // to commit
    // todo with lsu //
    val lsu_ack = out Bool()  // to scb
  }
  val load_raddr = Reg(UInt(DataAddrBus bits)) init(0)
  val load_rden = Reg(Bool()) init(False)
  val load_byte = Reg(Bits(4 bits)) init(B"1111")
  val load_sign = Reg(Bool()) init(False)
  val load_result = Reg(UInt(DataBus bits)) init(0)
  val store_waddr = Reg(UInt(DataAddrBus bits)) init(0)
  val store_wten = Reg(Bool()) init(False)
  val store_byte = Reg(Bits(4 bits)) init(B"1111")
  val store_wdata = Reg(UInt(DataBus bits)) init(0)
  val ex_operand_entry_instr = Reg(UInt(InstBus bits)) init(0)
  val ex_operand_entry_trans_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  val ex_operand_entry_pc = Reg(UInt(InstAddrBus bits)) init(0)

  val rs1_data = io.ex_operand_entry.rs1_data
  val rs2_data = io.ex_operand_entry.rs2_data
  val imm = io.ex_operand_entry.imm
  val rd_addr = io.ex_operand_entry.rd_addr

  /*
  load_raddr := 0
  load_rden := False
  load_byte := B"1111"
  load_sign := False
  store_waddr := 0
  store_wten := False
  store_byte := B"1111"
  store_wdata := 0
  load_result := 0
  */


  io.lsu_ex_entry.store_wb_en := store_wten
  io.lsu_ex_entry.store_wb_addr := store_waddr
  io.lsu_ex_entry.store_wb_data := store_wdata
  io.lsu_ex_entry.store_wb_byte := store_byte

  io.lsu_ex_entry.instr := ex_operand_entry_instr
  io.lsu_ex_entry.trans_id := ex_operand_entry_trans_id
  io.lsu_ex_entry.pc := ex_operand_entry_pc

  io.lsu_ex_entry.load_rd_en := load_rden   // to dcache
  io.lsu_ex_entry.load_rd_addr := load_raddr  // to dcache
  io.lsu_ex_entry.load_rd_byte := load_byte
  io.lsu_ex_entry.result := load_result // from dcache

  when(io.ex_operand_entry.dec_valid){
    io.ex_operand_entry.busy := True
    ex_operand_entry_instr := io.ex_operand_entry.instr
    ex_operand_entry_trans_id := io.ex_operand_entry.trans_id
    ex_operand_entry_pc := io.ex_operand_entry.pc
  } . otherwise{
    io.ex_operand_entry.busy := False
  }

  when(io.ex_operand_entry.busy){
    switch(io.ex_operand_entry.instr){
      is(LB){
        load_raddr := rs1_data+U(imm)
        load_rden := True
        load_byte := B"0001"
        load_sign := True
        load_result := U(DataBus bits,default->io.lsu_ex_entry.load_rd_data(DataBus-1),(7 downto 0)->io.lsu_ex_entry.load_rd_data(7 downto 0))
      }
      is(LH){
        load_raddr := rs1_data+U(imm)
        load_rden := True
        load_byte := B"0011"
        load_sign := True
        load_result := U(DataBus bits,default->io.lsu_ex_entry.load_rd_data(DataBus-1),(15 downto 0)->io.lsu_ex_entry.load_rd_data(15 downto 0))
      }
      is(LW){
        load_raddr := rs1_data+U(imm)
        load_rden := True
        load_byte := B"1111"
        load_sign := True
        load_result := io.lsu_ex_entry.load_rd_data
      }
      is(LBU){
        load_raddr := rs1_data+U(imm)
        load_rden := True
        load_byte := B"0001"
        load_sign := False
        load_result := io.lsu_ex_entry.load_rd_data(7 downto 0).resized
      }
      is(LHU){
        load_raddr := rs1_data+U(imm)
        load_rden := True
        load_byte := B"1111"
        load_sign := False
        load_result := io.lsu_ex_entry.load_rd_data(15 downto 0).resized
      }
      is(SB){
        store_waddr := rs1_data+U(imm)
        store_wten := True
        store_byte := B"0001"
        store_wdata := rs2_data
      }
      is(SH){
        store_waddr := rs1_data+U(imm)
        store_wten := True
        store_byte := B"0011"
        store_wdata := rs2_data
      }
      is(SW){
        store_waddr := rs1_data+U(imm)
        store_wten := True
        store_byte := B"1111"
        store_wdata := rs2_data
      }
      default{
        /*
        load_raddr := 0
        load_rden := False
        load_byte := B"1111"
        load_sign := False
        store_waddr := 0
        store_wten := False
        store_byte := 4
        store_wdata := 0
         */
      }
    }
  } .otherwise{
    /*
    load_raddr := 0
    load_rden := False
    load_byte := B"1111"
    load_sign := False
    store_waddr := 0
    store_wten := False
    store_byte := 4
    store_wdata := 0
     */
  }

}
