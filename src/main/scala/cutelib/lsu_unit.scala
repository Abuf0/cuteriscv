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
    val read_interfacec = master(memory_read_interface(CoreConfig()))  // to dcache/icache
    val toload_addr = out UInt(DataAddrBus bits)  // to wb
    val toload_hit = in UInt(4 bits)
    val toload_data = in UInt(DataBus bits)
    // todo with lsu //
    val lsu_ack = out Bool()  // to scb
  }
  //val load_raddr = Reg(UInt(DataAddrBus bits)) init(0)
  //val load_rden = Reg(Bool()) init(False)
  //val load_byte = Reg(Bits(4 bits)) init(B"1111")
  //val load_sign = Reg(Bool()) init(False)
  val load_raddr = UInt(DataAddrBus bits)
  val load_raddr_align = UInt(DataAddrBus bits)
  val load_rden  = Bool()
  val load_byte  = Bits(4 bits)
  val load_sign  = Bool()

  load_raddr := 0
  load_raddr_align := 0
  load_rden  := False
  load_byte  := B"1111"
  load_sign  := True

  val load_result = Reg(UInt(DataBus bits)) init(0)
  //val load_result = UInt(DataBus bits)
  val store_waddr = UInt(DataAddrBus bits)
  store_waddr := 0
  val store_waddr_align = Reg(UInt(DataAddrBus bits)) init(0)
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
  io.lsu_ex_entry.store_wb_addr := store_waddr_align
  io.lsu_ex_entry.store_wb_data := store_wdata
  io.lsu_ex_entry.store_wb_byte := store_byte

  io.lsu_ex_entry.instr := ex_operand_entry_instr
  io.lsu_ex_entry.trans_id := ex_operand_entry_trans_id
  io.lsu_ex_entry.pc := ex_operand_entry_pc

  io.lsu_ex_entry.load_rd_en := load_rden   // to dcache
  io.lsu_ex_entry.load_rd_addr := load_raddr_align  // to dcache
  io.lsu_ex_entry.load_rd_byte := load_byte
  io.lsu_ex_entry.result := load_result // from dcache

  io.read_interfacec.re := load_rden
  io.read_interfacec.raddr := load_raddr
  io.read_interfacec.sel := U(load_byte)
  val dcache_rdata = io.read_interfacec.rdata
  val dcache_rdata_real = UInt(DataBus bits)

  //io.toload_addr := load_raddr_align
  io.toload_addr := load_raddr  // fix sfind
  /*
  when(io.toload_hit){ // 如果wb buffer中有待提交的SW指令，且写地址==读地址，则forwarding（类似于store buffer）
    dcache_rdata_real := io.toload_data
  } .otherwise{
    dcache_rdata_real := dcache_rdata
  }
   */

  val dcache_rdata_real_0 = UInt(8 bits)
  val dcache_rdata_real_1 = UInt(8 bits)
  val dcache_rdata_real_2 = UInt(8 bits)
  val dcache_rdata_real_3 = UInt(8 bits)

  when(io.toload_hit(0)){
    dcache_rdata_real_0 := io.toload_data(7 downto 0)
  } .otherwise{
    dcache_rdata_real_0 := dcache_rdata(7 downto 0)
  }
  when(io.toload_hit(1)){
    dcache_rdata_real_1 := io.toload_data(15 downto 8)
  } .otherwise{
    dcache_rdata_real_1 := dcache_rdata(15 downto 8)
  }
  when(io.toload_hit(2)){
    dcache_rdata_real_2 := io.toload_data(23 downto 16)
  } .otherwise{
    dcache_rdata_real_2 := dcache_rdata(23 downto 16)
  }
  when(io.toload_hit(3)){
    dcache_rdata_real_3 := io.toload_data(31 downto 24)
  } .otherwise{
    dcache_rdata_real_3 := dcache_rdata(31 downto 24)
  }

  dcache_rdata_real := dcache_rdata_real_3 @@ dcache_rdata_real_2 @@ dcache_rdata_real_1 @@ dcache_rdata_real_0

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
        load_raddr_align := load_raddr(DataAddrBus-1 downto 2)@@U"2'b00"  // todo:也可以不做，因为cache里会丢弃最后2bit
        load_rden := True
        switch(load_raddr(1 downto 0)) {
          is(U"00") {
            load_byte := B"0001"
            load_sign := True
            load_result := U(DataBus bits, default -> dcache_rdata_real(7), (7 downto 0) -> dcache_rdata_real(7 downto 0))
          }
          is(U"01") {
            load_byte := B"0010"
            load_sign := True
            load_result := U(DataBus bits, default -> dcache_rdata_real(15), (7 downto 0) -> dcache_rdata_real(15 downto 8))
          }
          is(U"10") {
            load_byte := B"0100"
            load_sign := True
            load_result := U(DataBus bits, default -> dcache_rdata_real(23), (7 downto 0) -> dcache_rdata_real(23 downto 16))
          }
          is(U"11") {
            load_byte := B"1000"
            load_sign := True
            load_result := U(DataBus bits, default -> dcache_rdata_real(31), (7 downto 0) -> dcache_rdata_real(31 downto 24))
          }
        }
        store_wten := False
      }
      is(LH){ // todo 跨地址问题，按理编译器不会支持的
        load_raddr := rs1_data+U(imm)
        load_raddr_align := load_raddr(DataAddrBus-1 downto 2)@@U"2'b00"
        load_rden := True
        switch(load_raddr(1 downto 0)) {
          is(U"00") {
            load_byte := B"0011"
            load_sign := True
            load_result := U(DataBus bits, default -> dcache_rdata_real(15), (15 downto 0) -> dcache_rdata_real(15 downto 0))
          }
          /*
          is(U"01") { // todo 跨地址了
            load_byte := B"0110"
            load_sign := True
            load_result := U(DataBus bits, default -> dcache_rdata_real(23), (15 downto 0) -> dcache_rdata_real(23 downto 8))
          }
           */
          is(U"10") {
            load_byte := B"1100"
            load_sign := True
            load_result := U(DataBus bits, default -> dcache_rdata_real(31), (15 downto 0) -> dcache_rdata_real(31 downto 16))
          }
          /*
          is(U"11") { // todo
            load_byte := B"1000"
            load_sign := True
            load_result := U(DataBus bits, default -> dcache_rdata_real(31), (7 downto 0) -> dcache_rdata_real(31 downto 24))
          }
          */
        }
        store_wten := False
      }
      is(LW){
        load_raddr := rs1_data+U(imm)
        load_raddr_align := load_raddr(DataAddrBus-1 downto 2)@@U"2'b00"
        load_rden := True
        load_byte := B"1111"
        load_sign := True
        load_result := dcache_rdata_real
        store_wten := False
      }
      is(LBU){
        load_raddr := rs1_data+U(imm)
        load_raddr_align := load_raddr(DataAddrBus-1 downto 2)@@U"2'b00"
        load_rden := True
        switch(load_raddr(1 downto 0)) {
          is(U"00") {
            load_byte := B"0001"
            load_sign := True
            load_result := U(DataBus bits, default -> False, (7 downto 0) -> dcache_rdata_real(7 downto 0))
          }
          is(U"01") {
            load_byte := B"0010"
            load_sign := True
            load_result := U(DataBus bits, default -> False, (7 downto 0) -> dcache_rdata_real(15 downto 8))
          }
          is(U"10") {
            load_byte := B"0100"
            load_sign := True
            load_result := U(DataBus bits, default -> False, (7 downto 0) -> dcache_rdata_real(23 downto 16))
          }
          is(U"11") {
            load_byte := B"1000"
            load_sign := True
            load_result := U(DataBus bits, default -> False, (7 downto 0) -> dcache_rdata_real(31 downto 24))
          }
        }
        store_wten := False
      }
      is(LHU){
        load_raddr := rs1_data+U(imm)
        load_raddr_align := load_raddr(DataAddrBus-1 downto 2)@@U"2'b00"
        load_rden := True
        switch(load_raddr(1 downto 0)) {
          is(U"00") {
            load_byte := B"0011"
            load_sign := True
            load_result := U(DataBus bits, default -> False, (15 downto 0) -> dcache_rdata_real(15 downto 0))
          }
          /*
          is(U"01") { // todo 跨地址了
            load_byte := B"0110"
            load_sign := True
            load_result := U(DataBus bits, default -> dcache_rdata_real(23), (15 downto 0) -> dcache_rdata_real(23 downto 8))
          }
           */
          is(U"10") {
            load_byte := B"1100"
            load_sign := True
            load_result := U(DataBus bits, default -> False, (15 downto 0) -> dcache_rdata_real(31 downto 16))
          }
          /*
          is(U"11") { // todo
            load_byte := B"1000"
            load_sign := True
            load_result := U(DataBus bits, default -> dcache_rdata_real(31), (7 downto 0) -> dcache_rdata_real(31 downto 24))
          }
          */
        }
        store_wten := False
      }
      is(SB){
        store_waddr := rs1_data+U(imm)
        store_waddr_align := store_waddr(DataAddrBus-1 downto 2)@@U"2'b00"
        store_wten := True
        switch(store_waddr(1 downto 0)) {
          is(U"00") {
            store_byte := B"0001"
            store_wdata := U(DataBus bits, default -> False, (7 downto 0) -> rs2_data(7 downto 0))
          }
          is(U"01") {
            store_byte := B"0010"
            store_wdata := U(DataBus bits, default -> False, (15 downto 8) -> rs2_data(7 downto 0))
          }
          is(U"10") {
            store_byte := B"0100"
            store_wdata := U(DataBus bits, default -> False, (23 downto 16) -> rs2_data(7 downto 0))
          }
          is(U"11") {
            store_byte := B"1000"
            store_wdata := U(DataBus bits, default -> False, (31 downto 24) -> rs2_data(7 downto 0))
          }
        }
        load_rden := False
      }
      is(SH){
        store_waddr := rs1_data+U(imm)
        store_waddr_align := store_waddr(DataAddrBus-1 downto 2)@@U"2'b00"
        store_wten := True
        switch(store_waddr(1 downto 0)) { // todo:同样跨边界问题
          is(U"00") {
            store_byte := B"0011"
            store_wdata := U(DataBus bits, default -> False, (15 downto 0) -> rs2_data(15 downto 0))
          }
          /*
          is(U"01") {
            store_byte := B"0010"
            store_wdata := U(DataBus bits, default -> 0, (15 downto 8) -> rs2_data(7 downto 0))
          }
           */
          is(U"10") {
            store_byte := B"1100"
            store_wdata := U(DataBus bits, default -> False, (31 downto 16) -> rs2_data(15 downto 0))
          }
          /*
          is(U"11") {
            store_byte := B"1000"
            store_wdata := U(DataBus bits, default -> 0, (31 downto 24) -> rs2_data(7 downto 0))
          }
           */
        }
        load_rden := False
      }
      is(SW){
        store_waddr := rs1_data+U(imm)
        store_waddr_align := store_waddr(DataAddrBus-1 downto 2)@@U"2'b00"
        store_wten := True
        store_byte := B"1111"
        store_wdata := rs2_data
        load_rden := False
      }
      default{
        load_rden := False
        store_wten := False
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
    load_rden := False
    store_wten := False
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
