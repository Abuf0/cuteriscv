package mylib
import spinal.core._
import spinal.lib._   // IMasterSlave
//class core(){
//}
class pc_reg extends Component with Global_parameter  with Interface_MS{
  val io = new Bundle{
    val clk = in Bool()
    val rst = in Bool()
    val pc = out UInt(InstAddrBus bits)
    val ce = out Bool()
    val stall = in UInt(stallDir bits)
    val is_branch = in Bool()
    val branch_target_pc = in UInt(InstAddrBus bits)
    val flush = in Bool()
    val new_pc = in UInt(InstAddrBus bits)
  }

  val ce_r = Reg(Bool()) init(ChipDisable)
  ce_r := ChipEnable
  io.ce := ce_r

  val pc_r = Reg(UInt(InstAddrBus bits))  init(ZeroWord)
  when(ce_r===ChipDisable){
    pc_r := ZeroWord
  }.elsewhen(io.flush===hasException){
    pc_r := io.new_pc
  } .otherwise{
    when(io.stall(0)===NoStop){
      when(io.is_branch){
        pc_r := io.branch_target_pc
      } .otherwise{
        pc_r := pc_r + 4
      }
    }
  }
  io.pc := pc_r
}

class if_id extends Component with Global_parameter with Interface_MS{
  val io = new Bundle{
    val clk = in Bool()
    val rst = in Bool()
    val stall = in UInt(stallDir bits)
    val flush = in Bool()
    val if_pc = in UInt (InstAddrBus bits)
    val if_inst = in UInt (InstBus bits)
    val M_if_id2id = master(if_id2id_interface(CoreConfig()))
  }
  val if_pc_r = Reg(UInt(InstAddrBus bits))  init(ZeroWord)
  val if_inst_r = Reg(UInt(InstAddrBus bits))  init(ZeroWord)
  when(io.flush===hasException){
    if_pc_r := ZeroWord
    if_inst_r := ZeroWord
  } .elsewhen(io.stall(1)===Stop && io.stall(2)===NoStop){
    if_pc_r := ZeroWord
    if_inst_r := ZeroWord
  } .otherwise{
    if_pc_r := io.if_pc
    if_inst_r := io.if_inst
  }
  io.M_if_id2id.pc := if_pc_r
  io.M_if_id2id.inst := if_inst_r
}

class regfile extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()
    val S_id2regfile = slave(id2regfile_interface(CoreConfig())) // for ID read
    val M_wb2regfile = master(wb2regfileInterface(CoreConfig()))  // for WB wirte
  }
  val mem = Mem(UInt(DataBus bits), wordCount = DataMemNum)
  mem.write(
    enable  = io.rst===RstDisable && io.M_wb2regfile.we===WriteEnable,
    address = io.M_wb2regfile.waddr,
    data    = io.M_wb2regfile.wdata
  )
  // 考虑到同时读写问题，writethrough //
  val reg1_raw = io.S_id2regfile.reg1_rden===ReadEnable && io.M_wb2regfile.waddr=/=io.S_id2regfile.reg1_addr
  val reg2_raw = io.S_id2regfile.reg2_rden===ReadEnable && io.M_wb2regfile.waddr=/=io.S_id2regfile.reg2_addr
  val reg1_data = UInt(RegBus bits)
  val reg2_data = UInt(RegBus bits)
  reg1_data := mem.readSync(
    enable  = io.rst===RstDisable && reg1_raw,
    address = io.S_id2regfile.reg1_addr
  )
  reg2_data := mem.readSync(
    enable  = io.rst===RstDisable && reg2_raw,
    address = io.S_id2regfile.reg2_addr
  )

  when(io.rst===RstDisable) {
    io.S_id2regfile.reg1_data := ZeroWord
  } .elsewhen(io.S_id2regfile.reg1_rden===ReadDisable){
    io.S_id2regfile.reg1_data := ZeroWord
  } .elsewhen(reg1_raw) {
    io.S_id2regfile.reg1_data := io.M_wb2regfile.wdata
  } .otherwise{
    io.S_id2regfile.reg1_data := reg1_data
  }
  when(io.rst===RstDisable) {
    io.S_id2regfile.reg2_data := ZeroWord
  } .elsewhen(io.S_id2regfile.reg2_rden===ReadDisable){
    io.S_id2regfile.reg2_data := ZeroWord
  } .elsewhen(reg2_raw) {
    io.S_id2regfile.reg2_data := io.M_wb2regfile.wdata
  } .otherwise{
    io.S_id2regfile.reg2_data := reg2_data
  }

}

class id extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()
    val stall = in UInt(stallDir bits)
    val flush = in Bool()
    val S_if_id2id = slave(if_id2id_interface(CoreConfig()))
    val M_id2regfile = master(id2regfile_interface(CoreConfig()))
    val M_id2ex = master(id2id_ex_interface(CoreConfig()))
  }
  // 指令判断过程：
  // op [31:26] -->  ==SPECIAL  --> op2 [10:6]=0 --> op3 [5:0] 决定 or/and/xor/nor/sllv/srlv/srav/sync
  //                                                              movn/movz/mthi/mtlo/mfhi/mflo
  //                                                              add/addu/sub/subu/slt/sltu/mult/multu
  //                                                              div/divu
  //                                                              jr/jalr
  //                                                              （其余无效）
  //                           --> op3 [5:0] 决定 teq/tge/tgeu/tlt/tltu/tne/syscall
  //                           -->  其余无效
  //
  //            -->  ori/andi/xori/lui/pref
  //                 addi/addiu/slti/sltiu
  //                 j/jal/beq/bgtz/blez/bne
  //                 lb/lbu/lh/lhu/lw/lwl/lwr/sb/sh/sw/swl/swr
  //                 ll/sc
  //            -->  ==COP0      -->   [25:21] 决定 mtc0/mfc0
  //                             -->   [5:0] 决定 eret
  //            -->  ==SPECIAL2  -->   op3 [5:0] 决定 clz/clo/mul
  //                                                 madd/maddu/msub/msubu
  //            -->  ==REGIMM    -->   op4 [20:16] 决定 bltz/bltzal/bgez/bgezal/bal
  //                                                   teqi/tgei/tgeiu/tlti/tltiu/tnei
  //            --> 其余无效
  //
  // [31:21] == 0 --> op3 [5:0] 决定 sll/srl/sra
  //               --> 其余无效
  val op1 = io.S_if_id2id.inst(31 downto 26)
  val op2 = io.S_if_id2id.inst(25 downto 21)
  val op3 = io.S_if_id2id.inst(20 downto 16)
  val op4 = io.S_if_id2id.inst(15 downto 11)
  val op5 = io.S_if_id2id.inst(10 downto 6)
  val op6 = io.S_if_id2id.inst(5 downto 0)

  val isInstValid = Bool()
  val imm = UInt(RegBus bits)

  when(io.rst===RstEnable) {
    isInstValid := InstInvalid  // 指令有效标记位，初始时记为无效指令
    //r_isSyscall := noException  // 系统调用异常，初始为无
    //r_isEret := noException  // 异常返回，初始不返回
    io.M_id2ex.alusel := EXE_RES_NOP // 运算类型（逻辑or算数）
    io.M_id2ex.aluop := EXE_NOP_OP // 运算子类型（如：逻辑类型下的 and、or、not,默认为NOP
    io.M_id2ex.wreg_addr := NOPRegAddr
    io.M_id2ex.wreg_we := WriteDisable
    io.M_id2regfile.reg1_rden := ReadDisable
    io.M_id2regfile.reg2_rden := ReadDisable
    io.M_id2regfile.reg1_addr := NOPRegAddr
    io.M_id2regfile.reg2_addr := NOPRegAddr
    imm := ZeroWord
    // 转移/分支相关
    // pc相关
    //io.M_branch_id2pcReg.brach_flag := NotBranch
    //io.M_branch_id2pcReg.brach_targetAddress := ZeroWord
    // idEx相关
    //io.M_branch_id2idEx.link_addr := ZeroWord
    //io.M_branch_id2idEx.next_inst_inDelayslot := NotInDelaySlot
    //io.M_branch_id2idEx.is_inDelayslot := NotInDelaySlot
  } .otherwise {
    isInstValid := InstInvalid  // 指令有效标记位，初始时记为无效指令
    //r_isSyscall := noException  // 系统调用异常，初始为无
    //r_isEret := noException  // 异常返回，初始不返回
    io.M_id2ex.alusel := EXE_RES_NOP // 运算类型（逻辑or算数）
    io.M_id2ex.aluop := EXE_NOP_OP // 运算子类型（如：逻辑类型下的 and、or、not,默认为NOP
    io.M_id2ex.wreg_addr := op4
    io.M_id2ex.wreg_we := WriteDisable
    io.M_id2regfile.reg1_rden := ReadDisable
    io.M_id2regfile.reg2_rden := ReadDisable
    io.M_id2regfile.reg1_addr := op2
    io.M_id2regfile.reg2_addr := op3
    imm := ZeroWord
    // 转移/分支相关
    // pc相关
    //io.M_branch_id2pcReg.brach_flag := NotBranch
    //io.M_branch_id2pcReg.brach_targetAddress := ZeroWord
    // idEx相关
    //io.M_branch_id2idEx.link_addr := ZeroWord
    //io.M_branch_id2idEx.next_inst_inDelayslot := NotInDelaySlot
    //io.M_branch_id2idEx.is_inDelayslot := NotInDelaySlot
    switch(op1){
      is(EXE_SPECIAL_INST) { // [31:26] 为特殊指令 SPEICAL
        is(op5===U"5'b00000"){  // [10:6] 为全0
          switch(op6){  // 根据[5:0] 判断特定指令
            is(EXE_AND){
              isInstValid := InstValid  // 指令有效标记位，初始时记为无效指令
              //r_isSyscall := noException  // 系统调用异常，初始为无
              //r_isEret := noException  // 异常返回，初始不返回
              io.M_id2ex.alusel := EXE_RES_LOGIC // 运算类型（逻辑or算数）
              io.M_id2ex.aluop := EXE_AND_OP // 运算子类型（如：逻辑类型下的 and、or、not,默认为NOP
              //io.M_id2ex.wreg_addr := op4 // [15:11]
              io.M_id2ex.wreg_we := WriteEnable
              io.M_id2regfile.reg1_rden := ReadEnable
              io.M_id2regfile.reg2_rden := ReadEnable
              //io.M_id2regfile.reg1_addr := op2
              //io.M_id2regfile.reg2_addr := op3
              //imm := ZeroWord
            }
            is(EXE_SRLV){
              isInstValid := InstValid
              io.M_id2ex.alusel := EXE_RES_SHIFT
              io.M_id2ex.aluop := EXE_SRLV_OP
              io.M_id2ex.wreg_we := WriteEnable
              io.M_id2regfile.reg1_rden := ReadEnable
              io.M_id2regfile.reg2_rden := ReadEnable
            }
            is(EXE_MOVN){
              isInstValid := InstValid
              io.M_id2ex.alusel := EXE_RES_MOVE
              io.M_id2ex.aluop := EXE_MOVN_OP
              io.M_id2ex.wreg_we := (io.M_id2regfile.reg2_data===ZeroWord)?   WriteDisable | WriteEnable
              io.M_id2regfile.reg1_rden := ReadEnable
              io.M_id2regfile.reg2_rden := ReadEnable
            }
            is(EXE_AND){
              isInstValid := InstValid
              io.M_id2ex.alusel := EXE_RES_ARITHMETIC
              io.M_id2ex.aluop := EXE_AND_OP
              io.M_id2ex.wreg_we := WriteEnable
              io.M_id2regfile.reg1_rden := ReadEnable
              io.M_id2regfile.reg2_rden := ReadEnable
            }
            is(EXE_MULT){
              isInstValid := InstValid
              io.M_id2ex.alusel := EXE_RES_NOP  // 不写入common regfile，涉及HILO
              io.M_id2ex.aluop := EXE_MULT_OP
              io.M_id2ex.wreg_we := WriteEnable
              io.M_id2regfile.reg1_rden := ReadEnable
              io.M_id2regfile.reg2_rden := ReadEnable
            }
            is(EXE_DIV){
              isInstValid := InstValid
              io.M_id2ex.alusel := EXE_RES_NOP  // 不写入common regfile，涉及HILO
              io.M_id2ex.aluop := EXE_DIV_OP
              io.M_id2ex.wreg_we := WriteEnable
              io.M_id2regfile.reg1_rden := ReadEnable
              io.M_id2regfile.reg2_rden := ReadEnable
            }
            is(EXE_JR){
              isInstValid := InstValid
              io.M_id2ex.alusel := EXE_RES_JUMP_BRANCH
              io.M_id2ex.aluop := EXE_JR_OP
              io.M_id2ex.wreg_we := WriteDisable
              io.M_id2regfile.reg1_rden := ReadEnable
              io.M_id2regfile.reg2_rden := ReadDisable
              // TODO with branch & jump //
              // pc相关
              //io.M_branch_id2pcReg.brach_flag := Branch
              //io.M_branch_id2pcReg.brach_targetAddress := io.M_id2idEx.reg1  // 取rs的值作为新的指令地址
              // idEx相关
              //io.M_branch_id2idEx.next_inst_inDelayslot := InDelaySlot
            }
          }

        }
      }
      is(EXE_ANDI){
        isInstValid := InstValid  // 指令有效标记位，初始时记为无效指令
        io.M_id2ex.alusel := EXE_RES_LOGIC // 运算类型（逻辑or算数）
        io.M_id2ex.aluop := EXE_ANDI_OP // 运算子类型（如：逻辑类型下的 and、or、not,默认为NOP
        io.M_id2ex.wreg_addr := op3 // [20:16]
        io.M_id2ex.wreg_we := WriteEnable
        io.M_id2regfile.reg1_rden := ReadEnable
        io.M_id2regfile.reg2_rden := ReadDisable
        imm := io.S_if_id2id.inst(15 downto 0).resize(RegBus bits)
      }
      is(EXE_ADDI){
        isInstValid := InstValid
        io.M_id2ex.alusel := EXE_RES_ARITHMETIC
        io.M_id2ex.aluop := EXE_ADDI_OP
        io.M_id2ex.wreg_addr := op3 // [20:16]
        io.M_id2ex.wreg_we := WriteEnable
        io.M_id2regfile.reg1_rden := ReadEnable
        io.M_id2regfile.reg2_rden := ReadDisable
        imm := U(16 bits,default -> io.S_if_id2id.inst(15))  @@ io.S_if_id2id.inst(15 downto 0)  // 符号位扩展
      }
      is(EXE_LW){
        isInstValid := InstValid
        io.M_id2ex.alusel := EXE_RES_LOAD_STORE
        io.M_id2ex.aluop := EXE_LW_OP
        io.M_id2ex.wreg_addr := op3
        io.M_id2ex.wreg_we := WriteEnable
        io.M_id2regfile.reg1_rden := ReadEnable
        io.M_id2regfile.reg2_rden := ReadDisable
      }
      is(EXE_SW){
        isInstValid := InstValid
        io.M_id2ex.alusel := EXE_RES_LOAD_STORE
        io.M_id2ex.aluop := EXE_SW_OP
        io.M_id2ex.wreg_addr := op3
        io.M_id2ex.wreg_we := WriteEnable
        io.M_id2regfile.reg1_rden := ReadEnable
        io.M_id2regfile.reg2_rden := ReadDisable
      }
      // TODO with others instructions //
    }
  }
  // TODO 先不考虑hazard //
  when(io.rst===RstEnable){
    io.M_id2ex.reg1_data := ZeroWord
  } .elsewhen(io.M_id2regfile.reg1_rden===ReadDisable){
    io.M_id2ex.reg1_data := ZeroWord  // imm X
  } .elsewhen(True){  // ignore hazard
    io.M_id2ex.reg1_data := io.M_id2regfile.reg1_data
  } .otherwise{
    io.M_id2ex.reg1_data := ZeroWord
  }
  when(io.rst===RstEnable){
    io.M_id2ex.reg2_data := ZeroWord
  } .elsewhen(io.M_id2regfile.reg2_rden===ReadDisable){
    io.M_id2ex.reg2_data := imm
  } .elsewhen(True){  // ignore hazard
    io.M_id2ex.reg2_data := io.M_id2regfile.reg2_data
  } .otherwise{
    io.M_id2ex.reg2_data := ZeroWord
  }
}

class id_ex extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()
    val stall = in UInt (stallDir bits)
    val flush = in Bool()
    val S_id2id_ex = slave(id2id_ex_interface(CoreConfig()))
    val M_id_ex2ex = master(id_ex2ex_interface(CoreConfig()))
  }
  val aluop_r = Reg(UInt(AluOpBus bits)) init(EXE_NOP_OP)
  val alusel_r = Reg(UInt(AluSelBus bits))  init(EXE_RES_NOP)
  val reg1_data_r = Reg(UInt(RegBus bits))  init(ZeroWord)
  val reg2_data_r = Reg(UInt(RegBus bits))  init(ZeroWord)
  val wreg_we_r = Reg(Bool()) init(WriteDisable)
  val wreg_addr_r = Reg(UInt(RegAddrBus bits))  init(ZeroWord)
  // 转移/分支指令相关缓存
  //val is_inDelayslot_r = Reg(io.S_branch_id2idEx.is_inDelayslot) init(NotInDelaySlot)
  //val link_addr_r = Reg(io.S_branch_id2idEx.link_addr) init(ZeroWord)
  //val next_inDelayslot_r = Reg(io.S_branch_id2idEx.next_inst_inDelayslot) init(NotInDelaySlot)
  // 异常指令相关缓存
  //val r_except = Reg(io.S_except_id2idEx)
  //r_except.exceptType init(ZeroWord)
  //r_except.currentInst_addr init(ZeroWord)
  when(io.flush===hasException){
    aluop_r := EXE_NOP_OP
    alusel_r := EXE_RES_NOP
    reg1_data_r := ZeroWord
    reg2_data_r := ZeroWord
    wreg_we_r := WriteDisable
    wreg_addr_r := ZeroWord
    // TODO with branch & jump & except //
  } .elsewhen(io.stall(2)===Stop && io.stall(3)===NoStop){
    aluop_r := EXE_NOP_OP
    alusel_r := EXE_RES_NOP
    reg1_data_r := ZeroWord
    reg2_data_r := ZeroWord
    wreg_we_r := WriteDisable
    wreg_addr_r := ZeroWord
  } .elsewhen(io.stall(2)===NoStop){
    aluop_r := io.S_id2id_ex.aluop
    alusel_r := io.S_id2id_ex.alusel
    reg1_data_r := io.S_id2id_ex.reg1_data
    reg2_data_r := io.S_id2id_ex.reg2_data
    wreg_we_r := io.S_id2id_ex.wreg_we
    wreg_addr_r := io.S_id2id_ex.wreg_addr
  } .otherwise{
    // Empty
  }
  io.M_id_ex2ex.aluop := aluop_r
  io.M_id_ex2ex.alusel := alusel_r
  io.M_id_ex2ex.reg1_data := reg1_data_r
  io.M_id_ex2ex.reg2_data := reg2_data_r
  io.M_id_ex2ex.wreg_we := wreg_we_r
  io.M_id_ex2ex.wreg_addr := wreg_addr_r
}

class ex extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()
    val stall = in UInt (stallDir bits)
    val flush = in Bool()
    val S_id_ex2ex = slave(id_ex2ex_interface(CoreConfig()))
    val M_ex2ex_mem = master(wb2regfileInterface(CoreConfig()))
  }
  val res = UInt(RegBus bits)
  val data1 = io.S_id_ex2ex.reg1_data
  val data2 = io.S_id_ex2ex.reg2_data
  switch(io.S_id_ex2ex.aluop){
    is(EXE_AND_OP | EXE_ANDI_OP){
      res := data1 & data2
    }
    is(EXE_ADD_OP | EXE_ADDI_OP){
      res := data1 + data2
    }
    is(EXE_SRLV_OP){
      res := data2 |<< data1(4 downto 0)
    }
  }

  when(io.rst===RstEnable){
    io.M_ex2ex_mem.wdata := ZeroWord
    io.M_ex2ex_mem.we := WriteDisable
    io.M_ex2ex_mem.waddr := ZeroWord
  } .elsewhen(False) {  // TODO :若为加、减法，且为溢出即不写入的操作指令，写入标记位记为0
    io.M_ex2ex_mem.wdata := res
    io.M_ex2ex_mem.we := WriteDisable
    io.M_ex2ex_mem.waddr := io.S_id_ex2ex.wreg_addr
  } .otherwise{
    io.M_ex2ex_mem.wdata := res
    io.M_ex2ex_mem.we := io.S_id_ex2ex.wreg_we
    io.M_ex2ex_mem.waddr := io.S_id_ex2ex.wreg_addr
  }

}

class ex_mem extends Component with Global_parameter with Interface_MS{

}

trait Interface_MS extends Global_parameter {
  // if_id2id_interface case
  case class if_id2id_interface(config: CoreConfig) extends Bundle with IMasterSlave {
    //  Address and control
    val pc = UInt(InstAddrBus bits)
    val inst = UInt(InstBus bits)

    override def asMaster(): Unit = {
      out(pc,inst)
      }
    }

  // id2regfile_interface case
  case class id2regfile_interface(config: CoreConfig) extends Bundle with IMasterSlave {
    val reg1_rden = Bool()
    val reg2_rden = Bool()
    val reg1_addr = UInt(RegAddrBus bits)
    val reg2_addr = UInt(RegAddrBus bits)
    val reg1_data = UInt(RegBus bits)
    val reg2_data = UInt(RegBus bits)

    override def asMaster(): Unit = {
      out(reg1_rden,reg2_rden,reg1_addr,reg2_addr)
      in(reg1_data,reg2_data)
    }
  }

  // id2id_ex_interface case
  case class id2id_ex_interface(config: CoreConfig) extends Bundle with IMasterSlave {
    //  Address and control
    val aluop = UInt(AluOpBus bits)
    val alusel = UInt(AluSelBus bits)
    val reg1_data = UInt(RegBus bits)
    val reg2_data = UInt(RegBus bits)
    val wreg_we = Bool()
    val wreg_addr = UInt(RegAddrBus bits)
    //val inst = UInt(InstBus bits)
    //val is_in_delayslot = Bool()
    //val link_addr = UInt(InstAddrBus bits)
    override def asMaster(): Unit = {
      out(aluop,alusel,reg1_data,reg2_data,wreg_we,wreg_addr)
    }
  }

  // id_ex2ex_interface
  case class id_ex2ex_interface(config: CoreConfig) extends Bundle with IMasterSlave {
    //  Address and control
    val aluop = UInt(AluOpBus bits)
    val alusel = UInt(AluSelBus bits)
    val reg1_data = UInt(RegBus bits)
    val reg2_data = UInt(RegBus bits)
    val wreg_we = Bool()
    val wreg_addr = UInt(RegAddrBus bits)
    //val inst = UInt(InstBus bits)
    //val is_in_delayslot = Bool()
    //val link_addr = UInt(InstAddrBus bits)
    override def asMaster(): Unit = {
      out(aluop,alusel,reg1_data,reg2_data,wreg_we,wreg_addr)
    }
  }



  // 使用wInterface贯穿 EX-->EX/MEM-->MEM-->MEM/WB-->regFile过程
  case class wb2regfileInterface(config: CoreConfig)extends Bundle with IMasterSlave {
    val wdata = UInt(DataBus bits)
    val waddr = UInt(DataAddrBus bits)
    val we = Bool()
    override def asMaster(): Unit = {
      out(wdata)
      out(waddr)
      out(we)
    }
  }
}

case class CoreConfig(){
  // TODO as Global Parameters //
}


//Generate the MIPS32's Verilog
object MIPS32Verilog {
  def main(args: Array[String]) {
    SpinalVerilog(new pc_reg)
  }
}

trait Global_parameter{
  // 全局参数
  val RstEnable: Bool = True
  val RstDisable: Bool = False
  val ZeroWord = U"32'h0"
  val WriteEnable: Bool = True
  val WriteDisable: Bool = False
  val ReadEnable: Bool = True
  val ReadDisable: Bool = False
  val AluOpBus = 8  // 译码阶段输出 aluop_o的宽度
  val AluSelBus = 3 // 译码阶段输出 alusel_o输出
  val InstValid: Bool = False  // 指令有效，则非无效指令，记为False（用于无效指令异常判断）
  val InstInvalid: Bool = True  // 指令无效
  val Stop: Bool = True
  val NoStop: Bool = False
  val InDelaySlot: Bool = True  // 是否为延迟槽
  val NotInDelaySlot: Bool = False
  val Branch: Bool = True  // 分支预测
  val NotBranch: Bool = False
  val InterruptAssert: Bool = True
  val InterruptNotAssert: Bool = False
  val TrapAssert: Bool = True  // 系统调用产生的中断
  val TrapNotAssert: Bool = False
  val True_v: Bool = True  // 用于表示存在异常
  val False_v: Bool = False  // 用于表示无异常
  val ChipEnable: Bool = True  // 芯片使能
  val ChipDisable: Bool = False

  val Reg_31 = U"5'b11111"  // 一般寄存器堆的$31地址

  val stallDir:Int = 6  // 暂停指令的涉及阶段数目

  // 原始指令
  // 逻辑指令
  val EXE_AND =  U"6'b100100"
  val EXE_OR  =  U"6'b100101"
  val EXE_XOR =  U"6'b100110"
  val EXE_NOR =  U"6'b100111"
  val EXE_ANDI = U"6'b001100"
  val EXE_ORI  = U"6'b001101"
  val EXE_XORI = U"6'b001110"
  val EXE_LUI  = U"6'b001111"
  // 移位指令
  val EXE_SLL  = U"6'b000000"
  val EXE_SLLV = U"6'b000100"
  val EXE_SRL  = U"6'b000010"
  val EXE_SRLV = U"6'b000110"
  val EXE_SRA  = U"6'b000011"
  val EXE_SRAV = U"6'b000111"
  // 移动指令
  val EXE_MOVZ = U"6'b001010"
  val EXE_MOVN = U"6'b001011"
  val EXE_MFHI = U"6'b010000"
  val EXE_MTHI = U"6'b010001"
  val EXE_MFLO = U"6'b010010"
  val EXE_MTLO = U"6'b010011"
  // 算数指令
  val EXE_SLT   = U"6'b101010"
  val EXE_SLTU  = U"6'b101011"
  val EXE_SLTI  = U"6'b001010"
  val EXE_SLTIU = U"6'b001011"
  val EXE_ADD   = U"6'b100000"
  val EXE_ADDU  = U"6'b100001"
  val EXE_SUB   = U"6'b100010"
  val EXE_SUBU  = U"6'b100011"
  val EXE_ADDI  = U"6'b001000"
  val EXE_ADDIU = U"6'b001001"
  val EXE_CLZ   = U"6'b100000"
  val EXE_CLO   = U"6'b100001"
  // 乘法指令
  val EXE_MULT  = U"6'b011000"
  val EXE_MULTU = U"6'b011001"
  val EXE_MUL   = U"6'b000010"
  // 乘累加、乘累减指令
  val EXE_MADD  = U"6'b000000"
  val EXE_MADDU = U"6'b000001"
  val EXE_MSUB  = U"6'b000100"
  val EXE_MSUBU = U"6'b000101"
  // 除法指令
  val EXE_DIV  = U"6'b011010"
  val EXE_DIVU = U"6'b011011"
  // 转移/分支指令
  val EXE_J      = U"6'b000010"
  val EXE_JAL    = U"6'b000011"
  val EXE_JALR   = U"6'b001001"
  val EXE_JR     = U"6'b001000"
  val EXE_BEQ    = U"6'b000100"
  val EXE_BGEZ   = U"5'b00001"
  val EXE_BGEZAL = U"5'b10001"
  val EXE_BGTZ   = U"6'b000111"
  val EXE_BLEZ   = U"6'b000110"
  val EXE_BLTZ   = U"5'b00000"
  val EXE_BLTZAL = U"5'b10000"
  val EXE_BNE    = U"6'b000101"
  // 加载/储存指令
  val EXE_LB     = U"6'b100000"
  val EXE_LBU    = U"6'b100100"
  val EXE_LH     = U"6'b100001"
  val EXE_LHU    = U"6'b100101"
  val EXE_LL     = U"6'b110000"
  val EXE_LW     = U"6'b100011"
  val EXE_LWL    = U"6'b100010"
  val EXE_LWR    = U"6'b100110"
  val EXE_SB     = U"6'b101000"
  val EXE_SC     = U"6'b111000"
  val EXE_SH     = U"6'b101001"
  val EXE_SW     = U"6'b101011"
  val EXE_SWL    = U"6'b101010"
  val EXE_SWR    = U"6'b101110"
  // 特殊指令
  val EXE_SYNC = U"6'b001111"
  val EXE_PREF = U"6'b110011"
  val EXE_NOP  = U"6'b000000"
  val SSNOP    = U"32'b00000000000000000000000001000000"
  // 协处理器相关指令
  val COP0    = U"6'b010000"
  val EXE_MTC0 = U"5'b00100"
  val EXE_MFC0 = U"5'b00000"
  // 异常相关指令
  val EXE_SYSCALL       = U"6'b001100"
  val EXE_TEQ           = U"6'b110100"
  val EXE_TEQI          = U"5'b01100"
  val EXE_TGE           = U"6'b110000"
  val EXE_TGEI          = U"5'b01000"
  val EXE_TGEIU         = U"5'b01001"
  val EXE_TGEU          = U"6'b110001"
  val EXE_TLT           = U"6'b110010"
  val EXE_TLTI          = U"5'b01010"
  val EXE_TLTIU         = U"5'b01011"
  val EXE_TLTU          = U"6'b110011"
  val EXE_TNE           = U"6'b110110"
  val EXE_TNEI          = U"5'b01110"
  val EXE_ERET          = U"6'b011000"
  //  val EXE_ERET          = U"32'b01000010000000000000000000011000"

  val EXE_SPECIAL_INST  = U"6'b000000"
  val EXE_REGIMM_INST   = U"6'b000001"
  val EXE_SPECIAL2_INST = U"6'b011100"

  // AluOp  ALU操作
  // 逻辑操作
  val EXE_AND_OP  =  U"8'b00100100"
  val EXE_OR_OP   =  U"8'b00100101"
  val EXE_XOR_OP  =  U"8'b00100110"
  val EXE_NOR_OP  =  U"8'b00100111"
  val EXE_ANDI_OP =  U"8'b01011001"  // I指使用立即数Immediate而不是二元操作数
  val EXE_ORI_OP  =  U"8'b01011010"
  val EXE_XORI_OP =  U"8'b01011011"
  val EXE_LUI_OP  =  U"8'b01011100"
  // 移位操作
  val EXE_SLL_OP  =  U"8'b01111100"
  val EXE_SLLV_OP =  U"8'b00000100"
  val EXE_SRL_OP  =  U"8'b00000010"
  val EXE_SRLV_OP =  U"8'b00000110"
  val EXE_SRA_OP  =  U"8'b00000011"
  val EXE_SRAV_OP =  U"8'b00000111"
  // 移动操作
  val EXE_MOVZ_OP =  U"8'b00001010"
  val EXE_MOVN_OP =  U"8'b00001011"
  val EXE_MFHI_OP =  U"8'b00010000"
  val EXE_MTHI_OP =  U"8'b00010001"
  val EXE_MFLO_OP =  U"8'b00010010"
  val EXE_MTLO_OP =  U"8'b00010011"
  // 算术操作
  val EXE_SLT_OP   = U"8'b00101010"
  val EXE_SLTU_OP  = U"8'b00101011"
  val EXE_SLTI_OP  = U"8'b01010111"
  val EXE_SLTIU_OP = U"8'b01011000"
  val EXE_ADD_OP   = U"8'b00100000"
  val EXE_ADDU_OP  = U"8'b00100001"
  val EXE_SUB_OP   = U"8'b00100010"
  val EXE_SUBU_OP  = U"8'b00100011"
  val EXE_ADDI_OP  = U"8'b01010101"
  val EXE_ADDIU_OP = U"8'b01010110"
  val EXE_CLZ_OP   = U"8'b10110000"
  val EXE_CLO_OP   = U"8'b10110001"
  // 乘法操作
  val EXE_MULT_OP  = U"8'b00011000"
  val EXE_MULTU_OP = U"8'b00011001"
  val EXE_MUL_OP   = U"8'b10101001"
  // 乘累加、乘累减操作
  val EXE_MADD_OP  = U"8'b10100110"
  val EXE_MADDU_OP = U"8'b10101000"
  val EXE_MSUB_OP  = U"8'b10101010"
  val EXE_MSUBU_OP = U"8'b10101011"
  // 除法操作
  val EXE_DIV_OP   = U"8'b00011010"
  val EXE_DIVU_OP  = U"8'b00011011"
  // 转移/分支操作
  val EXE_J_OP       = U"8'b01001111"
  val EXE_JAL_OP     = U"8'b01010000"
  val EXE_JALR_OP    = U"8'b00001001"
  val EXE_JR_OP      = U"8'b00001000"
  val EXE_BEQ_OP     = U"8'b01010001"
  val EXE_BGEZ_OP    = U"8'b01000001"
  val EXE_BGEZAL_OP  = U"8'b01001011"
  val EXE_BGTZ_OP    = U"8'b01010100"
  val EXE_BLEZ_OP    = U"8'b01010011"
  val EXE_BLTZ_OP    = U"8'b01000000"
  val EXE_BLTZAL_OP  = U"8'b01001010"
  val EXE_BNE_OP     = U"8'b01010010"
  // 加载/储存操作
  val EXE_LB_OP      = U"8'b11100000"
  val EXE_LBU_OP     = U"8'b11100100"
  val EXE_LH_OP      = U"8'b11100001"
  val EXE_LHU_OP     = U"8'b11100101"
  val EXE_LL_OP      = U"8'b11110000"
  val EXE_LW_OP      = U"8'b11100011"
  val EXE_LWL_OP     = U"8'b11100010"
  val EXE_LWR_OP     = U"8'b11100110"
  val EXE_PREF_OP    = U"8'b11110011"
  val EXE_SB_OP      = U"8'b11101000"
  val EXE_SC_OP      = U"8'b11111000"
  val EXE_SH_OP      = U"8'b11101001"
  val EXE_SW_OP      = U"8'b11101011"
  val EXE_SWL_OP     = U"8'b11101010"
  val EXE_SWR_OP     = U"8'b11101110"
  val EXE_SYNC_OP    = U"8'b00001111"
  // 协处理器相关操作
  val EXE_MFC0_OP    = U"8'b01011101"
  val EXE_MTC0_OP    = U"8'b01100000"
  // 异常相关操作
  val EXE_SYSCALL_OP  = U"8'b00001100"
  val EXE_TEQ_OP      = U"8'b00110100"
  val EXE_TEQI_OP     = U"8'b01001000"
  val EXE_TGE_OP      = U"8'b00110000"
  val EXE_TGEI_OP     = U"8'b01000100"
  val EXE_TGEIU_OP    = U"8'b01000101"
  val EXE_TGEU_OP     = U"8'b00110001"
  val EXE_TLT_OP      = U"8'b00110010"
  val EXE_TLTI_OP     = U"8'b01000110"
  val EXE_TLTIU_OP    = U"8'b01000111"
  val EXE_TLTU_OP     = U"8'b00110011"
  val EXE_TNE_OP      = U"8'b00110110"
  val EXE_TNEI_OP     = U"8'b01001001"
  val EXE_ERET_OP     = U"8'b01101011"
  // 空
  val EXE_NOP_OP = U"8'b00000000"

  //AluSel
  val EXE_RES_LOGIC           = U"3'b001"  // 逻辑操作
  val EXE_RES_SHIFT           = U"3'b010"  // 移位操作
  val EXE_RES_MOVE            = U"3'b011"  // 移动操作
  val EXE_RES_ARITHMETIC      = U"3'b100"  // 算术操作
  val EXE_RES_MUL             = U"3'b101"  // 乘法操作
  val EXE_RES_JUMP_BRANCH     = U"3'b110" // 转移/分支操作
  val EXE_RES_LOAD_STORE      = U"3'b111"  // 加载/储存操作
  val EXE_RES_NOP             = U"3'b000"

  // 除法相关
  val DivResultReady:Bool = True
  val DivResultNotReady: Bool = False
  val DivStart: Bool = True
  val DivStop: Bool = False
  val DivIsAnnul:Bool = True
  val DivNotAnnul:Bool = False
  val DivIsSignal:Bool = True
  val DivNotSignal:Bool = False

  //指令存储器inst_rom
  val InstAddrBus = 32  // ROM地址总线宽度
  val InstBus = 32  // ROM数据总线宽度
  //  val InstMemNum = 131071  // ROM实际大小：128KB
  //  val InstMemNumLog2 = 17  // ROM实际使用的地址宽度,即 2^17=131071，PC计数为32bits，实际上对于ROM储存区只用到了17bits即可
  val InstMemNum = 65536  // ROM实际大小：64KB
  val InstMemNumLog2 = 16  // ROM实际使用的地址宽度,即 2^16=65536，PC计数为32bits，实际上对于ROM储存区只用到了16bits即可

  // cpu通用寄存器regfile
  val RegAddrBus = 5  // Regfile模块地址线宽度
  val RegBus = 32  // Regfile模块数据线宽度
  val RegWidth = 32  // 通用寄存器宽度
  val DoubleRegWidth = 64  // 双倍的通用寄存器宽度
  val DoubleRegBus = 64 // 双倍的通用寄存器数据线宽度
  val cntWidth = 2 // 流水线暂停相关的计数长度
  val RegNum = 32  // 通用寄存器数量
  val RegNumLog2 = 5  // 通用寄存器使用的地址位数
  val NOPRegAddr = U"5'b00000"

  // 数据储存器（访存涉及的内存RAM）
  val DataAddrBus   = 32
  val DataBus       = 32
  val DataMemNum    = 65536  // // 单个RAM实际大小：64KB
  val DataMemNumLog2  = 16  // 单个RAM实际使用的地址宽度,即 2^16=65536
  val ByteWidth       = 8
  val MemSelBus = 4 // mem的字节选择宽度（0000-1111）
  val MemSelZero = U"4'b0000"  // 内存字节的初始化选择为第0位
  val MemSelOne = U"4'b1111"  // 内存字节的初始化选择为无效的全1
  val Memsel_1 = U"4'b1000"  // 内存返回字的第一个字节（首字节）
  val Memsel_2 = U"4'b0100"
  val Memsel_3 = U"4'b0010"
  val Memsel_4 = U"4'b0001"
  val Memsel_1H = U"4'b1100"  // 第一个半字
  val Memsel_2H = U"4'b0011"
  val Memsel_maskL1 = U"4'b0111"  // SWL 一个字节空悬的的掩码
  val Memsel_maskL2 = U"4'b0011"
  val Memsel_maskL3 = U"4'b0001"
  val Memsel_maskR1 = U"4'b1110"  // SWR 一个字节空悬的的掩码
  val Memsel_maskR2 = U"4'b1100"
  val Memsel_maskR3 = U"4'b1000"

  // llBit 链接状态位相关
  val llBitOne = True
  val llBitZero = False

  // CP0寄存器地址
  val CP0_REG_COUNT     = U"5'b01001"        //可读写
  val CP0_REG_COMPARE   = U"5'b01011"      //可读写
  val CP0_REG_STATUS    = U"5'b01100"       //可读写
  val CP0_REG_CAUSE     = U"5'b01101"        //只读
  val CP0_REG_EPC       = U"5'b01110"          //可读写
  val CP0_REG_PrId      = U"5'b01111"         //只读
  val CP0_REG_CONFIG    = U"5'b10000"       //只读
  val CPU0AddrBus:Int  =  5  // cpu0的地址宽度
  val CPU0RegBus:Int   = 32  // cpu0内部寄存器宽度
  val InterruptBus:Int = 6  // 输入外界中断的宽度

  // 异常处理相关
  val ExceptTypeBus   = 32  // 异常类型的宽度
  val hasException = True  // 有异常发生
  val noException = False  // 无异常
  val IsInException = True  // 处于异常处理状态
  val NotInException = False  // 不处于异常处理状态
  val AllowException = True  // 允许中断
  val NotAllowException = False  // 不允许中断

  // Configure the clock domain
  val ClockConfig_rstH = ClockDomainConfig(
    clockEdge        = RISING,
    resetKind        = SYNC,
    resetActiveLevel = HIGH  // RstEnable为1
  )
  val ClockConfig_rstL = ClockDomainConfig(
    clockEdge        = RISING,
    resetKind        = SYNC,
    resetActiveLevel = LOW  // RstEnable为0
  )
}