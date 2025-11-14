package mainelib
import spinal.core._
import spinal.lib._

/** 统一配置中心：基础字段可覆写；派生字段自动计算 */
case class CoreConfig(
                    // —— 基础可配置项（需要时在顶层覆写）——
                    rv32            : Boolean = true,   // true=RV32 → XLEN=32; false=RV64 → XLEN=64
                    rvc             : Boolean = false,
                    withbranch_predict: Boolean = true,
                    //addrWidthBase   : Int     = 32,     // 如果想让地址位宽≠xlen，可改这个
                    issueWidth      : Int     = 1,
                    PipeDepth       : Int     = 8,
                    withFpu         : Boolean = false,
                    withVpu         : Boolean = false,
                    debug_en        : Boolean = true,
                    INITVTOR        : Int     = 0,

                    BHT_OFFSET      : Int     = 4,      // 先按你现有默认值
                    BTB_PAGE_WIDTH  : Int     = 3,
                    BTB_PAGE_NUM    : Int     = 8,
                    BTB_TARGET_NUM  : Int     = 32,

                    REG_NUM         : Int     = 32, // ARF
                    REG_PRF_NUM     : Int     = 64,

                    DivCycle_WIDTH  : Int     = 5,
                    DivCycle_NUM    : Int     = 32,      // TODO pipeline

                    Instr_FIFO_DEPTH: Int     = 8,      // 你的注释：原 16
                    RAS_STACK_DEPTH : Int     = 8,

                    SCB_INSTR_DEPTH : Int     = 8,      // 你的注释：原 16

                    // 存储空间规模（单位：字节）
                    InstMemNum      : Int     = 65536,  // 64KB
                    DataMemNum      : Int     = 65536,  // 64KB
                    InstMemStart    : BigInt  = 0,
                    DataMemStart    : BigInt  = 65536,
                    InstBank        : Int     = 1,  // IMemDataWidth可以分成N个Bank分别读出部分数据
                    DataBank        : Int     = 1,  // DMemDataWidth可以分成N个Bank分别读出部分数据
                    IROMSize        : Int     = 4096,//16384,  // 16KB
                    DROMSize        : Int     = 4096,//16384,  // 16KB
                    ByteWidth       : Int     = 8,
                    IMemDataWidth   : Int     = 32,       // TODO with issuewidth
                    DMemDataWidth   : Int     = 32
                  ) {
  // —— 派生量（自动计算，不建议外部直接覆写）——
  def xlen: Int            = if(rv32) 32 else 64  // 只能小写xlen  // 暂时可能只支持32，for instr_realign
  def CSR_NUM: Int     = if(debug_en) 32 else 4096   // 理论可到 4096

  // 总线位宽（按你的定义把它们都跟 xlen 绑死；若要解耦可改成独立字段）
  def InstAddrBus : Int    = xlen
  def InstBus     : Int    = xlen
  def DataAddrBus : Int    = xlen
  def DataBus     : Int    = xlen
  def InstLen     : Int    = IMemDataWidth/ByteWidth
  def InstStep    : Int    = if(rvc) InstLen/2 else InstLen/4
  def InstSlice   : Int    = InstStep/issueWidth
  def RegAddrBus  : Int    = log2Up(REG_NUM)
  def RegDataBus  : Int    = xlen

  /** BIU **/
  def BusAW       : Int    = 32
  def BusDW       : Int    = 32

  def Reg_PID_WIDTH : Int  = log2Up(REG_PRF_NUM)
  def Reg_AID_WIDTH : Int  = log2Up(REG_NUM)
  def zeroArchId    : Int  = 0

  def ImmBus      : Int    = xlen

  def IMemSelBus   : Int    = IMemDataWidth/ByteWidth
  def IMemOffset   : Int    = log2Up(IMemSelBus)

  def DMemSelBus   : Int    = DMemDataWidth/ByteWidth
  def DataSelBus   : Int    = DataBus/ByteWidth
  def DMemOffset   : Int    = log2Up(DMemSelBus)

  def RAS_PTR_WIDTH    : Int = log2Up(RAS_STACK_DEPTH)
  def SCB_INSTR_WIDTH  : Int = log2Up(SCB_INSTR_DEPTH)
  def SCB_IU_DEPTH     : Int = SCB_INSTR_DEPTH
  def SCB_ID_WIDTH     : Int = log2Up(SCB_IU_DEPTH)+1

  def CSRAddrBus       : Int = log2Up(CSR_NUM)
  def CSRDataBus       : Int = xlen

  def InstMemNumLog2   : Int = log2Up(InstMemNum)
  def DataMemNumLog2   : Int = log2Up(DataMemNum)

  def InstMemEnd       : BigInt = InstMemStart + InstMemNum - 1

  def DataMemEnd       : BigInt = DataMemStart + DataMemNum - 1



}

