package mainelib
import spinal.core._
import spinal.lib._
/** chatgpt **/

/** RegRename with fix-A (read-old-before-write) */


case class RegRename(cfg : CoreConfig) extends Component {
  // ===== IO =====
  val io = new Bundle {
    // 上游发射（同拍 W 路）
    val reg_in   = Vec.fill(cfg.issueWidth)(slave(DispatchIn(cfg)))
    // 下游输出（同拍 W 路）
    val reg_out  = Vec.fill(cfg.issueWidth)(master(RenamedOut(cfg)))
    // 可用性：free-list 是否够本拍需求；不够则要求上游停发
    val out_stall = out Bool()


    // 提交（由 ROB 头部传来），同拍可回收多路
    val commit = Vec(slave(CommitIn(cfg)), cfg.issueWidth)
    val wb = Vec(slave(CommitIn(cfg)), cfg.issueWidth)

    // 分支检查点建立/恢复
    val ckptOut = master(Flow(CkptToken(cfg)))   // 当本拍遇到分支时吐出快照
    val rollback= slave(Flow(CkptToken(cfg)))    // 回滚到该快照
  }

  // ===== 状态：RAT =====
  val rat = Vec(Reg(UInt(cfg.Reg_PID_WIDTH bits)) init 0, cfg.REG_NUM)
  val rat_ready = Vec(Reg(Bool()) init(False), cfg.REG_NUM)
  // 初始把 arch i 映射到 phys i（x0 单独特殊化）
  val initDone = RegNext(True) init(False)
  val initDone_d1 = RegNext(initDone) init(False)
  val initDone_d2 = RegNext(initDone_d1) init(False)

  when(initDone && !initDone_d1){
    for(i <- 0 until cfg.REG_NUM) {
      if (i == cfg.zeroArchId) { // x0 固定映射到 0
        rat(cfg.zeroArchId) := U(0, cfg.Reg_PID_WIDTH bits)
        rat_ready(cfg.zeroArchId) := True
      } else {
        rat(i) := U(i min (cfg.REG_PRF_NUM - 1), cfg.Reg_PID_WIDTH bits)
        rat_ready(i) := True
      }
    }
  }

  // ===== 状态：Free-List（循环队列，支持多路分配/回收） =====
  //val flMem   = Mem(UInt(cfg.Reg_PID_WIDTH bits), wordCount = cfg.REG_PRF_NUM)
  val flMem  = Vec.fill(cfg.REG_PRF_NUM)(Reg(UInt(cfg.Reg_PID_WIDTH bits)) init 0)
  val flHead  = Reg(UInt(log2Up(cfg.REG_PRF_NUM) bits)) init 0
  val flTail  = Reg(UInt(log2Up(cfg.REG_PRF_NUM) bits)) init 0
  val flCount = Reg(UInt(log2Up(cfg.REG_PRF_NUM+1) bits)) init 0

  // 建议：用 flCount 的位宽来生成 nReturn（避免过窄/0 位宽）
  // 小工具：把 inc 扩到 reg 位宽，再 +^，最后裁回
  def addTo(reg: UInt, inc: UInt): UInt =
    (reg +^ inc.resize(reg.getWidth)).resize(reg.getWidth)


  // ===== 提交回收：把 old_p 放回 free-list（忽略 x0 / 无目的 / 无效） =====
  // 同拍可回收多路，顺序写入 tail
  val retVec  = Vec(Bool(), cfg.issueWidth)
  val retIds  = Vec(UInt(cfg.Reg_PID_WIDTH bits), cfg.issueWidth)
  for(i <- 0 until cfg.issueWidth){
    val ok = io.commit(i).valid && io.commit(i).hasDest &&
      (io.commit(i).rd_arch =/= U(cfg.zeroArchId, cfg.Reg_AID_WIDTH bits))
    retVec(i) := ok
    retIds(i) := io.commit(i).old_p
  }
  val nReturnW = flCount.getWidth
  val nReturn: UInt =
    if (retVec.nonEmpty)
      retVec.map(b => b.asUInt.resize(nReturnW)).reduce(_ +^ _)
    else
      U(0, nReturnW bits)

  // 上电：把 [REG_NUM, REG_PRF_NUM) 放进 free-list（保留 0..REG_NUM-1 给初始映射）
  /*
  val initPtr = Reg(UInt(log2Up(cfg.REG_PRF_NUM+1) bits)) init U(cfg.REG_NUM)
  when(initPtr < cfg.REG_PRF_NUM && !ClockDomain.current.isResetActive){
    flMem.write(flTail, initPtr.resized)
    flTail  := flTail + 1
    flCount := flCount + 1
    initPtr := initPtr + 1
  } .otherwise{
    // 这样写就不会报错了
    flTail  := addTo(flTail,  nReturn)
    flCount := addTo(flCount, nReturn)
  }
   */

  for(i <- 0 until cfg.REG_PRF_NUM){
    when(initDone && !initDone_d1){
      flMem(i) := i
    }
  }

  when(initDone && !initDone_d1){
    flTail  := cfg.REG_PRF_NUM - 1
    flCount := cfg.REG_PRF_NUM - 1
  } .otherwise{
    // 这样写就不会报错了
    flTail  := addTo(flTail,  nReturn)
    flCount := addTo(flCount, nReturn)
  }



  // 计算本拍需要分配的个数（忽略 x0 / 无目的）
  val needAllocVec = Vec(Bool(), cfg.issueWidth)
  for(i <- 0 until cfg.issueWidth){
    val isX0 = (io.reg_in(i).rd === U(cfg.zeroArchId, cfg.Reg_AID_WIDTH bits))
    needAllocVec(i) := io.reg_in(i).valid && io.reg_in(i).hasDest && !isX0
  }
  //val needAlloc = needAllocVec.asBits.popCount
  val needAllocWidth = log2Up(cfg.issueWidth + 1)
  val needAlloc = needAllocVec.foldLeft(U(0, needAllocWidth bits)) { (acc, b) =>
    acc + b.asUInt
  }



  // 是否可分配
  val canAlloc = (flCount >= needAlloc)

  // ===== 同拍“预分配”若干物理号（顺序弹出） =====
  val allocIds = Vec(UInt(cfg.Reg_PID_WIDTH bits), cfg.issueWidth)
  // 只在 canAlloc 时使用这些值；否则保持不变（不会被下游读取）
  var rHead  = 0
  //val rHead  = Reg(UInt(log2Up(cfg.REG_PRF_NUM) bits)) init 0
  //rHead := flHead
  val flag = False
  for(i <- 0 until cfg.issueWidth){
    when(needAllocVec(i) && canAlloc){
      //allocIds(i) := flMem.readAsync(rHead)
      //allocIds(i) := flMem(rHead)
      //allocIds(i) := flMem(flHead+i)  //
      allocIds(i) := flMem(flHead+rHead)
      rHead = rHead + 1
      flag := True
    } otherwise {
      allocIds(i) := U(0) // 占位
    }
  }

/*
  // ===== 生成 reg_out（含同拍旁路 + RAT 更新影子） =====
  val ratShadow = Vec(UInt(cfg.Reg_PID_WIDTH bits), cfg.REG_NUM)
  val ratShadow_ready = Vec(Reg(Bool()) init(False), cfg.REG_NUM)
  for(i <- 0 until cfg.REG_NUM) {
    ratShadow(i) := rat(i)
    ratShadow_ready(i) := rat_ready(i)
  }

  // 输出默认
  for(i <- 0 until cfg.issueWidth){
    io.reg_out(i).valid    := io.reg_in(i).valid && canAlloc
    io.reg_out(i).hasDest  := io.reg_in(i).hasDest
    io.reg_out(i).rd_arch  := io.reg_in(i).rd
    io.reg_out(i).isBranch := io.reg_in(i).isBranch
    io.reg_out(i).dst_p    := U(0)
    io.reg_out(i).old_p    := U(0)

    // 源默认从 ratShadow 读（会被前槽覆盖）
    val src1p = ratShadow(io.reg_in(i).rs1).resize(cfg.Reg_PID_WIDTH+1 bits)
    val src2p = ratShadow(io.reg_in(i).rs2).resize(cfg.Reg_PID_WIDTH+1 bits)
    val src1p_ready = ratShadow_ready(io.reg_in(i).rs1)
    val src2p_ready = ratShadow_ready(io.reg_in(i).rs2)

    when(io.reg_in(i).rs1_rden === False) {
      src1p := U(cfg.REG_PRF_NUM, cfg.Reg_PID_WIDTH+1 bits) // 超出物理寄存器范围
      src1p_ready := True
    }
    when(io.reg_in(i).rs2_rden === False) {
      src2p := U(cfg.REG_PRF_NUM, cfg.Reg_PID_WIDTH+1 bits) // 超出物理寄存器范围
      src2p_ready := True
    }

//    val src1p = U(cfg.REG_PRF_NUM, cfg.Reg_PID_WIDTH+1 bits)  // 超出物理寄存器范围
//    val src2p = U(cfg.REG_PRF_NUM, cfg.Reg_PID_WIDTH+1 bits)
//    val src1p_ready = False
//    val src2p_ready = False

    // 同拍旁路：查找 0..i-1 槽是否重命名了对应 arch
    for(j <- 0 until i){
      val jWrites = io.reg_in(j).valid && io.reg_in(j).hasDest && (io.reg_in(j).rd =/= U(cfg.zeroArchId, cfg.Reg_AID_WIDTH bits)) && canAlloc
      when(jWrites && (io.reg_in(j).rd === io.reg_in(i).rs1) && io.reg_in(i).rs1_rden){
        src1p := allocIds(j)
        src1p_ready := False
      }
      when(jWrites && (io.reg_in(j).rd === io.reg_in(i).rs2) && io.reg_in(i).rs2_rden){
        src2p := allocIds(j)
        src2p_ready := False
      }
    }

    io.reg_out(i).src1_p := src1p
    io.reg_out(i).src2_p := src2p
    io.reg_out(i).src1_p_ready := src1p_ready
    io.reg_out(i).src2_p_ready := src2p_ready

    // 目的：若需要，分配一个 new_p，并记录 old_p；同时更新 ratShadow
    val need = needAllocVec(i) && canAlloc
    when(need){
      val newp = allocIds(i)
      val oldp = ratShadow(io.reg_in(i).rd)
      io.reg_out(i).dst_p := newp
      io.reg_out(i).old_p := oldp
      // 更新影子表
      ratShadow(io.reg_in(i).rd) := newp
      ratShadow_ready(io.reg_in(i).rd) := False
    }
  }
*/
  // ===== 生成 reg_out（写前读 + 同拍前递，使用逐槽视图）=====

  // 视图链：view(0) = 提交后的表；每个槽生成一个新的 next 视图
  val viewP  = Array.fill(cfg.issueWidth + 1)(Vec(UInt(cfg.Reg_PID_WIDTH bits), cfg.REG_NUM))
  val viewR  = Array.fill(cfg.issueWidth + 1)(Vec(Bool(),                          cfg.REG_NUM))

  // view(0) 取自当前时序 RAT
  for (k <- 0 until cfg.REG_NUM) {
    viewP(0)(k) := rat(k)
    viewR(0)(k) := rat_ready(k)
  }


  // todo try
  val pReady = Vec(Reg(Bool()) init(True), cfg.REG_PRF_NUM)
  //pReady(0) := True // x0 永远就绪（若你把 x0 固定映射到 phys0）

  // 写回/提交时把对应物理号置就绪（按你的系统选择 WB 或 COMMIT）
  for(i <- 0 until cfg.issueWidth) {
    when(io.commit(i).valid && io.commit(i).hasDest) {
      pReady(io.commit(i).old_p) := True // 或者用写回口 wb_p
    }
//    when(io.wb(i).valid && io.wb(i).hasDest) {
//      pReady(io.wb(i).old_p) := True // 或者用写回口 wb_p
//    }
  }

  for (i <- 0 until cfg.issueWidth) {
    val din  = io.reg_in(i)
    val dout = io.reg_out(i)

    // 先把 next 视图默认拷贝为当前视图（注意：这是组合赋值，不会覆盖写前读）
    for (k <- 0 until cfg.REG_NUM) {
      viewP(i+1)(k) := viewP(i)(k)
      viewR(i+1)(k) := viewR(i)(k)
    }

    // 默认输出
    dout.valid    := din.valid && canAlloc
    dout.hasDest  := din.hasDest
    dout.rd_arch  := din.rd
    dout.isBranch := din.isBranch
    dout.dst_p    := U(0)
    dout.old_p    := U(0)

    val rs1  = din.rs1
    val rs2  = din.rs2
    val rd   = din.rd
    val need = needAllocVec(i) && canAlloc

    // —— 写前读：从 view(i) 取 old_p / 源 —— //
    val oldp = viewP(i)(rd)

    val src1p_w = UInt(cfg.Reg_PID_WIDTH bits)
    val src2p_w = UInt(cfg.Reg_PID_WIDTH bits)
    val src1r_w = Bool()
    val src2r_w = Bool()


    src1p_w := viewP(i)(rs1)
    src2p_w := viewP(i)(rs2)
    src1r_w := viewR(i)(rs1)
    src2r_w := viewR(i)(rs2)



    // 无读端：ready=1（物理号无关）
    when(!din.rs1_rden){ src1r_w := True }
    when(!din.rs2_rden){ src2r_w := True }


    // 本条若 rs* == rd，则源应取“写前值” oldp
    when(need && (rs1 === rd) && din.rs1_rden){ src1p_w := oldp; src1r_w := viewR(i)(rd) }
    when(need && (rs2 === rd) && din.rs2_rden){ src2p_w := oldp; src2r_w := viewR(i)(rd) }

    // 同拍前递：仅看更早槽 j<i；命中则覆盖为 allocIds(j)，ready 置 False
    for (j <- 0 until i) {
      val jn  = io.reg_in(j)
      val jWr = jn.valid && jn.hasDest &&
        (jn.rd =/= U(cfg.zeroArchId, cfg.Reg_AID_WIDTH bits)) && canAlloc
      when(jWr && (jn.rd === rs1) && din.rs1_rden){ src1p_w := allocIds(j); src1r_w := False }
      when(jWr && (jn.rd === rs2) && din.rs2_rden){ src2p_w := allocIds(j); src2r_w := False }
    }

    // 驱动输出
    dout.src1_p       := src1p_w.resize(cfg.Reg_PID_WIDTH + 1 bits)
    dout.src2_p       := src2p_w.resize(cfg.Reg_PID_WIDTH + 1 bits)

    //dout.src1_p_ready := src1r_w
    //dout.src2_p_ready := src2r_w

    dout.src1_p_ready := (!din.rs1_rden) || pReady(src1p_w) //|| pReady_next(src1p_w) // 考虑到提交时撞上reg_in
    dout.src2_p_ready := (!din.rs2_rden) || pReady(src2p_w) //|| pReady_next(src2p_w)


    // 目的分配：写到“下一视图” view(i+1)，不改动本槽读到的 view(i)

    when(need){
      val newp = allocIds(i)
      dout.dst_p := newp
      dout.old_p := oldp
      viewP(i+1)(rd) := newp
      viewR(i+1)(rd) := False
      // 关键：新物理未写回，置未就绪
      pReady(newp) := False
    }
  }

  // 把最终视图作为 shadow，供后面 “canAlloc 时落表” 使用
  val ratShadow       = viewP(cfg.issueWidth)
  val ratShadow_ready = viewR(cfg.issueWidth)



  // ===== 真正提交：在 canAlloc 时把 rat := ratShadow，并更新 freeList head/count =====
  io.out_stall := !canAlloc
  when(canAlloc && initDone_d1){
    for(i <- 0 until cfg.REG_NUM) {
      rat(i) := ratShadow(i)
      rat_ready(i) := ratShadow_ready(i)
    }
    flHead  := flHead + needAlloc
    flCount := flCount - needAlloc + nReturn
  }



  // 顺序 push
  var wTail = flTail
  for(i <- 0 until cfg.issueWidth){
    when(retVec(i)){
      flMem.write(wTail, retIds(i))
      wTail := wTail + 1
    }
  }

  // ===== 检查点建立（遇到分支就吐快照；若同拍多分支，可按槽序吐多个） =====
  val firstBr = UInt(log2Up(cfg.issueWidth) bits)
  val anyBr   = io.reg_in.map(x => x.valid && x.isBranch && canAlloc).asBits.orR
  firstBr := 0
  for(i <- 0 until cfg.issueWidth) {
    when(io.reg_in(i).valid && io.reg_in(i).isBranch && canAlloc && (firstBr === 0)){
      firstBr := U(i)
    }
  }
  io.ckptOut.valid := anyBr
  io.ckptOut.payload.rat     := ratShadow
  io.ckptOut.payload.flHead  := (flHead + needAlloc).resized
  io.ckptOut.payload.flTail  := (flTail + nReturn).resized
  io.ckptOut.payload.flCount := (flCount - needAlloc) + nReturn

  // ===== 回滚：恢复 RAT 与 free-list 指针 =====
  // todo:需要区分mispredict和trap回滚
  when(io.rollback.valid){
    for(i <- 0 until cfg.REG_NUM) {
      rat(i) := io.rollback.payload.rat(i)
    }
    flHead  := io.rollback.payload.flHead
    //flTail  := io.rollback.payload.flTail
    //flCount := io.rollback.payload.flCount
//    flTail  := addTo(flTail,  nReturn)
//    flCount := addTo(flCount, nReturn)
    // 4) 依据环形距离重算 count：count = (tail - head) mod REG_PRF_NUM
    val tailW  = flTail
    val headW  = io.rollback.payload.flHead
    //flCount := (tailW -^ headW).resize(distW)   // 两数相减的模环距离
    when(tailW > headW) {
      flCount := (tailW - headW).resized
    } .otherwise{
      flCount := (cfg.REG_PRF_NUM + tailW - headW).resized
    }
  }
}

