package mainelib
import spinal.core.sim._

//MyTopLevel's testbench
object maine_sim {
  def main(args: Array[String]) {
    val FU_ID_selDef = FU_ID.build(CoreConfig().issueWidth)
    val ccfg = CoreConfig()
    CfgPrinter.printAll(ccfg)
    SimConfig.withWave.doSim(new maineriscv(ccfg)){dut =>   // 实例化top level dut
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      //var modelState = 0
      for(idx <- 0 to 1999){
        //Drive the dut inputs with random values
        //dut.io.stall #= Random.nextBoolean()
        //dut.io.stall.randomize()
        //dut.io.is_branch #= false //Random.nextBoolean()
        //dut.io.branch_target_pc.randomize()

        //Wait a rising edge on the clock
        dut.clockDomain.waitRisingEdge()

        // 遍历所有 cosim_dpi_if 端口（0 ~ issueWidth-1）
        for (portIdx <- 0 until ccfg.issueWidth) {
          // 读取当前端口的 valid 信号（转换为布尔值判断）
          val isvalid = dut.io.cosim_dpi_if(portIdx).commit_info.commit_vld.toBoolean

          // 仅当 valid=1 时打印信息
          if (isvalid) {
            // 读取当前端口的所有变量（根据你的 trace_entry 实际字段调整）
            val pc = dut.io.cosim_dpi_if(portIdx).commit_info.pc.toLong    // PC转十进制
            val inst = dut.io.cosim_dpi_if(portIdx).commit_info.instr.toLong // 指令转十进制（或十六进制）
            // 读取使能信号和相关字段（根据你的实际定义调整）
            val regWten = dut.io.cosim_dpi_if(portIdx).commit_info.reg_wif.reg_wten.toBoolean  // 寄存器写使能
            val regWaddr = dut.io.cosim_dpi_if(portIdx).commit_info.reg_wif.areg_addr.toLong  // 寄存器写使能
            val regWdata = dut.io.cosim_dpi_if(portIdx).commit_info.reg_wif.reg_wdata.toLong  // 寄存器写使能

            val csrWten = dut.io.cosim_dpi_if(portIdx).commit_info.csr_wif.reg_wten.toBoolean  // 寄存器写使能
            val csrWaddr = dut.io.cosim_dpi_if(portIdx).commit_info.csr_wif.reg_addr.toLong  // 寄存器写使能
            val csrWdata = dut.io.cosim_dpi_if(portIdx).commit_info.csr_wif.reg_wdata.toLong  // 寄存器写使能

            val memWen = dut.io.cosim_dpi_if(portIdx).commit_info.mem_wif.mem_wen.toBoolean    // 内存写使能
            val memWAddr = dut.io.cosim_dpi_if(portIdx).commit_info.mem_wif.mem_waddr.toLong     // 内存地址（示例）
            val memWSel = dut.io.cosim_dpi_if(portIdx).commit_info.mem_wif.mem_sel.toLong     // 内存地址（示例）
            val memWdata = dut.io.cosim_dpi_if(portIdx).commit_info.mem_wif.mem_wdata.toLong     // 内存数据（示例）


            val memRen = dut.io.cosim_dpi_if(portIdx).commit_info.mem_rif.mem_ren.toBoolean    // 内存读使能
            val memRAddr = dut.io.cosim_dpi_if(portIdx).commit_info.mem_rif.mem_raddr.toLong     // 内存地址（示例）
            val memRSel = dut.io.cosim_dpi_if(portIdx).commit_info.mem_rif.mem_sel.toLong     // 内存地址（示例）
            val memRdata = dut.io.cosim_dpi_if(portIdx).commit_info.mem_rif.mem_rdata.toLong     // 内存数据（示例）

            // 基础日志（必打印部分）
            var logMsg = f"[cosim_dpi_if] issue($portIdx) 0x${pc & 0xFFFFFFFFL}%08x (0x${inst & 0xFFFFFFFFL}%08x)"

            // 条件拼接：reg_wten=1 时才添加 rd_arch
            if (regWten) {
              logMsg += f" x$regWaddr%-2d 0x${regWdata & 0xFFFFFFFFL}%08x"
            }

            if (csrWten) {
              logMsg += f" csr$csrWaddr%-2d 0x${csrWdata & 0xFFFFFFFFL}%08x"
            }

            if (memRen) {
              logMsg += f" mem 0x${memRAddr & 0xFFFFFFFFL}%08x"
            }

            if (memWen) {
              logMsg += f" mem 0x${memWAddr & 0xFFFFFFFFL}%08x 0x${memWdata & 0xFFFFFFFFL}%08x"
            }

            // 打印最终日志
            println(logMsg)

          }
        }

        //Check that the dut values match with the reference model ones
        //val modelFlag = modelState == 0 || dut.io.cond1.toBoolean
        //assert(dut.io.state.toInt == modelState)
        //assert(dut.io.flag.toBoolean == modelFlag)

        //Update the reference model value
        //if(dut.io.cond0.toBoolean) {
        //  modelState = (modelState + 1) & 0xFF
        //}
      }
    }
  }
}