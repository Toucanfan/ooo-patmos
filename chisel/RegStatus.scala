package patmos

import Chisel._
import Node._

import Constants._

class RegStatus() extends Module {

   // io variables
   val io = new RegStatusIO()
   val issue_io = io.issue_io
   val RF_io = io.RF_io
   val tag_rs = io.CDB_io.tag

   // 32 register status, initilized to 0
   val reg_stat = Vec.fill(32) { Reg(Uint(0), REG_BITS) }

   when (io.ena) {

      tag_rs := reg_stat(issue_io.rs)
      tag_rt := reg_stat(issue_io.rt)

      reg_stat(issue_io.rd) := issue_io.tag_rd

      for (i <- 0 until reg_stat.length) {

         if (reg_stat(i) === tag_rs) {

            reg_stat(i) := UInt(0)
            RF_io.rd_en := Bool(true)
            RF_io.rd_addr := Bits(REG_BITS)

            i = reg_stat.length

         }.otherwise {
            RF_io.rd_en := Bool(false)
            RF_io.rd_addr := 0
         }
      }
   }

}
