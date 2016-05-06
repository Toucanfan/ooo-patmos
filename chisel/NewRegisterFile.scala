package patmos

import Chisel._
import Node._

import Constants._

class NewRegisterFile() extends Module {
  val io = new NewRegFileIO()

  val rf = Vec.fill(REG_COUNT) { Reg(Bits(width = DATA_WIDTH)) }

  io.issue_io.val_rs := rf(UInt(io.issue_io.rs))
  io.issue_io.val_rt := rf(UInt(io.issue_io.rt))

  when (io.regstat_io.rd_en) {
    for (i <- 1 until REG_COUNT) { // we start from 1 because it's not possible to write to r0

      when (io.regstat_io.rd_addr(i) === Bits(1)) {
        rf(i) := io.regstat_io.result

        /* forwarding rs */
        when (UInt(io.issue_io.rs) === UInt(i)) {
          io.issue_io.val_rs := io.regstat_io.result
        }

        /* forwarding rt */
        when (UInt(io.issue_io.rt) === UInt(i)) {
          io.issue_io.val_rt := io.regstat_io.result
        }
      }
    }
  }
}
