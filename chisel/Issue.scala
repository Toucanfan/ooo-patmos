package patmos

import Chisel._
import Node._

import Constants._

class Issue() extends Module {
  val io = new IssueIO()

  val RSTYPE_INT::Nil = Enum(UInt(), 1)

  /* RS SELECT */
  for (RS <- io.RS_io) {
    RS.val_rs := io.RF_io.val_rs
    RS.val_rt := if (io.useImm) io.imm else io.RF_io.val_rt
    RS.tag_rs := io.regstat_io.tag_rs
    RS.tag_rt := io.regstat_io.tag_rt
    RS.func := io.func
    RS.sel := Bool(false)
  }
  val RS_idx = UInt(width = log2up(RS_NUM))
  RS_idx := io.RS_io.indexWhere(!(_.busy))
  val wait = Bool()
  wait := io.RS_io.forall(_.busy)
  when (!wait) {
    io.RS_io(RS_idx).sel := Bool(true)
  }

  io.busy := wait

  /* DECODE */
  io.regstat_io.tag_rd := RS_idx + UInt(1)
  io.regstat_io.rd := io.rd
  io.regstat_io.rs := io.rs
  io.regstat_io.rt := io.rt
  io.RF_io.rs := io.rs
  io.RF_io.rt := io.rt
}
