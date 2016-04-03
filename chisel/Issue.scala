package patmos

import Chisel._
import Node._

import Constants._
//import connections._

class Issue() extends Module {
  val io = new IssueIO()
/*
  val RSTYPE_INT::Nil = Enum(UInt(), 1)

  /* DECODE */

  /* RS SELECT */
  for (RS <- io.RS_io) {
    RS.val_rs := io.RF_io.val_rs
    RS.val_rt := io.RF_io.val_rt
    RS.tag_rs := io.regstat_io.tag_rs
    RS.tag_rt := io.regstat_io.tag_rt
    RS.sel := Bool(false)
  }
  val RS_idx = io.RS_io.indexWhere(!(_.busy))
  val wait = io.RS_io.forall(_.busy)
  when (!wait) {
    io.RS_io(RS_idx).sel := Bool(true)
  }

  io.busy := wait*/
}
