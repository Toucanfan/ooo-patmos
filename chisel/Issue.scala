package patmos

import Chisel._
import Node._

import Constants._

class Issue() extends Module {
  val io = new IssueIO()

  /* RS SELECT */
  for (RS <- io.RS_io) {
    RS.val_rs := io.RF_io.val_rs
    RS.val_rt := Mux(io.useImm, io.imm, io.RF_io.val_rt)
    RS.tag_rs := io.regstat_io.tag_rs
    RS.tag_rt := io.regstat_io.tag_rt
    RS.func := io.func
    RS.sel := Bool(false)
  }
  val RS_idx = UInt(width = log2Up(RS_NUM))
  RS_idx := io.RS_io.indexWhere(!(_: IssueRS).busy)
  val RS_busy = Bool()
  RS_busy := io.RS_io.forall((_: IssueRS).busy)
  when (!RS_busy && !io.itype) {
    io.RS_io(RS_idx).sel := Bool(true)
  }

  /* LoadStoreQ select */
  io.LSQ_io.val_rs := io.RF_io.val_rs
  io.LSQ_io.val_rt := Mux(io.useImm, io.imm, io.RF_io.val_rt)
  io.LSQ_io.tag_rs := io.regstat_io.tag_rs
  io.LSQ_io.tag_rt := io.regstat_io.tag_rt
  io.LSQ_io.mem_op := io.mem_op
  io.LSQ_io.mem_siz := io.mem_siz
  io.LSQ_io.sel := Bool(false)
  when (!io.LSQ_io.busy && io.itype) {
    io.LSQ_io.sel := Bool(true)
  }

  io.busy := (RS_busy || io.LSQ_io.busy)

  /* DECODE */
  when (!io.itype) {
    /* We're doing an ALU op */
    io.regstat_io.tag_rd := io.RS_io(RS_idx).rs_id
  } .otherwise { 
    /* We're doing a MEM op */
    io.regstat_io.tag_rd := io.LSQ_io.rs_id
  }

  io.regstat_io.rd := io.rd
  io.regstat_io.rs := io.rs
  io.regstat_io.rt := io.rt
  io.RF_io.rs := io.rs
  io.RF_io.rt := io.rt
}

class IssueTest(dut: Issue) extends Tester(dut) {
  poke(dut.io.rs, 3)
  poke(dut.io.rt, 2)
  poke(dut.io.rd, 1)
  poke(dut.io.useImm, 0)
  poke(dut.io.itype, 0) // this is an ALU instr.
  poke(dut.io.func, int(FUNC_ADD))
  poke(dut.io.RS_io(0).busy, 1)
  poke(dut.io.RS_io(0).rs_id, 2)
  poke(dut.io.RS_io(1).rs_id, 3)
  poke(dut.io.regstat_io.tag_rs, 2)
  poke(dut.io.regstat_io.tag_rt, 0)
  poke(dut.io.RF_io.val_rs, 20)
  poke(dut.io.RF_io.val_rt, 16)

  expect(dut.io.RS_io(0).val_rs, 20)
  expect(dut.io.RS_io(0).val_rt, 16)
  expect(dut.io.RS_io(0).tag_rs, 2)
  expect(dut.io.RS_io(0).tag_rt, 0)
  expect(dut.io.RS_io(0).func, int(FUNC_ADD))
  expect(dut.io.RS_io(0).sel, 0)
  expect(dut.io.RS_io(1).sel, 1)
  expect(dut.io.RF_io.rs, 3)
  expect(dut.io.RF_io.rt, 2)
  expect(dut.io.regstat_io.rd, 1)
  expect(dut.io.regstat_io.tag_rd, 3)
  expect(dut.io.regstat_io.rs, 3)
  expect(dut.io.regstat_io.rt, 2)
  expect(dut.io.LSQ_io.sel, 0)

  step(1)

  poke(dut.io.itype, 1) //This is a MEM instr.
  poke(dut.io.LSQ_io.busy, 0)
  poke(dut.io.mem_op, 1)
  poke(dut.io.mem_siz, 3)

  expect(dut.io.LSQ_io.val_rs, 20)
  expect(dut.io.LSQ_io.val_rt, 16)
  expect(dut.io.LSQ_io.tag_rs, 2)
  expect(dut.io.LSQ_io.tag_rt, 0)
  expect(dut.io.LSQ_io.mem_op, 1)
  expect(dut.io.LSQ_io.mem_siz, 3)
  expect(dut.io.LSQ_io.sel, 1)
  expect(dut.io.RS_io(0).sel, 0)
  expect(dut.io.RS_io(1).sel, 0)
}

object IssueTest {
  def main(args: Array[String]): Unit = {
    chiselMainTest(args, () => Module(new Issue())) {
      c => new IssueTest(c)
    }
  }
}
