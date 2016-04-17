package patmos

import Chisel._
import Node._

import Constants._


class LoadStoreQ(tagstart: Int, length: Int) extends Module {
  val io = new LoadStoreQIO()
  
  /* FIFO state */
  val head = Reg(init = UInt(0, width = log2Up(length)))
  val tail = Reg(init = UInt(0, width = log2Up(length)))

  val fifoCount = Reg(init = UInt(0, width = log2Up(length+1)))
  val incNeeded = Bool()
  val decNeeded = Bool()

  val full = Bool()
  val empty = Bool()
  val tailNext = UInt(width = log2Up(length))
  val headNext = UInt(width = log2Up(length))

  incNeeded := Bool(false)
  decNeeded := Bool(false)
  tailNext := Mux(tail === UInt(length), UInt(0), tail + UInt(1))
  headNext := Mux(head === UInt(length), UInt(0), head + UInt(1))
  full := (fifoCount === UInt(length))
  empty := (fifoCount === UInt(0))

  /* Unit busy when FIFO is full */
  io.issue_io.busy := full
  io.issue_io.rs_id := UInt(tagstart, width = TAG_BITS) + tail

  /* The FIFO queue itself is created */
  val s_ready::s_waiting::s_busy::s_done::Nil = Enum(UInt(), 4)

  class QElem() extends Bundle {
    val state = Reg(init = s_ready)
    val mem_op = Reg(init = Bool(false))
    val val_rs = Reg(init = Bits(0, width = DATA_WIDTH))
    val val_ra = Reg(init = Bits(0, width = DATA_WIDTH))
    val tag_rs = Reg(init = Bits(0, width = TAG_BITS))
    val tag_ra = Reg(init = Bits(0, width = TAG_BITS))

    override def clone() = {
      val res = new QElem()
      res.asInstanceOf[this.type]
    }
  }

  val queue = Vec.fill(length) { new QElem() }

  /* Add new element to FIFO if not full */
  when (io.issue_io.sel && !full) {
    queue(tail).val_rs := io.issue_io.val_rs
    queue(tail).val_ra := io.issue_io.val_rt
    queue(tail).tag_rs := io.issue_io.tag_rs
    queue(tail).tag_ra := io.issue_io.tag_rt
    queue(tail).state := s_waiting
    tail := tailNext
    incNeeded := Bool(true)
  }

  /* Make tag->value replacements for all FIFO elems */
  for (elem <- queue) {
    when (elem.state === s_waiting) {
      when (elem.tag_rs === io.CDB_io.tag_in) {
        elem.val_rs := io.CDB_io.result_in
        elem.tag_rs := Bits(0)
      }
      when (elem.tag_ra === io.CDB_io.tag_in) {
        elem.val_ra := io.CDB_io.result_in
        elem.tag_ra := Bits(0)
      }
      when ((elem.tag_rs === Bits(0)) && (elem.tag_ra === Bits(0))) {
        elem.state := s_done
      }
    }
  }
    
  /* Do load/store for head of FIFO if not empty */
  val result = Reg(init = Bits(0, width = DATA_WIDTH))
  io.CDB_io.result_out := result
  io.CDB_io.rtw := Bool(false)
  io.CDB_io.tag_out := UInt(tagstart, width = TAG_BITS) + head

  switch (queue(head).state) {
    is(s_busy) {
      when (!empty) {
        /* Currently, just return dummy value */
        result := Bits(1)
        queue(head).state := s_done
      }
    }
      /* Broadcast on CDB */
    is(s_done) {
      when (!empty) {
        io.CDB_io.rtw := queue(head).mem_op // true if load op
        when (io.CDB_io.ack) {
          io.CDB_io.rtw := Bool(false)
          queue(head).state := s_ready
          head := headNext
          decNeeded := Bool(true)
        }
      }
    }
  }

  when (incNeeded && !decNeeded) {
    fifoCount := fifoCount + UInt(1)
  } 
  .elsewhen (!incNeeded && decNeeded) {
    fifoCount := fifoCount - UInt(1)
  }

}

class LoadStoreQTest(dut: LoadStoreQ) extends Tester(dut) {
  poke(dut.io.issue_io.tag_rs, 1)
  poke(dut.io.issue_io.tag_rt, 2)
  poke(dut.io.issue_io.mem_op, 1)
  poke(dut.io.issue_io.sel, 1)
  poke(dut.io.CDB_io.tag_in, 7)
  expect(dut.io.issue_io.rs_id, 0)
  expect(dut.io.issue_io.busy, 0)
  expect(dut.io.CDB_io.rtw, 0)

  step(1)

  poke(dut.io.issue_io.tag_rs, 3)
  poke(dut.io.issue_io.tag_rt, 4)
  poke(dut.io.issue_io.mem_op, 1)
  poke(dut.io.issue_io.sel, 1)
  poke(dut.io.CDB_io.tag_in, 7)
  expect(dut.io.issue_io.rs_id, 1)
  expect(dut.io.issue_io.busy, 0)
  expect(dut.io.CDB_io.rtw, 0)

  step(1)

  expect(dut.io.issue_io.busy, 1)
  expect(dut.io.CDB_io.rtw, 0)


}

object LoadStoreQTest {
  def main(args: Array[String]): Unit = {
    chiselMainTest(args, () => Module(new LoadStoreQ(0, 2))) {
      c => new LoadStoreQTest(c)
    }
  }
}
