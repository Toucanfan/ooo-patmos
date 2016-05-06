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
    val state = UInt(width = log2Up(4))
    val mem_op = Bool()
    val val_rs = Bits(width = DATA_WIDTH)
    val val_ra = Bits(width = DATA_WIDTH)
    val tag_rs = Bits(width = TAG_BITS)
    val tag_ra = Bits(width = TAG_BITS)
//    val state = Reg(init = s_ready)
//    val mem_op = Reg(init = Bool(false))
//    val val_rs = Reg(init = Bits(0, width = DATA_WIDTH))
//    val val_ra = Reg(init = Bits(0, width = DATA_WIDTH))
//    val tag_rs = Reg(init = Bits(0, width = TAG_BITS))
//    val tag_ra = Reg(init = Bits(0, width = TAG_BITS))

    override def clone() = {
      val res = new QElem()
      res.asInstanceOf[this.type]
    }
  }

  val qelem_init_state = new QElem()
  qelem_init_state.state := s_ready
  qelem_init_state.mem_op := Bool(false)
  qelem_init_state.val_rs := Bits(0)
  qelem_init_state.val_ra := Bits(0)
  qelem_init_state.tag_rs := Bits(0)
  qelem_init_state.tag_ra := Bits(0)
  
  val queue = Vec.fill(length) { Reg(init = qelem_init_state) }

  /* Add new element to FIFO if not full */
  when (io.issue_io.sel && !full) {
    queue(tail).mem_op := io.issue_io.mem_op
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
    val haveOneVal = Bool()
    val haveBothVals = Bool()
    haveOneVal := ((elem.tag_rs === Bits(0)) || (elem.tag_ra === Bits(0)))
    haveBothVals := ((elem.tag_rs === Bits(0)) && (elem.tag_ra === Bits(0)))
    when (elem.state === s_waiting) {
      when (haveBothVals) {
        elem.state := s_busy
      }
      when ((elem.tag_rs === io.CDB_io.tag_in) && io.CDB_io.valid) {
        elem.val_rs := io.CDB_io.result_in
        elem.tag_rs := Bits(0)
        when (haveOneVal) {
          elem.state := s_busy
        }
      }
      when ((elem.tag_ra === io.CDB_io.tag_in) && io.CDB_io.valid) {
        elem.val_ra := io.CDB_io.result_in
        elem.tag_ra := Bits(0)
        when (haveOneVal) {
          elem.state := s_busy
        }
      }
    }
  }
    
  /* Do load/store for head of FIFO if not empty */
  val result = Reg(init = Bits(0, width = DATA_WIDTH))
  val tag = Reg(init = Bits(0, width = TAG_BITS))
  val rtw = Reg(init = Bool(false))
  val wrBusy = Bool()
  io.CDB_io.result_out := result
  io.CDB_io.rtw := rtw // true if load op
  io.CDB_io.tag_out := tag
  wrBusy := (rtw && !io.CDB_io.ack)

  when (queue(head).state === s_busy) {
    /* Currently, just return dummy value */
    when (!wrBusy) {
      result := Bits(1)
      tag := UInt(tagstart) + head
      rtw := queue(head).mem_op // true if load op
      queue(head).state := s_ready
      head := headNext
      decNeeded := Bool(true)
    }
  } .otherwise {
    when (!wrBusy) {
      rtw := Bool(false)
      tag := Bits(0)
      result := Bits(0)
    }
  }

  when (incNeeded && !decNeeded) {
    fifoCount := fifoCount + UInt(1)
  } 
  .elsewhen (!incNeeded && decNeeded) {
    fifoCount := fifoCount - UInt(1)
  }

  io.q0_state := queue(0).state
  io.q0_mem_op := queue(0).mem_op
  io.q0_tag_rs := queue(0).tag_rs
  io.q0_tag_ra := queue(0).tag_ra
  io.q1_state := queue(1).state
  io.q1_mem_op := queue(1).mem_op
  io.q1_tag_rs := queue(1).tag_rs
  io.q1_tag_ra := queue(1).tag_ra

}

class LoadStoreQTest(dut: LoadStoreQ) extends Tester(dut) {
  /* OK, we're testing a LoadStoreQ with 2 elements.
   * First, we add an item to the queue.
   * We don't supply any values through CDB,
   * and we don't expect it to want to write */
  poke(dut.io.issue_io.tag_rs, 1)
  poke(dut.io.issue_io.tag_rt, 2)
  poke(dut.io.issue_io.mem_op, 1)
  poke(dut.io.issue_io.sel, 1)
  poke(dut.io.CDB_io.tag_in, 7)
  poke(dut.io.CDB_io.valid, 0)
  poke(dut.io.CDB_io.ack, 0)
  expect(dut.io.issue_io.rs_id, 0)
  expect(dut.io.issue_io.busy, 0)
  expect(dut.io.CDB_io.rtw, 0)

  step(1)

  /* Then we add another item
   * We don't supply any values through CDB,
   * and we don't expect it to want to write */
  poke(dut.io.issue_io.tag_rs, 3)
  poke(dut.io.issue_io.tag_rt, 4)
  poke(dut.io.issue_io.mem_op, 1)
  poke(dut.io.issue_io.sel, 1)
  poke(dut.io.CDB_io.tag_in, 7)
  poke(dut.io.CDB_io.valid, 0)
  expect(dut.io.issue_io.rs_id, 1)
  expect(dut.io.issue_io.busy, 0)
  expect(dut.io.CDB_io.rtw, 0)

  step(1)

  /* Now, we expect the queue to be full/busy,
   * since it's only 2 elems long
   * We don't supply any values through CDB,
   * and we don't expect it to want to write */
  expect(dut.io.issue_io.busy, 1)
  expect(dut.io.CDB_io.rtw, 0)

  step(1)

  /* For elem 2, replace tag 1 */
  poke(dut.io.issue_io.sel, 0)
  poke(dut.io.CDB_io.tag_in, 3)
  poke(dut.io.CDB_io.result_in, 3)
  poke(dut.io.CDB_io.valid, 1)
  expect(dut.io.CDB_io.rtw, 0)
  expect(dut.io.issue_io.busy, 1)

  step(1)

  /* For elem 2, replace tag 2 */
  poke(dut.io.issue_io.sel, 0)
  poke(dut.io.CDB_io.tag_in, 4)
  poke(dut.io.CDB_io.result_in, 4)
  poke(dut.io.CDB_io.valid, 1)
  expect(dut.io.CDB_io.rtw, 0)
  expect(dut.io.issue_io.busy, 1)

  step(1)

  /* For elem 1, replace tag 1 */
  poke(dut.io.issue_io.sel, 0)
  poke(dut.io.CDB_io.tag_in, 1)
  poke(dut.io.CDB_io.result_in, 1)
  poke(dut.io.CDB_io.valid, 1)
  expect(dut.io.CDB_io.rtw, 0)
  expect(dut.io.issue_io.busy, 1)

  step(1)

  /* For elem 1, replace tag 2 */
  poke(dut.io.issue_io.sel, 0)
  poke(dut.io.CDB_io.tag_in, 2)
  poke(dut.io.CDB_io.result_in, 2)
  poke(dut.io.CDB_io.valid, 1)
  expect(dut.io.CDB_io.rtw, 0)
  expect(dut.io.issue_io.busy, 1)

  step(1)

//  /* Wait for state transistion */
//  poke(dut.io.CDB_io.valid, 0)
//  expect(dut.io.CDB_io.rtw, 0)
//  expect(dut.io.issue_io.busy, 1)
//
//  step(1)

  /* Wait for elem 1 to execute */
  poke(dut.io.CDB_io.valid, 0)
  expect(dut.io.CDB_io.rtw, 0)
  expect(dut.io.issue_io.busy, 1)

  step(1)

  /* Expect elem 1 is done and wants to write */
  expect(dut.io.CDB_io.rtw, 1)
  expect(dut.io.CDB_io.tag_out, 0)
  expect(dut.io.CDB_io.result_out, 1)
  expect(dut.io.issue_io.busy, 0)

  step(1)

  /* Tell elem 1 that its result has been written */
  poke(dut.io.CDB_io.ack, 1)
  expect(dut.io.CDB_io.rtw, 1)
  expect(dut.io.CDB_io.tag_out, 0)
  expect(dut.io.CDB_io.result_out, 1)
  expect(dut.io.issue_io.busy, 0)

  step(1)

  /* Wait for elem 2 to execute */
  //poke(dut.io.CDB_io.valid, 0)
  //expect(dut.io.CDB_io.rtw, 0)
  //expect(dut.io.issue_io.busy, 0)

  //step(1)

  /* Expect elem 2 is done and wants to write
   * Tell it has been written in the same cycle */
  poke(dut.io.CDB_io.ack, 1)
  expect(dut.io.CDB_io.rtw, 1)
  expect(dut.io.CDB_io.tag_out, 1)
  expect(dut.io.CDB_io.result_out, 1)
  expect(dut.io.issue_io.busy, 0)

  step(1)

  /* Expect nobody wants to write */
  poke(dut.io.CDB_io.ack, 0)
  expect(dut.io.CDB_io.rtw, 0)
  expect(dut.io.issue_io.busy, 0)



}

object LoadStoreQTest {
  def main(args: Array[String]): Unit = {
    chiselMainTest(args, () => Module(new LoadStoreQ(0, 2))) {
      c => new LoadStoreQTest(c)
    }
  }
}
