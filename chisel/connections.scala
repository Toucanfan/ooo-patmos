package patmos

import Chisel._
import Node._

import Constants._

class AluIO() extends Bundle() {
  val ena = Bool(INPUT)
  val func = Bits(INPUT, FUNC_WIDTH)
  val op1 = UInt(INPUT, DATA_WIDTH)
  val op2 = UInt(INPUT, DATA_WIDTH)
  val done = Bool(OUTPUT)
  val result = Bits(OUTPUT, DATA_WIDTH)
}

class IssueIO() extends Bundle() {
  val ena = Bool(INPUT)
  val instr = Bits(INPUT, INSTR_WIDTH)
  val busy = Vec.fill(RS_NUM){ new Bool(INPUT) }

  val rd = Bits(OUTPUT, REG_BITS)
  val rs = Bits(OUTPUT, REG_BITS)
  val rt = Bits(OUTPUT, REG_BITS)
  val RS_sel = Vec.fill(RS_NUM){ new Bool(OUTPUT) }
}

class CDBIO() extends Bundle() {
  val ena = Bool(INPUT)
}

class RegStatusIO() extends Bundle() {
  val ena = Bool(INPUT)
}

class ReservationStationIO() extends Bundle() {
  val ena = Bool(INPUT)
} 

class MainIO() extends Bundle() {
}

class RegFileRead() extends Bundle() {
  // first two are for pipeline A, second two for pipeline B
  val rsAddr = Vec.fill(2*PIPE_COUNT) { Bits(INPUT, REG_BITS) }
  val rsData = Vec.fill(2*PIPE_COUNT) { Bits(OUTPUT, DATA_WIDTH) }
}

class RegFileIO() extends Bundle() {
  val ena = Bool(INPUT)
  val rfRead = new RegFileRead()
  val rfWrite = Vec.fill(PIPE_COUNT) { new Result().asInput }
}

class Result() extends Bundle() {
  val addr = Bits(width = REG_BITS)
  val data = Bits(width = DATA_WIDTH)
  val valid = Bool()

  def flush() = {
    valid := Bool(false)
  }
}
