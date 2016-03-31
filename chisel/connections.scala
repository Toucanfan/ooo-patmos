package patmos

import Chisel._
import Node._

import Constants._

class AluIO() extends Bundle(){
}

class IssueIO() extends Bundle(){
}

class CDBIO() extends Bundle(){
}
class RegStatusIO() extends Bundle(){
}

class ReservationStationIO() extends Bundle(){
} 

class MainIO() extends Bundle(){
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
