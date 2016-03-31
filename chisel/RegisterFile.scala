package patmos

import Chisel._
import Node._

import Constants._

class RegisterFile() extends Module {
  val io = new RegFileIO()

  // Using Mem (instead of Vec) leads to smaller HW for single-issue config
  val rf = Mem(Bits(width = DATA_WIDTH), REG_COUNT)

  // We are registering the inputs here, similar as it would
  // be with an on-chip memory for the register file
  val addrReg = Vec.fill(2*PIPE_COUNT) { Reg(UInt(width=REG_BITS)) }
  val wrReg   = Vec.fill(PIPE_COUNT)   { Reg(new Result()) }
  val fwReg   = Vec.fill(2*PIPE_COUNT) { Vec.fill(PIPE_COUNT) { Reg(Bool()) } }

  when (io.ena) {
    addrReg := io.rfRead.rsAddr
    wrReg := io.rfWrite
    for (i <- 0 until 2*PIPE_COUNT) {
      for (k <- 0 until PIPE_COUNT) {
        fwReg(i)(k) := io.rfRead.rsAddr(i) === io.rfWrite(k).addr && io.rfWrite(k).valid
      }
    }
  }

  // RF internal forwarding
  for (i <- 0 until 2*PIPE_COUNT) {
    io.rfRead.rsData(i) := rf(addrReg(i))
    for (k <- 0 until PIPE_COUNT) {
      when (fwReg(i)(k)) {
        io.rfRead.rsData(i) := wrReg(k).data
      }
    }
    when(addrReg(i) === Bits(0)) {
      io.rfRead.rsData(i) := Bits(0)
    }
  }

  // Don't care about R0 here: reads return zero and writes to
  // register R0 are disabled in decode stage anyway
  for (k <- (0 until PIPE_COUNT).reverse) {
    when(io.rfWrite(k).valid) {
      rf(io.rfWrite(k).addr.toUInt) := io.rfWrite(k).data
    }
  }

  // Signal for debugging register values
  val rfDebug = Vec.fill(REG_COUNT) { Reg(Bits(width = DATA_WIDTH)) }
  for(i <- 0 until REG_COUNT) {
    rfDebug(i) := rf(Bits(i))
    // Keep signal alive
    if(Driver.isVCD){
     debug(rfDebug(i))
    }
  }
}
