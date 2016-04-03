package patmos

import Chisel._
import Node._

import Constants._


class IssueRegStat() extends Bundle() {

  val rd = Bits(INPUT, REG_BITS)
  val tag_rd = Bits(INPUT, TAG_BITS)

  val rs = Bits(INPUT, REG_BITS)
  val rt = Bits(INPUT, REG_BITS)
  val tag_rs = Bits(OUTPUT, TAG_BITS)
  val tag_rt = Bits(OUTPUT, TAG_BITS)
}

class IssueRF() extends Bundle() {
  val rs = Bits(INPUT, REG_BITS)
  val rt = Bits(INPUT, REG_BITS)
  val val_rs = Bits(OUTPUT, DATA_WIDTH)
  val val_rt = Bits(OUTPUT, DATA_WIDTH)
}

class RegStatRF() extends Bundle() {
  val rd_en = Bool(INPUT)
  val rd_addr = Bits(INPUT, REG_BITS)
}

class IssueRS() extends Bundle() {
  val val_rs = Bits(INPUT, DATA_WIDTH)
  val val_rt = Bits(INPUT, DATA_WIDTH)
  val tag_rs = Bits(INPUT, TAG_BITS)
  val tag_rt = Bits(INPUT, TAG_BITS)
  val func = Bits(INPUT, FUNC_WIDTH)

  val busy = Bool(OUTPUT) 
  val sel  = Bool(INPUT)
 
}

class RSCDB() extends Bundle() {
  val result_in = Bits(OUTPUT, DATA_WIDTH) // from CDB
  val tag_in = Bits(OUTPUT, TAG_BITS)
  val rtw = Bool(INPUT)
  val ack = Bool(OUTPUT)
  val result_out = Bits(INPUT, DATA_WIDTH) // from RS
  val tag_out = Bits(INPUT, TAG_BITS)
}

class CDBRegStat() extends Bundle() {
  val tag = Bits(INPUT, TAG_BITS)
}

class RegFileRead() extends Bundle() {
  // first two are for pipeline A, second two for pipeline B
  val rsAddr = Vec.fill(2*PIPE_COUNT) { Bits(INPUT, REG_BITS) }
  val rsData = Vec.fill(2*PIPE_COUNT) { Bits(OUTPUT, DATA_WIDTH) }
}

class Result() extends Bundle() {
  val addr = Bits(width = REG_BITS)
  val data = Bits(width = DATA_WIDTH)
  val valid = Bool()

  def flush() = {
    valid := Bool(false)
  }
}




class CDBIO() extends Bundle() {
  /* This is ONE arbiter */
  val ena = Bool(INPUT)
  val RS_io = new RSCDB()
  val regstat_io = new CDBRegStat().flip()
}

class RegStatusIO() extends Bundle() {
  val ena = Bool(INPUT)
  val issue_io = new IssueRegStat()
  val RF_io = new RegStatRF().flip()
  val CDB_io = new CDBRegStat()
  
}

class ReservationStationIO() extends Bundle() {
  /* This in ONE reservation station */
  val ena = Bool(INPUT)
  val issue_io = new IssueRS()
  val CDB_io = new RSCDB().flip()

} 

class IssueIO() extends Bundle() {
  val ena = Bool(INPUT)
  val rs = Bits(INPUT, REG_BITS)
  val rt = Bits(INPUT, REG_BITS)
  val rd = Bits(INPUT, REG_BITS)
  val useImm = Bool(INPUT)
  val imm = Bits(INPUT, DATA_WIDTH)
  val func = Bits(INPUT, FUNC_WIDTH)
  val busy = Bool(OUTPUT)
  val RS_io = Vec.fill(RS_NUM){ new IssueRS().flip() }
  val regstat_io = new IssueRegStat().flip()
  val RF_io = new IssueRF().flip()
}

class MainIO() extends Bundle() {
}

class RegFileIO() extends Bundle() {
  val ena = Bool(INPUT)
  val rfRead = new RegFileRead()
  val rfWrite = Vec.fill(PIPE_COUNT) { new Result().asInput }
}

