package patmos

import Chisel._
import Node._

import Constants._


class IssueRegStat() extends Bundle() {

   val rd = Bits(INPUT, REG_BITS)
   val tag_rd = Bits(INPUT, TAG_BITS)
   val valid = Bool(INPUT)

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
   val result = Bits(INPUT, DATA_WIDTH)
}

class IssueRS() extends Bundle() {
   val val_rs = Bits(INPUT, DATA_WIDTH)
   val val_rt = Bits(INPUT, DATA_WIDTH)
   val tag_rs = Bits(INPUT, TAG_BITS)
   val tag_rt = Bits(INPUT, TAG_BITS)
   val func = Bits(INPUT, FUNC_WIDTH)
   val mem_op = Bool(INPUT) // 0: Store, 1: Load
   val rs_id = Bits(OUTPUT, TAG_BITS)

   val busy = Bool(OUTPUT)
   val sel  = Bool(INPUT)
}

class RSCDB() extends Bundle() {
   val valid = Bool(OUTPUT) //! /* Do we need this signal? */
   val result_in = Bits(OUTPUT, DATA_WIDTH) // from CDB
   val tag_in = Bits(OUTPUT, TAG_BITS)
   val rtw = Bool(INPUT) // comes from every RS
   val ack = Bool(OUTPUT) //to every RS
   val result_out = Bits(INPUT, DATA_WIDTH) // from RS
   val tag_out = Bits(INPUT, TAG_BITS)
}

class CDBRegStat() extends Bundle() {
   val tag = Bits(INPUT, TAG_BITS)
   val valid= Bool(INPUT)
   val result = Bits(INPUT,DATA_WIDTH)
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
   //val RS_io = new RSCDB()
   val RS_io = Vec.fill(RS_NUM) { new RSCDB() }
   val regstat_io = new CDBRegStat().flip()
   //val token = Bits(OUTPUT,1)
   //val token1 = Bits(OUTPUT,1)
   //val token2 = Bits(OUTPUT,1)
   //val token3 = Bits(OUTPUT,1)

}

class RegStatusIO() extends Bundle() {
   val ena = Bool(INPUT)
   val issue_io = new IssueRegStat()
   val RF_io = new RegStatRF().flip()
   val CDB_io = new CDBRegStat()

   // FOR debugging
   //val test = Bits(OUTPUT)
}

class ReservationStationIO() extends Bundle() {
   /* This in ONE reservation station */
   val ena = Bool(INPUT)
   val issue_io = new IssueRS()
   val CDB_io = new RSCDB().flip()
   /* used for debugging */
   //  val rs_state = UInt(OUTPUT)
   //  val result = Bits(OUTPUT)
   // val rs_value_rs = Bits(OUTPUT)

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

class LoadStoreQIO() extends ReservationStationIO() {
   val q0_state = UInt(OUTPUT, 4)
   val q0_mem_op = Bool(OUTPUT)
   val q0_tag_rs = Bits(OUTPUT, TAG_BITS)
   val q0_tag_ra = Bits(OUTPUT, TAG_BITS)
   val q1_state = UInt(OUTPUT, 4)
   val q1_mem_op = Bool(OUTPUT)
   val q1_tag_rs = Bits(OUTPUT, TAG_BITS)
   val q1_tag_ra = Bits(OUTPUT, TAG_BITS)
   val testtrue = Bool(OUTPUT)
}

class MainIO() extends Bundle() {
   val ena = Bool(INPUT)
   val rs = Bits(INPUT, REG_BITS)
   val rt = Bits(INPUT, REG_BITS)
   val rd = Bits(INPUT, REG_BITS)
   val useImm = Bool(INPUT)
   val sel = Bool(INPUT)
   val imm = Bits(INPUT, DATA_WIDTH)
   val func = Bits(INPUT, FUNC_WIDTH)
   val busy = Bool(OUTPUT)
   val wdata = Bits(INPUT, DATA_WIDTH)
   val waddr = Bits(INPUT, REG_BITS)
   val wvalid = Bool(INPUT)
   val rdata = Bits(OUTPUT,DATA_WIDTH)
}

class RegFileIO() extends Bundle() {
   val ena = Bool(INPUT)
   val rfRead = new RegFileRead()
   val rfWrite = Vec.fill(PIPE_COUNT) { new Result().asInput }
}

class tomSelIO() extends Bundle(){
   val ena = Bool(INPUT)
   val instr = Bits(INPUT,INSTR_WIDTH)
   val stopFetch = Bool(OUTPUT)
   val busy = Bool(OUTPUT)
   val switch = Bool(OUTPUT)
   val transmit = Bool(OUTPUT)
   val tWbDone = Bool(INPUT)
   val tMemDone = Bool(INPUT)
   val tExDone = Bool(INPUT)
}
