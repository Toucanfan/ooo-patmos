package patmos

import Chisel._
import Node._

import Constants._

class Main() extends Module {
  val io = new MainIO()
  	


  /*Create Modules*/
  val issue = Module(new Issue())
  val cdb = Module (new CDB() )
  val rStations = Module (new ReservationStation(RS_NUM))
  val regStatus = Module (new RegStatus())
  val regFile = Module(new RegisterFile())
  val regstat_io = new IssueRegStat().flip()
//  val LSQ = Module (new LoadStoreQ(RS_NUM+1))
//  io.rs <> issue.io.regstat_io.rs

issue.io.ena <> io.ena
issue.io.rs <> io.rs
issue.io.rt <> io.rt
issue.io.rd <> io.rd
regFile.io.ena <> io.ena
regFile.io.rfWrite(0).addr <> io.waddr
regFile.io.rfWrite(0).data <> io.wdata
regFile.io.rfWrite(0).valid <> io.wvalid

regFile.io.rfRead.rsAddr(0) :=Bits(2)
io.rdata:= regFile.io.rfRead.rsData(0)

issue.io.regstat_io <> regStatus.io.issue_io
issue.io.RF_io.rs <> regFile.io.rfRead.rsAddr(0)
issue.io.RF_io.val_rs <> regFile.io.rfRead.rsData(0)

issue.io.RF_io.rt <> regFile.io.rfRead.rsAddr(1)
issue.io.RF_io.val_rt <> regFile.io.rfRead.rsData(1)



}
class TestMain(dut: Main) extends Tester(dut) {
poke(dut.io.ena,1)
poke(dut.io.waddr,2)
poke(dut.io.wdata,10)
poke(dut.io.wvalid,1)
poke(dut.io.rs,2)

poke(dut.io.rt,4)
poke(dut.io.rd,5)

step(1)

peek(dut.io.rdata)

step(1)
/* From io -> issue unit -> regstat */
poke(dut.io.rs,2)
expect(dut.issue.io.regstat_io.rs,2)

expect(dut.regStatus.io.issue_io.rs,2)
expect(dut.regStatus.io.issue_io.tag_rs,0) 
step(1)

/* From issue unit -> register file read rs value*/
expect(dut.issue.io.RF_io.rs,2) 
expect(dut.regFile.io.rfRead.rsAddr(0),2)
expect(dut.issue.io.RF_io.val_rs,10)
expect(dut.regFile.io.rfRead.rsData(0),10)
step(1)


/* From issue unit -> register file read rt value*/
expect(dut.issue.io.RF_io.rt,4) 
expect(dut.regFile.io.rfRead.rsAddr(1),4)
expect(dut.issue.io.RF_io.val_rt,0)
expect(dut.regFile.io.rfRead.rsData(1),0)

step(1)
}


object tMain {
  def main(args: Array[String]): Unit = {
    chiselMainTest(args,
      () => Module(new Main())) {
        dut => new TestMain(dut)
      }
  }
}

