package patmos

import Chisel._
import Node._

import Constants._

class Main() extends Module {
   val io = new MainIO()

   /* =================================================*/
   /*Create Modules*/
   /* =================================================*/
   val issue = Module(new Issue())
   val cdb = Module (new CDB() )
   //val rStations = Module (new ReservationStation(RS_NUM))
   val regStatus = Module (new RegStatus())
   val regFile = Module(new RegisterFile())
   val regstat_io = new IssueRegStat().flip()
   //val LSQ = Module (new LoadStoreQ(RS_NUM+1))

   // reservation station setup
   val ids = Seq(1,2,3,4)
   val rStations = for (i <- 0 until RS_NUM) yield
   {
      val rs = Module(new ReservationStation(ids(i)))
      rs
   }

   for (i <- 0 until RS_NUM) {

      /* enable each reservation station */
      rStations(i).io.ena <> io.ena

      /* issue unit connections to each reservation station */
      issue.io.RS_io(i).val_rs <> rStations(i).io.issue_io.val_rs
      issue.io.RS_io(i).val_rt <> rStations(i).io.issue_io.val_rt
      issue.io.RS_io(i).tag_rs <> rStations(i).io.issue_io.tag_rs
      issue.io.RS_io(i).tag_rt <> rStations(i).io.issue_io.tag_rt
      issue.io.RS_io(i).rs_id <> rStations(i).io.issue_io.rs_id

      /* reservation station to CDB */
      cdb.io.RS_io(i) <> rStations(i).io.CDB_io
   }

   /* =================================================*/
   /* Enable all the units*/
   /* =================================================*/

   issue.io.ena <> io.ena
   cdb.io.ena <> io.ena
   //rStations.io.ena <> io.ena
   regStatus.io.ena <> io.ena
   regFile.io.ena <> io.ena
   //LSQ.io.ena <> io.ena

   issue.io.rs <> io.rs
   issue.io.rt <> io.rt
   issue.io.rd <> io.rd


   /* =================================================*/
   /*Data used just to TEST communication to be removed*/
   /* =================================================*/
   regFile.io.rfWrite(0).addr <> io.waddr
   regFile.io.rfWrite(0).data <> io.wdata
   regFile.io.rfWrite(0).valid <> io.wvalid
   regFile.io.rfRead.rsAddr(0) :=Bits(2)
   io.rdata:= regFile.io.rfRead.rsData(0)

   /* =================================================*/
   /*  connect issue unit to register status unit*/
   /* =================================================*/
   issue.io.regstat_io <> regStatus.io.issue_io

   /* =================================================*/
   /*  connect issue unit to register file unit*/
   /* =================================================*/
   issue.io.RF_io.rs <> regFile.io.rfRead.rsAddr(0)
   issue.io.RF_io.val_rs <> regFile.io.rfRead.rsData(0)

   issue.io.RF_io.rt <> regFile.io.rfRead.rsAddr(1)
   issue.io.RF_io.val_rt <> regFile.io.rfRead.rsData(1)

   /* =================================================*/
   /*  connect CDB to register status unit*/
   /* =================================================*/
   cdb.io.regstat_io <> regStatus.io.CDB_io

   /* =================================================*/
   /*  connect CDB to register file unit*/
   /* =================================================*/

}
class TestMain(dut: Main) extends Tester(dut) {
   poke(dut.io.ena,1)
   poke(dut.io.waddr,2)
   poke(dut.io.wdata,1)
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
   //expect(dut.issue.io.regstat_io.RF_io.rs,2)
   expect(dut.regFile.io.rfRead.rsAddr(0),2)
   //expect(dut.issue.io.regstat_io.RF_io.val_rs,10)
   expect(dut.regFile.io.rfRead.rsData(0),10)
   step(1)


   /* From issue unit -> register file read rt value*/
   //expect(dut.issue.io.regstat_io.RF_io.rt,4)
   expect(dut.regFile.io.rfRead.rsAddr(1),4)
   //expect(dut.issue.io.regstat_io.RF_io.val_rt,0)
   expect(dut.regFile.io.rfRead.rsData(1),0)

   step(1)

   /* From issue unit -> reservation stations */
   expect(dut.issue.io.RS_io(0).val_rs, 1)
   expect(dut.issue.io.RS_io(0).val_rt, 1)
   expect(dut.issue.io.RS_io(0).tag_rt, 0)
   expect(dut.issue.io.RS_io(0).tag_rs, 0)
   expect(dut.issue.io.RS_io(0).func, 0)
   expect(dut.issue.io.RS_io(0).sel,1)

   expect(dut.rStations(0).io.issue_io.val_rs,1)
   expect(dut.rStations(0).io.issue_io.val_rt, 1)
   expect(dut.rStations(0).io.issue_io.tag_rt, 0)
   expect(dut.rStations(0).io.issue_io.tag_rs, 0)
   /* FUNC AND SEL ARE NOT PASSED CORRECTLY ?!*/
   expect(dut.rStations(0).io.issue_io.func, 0)
   expect(dut.rStations(0).io.issue_io.sel,1)

   step(1)

   expect(dut.rStations(0).io.CDB_io.result_in, 2)
   expect(dut.rStations(0).io.CDB_io.result_out, 2)

   step(1)
   expect(dut.rStations(0).io.CDB_io.result_in, 2)
   expect(dut.rStations(0).io.CDB_io.result_out, 2)
   step(1)


   expect(dut.rStations(0).io.CDB_io.result_in, 2)
   expect(dut.rStations(0).io.CDB_io.result_out, 2)
   expect(dut.cdb.io.RS_io(0).result_out, 2)
   step(1)
   expect(dut.cdb.io.RS_io(0).result_out, 2)
   step(1)
   expect(dut.cdb.io.RS_io(0).result_out, 2)
   step(10) // wait for ack before writing to CDB
   expect(dut.cdb.io.RS_io(0).result_out, 2)
   //poke(dut.cdb.io.ack, (true))
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
