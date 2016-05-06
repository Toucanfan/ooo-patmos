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
   val regStatus = Module (new RegStatus())
   val regFile = Module(new RegisterFile())

   // reservation station setup
   val rStations = for (i <- 0 until RS_NUM) yield
   {
      val rs = Module(new ReservationStation(i+1))
      rs
   }

   for (i <- 0 until RS_NUM) {

      /* enable each reservation station */
      rStations(i).io.ena <> io.ena

      /* issue unit connections to each reservation station */
      issue.io.RS_io(i) <> rStations(i).io.issue_io

      /* reservation station to CDB */
      cdb.io.RS_io(i) <> rStations(i).io.CDB_io
      //rStations(i).io.CDB_io <> cdb.io.RS_io(i)
   }

   val lsqueue = Module(new LoadStoreQ(RS_NUM+1, LSQUEUE_NELEMS))
   lsqueue.io.ena <> io.ena
   lsqueue.io.issue_io <> issue.io.LSQ_io
   lsqueue.io.CDB_io <> cdb.io.LSQ_io

   /* =================================================*/
   /* Enable all the units*/
   /* =================================================*/

   issue.io.ena <> io.ena
   cdb.io.ena <> io.ena
   regStatus.io.ena <> io.ena
   regFile.io.ena <> io.ena

   issue.io.rs <> io.rs
   issue.io.rt <> io.rt
   issue.io.rd <> io.rd

   /* =================================================*/
   /*  connect issue unit to register status unit*/
   /* =================================================*/
   issue.io.regstat_io <> regStatus.io.issue_io

   /* =================================================*/
   /*  connect register file unit*/
   /* =================================================*/

   /* connect to issue unit */
   regFile.io.rfRead.rsAddr(0) := issue.io.RF_io.rs
   issue.io.RF_io.val_rs := regFile.io.rfRead.rsData(0)

   regFile.io.rfRead.rsAddr(1) := issue.io.RF_io.rt
   issue.io.RF_io.val_rt := regFile.io.rfRead.rsData(1)

   /* connect to register status */
   regFile.io.rfWrite(0).valid := regStatus.io.RF_io.rd_en
   regFile.io.rfWrite(0).addr := regStatus.io.RF_io.rd_addr
   regFile.io.rfWrite(0).data := regStatus.io.RF_io.result

   /* =================================================*/
   /*  connect CDB to register status unit*/
   /* =================================================*/
   cdb.io.regstat_io <> regStatus.io.CDB_io
}
class TestMain(dut: Main) extends Tester(dut) {

   poke(dut.io.ena, 1)
   poke(dut.io.waddr, 5)
   poke(dut.io.wdata, 5)
   poke(dut.io.wvalid, 1)
   step(1)

//   poke(dut.io.waddr, 3)
   //poke(dut.io.wdata, 5)
   poke(dut.io.wvalid, 1)
   step(1)

   poke(dut.io.rs, 5)
   poke(dut.io.rt, 5)
   poke(dut.io.rd, 1)
   poke(dut.io.useImm, 0)
   poke(dut.io.func, 0)
   step(1)

   poke(dut.io.rs, 3)
   poke(dut.io.rt, 4)
   poke(dut.io.rd, 2)
   poke(dut.io.func, 1)
   step(8)

   poke(dut.io.rs, 0)
   poke(dut.io.rt, 0)
   poke(dut.io.rd, 0)
   step(30)

}

object tMain {
   def main(args: Array[String]): Unit = {
      chiselMainTest(args,
         () => Module(new Main())) {
            dut => new TestMain(dut)
         }
      }
   }
