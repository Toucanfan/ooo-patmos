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
   val regFile = Module(new NewRegisterFile())

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
   issue.io.imm <> io.imm
   issue.io.useImm <> io.useImm
   issue.io.func <> io.func
   issue.io.mem_op <> io.mem_op
   issue.io.mem_siz <> io.mem_siz
   issue.io.itype <> io.itype
   issue.io.busy <> io.busy

   /* =================================================*/
   /*  connect issue unit to register status unit*/
   /* =================================================*/
   issue.io.regstat_io <> regStatus.io.issue_io

   /* =================================================*/
   /*  connect register file unit*/
   /* =================================================*/

   /* connect to issue unit */
   regFile.io.issue_io <> issue.io.RF_io
   //regFile.io.rfRead.rsAddr(0) := issue.io.RF_io.rs
   //issue.io.RF_io.val_rs := regFile.io.rfRead.rsData(0)

   //regFile.io.rfRead.rsAddr(1) := issue.io.RF_io.rt
   //issue.io.RF_io.val_rt := regFile.io.rfRead.rsData(1)

   /* connect to register status */
   regFile.io.regstat_io <> regStatus.io.RF_io
   //regFile.io.rfWrite <> regStatus.io.RF_io
   //regFile.io.rfWrite(0).valid := regStatus.io.RF_io.rd_en
   //regFile.io.rfWrite(0).addr := regStatus.io.RF_io.rd_addr
   //regFile.io.rfWrite(0).data := regStatus.io.RF_io.result

   /* =================================================*/
   /*  connect CDB to register status unit*/
   /* =================================================*/
   cdb.io.regstat_io <> regStatus.io.CDB_io
}
class TestMain(dut: Main) extends Tester(dut) {
   poke(dut.io.ena, 1)

   /* LD r6, 34(r2) */
   poke(dut.io.rd, 6)
   poke(dut.io.rs, 2)
   poke(dut.io.useImm, 1)
   poke(dut.io.imm, 34)
   poke(dut.io.itype, 1)
   poke(dut.io.mem_op, 1)

   step(1)

   /* LD r2, 45(r3) */
   poke(dut.io.rd, 2)
   poke(dut.io.rs, 3)
   poke(dut.io.useImm, 1)
   poke(dut.io.imm, 45)
   poke(dut.io.itype, 1)
   poke(dut.io.mem_op, 1)

   step(1)

   /* ADDI r4, r0, 10 */
   poke(dut.io.rd, 4)
   poke(dut.io.rs, 0)
   poke(dut.io.useImm, 1)
   poke(dut.io.imm, 10)
   poke(dut.io.itype, 0)
   poke(dut.io.func, 0)

   step(1)

   /* ADDI r5, r0, 7 */
   poke(dut.io.rd, 5)
   poke(dut.io.rs, 0)
   poke(dut.io.useImm, 1)
   poke(dut.io.imm, 7)
   poke(dut.io.itype, 0)
   poke(dut.io.func, 0)

   step(1)

   /* ADD r5, r6, r2 */
   poke(dut.io.rs, 6)
   poke(dut.io.rt, 2)
   poke(dut.io.rd, 5)
   poke(dut.io.useImm, 0)
   poke(dut.io.imm, 0)
   poke(dut.io.func, 0)
   poke(dut.io.itype, 0)

   step(1)

   /* ADD r9, r7, r2 */
   poke(dut.io.rs, 7)
   poke(dut.io.rt, 2)
   poke(dut.io.rd, 9)
   poke(dut.io.useImm, 0)
   poke(dut.io.imm, 0)
   poke(dut.io.func, 0)
   poke(dut.io.itype, 0)

   step(1)

   /* SUB r7, r2, r4 */
   poke(dut.io.rs, 2)
   poke(dut.io.rt, 4)
   poke(dut.io.rd, 7)
   poke(dut.io.useImm, 0)
   poke(dut.io.imm, 0)
   poke(dut.io.func, 1)
   poke(dut.io.itype, 0)

   step(1)

   /* ADD r6, r2, r4 */
   poke(dut.io.rs, 2)
   poke(dut.io.rt, 4)
   poke(dut.io.rd, 6)
   poke(dut.io.useImm, 0)
   poke(dut.io.imm, 0)
   poke(dut.io.func, 0)
   poke(dut.io.itype, 0)

   step(1)

   /* here it's busy */
   poke(dut.io.rs, 0)
   poke(dut.io.rt, 0)
   poke(dut.io.rd, 0)
   poke(dut.io.useImm, 0)
   poke(dut.io.func, 0)
   poke(dut.io.itype, 0)

   step(6)

   /* ADD r10, r5, r6 */
   poke(dut.io.rs, 5)
   poke(dut.io.rt, 6)
   poke(dut.io.rd, 10)
   poke(dut.io.useImm, 0)
   poke(dut.io.imm, 0)
   poke(dut.io.func, 0)
   poke(dut.io.itype, 0)

   step(1)

  // /* ADD r1, r0, 10 */
  // poke(dut.io.rs, 0)
  // poke(dut.io.rt, 0)
  // poke(dut.io.rd, 1)
  // poke(dut.io.useImm, 1)
  // poke(dut.io.imm, 10)
  // poke(dut.io.func, 0)
  // poke(dut.io.itype, 0)
  // step(1)

  // /* ADD r2, r0, 5 */
  // poke(dut.io.rs, 0)
  // poke(dut.io.rt, 0)
  // poke(dut.io.rd, 2)
  // poke(dut.io.useImm, 1)
  // poke(dut.io.imm, 5)
  // poke(dut.io.func, 0)
  // poke(dut.io.itype, 0)
  // step(1)

  // /* ADD r3, r2, r1 */
  // poke(dut.io.rs, 2)
  // poke(dut.io.rt, 1)
  // poke(dut.io.rd, 3)
  // poke(dut.io.useImm, 0)
  // poke(dut.io.func, 0)
  // poke(dut.io.itype, 0)
  // step(1)

   poke(dut.io.rs, 0)
   poke(dut.io.rt, 0)
   poke(dut.io.rd, 0)
   poke(dut.io.useImm, 0)
   poke(dut.io.func, 0)
   poke(dut.io.itype, 0)
   step(100)

}

object tMain {
   def main(args: Array[String]): Unit = {
      chiselMainTest(args,
         () => Module(new Main())) {
            dut => new TestMain(dut)
         }
      }
   }
