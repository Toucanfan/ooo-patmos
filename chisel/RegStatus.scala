package patmos

import Chisel._
import Node._

import Constants._

class RegStatus() extends Module {

   // io variables
   val io = new RegStatusIO()
   val reg_stat = Vec.fill(32) { Reg(init = Bits(0, width = TAG_BITS)) }

   // RF_io.rd_en := Bool(false)
   // RF_io.rd_addr := Bits(0)
   io.RF_io.rd_addr := Bits(0)
   io.RF_io.result := io.CDB_io.result
   io.RF_io.rd_en := Bool(false)

   // tag retreival
   io.issue_io.tag_rs := reg_stat(io.issue_io.rs)
   io.issue_io.tag_rt := reg_stat(io.issue_io.rt)

   // tag insertion
   reg_stat(UInt(io.issue_io.rd)) := io.issue_io.tag_rd

      // when (issue_io.valid) {
      // reg_stat(UInt(issue_io.rd)) := Cat(Bits(1), issue_io.tag_rd)
      // }

      // RIGHT NOW TAG CANNOT BE 0
   when (io.CDB_io.valid) {
     io.RF_io.rd_en := Bool(true)
     io.RF_io.result := io.CDB_io.result

     for (i <- 0 until REG_COUNT) {
       when (reg_stat(i) === io.CDB_io.tag) {
         io.RF_io.rd_addr(i) := Bits(1)
         reg_stat(i) := Bits(0)
         when (UInt(io.issue_io.rs) === UInt(i)) {
           io.issue_io.tag_rs := Bits(0)
         }
         when (UInt(io.issue_io.rt) === UInt(i)) {
           io.issue_io.tag_rt := Bits(0)
         }
       }
     }
   }
}

class RSTest(d: RegStatus) extends Tester(d) {

   /*poke(d.io.ena, (true))
   step(1)

   // reg3 initial value
   poke(d.io.issue_io.rs, 3)
   expect(d.io.issue_io.tag_rs, 0)
   step(1)

   //peek(d.io.test)
   // set reg3 with tag 0
   poke(d.io.issue_io.valid, (true))
   poke(d.io.issue_io.rd, 3)
   poke(d.io.issue_io.tag_rd, 5)
   step(1)

   poke(d.io.issue_io.valid, (false))

   // retrieve addr of tag 0
   poke(d.io.CDB_io.tag, 5)
   poke(d.io.CDB_io.valid, (true))
   step(2)

   expect(d.io.RF_io.rd_addr, 3)
   expect(d.io.RF_io.rd_en, 1)
   step(1)

   poke(d.io.CDB_io.valid, (false))
   step(1)

   expect(d.io.RF_io.rd_en, 0)
   poke(d.io.issue_io.rs, 3)
   step(1)
   peek(d.io.RF_io.rd_addr)
   expect(d.io.issue_io.tag_rs, 0)
   step(1)
   poke(d.io.CDB_io.valid, (false))
   step(1)*/
}

object RSTest {
   def main(args: Array[String]): Unit = {
      chiselMainTest(args, () => Module(new RegStatus())) {
         d => new RSTest(d)
      }
   }
}
