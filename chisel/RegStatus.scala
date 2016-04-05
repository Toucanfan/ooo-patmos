package patmos

import Chisel._
import Node._

import Constants._

class RegStatus() extends Module {

   // io variables
   val io = new RegStatusIO()
   val issue_io = io.issue_io
   val RF_io = io.RF_io
   val tag_rs = io.CDB_io.tag // Bits

   // 32 register status, all initilized to 0
   val reg_stat = Vec.fill(32) { Reg(UInt(width = 4)) }

   // default values for outputs
   issue_io.tag_rs := Bits(0)
   issue_io.tag_rt := Bits(0)
   RF_io.rd_en := Bool(false)
   RF_io.rd_addr := Bits(0)

   // debug
   io.test := io.CDB_io.valid

   when (io.ena) {

      reg_stat(UInt(issue_io.rd)) := issue_io.tag_rd

      issue_io.tag_rs := reg_stat(issue_io.rs)
      issue_io.tag_rt := reg_stat(issue_io.rt)

      when (io.CDB_io.valid) {

         for (i <- 0 until 31) {
            // need UInt(tag_rs)?
            when (reg_stat(i) === tag_rs) { // WRONG

               //reg_stat(i) := Bit(0)
               RF_io.rd_en := Bool(true)
               RF_io.rd_addr := Bits(i)

            }/*.otherwise {
               RF_io.rd_en := Bool(false)
               RF_io.rd_addr := Bits(0)
            }*/
         }
      }
   }
}

class RSTest(d: RegStatus) extends Tester(d) {


   poke(d.io.ena, (true))

   step(1)

   // saves tag_rd (4) in reg @ rd (2)
   poke(d.io.issue_io.tag_rd, 4)
   poke(d.io.issue_io.rd, 0)

   poke(d.io.issue_io.rs, 0)

   expect(d.io.issue_io.tag_rs, 0)

   step(1)

   // saves tag_rd (6) in reg @ rd (3)
   poke(d.io.issue_io.tag_rd, 6)
   poke(d.io.issue_io.rd, 3)

   poke(d.io.issue_io.rs, 0)

   step(1)

   expect(d.io.issue_io.tag_rs, 4)
   poke(d.io.issue_io.rt, 3)

   step(1)

   expect(d.io.issue_io.tag_rt, 6)

   // search for tag (6) => rd_addr(3), rd_en on
   poke(d.io.CDB_io.tag, 6)
   poke(d.io.CDB_io.valid, (true))
   //expect(d.io.test, 1)

   step(1)

   expect(d.io.RF_io.rd_addr, 3)
   expect(d.io.RF_io.rd_en, 1)

   step(1)

   poke(d.io.CDB_io.valid, (false))
   // should reset register back to 0
   expect(d.io.RF_io.rd_addr, 0)
}

object RSTest {
  def main(args: Array[String]): Unit = {
    chiselMainTest(args, () => Module(new RegStatus())) {
      d => new RSTest(d)
    }
  }
}
