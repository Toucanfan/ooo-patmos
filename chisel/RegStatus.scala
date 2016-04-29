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
   val change_addr = Reg(init = UInt(0))
   val st_read :: st_write :: Nil = Enum(UInt(),2)
   val state = Reg(init=st_read)

   // 32 register status, all initilized to 0
   // 4th bit indicates validity of tag
   // val reg_stat = Vec.fill(32) { Reg(init = Bits(0, width = TAG_BITS+1)) }
   val reg_stat = Vec.fill(32) { Reg(init = Bits(0, width = TAG_BITS)) }

   // default values for outputs
   issue_io.tag_rs := Bits(0)
   issue_io.tag_rt := Bits(0)
   RF_io.rd_en := Bool(false)
   RF_io.rd_addr := Bits(0)
   RF_io.result := io.CDB_io.result

   when (io.ena) {

      // tag retreival
      issue_io.tag_rs := reg_stat(issue_io.rs)
      issue_io.tag_rt := reg_stat(issue_io.rt)

      // when (issue_io.valid) {
         // adds valid bit in front of tag
         // reg_stat(UInt(issue_io.rd)) := Cat(Bits(1), issue_io.tag_rd)
            reg_stat(UInt(issue_io.rd)) := issue_io.tag_rd
      // }
      when (io.CDB_io.valid) {

         switch(state) {
            is(st_read) {

               // adds valid bit in front of tag 1XXX

               when (reg_stat.contains(tag_rs)) {
                  // change_addr := reg_stat.indexWhere((_: Bits) === Cat(Bits(1),tag_rs))
                  change_addr := reg_stat.indexWhere((_: Bits) === tag_rs)
               }.otherwise {
                  change_addr := Bits(0)
               }

               // outputs address and enable line
               RF_io.rd_en := Bool(true)
               RF_io.rd_addr := change_addr

               // ADDED RESULT line
               RF_io.result := io.CDB_io.result

               state := st_write
            }
            is(st_write) {

               // resets tag to 0000
               reg_stat(change_addr):= Bits(0)
               state := st_read
            }
         }
      }.otherwise {
         RF_io.rd_en := Bool(false)
         RF_io.result := io.CDB_io.result
      }
   }
}

class RSTest(d: RegStatus) extends Tester(d) {

   poke(d.io.ena, (true))
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
   step(1)
}

object RSTest {
   def main(args: Array[String]): Unit = {
      chiselMainTest(args, () => Module(new RegStatus())) {
         d => new RSTest(d)
      }
   }
}
