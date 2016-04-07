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
   val reg_stat = Vec.fill(32) { Reg(init = Bits(0, width = TAG_BITS)) }

   // default values for outputs
   issue_io.tag_rs := Bits(0)
   issue_io.tag_rt := Bits(0)
   RF_io.rd_en := Bool(false)
   RF_io.rd_addr := Bits(0)
   //io.test := Bits(7)
   // debug

   when (io.ena) {
      issue_io.tag_rs := reg_stat(issue_io.rs)
      issue_io.tag_rt := reg_stat(issue_io.rt)

      when (issue_io.valid) {
         reg_stat(UInt(issue_io.rd)) := issue_io.tag_rd
      }
      when (io.CDB_io.valid) {
         switch(state) {
            is(st_read) {
               //RF_io.rd_en := reg_stat.exists(_ === tag_rs)
               RF_io.rd_en := Bool(true)
               change_addr := reg_stat.indexWhere((_: Bits) === tag_rs)
               RF_io.rd_addr:= change_addr
               state := st_write

               //io.test := change_addr
            }
            is(st_write) {
               reg_stat(change_addr):= Bits(0)
               state := st_read
            }
         }

      }.otherwise {
         RF_io.rd_en := Bool(false)
      }
   }
}

class RSTest1(d: RegStatus) extends Tester(d) {


   poke(d.io.ena, (true))

   step(1)

   // saves tag_rd (4) in reg @ rd (0)
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
   poke(d.io.issue_io.rt, 3)
   // should reset register back to 0
   expect(d.io.RF_io.rd_addr, 0)

   step(1)

   expect(d.io.issue_io.tag_rt, 0)
}

class RSTest2(d: RegStatus) extends Tester(d) {

   poke(d.io.ena, (true))
   step(1)

   // reg3 initial value
   poke(d.io.issue_io.rs, 3)
   expect(d.io.issue_io.tag_rs, 0)
   step(1)

   // set reg3 with tag 5
   poke(d.io.issue_io.valid, (true))
   poke(d.io.issue_io.rd, 3)
   poke(d.io.issue_io.tag_rd, 5)
   step(1)

   poke(d.io.issue_io.valid, (false))

   // retrieve addr of tag 5
   poke(d.io.CDB_io.tag, 5)
   poke(d.io.CDB_io.valid, (true))
   step(2)
   expect(d.io.RF_io.rd_addr, 3)
   expect(d.io.RF_io.rd_en, 1)
   step(2)
   //peek(d.io.test)
   poke(d.io.CDB_io.valid, (false))
   step(2)

   //peek(d.io.test)

   expect(d.io.RF_io.rd_en, 0)
   poke(d.io.issue_io.rs, 5)
   step(2)
   peek(d.io.RF_io.rd_addr)
   expect(d.io.issue_io.tag_rs, 0)
   step(2)
   poke(d.io.CDB_io.valid, (false))
   step(2)


}

object RSTest {
   def main(args: Array[String]): Unit = {
      chiselMainTest(args, () => Module(new RegStatus())) {
         d => new RSTest2(d)
      }
   }
}
