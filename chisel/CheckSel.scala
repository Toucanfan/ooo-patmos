package patmos

import Chisel._
import Node._

import Constants._

class tomSel() extends Module {
  val io = new tomSelIO()

//val Reg_switch = Bits(0,1)
//val Reg_transmit= Bits(0,1)
  val instr   = io.instr
  val opcode  = instr(26, 22)
  val opc     = instr(6, 4)
//  io.tdec.instr := io.instr
  val st_idle :: st_wait_ex :: st_wait_mem :: st_wait_wb :: st_activ_ooo :: Nil = Enum(UInt(),5)
  val reg_st   = Reg(init = st_idle)


  io.stopFetch := Bool(false)
  io.busy:=Bool(false)
  io.switch:=Bool(false)
  io.transmit:=Bool(false)

 switch (reg_st){
  is(st_idle){
    io.switch  := Bool(false)
    io.transmit  := Bool(false)
    reg_st := st_idle

    when ((opcode(4, 3) === OPCODE_ALUI) || (opcode === OPCODE_LDT) || (opcode === OPCODE_STT)){
      io.switch  := Bool(true)
      io.transmit  := Bool(true)
      io.stopFetch := Bool(true)
      reg_st := st_wait_ex

      //kame signal gia ooop
    }
    when ((opcode === OPCODE_ALU) && (opc === OPC_ALUR)){
      io.switch  := Bool(true)
      io.transmit  := Bool(true)
        reg_st := st_wait_ex
      //kame signal gia ooop
    }
     
  }
  is(st_wait_ex){
    when (io.tExDone){
     reg_st := st_wait_mem
    }

  }
  is(st_wait_mem){
     when (io.tMemDone){
     reg_st := st_wait_wb
    }

  }
  is(st_wait_wb){
     when (io.tWbDone){
      io.stopFetch:= Bool(false)
      //send enable to OOO
     reg_st := st_activ_ooo
    }
  }
  is(st_activ_ooo){
    //execute until illegal instruction.
    io.busy:= Bool(true)
    //when illegal instruction is in then return to idle and go through wait states
    //to finish and write back all the possible values.
    //this is done when all the units busy lines are off. and when all the values are written back
    //to register file.
    // add two states more when busy lines off and when register file is written. ?  
    reg_st:= st_activ_ooo


  }
} 



  
}

class checkTest1(dut: tomSel) extends Tester(dut) {
  poke(dut.io.ena, (true) )
  poke(dut.io.instr,0 )

  poke(dut.io.tExDone,0)
  poke(dut.io.tMemDone,0)

  poke(dut.io.tWbDone,0)


  peek(dut.io.stopFetch)
  peek(dut.io.busy)
  peek(dut.io.switch )
  peek(dut.io.transmit )
  peek(dut.io.instr)



  step(1)

  poke(dut.io.ena, (true) )
  poke(dut.io.instr,0 )

  poke(dut.io.tExDone,0)
  poke(dut.io.tMemDone,0)

  poke(dut.io.tWbDone,0)

   

  peek(dut.io.stopFetch)
  peek(dut.io.busy)
  peek(dut.io.switch )
  peek(dut.io.transmit )
  peek(dut.io.instr)



  step(1)

  poke(dut.io.ena, (true) )
  poke(dut.io.instr,0 )

  poke(dut.io.tExDone,1)
  poke(dut.io.tMemDone,0)

  poke(dut.io.tWbDone,0)

   

  peek(dut.io.stopFetch)
  peek(dut.io.busy)
  peek(dut.io.switch )
  peek(dut.io.transmit )
  peek(dut.io.instr)



  step(1)

  poke(dut.io.ena, (true) )
  poke(dut.io.instr,0 )

  poke(dut.io.tExDone,0)
  poke(dut.io.tMemDone,1)

  poke(dut.io.tWbDone,0)

   

  peek(dut.io.stopFetch)
  peek(dut.io.busy)
  peek(dut.io.switch )
  peek(dut.io.transmit )
  peek(dut.io.instr)



  step(1)


  poke(dut.io.ena, (true) )
  poke(dut.io.instr,0 )

  poke(dut.io.tExDone,0)
  poke(dut.io.tMemDone,1)

  poke(dut.io.tWbDone,1)

   

  peek(dut.io.stopFetch)
  peek(dut.io.busy)
  peek(dut.io.switch )
  peek(dut.io.transmit )
  peek(dut.io.instr)



  step(1)



  poke(dut.io.ena, (true) )
  poke(dut.io.instr,0 )

  poke(dut.io.tExDone,0)
  poke(dut.io.tMemDone,1)

  poke(dut.io.tWbDone,0)

   

  peek(dut.io.stopFetch)
  peek(dut.io.busy)
  peek(dut.io.switch )
  peek(dut.io.transmit )
  peek(dut.io.instr)



  step(1)
}

object checkTest {
  def main(args: Array[String]): Unit = {
    chiselMainTest(args, () => Module(new tomSel())) {
      c => new checkTest1(c)
    }
  }
}
