package patmos


import Chisel._
import Node._

import Constants._

class CDB() extends Module {
  val io = new CDBIO()
 // val ack_send = Bool()
  val reg_tag = Reg(init= Bits(0, RS_NUM))
  val reg_result = Reg(init = Bits(0, DATA_WIDTH))
  val reg_valid = Reg(init = Bits(0,1))

  //===========================================
  /* Logic which implements a SHIFT REGISTER*/
  //===========================================
  val dff =  Vec.fill(RS_NUM) { Reg(init= Bits(0,1))}

  when (io.ena){
    dff(0):=Bits(1)
  }.otherwise{
    dff(0):= dff(RS_NUM-1)
  }

  for(i<- 0 until RS_NUM-1){
    dff(i+1) := dff(i)
  }
  //===========================================



  //===========================================
  /*Logic to give ACK to only one unit*/
  /* And update common data bus result/tag*/
  //* When a unit request to write the next
  /* Clock cycle from the time the request 
      is accepted then it can write*/
  //===========================================

      io.regstat_io.tag:= reg_tag
      io.regstat_io.valid:= reg_valid
      io.regstat_io.result:= reg_result


  for(i<-0 until RS_NUM-1){
    io.RS_io(i).result_in:= reg_result
    io.RS_io(i).tag_in:= reg_tag


    when ((io.RS_io(i).rtw) && (dff(i) === Bits(1))){
      io.RS_io(i).ack:= Bits(1)
      reg_result := io.RS_io(i).result_out
      reg_tag := io.RS_io(i).tag_out
    //  reg_valid := Bits(1)
    }.otherwise{
      io.RS_io(i).ack:= Bits(0)
      io.RS_io(i).result_in:= reg_result
      io.RS_io(i).tag_in:= reg_tag
    }
  }

  reg_valid := io.RS_io.exists((_: RSCDB).rtw)


  //===========================================
//DEBUG SIGNALS
//io.token := dff(0)
//io.token1 := dff(1)
//io.token2 := dff(2)
//io.token3 := dff(3)
}


class cdbTestRingCounter(dut: CDB) extends Tester(dut) {
poke(dut.io.ena,1)
/*peek(dut.io.token)
peek(dut.io.token1)
peek(dut.io.token2)
peek(dut.io.token3)
*/
step(1)

poke(dut.io.ena,0)

poke(dut.io.RS_io(0).result_out,0)
poke(dut.io.RS_io(1).result_out,0)
/*peek(dut.io.token)
peek(dut.io.token1)
peek(dut.io.token2)
peek(dut.io.token3)
*/

peek(dut.io.regstat_io.tag)
peek(dut.io.regstat_io.valid)

 
poke(dut.io.RS_io(0).rtw,0)
poke(dut.io.RS_io(1).rtw,1)
poke(dut.io.RS_io(0).result_out,1)
poke(dut.io.RS_io(1).result_out,2)
poke(dut.io.RS_io(0).tag_out,0)
poke(dut.io.RS_io(1).tag_out,1)


peek(dut.io.RS_io(0).result_in)
peek(dut.io.RS_io(1).result_in)
peek(dut.io.RS_io(0).tag_in)
peek(dut.io.RS_io(1).tag_in)


peek(dut.io.RS_io(0).ack)
peek(dut.io.RS_io(1).ack)

step(1) 
poke(dut.io.ena,0)

/*peek(dut.io.token)
peek(dut.io.token1)
peek(dut.io.token2)
peek(dut.io.token3)
*/

peek(dut.io.regstat_io.tag)
peek(dut.io.regstat_io.valid)

 
poke(dut.io.RS_io(0).rtw,0)
poke(dut.io.RS_io(1).rtw,1)
poke(dut.io.RS_io(0).result_out,1)
poke(dut.io.RS_io(1).result_out,2)
poke(dut.io.RS_io(0).tag_out,0)
poke(dut.io.RS_io(1).tag_out,1)


peek(dut.io.RS_io(0).result_in)
peek(dut.io.RS_io(1).result_in)
peek(dut.io.RS_io(0).tag_in)
peek(dut.io.RS_io(1).tag_in)


peek(dut.io.RS_io(0).ack)
peek(dut.io.RS_io(1).ack)

step(1) 
poke(dut.io.ena,0)


/*peek(dut.io.token)
peek(dut.io.token1)
peek(dut.io.token2)
peek(dut.io.token3)
*/

peek(dut.io.regstat_io.tag)
peek(dut.io.regstat_io.valid)

 
poke(dut.io.RS_io(0).rtw,1)
poke(dut.io.RS_io(1).rtw,0)
poke(dut.io.RS_io(0).result_out,1)
poke(dut.io.RS_io(1).result_out,2)
poke(dut.io.RS_io(0).tag_out,0)
poke(dut.io.RS_io(1).tag_out,1)


peek(dut.io.RS_io(0).result_in)
peek(dut.io.RS_io(1).result_in)
peek(dut.io.RS_io(0).tag_in)
peek(dut.io.RS_io(1).tag_in)


peek(dut.io.RS_io(0).ack)
peek(dut.io.RS_io(1).ack)



step(1) 

poke(dut.io.ena,0)


/*peek(dut.io.token)
peek(dut.io.token1)
peek(dut.io.token2)
peek(dut.io.token3)
*/

peek(dut.io.regstat_io.tag)
peek(dut.io.regstat_io.valid)

 
poke(dut.io.RS_io(0).rtw,1)
poke(dut.io.RS_io(1).rtw,0)
poke(dut.io.RS_io(0).result_out,1)
poke(dut.io.RS_io(1).result_out,2)
poke(dut.io.RS_io(0).tag_out,0)
poke(dut.io.RS_io(1).tag_out,1)


peek(dut.io.RS_io(0).result_in)
peek(dut.io.RS_io(1).result_in)
peek(dut.io.RS_io(0).tag_in)
peek(dut.io.RS_io(1).tag_in)


peek(dut.io.RS_io(0).ack)
peek(dut.io.RS_io(1).ack)

step(1) 

poke(dut.io.ena,0)


/*peek(dut.io.token)
peek(dut.io.token1)
peek(dut.io.token2)
peek(dut.io.token3)
*/

peek(dut.io.regstat_io.tag)
peek(dut.io.regstat_io.valid)

 
poke(dut.io.RS_io(0).rtw,1)
poke(dut.io.RS_io(1).rtw,0)
poke(dut.io.RS_io(0).result_out,1)
poke(dut.io.RS_io(1).result_out,2)
poke(dut.io.RS_io(0).tag_out,0)
poke(dut.io.RS_io(1).tag_out,1)


peek(dut.io.RS_io(0).result_in)
peek(dut.io.RS_io(1).result_in)
peek(dut.io.RS_io(0).tag_in)
peek(dut.io.RS_io(1).tag_in)


peek(dut.io.RS_io(0).ack)
peek(dut.io.RS_io(1).ack)

step(1)
poke(dut.io.ena,0)


/*peek(dut.io.token)
peek(dut.io.token1)
peek(dut.io.token2)
peek(dut.io.token3)
*/

peek(dut.io.regstat_io.tag)
peek(dut.io.regstat_io.valid)

 
poke(dut.io.RS_io(0).rtw,0)
poke(dut.io.RS_io(1).rtw,0)
poke(dut.io.RS_io(0).result_out,1)
poke(dut.io.RS_io(1).result_out,2)
poke(dut.io.RS_io(0).tag_out,0)
poke(dut.io.RS_io(1).tag_out,1)


peek(dut.io.RS_io(0).result_in)
peek(dut.io.RS_io(1).result_in)
peek(dut.io.RS_io(0).tag_in)
peek(dut.io.RS_io(1).tag_in)


peek(dut.io.RS_io(0).ack)
peek(dut.io.RS_io(1).ack)
step(1)

step(1)
poke(dut.io.ena,0)


/*peek(dut.io.token)
peek(dut.io.token1)
peek(dut.io.token2)
peek(dut.io.token3)
*/

peek(dut.io.regstat_io.tag)
peek(dut.io.regstat_io.valid)

 
poke(dut.io.RS_io(0).rtw,0)
poke(dut.io.RS_io(1).rtw,0)
poke(dut.io.RS_io(0).result_out,1)
poke(dut.io.RS_io(1).result_out,2)
poke(dut.io.RS_io(0).tag_out,0)
poke(dut.io.RS_io(1).tag_out,1)


peek(dut.io.RS_io(0).result_in)
peek(dut.io.RS_io(1).result_in)
peek(dut.io.RS_io(0).tag_in)
peek(dut.io.RS_io(1).tag_in)


peek(dut.io.RS_io(0).ack)
peek(dut.io.RS_io(1).ack)
step(1)
}


object cdbTester {
  def main(args: Array[String]): Unit = {
    chiselMainTest(args,
      () => Module(new CDB)) {
        dut => new cdbTestRingCounter(dut)
      }
  }
}

