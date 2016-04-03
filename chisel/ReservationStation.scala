package patmos


import Chisel._
import Node._

import Constants._

class ReservationStation(RS_id: UInt) extends Module {
  val io = new ReservationStationIO()


  val st_idle :: st_ready :: st_wait :: st_execute :: st_rtw :: st_ack :: Nil = Enum(UInt(),6) 
 
  val reg_st 	 = Reg(init = st_idle)
  val reg_val_rt = Reg(init = UInt(0, DATA_WIDTH))
  val reg_val_rs = Reg(init = UInt(0, DATA_WIDTH))
  val reg_tag_rt = Reg(init = UInt(0, RS_NUM))
  val reg_tag_rs = Reg(init = UInt(0, RS_NUM))
  val reg_alu_res= Reg(init = UInt(0, DATA_WIDTH))
  
  switch (reg_st){
	is(st_idle)    { 
		io.issue_io.busy := Bool(false)
		when (io.ena){
			reg_st := st_ready
		}
		reg_st:=st_idle	
	}
	is(st_ready)   {
		io.issue_io.busy := Bool(false)
		reg_val_rs := io.issue_io.val_rs
		reg_val_rt := io.issue_io.val_rt
		reg_tag_rt := io.issue_io.tag_rt
		reg_tag_rs := io.issue_io.tag_rs
		reg_st := st_ready
		when (io.issue_io.sel){
			io.issue_io.busy := Bool(true)
			reg_st := st_wait
		}
	}
	is(st_wait) {
		io.issue_io.busy := Bool(true)
		when ((reg_tag_rs === UInt(0)) && (reg_tag_rt === UInt(0))){
			reg_st := st_execute
		}.otherwise{
			when (reg_tag_rs === io.CDB_io.tag_in){ 
				reg_st:= st_wait
				reg_val_rs := io.CDB_io.result_in
				reg_tag_rs := UInt(0)
			}
			when (reg_tag_rt === io.CDB_io.tag_in){
				reg_val_rt := io.CDB_io.result_in
				reg_tag_rt := UInt(0)
				reg_st:= st_wait
			}
			when ((reg_tag_rs === io.CDB_io.tag_in)&&(reg_tag_rt === io.CDB_io.tag_in)){
				reg_val_rt := io.CDB_io.result_in
				reg_tag_rt := UInt(0)
				reg_st:= st_execute
				reg_val_rs := io.CDB_io.result_in
				reg_tag_rs := UInt(0)
			}
		}
	}
	is(st_execute) {
		io.issue_io.busy := Bool(true)
		// calculate
		reg_alu_res := alu(io.issue_io.func,reg_val_rs, reg_val_rt)
		reg_st:=st_ack
		io.CDB_io.rtw := Bool(true)
	}
	is(st_rtw){
		io.issue_io.busy := Bool(true)
		io.CDB_io.rtw := Bool(true)
		io.CDB_io.result_out := reg_alu_res
		io.CDB_io.tag_out := RS_id
		reg_st:= st_ack
	}
	is(st_ack){
		io.issue_io.busy := Bool(true)
		reg_st:= st_rtw
		when (io.CDB_io.ack){
			//now write
			reg_st:=st_idle
			io.CDB_io.tag_out := RS_id
			io.CDB_io.result_out := reg_alu_res
			io.issue_io.busy := Bool(false)
		}
	}
  }


  def alu(func: Bits, op1: UInt, op2: UInt): Bits ={
    val result = UInt(width = DATA_WIDTH)

    switch (func){
	is(FUNC_ADD)    { result := op1 + op2 } // change 
	is(FUNC_SUB)    { result := op1 - op2 }
	is(FUNC_XOR)    { result := (op1 ^ op2).toUInt }
	is(FUNC_SL)     { result := (op1)}// << shamt)(DATA_WIDTH-1, 0).toUInt }
	is(FUNC_SR, FUNC_SRA) { result := op2}//(srOp.toSInt >> shamt).toUInt }
	is(FUNC_OR)     { result := (op1 | op2).toUInt }
	is(FUNC_AND)    { result := (op1 & op2).toUInt }
	is(FUNC_NOR)    { result := (~(op1 | op2)).toUInt }
	is(FUNC_SHADD)  { result := op1 + op2 } //change
	is(FUNC_SHADD2) { result :=op1 + op2 } //change
	}
    result
  }

  }

class rsTester(dut: ReservationStation) extends Tester(dut) {

  step(10)
  poke(dut.io.ena, (true) )
  poke(dut.io.issue_io.val_rs, 1)
  poke(dut.io.issue_io.val_rt, 1)
  poke(dut.io.issue_io.tag_rt, 0)
  poke(dut.io.issue_io.tag_rs, 0)
  poke(dut.io.issue_io.func, 0)
  poke(dut.io.issue_io.func, 1)
  poke(dut.io.CDB_io.ack, (true))


  step(10)
  poke(dut.io.ena, (true))
  poke(dut.io.issue_io.val_rs, 4)
  poke(dut.io.issue_io.val_rt, 4)
  poke(dut.io.issue_io.tag_rt, 0)
  poke(dut.io.issue_io.tag_rs, 0)
  poke(dut.io.issue_io.func, 0)
  poke(dut.io.issue_io.func, 1)
  poke(dut.io.CDB_io.ack,(true))

  step(10)
}


object rsTester {
  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "v", "--compile", "--test",
      "--genHarness", "--vcd", "--targetDir", "generated"),
      () => Module(new ReservationStation(UInt(10)))) {
        dut => new rsTester(dut)
      }
  }
}

object ReservationStationMain {
  def main(args: Array[String]): Unit = {
    chiselMain(Array[String]("--backend", "v", "--targetDir", "generated"),
      () => Module(new ReservationStation(UInt(10))))
  }
}


