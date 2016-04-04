package patmos


import Chisel._
import Node._

import Constants._

class ReservationStation(RS_id: Int) extends Module {
  val io = new ReservationStationIO()
  val rs_id = Bits(RS_id)

  val st_idle :: st_ready :: st_wait :: st_execute :: st_rtw :: st_ack :: Nil = Enum(UInt(),6) 
 
  val reg_st 	 = Reg(init = st_idle)
  val reg_val_rt = Reg(init = Bits(0, DATA_WIDTH))
  val reg_val_rs = Reg(init = Bits(0, DATA_WIDTH))
  val reg_tag_rt = Reg(init = Bits(0, RS_NUM))
  val reg_tag_rs = Reg(init = Bits(0, RS_NUM))
  val reg_alu_res= Reg(init = Bits(0, DATA_WIDTH))
  io.issue_io.busy := Bool(false)
  io.CDB_io.rtw := Bool(false)
  io.CDB_io.result_out := reg_alu_res
  io.CDB_io.tag_out := rs_id
  io.rs_state:= UInt(0) 

		io.rs_value_rs := Bits(0)
		io.rs_value_rt :=  Bits(0)
  val result_alu =  alu(io.issue_io.func,reg_val_rs, reg_val_rt)
		
  
  switch (reg_st){
	is(st_idle){ 

		reg_st:=st_idle	
		io.issue_io.busy := Bool(false)
		  io.rs_state:= UInt(1) 
		when (io.ena){
			reg_st := st_ready
		}
	}
	is(st_ready)   {
		io.issue_io.busy := Bool(false)
		io.rs_state:= UInt(2) 
		reg_val_rs := io.issue_io.val_rs
		reg_val_rt := io.issue_io.val_rt
		reg_tag_rt := io.issue_io.tag_rt
		reg_tag_rs := io.issue_io.tag_rs
		reg_st := st_ready

		io.rs_value_rs := reg_val_rs
		io.rs_value_rt := reg_val_rt
		when (io.issue_io.sel){
			io.issue_io.busy := Bool(true)
			reg_st := st_wait
		}
	}
	is(st_wait) {
 	  	io.rs_state:= UInt(3) 
		io.issue_io.busy := Bool(true)
		reg_st :=st_wait

		io.rs_value_rs := reg_val_rs
		io.rs_value_rt := reg_val_rt
		when ((reg_tag_rs === Bits(0)) && (reg_tag_rt === Bits(0))){
			reg_st := st_execute
		}
		when ((reg_tag_rs === io.CDB_io.tag_in) && (reg_tag_rt === io.CDB_io.tag_in)){
			reg_val_rt := io.CDB_io.result_in
			reg_tag_rt := Bits(0)
			
			io.rs_value_rs := Bits(1)
			io.rs_value_rt := Bits(1)
			reg_st:= st_execute
			reg_val_rs := io.CDB_io.result_in
			reg_tag_rs := Bits(0)
		}
		when ((reg_tag_rs === io.CDB_io.tag_in)&& (reg_tag_rs> Bits(0))){ 
			reg_st:= st_wait

			io.rs_value_rs := Bits(2)
			io.rs_value_rt := Bits(2)
			reg_val_rs := io.CDB_io.result_in
			reg_tag_rs := Bits(0)
		}
		when((reg_tag_rt === io.CDB_io.tag_in)&& (reg_tag_rt> Bits(0))){
			reg_val_rt := io.CDB_io.result_in
			reg_tag_rt := Bits(0)

			io.rs_value_rs := Bits(3)
			io.rs_value_rt := Bits(3)
			reg_st:= st_wait
		}
	}
	is(st_execute) {
  		io.rs_state:= UInt(4) 
		io.issue_io.busy := Bool(true)
		io.rs_value_rs := reg_val_rs
		io.rs_value_rt := reg_val_rt
		reg_alu_res := result_alu
		reg_st:=st_ack
		io.CDB_io.rtw := Bool(true)
	}
	is(st_rtw){
		io.rs_state:= UInt(5) 
		io.issue_io.busy := Bool(true)
		io.CDB_io.rtw := Bool(true)
		io.CDB_io.result_out := reg_alu_res
		io.CDB_io.tag_out := rs_id
		reg_st:= st_ack
	}
	is(st_ack){
  		io.rs_state:= UInt(6) 
		io.issue_io.busy := Bool(true)
		reg_st:= st_rtw
		when (io.CDB_io.ack){
			//now write
			reg_st:=st_idle
			io.CDB_io.tag_out := rs_id
			io.CDB_io.result_out := reg_alu_res
			io.issue_io.busy := Bool(false)
		}
	}
  }


  def alu(func: Bits, op1: UInt, op2: UInt): Bits = {
    val result = UInt(width = DATA_WIDTH)
    val scaledOp1 = op1 << Mux(func === FUNC_SHADD2, UInt(2),
                               Mux(func === FUNC_SHADD, UInt(1),
                                   UInt(0)))
    val sum = scaledOp1 + op2
    result := sum// some default
    val shamt = op2(4, 0).toUInt
    val srOp = Mux(func === FUNC_SRA, op1(DATA_WIDTH-1), Bits(0)) ## op1
    // This kind of decoding of the ALU op in the EX stage is not efficient,
    // but we keep it for now to get something going soon.
    switch(func) {
      is(FUNC_ADD)    { result := sum }
      is(FUNC_SUB)    { result := op1 - op2 }
      is(FUNC_XOR)    { result := (op1 ^ op2).toUInt }
      is(FUNC_SL)     { result := (op1 << shamt)(DATA_WIDTH-1, 0).toUInt }
      is(FUNC_SR, FUNC_SRA) { result := (srOp.toSInt >> shamt).toUInt }
      is(FUNC_OR)     { result := (op1 | op2).toUInt }
      is(FUNC_AND)    { result := (op1 & op2).toUInt }
      is(FUNC_NOR)    { result := (~(op1 | op2)).toUInt }
      is(FUNC_SHADD)  { result := sum }
      is(FUNC_SHADD2) { result := sum }
    }
    result
  }
}

class rsTesterCase1(dut: ReservationStation) extends Tester(dut) {
/*io.rs_state :: io.rs_val_rs :: io.rs_val_rt used for debugging*/
/*Test 1: val_rs = 1 val_rt = 1 func = FUNC_ADD tag_rs = 0 tag_rt =0 */
  step(1)
  poke(dut.io.ena, (true) )
  poke(dut.io.issue_io.val_rs, 1)
  poke(dut.io.issue_io.val_rt, 1)
  poke(dut.io.issue_io.tag_rt, 0)
  poke(dut.io.issue_io.tag_rs, 0)
  poke(dut.io.issue_io.func, 0)
  expect(dut.io.rs_state, 1)
  poke(dut.io.issue_io.sel,1)
  step(1)
  expect(dut.io.rs_state, 2)
  step(1)
  expect(dut.io.rs_state, 3)
  step(1)
  expect(dut.io.CDB_io.result_out, 2)
  expect(dut.io.rs_state, 4)
  step(1)
  expect(dut.io.CDB_io.result_out, 2)
  expect(dut.io.rs_state, 5)
  step(1)
  expect(dut.io.CDB_io.result_out, 2)
  expect(dut.io.rs_state, 6)
  step(1)
  expect(dut.io.CDB_io.result_out, 2)
  expect(dut.io.rs_state, 6)
  step(10) // wait for ack before writing to CDB
  expect(dut.io.CDB_io.result_out, 2)
  expect(dut.io.rs_state, 6)
  poke(dut.io.CDB_io.ack, (true))
  step(1) 
  expect(dut.io.rs_state, 1)

}

class rsTesterCase2(dut: ReservationStation) extends Tester(dut) {
/*io.rs_state :: io.rs_val_rs :: io.rs_val_rt used for debugging*/
/*Test 1: val_rs = - val_rt = - func = FUNC_ADD tag_rs = TAG:2 tag_rt = TAG:2 */
  step(1)
  poke(dut.io.ena, (true) )
  poke(dut.io.issue_io.val_rs, 1)
  poke(dut.io.issue_io.val_rt, 0)
  poke(dut.io.issue_io.tag_rt, 2)
  poke(dut.io.issue_io.tag_rs, 2)
  poke(dut.io.issue_io.func, 0)
  expect(dut.io.rs_state, 1)
  poke(dut.io.issue_io.sel,1)
  expect(dut.io.rs_value_rs,0)
  expect(dut.io.rs_value_rt,0)

  step(1)
  expect(dut.io.rs_value_rs,0)
  expect(dut.io.rs_value_rt,0)

  expect(dut.io.rs_state, 2)
  step(1)

  expect(dut.io.rs_value_rs,0)
  expect(dut.io.rs_value_rt,0)
  expect(dut.io.rs_state, 3)
  step(1)

  expect(dut.io.rs_value_rs,0)
  expect(dut.io.rs_value_rt,0)
  expect(dut.io.rs_state, 3)
  step(1)

  expect(dut.io.rs_value_rs,0)  
  expect(dut.io.rs_value_rt,0)
  expect(dut.io.rs_state, 3)

  step(1) //remain in wait state until tag_rt and tag_rs are received 
  poke(dut.io.CDB_io.result_in, 4)
  poke(dut.io.CDB_io.tag_in, 2)
  expect(dut.io.rs_state, 4)

  expect(dut.io.rs_value_rs,0)
  expect(dut.io.rs_value_rt,0)
  step(1)

  expect(dut.io.rs_value_rs,0)  
  expect(dut.io.rs_value_rt,0)
  poke(dut.io.CDB_io.result_in, 4)
  poke(dut.io.CDB_io.tag_in, 2)
  expect(dut.io.rs_state, 5)
  step(1)

  expect(dut.io.CDB_io.result_out, 5)
  expect(dut.io.rs_state, 6)
  step(1)

  expect(dut.io.CDB_io.result_out, 5)
  expect(dut.io.rs_state, 6)
  step(10) // wait for ack before writing to CDB

  expect(dut.io.CDB_io.result_out, 5)
  expect(dut.io.rs_state, 6)
  poke(dut.io.CDB_io.ack, (true))

  step(1) 
  expect(dut.io.rs_state, 1)

}

class rsTesterCase3(dut: ReservationStation) extends Tester(dut) {
/*io.rs_state :: io.rs_val_rs :: io.rs_val_rt used for debugging*/
/*Test 3: val_rs = - val_rt =5 func = FUNC_ADD tag_rs = TAG:2 tag_rt = 0 */
  step(1)
  poke(dut.io.ena, (true) )
  poke(dut.io.issue_io.val_rs, 0)
  poke(dut.io.issue_io.val_rt, 5)
  poke(dut.io.issue_io.tag_rt, 0)
  poke(dut.io.issue_io.tag_rs, 2)
  poke(dut.io.issue_io.func, 0)
  expect(dut.io.rs_state, 1)
  poke(dut.io.issue_io.sel,1)

  expect(dut.io.rs_value_rs,0)
  
  expect(dut.io.rs_value_rt,0)

  step(1)

  expect(dut.io.rs_value_rs,0)
  
  expect(dut.io.rs_value_rt,0)

  expect(dut.io.rs_state, 2)
  step(1)

  expect(dut.io.rs_value_rs,0)
  
  expect(dut.io.rs_value_rt,0)

  expect(dut.io.rs_state, 3)
  step(1)

  expect(dut.io.rs_value_rs,0)
  
  expect(dut.io.rs_value_rt,0)

  expect(dut.io.rs_state, 3)
  step(1)

  expect(dut.io.rs_value_rs,0)
  
  expect(dut.io.rs_value_rt,0)

  expect(dut.io.rs_state, 3)
  step(1) //remain in wait state until tag_rt and tag_rs are received 
  poke(dut.io.CDB_io.result_in, 4)
  poke(dut.io.CDB_io.tag_in, 2)
  expect(dut.io.rs_state, 4)

  expect(dut.io.rs_value_rs,0)
  
  expect(dut.io.rs_value_rt,0)


  expect(dut.io.issue_io.busy, 0)
  step(1)

  expect(dut.io.rs_value_rs,0)
  
  expect(dut.io.rs_value_rt,0)

  expect(dut.io.issue_io.busy, 0)
  poke(dut.io.CDB_io.result_in, 4)
  poke(dut.io.CDB_io.tag_in, 2)
  expect(dut.io.rs_state, 5)
  step(1)
  expect(dut.io.CDB_io.result_out, 5)
  expect(dut.io.rs_state, 6)
  step(1)
  expect(dut.io.CDB_io.result_out, 5)
  expect(dut.io.rs_state, 6)
  step(10) // wait for ack before writing to CDB
  expect(dut.io.CDB_io.result_out, 5)
  expect(dut.io.rs_state, 6)
  poke(dut.io.CDB_io.ack, (true))
  step(1) 
  expect(dut.io.rs_state, 1)

}

class rsTesterCase4(dut: ReservationStation) extends Tester(dut) {
/*io.rs_state :: io.rs_val_rs :: io.rs_val_rt used for debugging*/
/*Test 3: val_rs = VAL val_rt =- func = FUNC_ADD tag_rs =0 tag_rt = TAG:2 */
  step(1)
  poke(dut.io.ena, (true) )
  poke(dut.io.issue_io.val_rs, 1)
  poke(dut.io.issue_io.val_rt, 0)
  poke(dut.io.issue_io.tag_rt, 2)
  poke(dut.io.issue_io.tag_rs, 0)
  poke(dut.io.issue_io.func, 0)
  expect(dut.io.rs_state, 1)
  poke(dut.io.issue_io.sel,1)

  expect(dut.io.rs_value_rs,0)
  
  expect(dut.io.rs_value_rt,0)

  step(1)

  expect(dut.io.rs_value_rs,0)
  
  expect(dut.io.rs_value_rt,0)

  expect(dut.io.rs_state, 2)
  step(1)

  expect(dut.io.rs_value_rs,0)
  
  expect(dut.io.rs_value_rt,0)

  expect(dut.io.rs_state, 3)
  step(1)

  expect(dut.io.rs_value_rs,0)
  
  expect(dut.io.rs_value_rt,0)

  expect(dut.io.rs_state, 3)
  step(1)

  expect(dut.io.rs_value_rs,0)
  
  expect(dut.io.rs_value_rt,0)

  expect(dut.io.rs_state, 3)
  step(1) //remain in wait state until tag_rt and tag_rs are received 
  poke(dut.io.CDB_io.result_in, 4)
  poke(dut.io.CDB_io.tag_in, 2)
  expect(dut.io.rs_state, 4)

  expect(dut.io.rs_value_rs,0)
  expect(dut.io.rs_value_rt,0)


  expect(dut.io.issue_io.busy, 0)
  step(1)

  expect(dut.io.rs_value_rs,0)
  
  expect(dut.io.rs_value_rt,0)

  expect(dut.io.issue_io.busy, 0)
  poke(dut.io.CDB_io.result_in, 4)
  poke(dut.io.CDB_io.tag_in, 2)
  expect(dut.io.rs_state, 5)
  step(1)
  expect(dut.io.CDB_io.result_out, 5)
  expect(dut.io.rs_state, 6)
  step(1)
  expect(dut.io.CDB_io.result_out, 5)
  expect(dut.io.rs_state, 6)
  step(10) // wait for ack before writing to CDB
  expect(dut.io.CDB_io.result_out, 5)
  expect(dut.io.CDB_io.tag_out, 1)
  expect(dut.io.rs_state, 6)
  poke(dut.io.CDB_io.ack, (true))
  step(1) 
  expect(dut.io.rs_state, 1)

}





object rsTester {
  def main(args: Array[String]): Unit = {
    chiselMainTest(args,
      () => Module(new ReservationStation(1))) {
        dut => new rsTesterCase4(dut)
      }
  }
}

object ReservationStationMain {
  def main(args: Array[String]): Unit = {
    chiselMain(Array[String]("--backend", "v", "--targetDir", "generated"),
      () => Module(new ReservationStation(1)))
  }
}


