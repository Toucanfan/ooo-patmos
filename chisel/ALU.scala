package patmos

import Chisel._
import Node._

import Constants._

class Alu() extends Module {
  val io = new AluIO()

 def alu(func: Bits, op1: UInt, op2: UInt): Bits = {
    val result = UInt(width = DATA_WIDTH)
    val scaledOp1 = op1 << Mux(func === FUNC_SHADD2, UInt(2),
                               Mux(func === FUNC_SHADD, UInt(1),
                                   UInt(0)))
    val sum = scaledOp1 + op2
    result := sum // some default
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
