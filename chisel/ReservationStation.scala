package patmos


import Chisel._
import Node._

import Constants._

class ReservationStation() extends Module {
  val io = new ReservationStationIO()
}


class AluIO() extends Bundle() {
  val ena = Bool(INPUT)
  val func = Bits(INPUT, FUNC_WIDTH)
  val op1 = UInt(INPUT, DATA_WIDTH)
  val op2 = UInt(INPUT, DATA_WIDTH)
  val done = Bool(OUTPUT)
  val result = Bits(OUTPUT, DATA_WIDTH)
}
