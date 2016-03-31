package patmos


import Chisel._
import Node._

import Constants._

class CDB() extends Module {
  val io = new CDBIO()
}
