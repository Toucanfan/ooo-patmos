package skeleton
import Chisel._

class Skeleton extends Module {
	val io = new Bundle {
		val led = UInt(OUTPUT, 1);
	}

  /* Constants */
	val CNT_LONG = UInt(20000000 + 1);
	val CNT_SHRT = UInt(20000000/8 + 1);
  val s_long::s_short::Nil = Enum(UInt(), 2)

  /* Registers */
  val regs = new Bundle {
    val state = Reg(init = s_long)
    val count = Reg(init = UInt(0, 25))
    val blink = Reg(init = UInt(0, 1))
  }

  /* Helpers */
  def toggleBlink() = {
    regs.count := UInt(0);
    regs.blink := ~regs.blink; 
  }


  /* Signal assignment */
	io.led := regs.blink;
	regs.count := regs.count + UInt(1);

  switch(regs.state) {

    is(s_long) {
      when (regs.count === CNT_LONG) {
        toggleBlink();
        regs.state := s_short; }}

    is(s_short) {
      when (regs.count === CNT_SHRT) {
        toggleBlink();
        regs.state := s_long; }}
  }
}


object Skeleton {
	def main(args: Array[String]): Unit = {
    chiselMain(args, () =>Module(new Skeleton()))
  }
}
