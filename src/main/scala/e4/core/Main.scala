// with Ints for NT and TM (even / odd)
package e4.core

import scala.collection.immutable.Map
import scala.collection.immutable.Set

import e4.core.Core_types._
//import e4.core.Core._

object Main {
  
  def main(args: Array[String]) {

    import p5.examples.Timing

    
    val core = new Core()
    def do_work = {
      println("Start")
      val r = core.fg_earley(core.at0.e)
      println("Finished")
    }
    //do_work
    Timing.time(4, do_work)
    Timing.time(1, do_work)
    // length 100, Elapsed time: 140961803 ns (0.140961803 secs) (avg: 0.140961803)
    // 200, Elapsed time: 783043467 ns (0.783043467 secs) (avg: 0.783043467)
    // 400, Elapsed time: 6431387957 ns (6.431387957 secs) (avg: 6.431387957)
  }

}