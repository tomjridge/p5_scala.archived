package p5.examples

/**
 * @author tr61
 */
object Timing {
  def time[R](num_loops:Int, block: => R): Unit = {
      val t0 = System.nanoTime()
      for(i <- 1 to num_loops) {
        val result = block    // call-by-name 
      }
      val t1 = System.nanoTime()
      val diff = t1 - t0
      val in_secs = diff / 1000000000.0
      val avg = in_secs / num_loops
      println(s"Elapsed time: $diff ns ($in_secs secs) (avg: $avg)")
    }
}