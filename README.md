# `p5_scala`

This is an implementation of a P3-like <www.tom-ridge.com/parsing>
library for parsing, in Scala.

## Features

  - combinator parsing library
  - good performance (not currently- the current version needs tuning)
  - no restrictions on the grammars (supports all BNF grammars)
  
  
## Examples

See the directory `src/main/scala/p5/examples`

Here is an example for the grammar `E -> E E E | "1" | epsilon`, which can be found in `p5.examples`


~~~{.scala}
package p5.examples

import p5._


object EEE_example {
  
  import P5_lib.lib
  import P5_lib.lib._
  import P5_lib.lib.implicits._
  
  val _1: Parser[String,Substring[String]] = lib.terminals.a("1")
  val eps: Parser[String,Substring[String]] = lib.terminals.a("")
  
  val p:Parser[String,Int] = ((p~p~p) ^^ { case x~y~z => x+y+z }) |
  (_1 ^^ ( (_) => 1)) |
  (eps ^^ ( (_) => 0))
  
  def main(args:Array[String]) {
    def do_work = {
      val s = "1"*100
      println("EEE example:"+lib.run_parser_string(s,p))
    }
    Timing.time(10,do_work)
    
  }
  
}
~~~

On my machine, this takes on average about 5s to parse a string of 100
"1"s, and apply the actions (in all possible ways) to compute the
length of the input.
