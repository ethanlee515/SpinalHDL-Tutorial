package basic

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.util.Random

object DeclareWire extends App {
  case class DeclareWire() extends Component {
    val w = UInt(10 bits)
    w.assignFromBits(B("0101010101"))
  }

  SpinalVerilog(DeclareWire())
}

object WireBit extends App {
  case class WireBit() extends Component {
    val i = in(Bool())
    val o = out(Bool())
    o := i
  }

  SpinalVerilog(WireBit())

  SimConfig
    .compile {
      val dut = WireBit()
      dut
    }
    .doSim { dut =>
      dut.i #= true
      sleep(1) // #= effects after 1 time unit
      println(s"${dut.o.toBoolean}")

      dut.i #= false
      println(s"${dut.o.toBoolean}")
      sleep(1)
      println(s"${dut.o.toBoolean}")
    }
}

object WireUInt extends App {
  case class WireUInt() extends Component {
    val i = in(UInt(10 bits))
    val o = out(UInt(10 bits))
    o := i
  }

  SpinalVerilog(WireUInt())

  SimConfig
    .compile {
      val dut = WireUInt()
      dut
    }
    .doSim { dut =>
      println(s"${dut.o.toBigInt}")
      dut.i #= 512
      sleep(1)
      println(s"${dut.o.toBigInt}")
    }
}

object InternalWireInSim extends App {
  case class InternalWireInSim() extends Component {
    val i = in(UInt(10 bits))
    val m = UInt(10 bits)
    val o = out(UInt(10 bits))
  }

  SpinalVerilog(InternalWireInSim())

  SimConfig
    .compile {
      val dut = InternalWireInSim()
      dut.m.simPublic() // use simPublic to access internal wires in simulation
      dut
    }
    .doSim { dut =>
      println(s"${dut.m.toBigInt}")
      dut.i #= 512
      sleep(1)
      println(s"${dut.m.toBigInt}")
    }
}

object SumOfSInt extends App {
  case class SumOfSInt() extends Component {
    val i = in(Vec.fill(16)(SInt(10 bits)))
    val o = out(SInt(10 bits))
    o := i.reduce(_ + _)
  }

  SpinalVerilog(SumOfSInt())

  SimConfig
    .compile {
      val dut = SumOfSInt()
      dut
    }
    .doSim { dut =>
      for (j <- 0 until 16) {
        dut.i(j) #= j
      }
      sleep(1)
      println(s"${dut.o.toBigInt}")
    }
}

object SimpleRegister extends App {
  case class SimpleRegister() extends Component {
    val i = in(Bool())
    val r = Reg(i)
    r := ~i
    val o = out(Bool())
    o := r
  }

  SimConfig
    .compile {
      val dut = SimpleRegister()
      dut
    }
    .doSim { dut =>
      // when we use registers, we must specify how long a clock cycle is
      val cd = dut.clockDomain
      cd.forkStimulus(10) // a clock cycle is 10 unit time

      dut.i #= true
      cd.waitRisingEdge()
      println(s"time: ${simTime}, o: ${dut.o.toBoolean}")

      dut.i #= false
      cd.waitRisingEdge()
      println(s"time: ${simTime}, o: ${dut.o.toBoolean}")
    }
}

object SimplePipeline extends App {
  case class SimplePipeline() extends Component {
    val x = in(SInt(10 bits))
    val y = in(SInt(10 bits))
    val z = in(SInt(10 bits))

    val o = out(SInt(10 bits))

    val xPlusY = Reg(SInt(10 bits))
    xPlusY := x + y

    val zBuffer = Reg(SInt(10 bits))
    zBuffer := z

    o := xPlusY + zBuffer
  }

  SimConfig
    .compile {
      val dut = SimplePipeline()
      dut.xPlusY.simPublic
      dut.zBuffer.simPublic
      dut
    }
    .doSim { dut =>
      // when we use registers, we must specify how long a clock cycle is
      val cd = dut.clockDomain
      cd.forkStimulus(10) // a clock cycle is 10 unit time

      cd.waitRisingEdge()
      println(s"time: ${simTime}, x: ${dut.x.toBigInt}, y: ${dut.y.toBigInt}, z: ${dut.z.toBigInt}, xPlusY: ${dut.xPlusY.toBigInt}, zBuffer: ${dut.zBuffer.toBigInt}, o: ${dut.o.toBigInt}")

      dut.x #= 3
      dut.y #= 2
      dut.z #= 1
      cd.waitRisingEdge()
      println(s"time: ${simTime}, x: ${dut.x.toBigInt}, y: ${dut.y.toBigInt}, z: ${dut.z.toBigInt}, xPlusY: ${dut.xPlusY.toBigInt}, zBuffer: ${dut.zBuffer.toBigInt}, o: ${dut.o.toBigInt}")

      cd.waitRisingEdge()
      println(s"time: ${simTime}, x: ${dut.x.toBigInt}, y: ${dut.y.toBigInt}, z: ${dut.z.toBigInt}, xPlusY: ${dut.xPlusY.toBigInt}, zBuffer: ${dut.zBuffer.toBigInt}, o: ${dut.o.toBigInt}")

      cd.waitRisingEdge()
      println(s"time: ${simTime}, x: ${dut.x.toBigInt}, y: ${dut.y.toBigInt}, z: ${dut.z.toBigInt}, xPlusY: ${dut.xPlusY.toBigInt}, zBuffer: ${dut.zBuffer.toBigInt}, o: ${dut.o.toBigInt}")
    }
}

object SumOfSIntPipeline extends App {
  case class SumOfSIntPipeline() extends Component {
    val dataType = HardType(SInt(10 bits))
    val i = in(Vec.fill(16)(dataType()))
    val o = out(dataType())

    val buffers = for (j <- 16 downto 1) yield Reg(Vec.fill(j)(dataType()))
    for (j <- 0 until 16) {
      buffers(0)(j) := i(j)
    }
    for (j <- 0 until 15) {
      buffers(j + 1)(0) := buffers(j)(0) + buffers(j)(1)
      for (k <- 1 until (15 - j)) {
        buffers(j + 1)(k) := buffers(j)(k + 1)
      }
    }

    o := buffers(15)(0)
  }

  SpinalVerilog(SumOfSIntPipeline())

  SimConfig
    .compile {
      val dut = SumOfSIntPipeline()
      dut.buffers.foreach(_.simPublic())
      dut
    }
    .doSim { dut =>
      val cd = dut.clockDomain
      cd.forkStimulus(10) // a clock cycle is 10 unit time

      cd.waitRisingEdge()

      for (j <- 0 until 16) {
        dut.i(j) #= j
      }

      for(t <- 0 until 20) {
        print("input: ")
        for (j <- 0 until 16) {
            dut.i(j) #= j + t
            print(s"${j + t}, ")
        }
        println("\nbuffer: ")
        for(i <- 0 until 16) {
            for(j <- 0 until (16 - i)) {
                print(s"${dut.buffers(i)(j).toBigInt}, ")
            }
            println("")
        }
        println("")
        cd.waitRisingEdge()
      }
    }
}

// exercise 1
object Ex1 extends App {
  case class SumOfSIntPipeline(w: Int, n: Int) extends Component {
    val dataType = HardType(SInt(w bits))
    val i = in(Vec.fill(n)(dataType()))
    val o = out(dataType())
    var delays = 1

    // implement here
    
    val sz = (n + 1) / 2
    var terms = Reg(Vec.fill(sz)(dataType()))
    for(j <- 0 until sz) {
      if(2 * j + 1 != i.size) {
        terms(j) := i(2 * j) + i(2 * j + 1)
      } else {
        terms(j) := i(2 * j)
      }
    }

    while(terms.size != 1) {
      val new_sz = (terms.size + 1) / 2
      var next_terms = Reg(Vec.fill(new_sz)(dataType()))
      for(j <- 0 until new_sz) {
        if(2 * j + 1 != terms.size) {
          next_terms(j) := terms(2 * j) + terms(2 * j + 1)
        } else {
          next_terms(j) := terms(2 * j)
        }
      }
      terms = next_terms
      delays += 1
    }
    o := terms(0)
  }

  SpinalVerilog(SumOfSIntPipeline(10, 16))

  // write your test here

  // generate inputs
  val nEvals = 8 // flexible parameter
  val nTerms = 6 // flexible parameter
  val w = 8
  val lst = List.fill(nEvals)(List.fill(nTerms)(Random.nextInt(11) - 5))
  // do computation
  var reference : List[Int] = List()
  var outcomes : List[Int] = List()
  SimConfig.compile { SumOfSIntPipeline(w, nTerms) }.doSim { dut =>
    // clock boilerplate
    val cd = dut.clockDomain
    cd.forkStimulus(10)
    cd.waitSampling()
    cd.assertReset()
    cd.waitRisingEdge()
    cd.deassertReset()
    cd.waitSampling()
    sleep(10)
    // run the actual simulation
    for(i <- 0 until nEvals) {
      var s = 0
      for(j <- 0 until nTerms) {
        s += lst(i)(j)
        dut.i(j) #= lst(i)(j)
      }
      outcomes = outcomes :+ dut.o.toInt
      reference = reference :+ s
      sleep(10)
    }

    for(_ <- 0 until dut.delays) {
      outcomes = outcomes :+ dut.o.toInt
      sleep(10)
    }

    outcomes = outcomes.drop(dut.delays)
  }
  // check results
  for(i <- 0 until nEvals) {
    print(lst(i)(0))
    for(j <- 1 until nTerms) {
      print(s" + ${lst(i)(j)}")
    }
    println(s" = ${outcomes(i)}")
    if(outcomes(i) != reference(i)) {
      println("wrong result!")
    }
  }
}

object MulDemo extends App {
  case class MulDemo() extends Component {
    val x = in(SInt(10 bits))
    val y = in(SInt(10 bits))
    val z = out(SInt(10 bits))

    val m = x * y // the width of m is 20
    println(s"m width: ${m.getBitsWidth}")
    // resized: https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Data%20types/Int.html#misc
    //z := m(0, 10 bits)
    //z := m(9 downto 0)
    z := m.resized
  }

  SpinalVerilog(MulDemo())
}

object Ex2 extends App {
  case class Complex(n: Int) extends Bundle {
    val r = SInt(n bits)
    val i = SInt(n bits)
  }

  // compute z = x y = (x.r * y.r - x.i * y.i, x.r * y.i + x.i * y.r) using pipeline
  // on each stage, we can only compute multiplication or addition with depth 1
  case class ComplexMul(n: Int) extends Component {
    val x = in(Complex(n))
    val y = in(Complex(n))
    val z = out(Complex(n))

    // implement here
    val xryr, xiyi, xryi, xiyr = Reg(SInt(n bits))
    xryr := (x.r * y.r).resized
    xiyi := (x.i * y.i).resized
    xryi := (x.r * y.i).resized
    xiyr := (x.i * y.r).resized
    z.r := xryr - xiyi
    z.i := xryi + xiyr
  }

  // write your test here

  // generate inputs
  case class Cx(re: Int, im: Int)
  val nEvals = 8 // flexible parameter
  val lst = List.fill(nEvals)((Cx(Random.nextInt(11) - 6, Random.nextInt(11) - 6), Cx(Random.nextInt(11) - 6, Random.nextInt(11) - 6)))
  // run computation
  var reference : List[Cx] = List()
  var outcomes : List[Cx] = List()
  SimConfig.compile { ComplexMul(8) }.doSim { dut =>
    // clock boilerplate
    val cd = dut.clockDomain
    cd.forkStimulus(10)
    cd.waitSampling()
    cd.assertReset()
    cd.waitRisingEdge()
    cd.deassertReset()
    cd.waitSampling()
    sleep(10)
    // run the actual simulation
    for(i <- 0 until nEvals) {
      val z1 = lst(i)._1
      val z2 = lst(i)._2
      val re = z1.re * z2.re - z1.im * z2.im
      val im = z1.re * z2.im + z1.im * z2.re
      reference = reference :+ Cx(re, im)

      dut.x.r #= z1.re
      dut.x.i #= z1.im
      dut.y.r #= z2.re
      dut.y.i #= z2.im
      outcomes = outcomes :+ Cx(dut.z.r.toInt, dut.z.i.toInt)
      sleep(10)
    }

    outcomes = outcomes.drop(1) :+ Cx(dut.z.r.toInt, dut.z.i.toInt)
  }
  // check output
  for(i <- 0 until nEvals) {
    val z1 = lst(i)._1
    val z2 = lst(i)._2
    val z1z2 = outcomes(i)
    println(s"(${z1.re} + ${z1.im}i)(${z2.re} + ${z2.im}i) = ${z1z2.re} + ${z1z2.im}i")
    if(z1z2 != reference(i)) {
      println("wrong result!")
    }
  }
}
