package scpu

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.misc.pipeline._
import scala.util.Random

object NodePayloadDemo extends App {
  case class NodePayloadDemo() extends Component {
    val i = in port SInt(16 bits)
    val o = out port SInt(16 bits)

    val node0, node1 = Node()
    val payload1 = Payload(SInt(16 bits))

    node0(payload1) := i
    node1(payload1) := node0(payload1)
    o := node1(payload1)
  }

  SpinalVerilog(NodePayloadDemo())
}

object StageLinkDemo extends App {
  case class StageLinkDemo() extends Component {
    val i = in port SInt(16 bits)
    val o = out port SInt(16 bits)

    val node0, node1, node2 = Node()
    val links = List(StageLink(node0, node1), StageLink(node1, node2))
    val payload1 = Payload(SInt(16 bits))

    val node0Logic = new node0.Area {
      payload1 := i // node0(payload1)
    }

    val node2Logic = new node2.Area {
      o := payload1 
    }

    Builder(links)
  }

  SpinalVerilog(StageLinkDemo())
}

object Ex5 extends App {
  case class Complex(n: Int) extends Bundle {
    val r = SInt(n bits)
    val i = SInt(n bits)
  }

  // https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Libraries/Pipeline/introduction.html#simple-cpu-example
  // implement the following algorithm using StageLink
  // common = (xr-xi)*yi
  // multr = (yr-yi)*xr
  // multi = (yr+yi)*xi
  // zr = multr+common
  // zi = multi+common
  case class ComplexMul(n: Int) extends Component {
    val x = in(Complex(n))
    val y = in(Complex(n))
    val z = out(Complex(n))

    // implement here
    val common, multr, multi = Payload(SInt(n bits))
    val node0, node1 = Node()
    val links = List(StageLink(node0, node1))

    new node0.Area {
      common := ((x.r - x.i) * y.i).resized
      multr := ((y.r - y.i) * x.r).resized
      multi := ((y.r + y.i) * x.i).resized
    }

    new node1.Area {
      z.r := multr + common
      z.i := multi + common
    }

    Builder(links)
  }
  /* -- test here -- */
  // flexible parameters
  val nEvals = 4
  // output verilog too
  SpinalVerilog(ComplexMul(8))
  // generate inputs
  case class Cx(re: Int, im: Int)
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


object NodeValidReadyDemo extends App {
  case class NodeValidReadyDemo() extends Component {
    val i = slave Stream (SInt(16 bits))
    val o = master Stream (SInt(16 bits))
    val node0, node1, node2 = Node()
    val links = List(StageLink(node0, node1), StageLink(node1, node2))
    val payload = Payload(SInt(16 bits))
    val node0Logic = new node0.Area {
      valid := i.valid // or node0.valid := flow.valid
      i.ready := ready
      payload := i.payload
    }
    val node2Logic = new node2.Area {
      o.valid := valid
      ready := o.ready
      o.payload := payload
    }
    Builder(links)
  }

  SpinalVerilog(NodeValidReadyDemo())
}

object CtrlLinkBasicDemo extends App {
  case class CtrlLinkBasicDemo() extends Component {
    val i = slave Stream (SInt(16 bits))
    val o = master Stream (SInt(16 bits))

    val node0 = CtrlLink()

    val PAYLOAD = Payload(SInt(16 bits))
    node0.up.valid := i.valid
    node0.up(PAYLOAD) := i.payload
    i.ready := node0.up.ready

    val node0Logic = new node0.Area {
      when(PAYLOAD === S(0, 16 bits)) {
        haltIt() // set down.valid := false; up.ready := false, stall the pipeline
        // throwIt() // set down.valid := false in this cycle and the next cycle
        // teminateIt() // set down.valid := false
      }
    }

    o.valid := node0.down.valid
    o.payload := node0.down(PAYLOAD)
    node0.down.ready := o.ready

    Builder(node0)
  }

  SpinalVerilog(CtrlLinkBasicDemo())
}

object CtrlLinkDemo extends App {
  case class CtrlLinkDemo() extends Component {
    val i = slave Stream (SInt(16 bits))
    val o = master Stream (SInt(16 bits))

    val node0, node1, node2 = CtrlLink()
    val links = List(node0, node1, node2, StageLink(node0.down, node1.up), StageLink(node1.down, node2.up))
    val payload = Payload(SInt(16 bits))

    node0.up.valid := i.valid
    i.ready := node0.up.ready
    node0(payload) := i.payload // node0(payload) is actually node0.down(payload)

    val node1Logic = new node1.Area {
      when(payload =/= S(0, 16 bits)) {
        haltIt()
      }
    }

    o.valid := node2.down.valid
    node2.down.ready := o.ready
    o.payload := node2(payload)

    Builder(links)
    // StageLink set payl
  }

  SpinalVerilog(CtrlLinkDemo())
}

object PipelineCpuDemo extends App {
  class Cpu extends Component {
    val fetch, decode, execute = CtrlLink()
    val f2d = StageLink(fetch.down, decode.up)
    val d2e = StageLink(decode.down, execute.up)

    val PC = Payload(UInt(8 bits))
    val INSTRUCTION = Payload(Bits(16 bits))

    val led = out(Reg(Bits(8 bits))) init (0)

    val fetcher = new fetch.Area {
      val pcReg = Reg(PC) init (0)
      up(PC) := pcReg
      up.valid := True
      when(up.isFiring) { // valid && ready
        pcReg := PC + 1
      }

      val mem = Mem.fill(256)(INSTRUCTION).simPublic
      INSTRUCTION := mem.readAsync(PC)
    }

    val decoder = new decode.Area {
      val opcode = INSTRUCTION(7 downto 0)
      // val IS_ADD = Payload(Bool())
      // IS_ADD := opcode === 0x1
      val IS_ADD = insert(opcode === 0x1)
      val IS_JUMP = insert(opcode === 0x2)
      val IS_LED = insert(opcode === 0x3)
      val IS_DELAY = insert(opcode === 0x4)
    }

    val alu = new execute.Area {
      val regfile = Reg(UInt(8 bits)) init (0)

      val flush = False
      for (stage <- List(fetch, decode)) {
        stage.throwWhen(flush, usingReady = true)
      }

      val delayCounter = Reg(UInt(8 bits)) init (0)

      when(isValid) {
        when(decoder.IS_ADD) {
          regfile := regfile + U(INSTRUCTION(15 downto 8))
        }
        when(decoder.IS_JUMP) {
          flush := True
          fetcher.pcReg := U(INSTRUCTION(15 downto 8))
        }
        when(decoder.IS_LED) {
          led := B(regfile)
        }
        when(decoder.IS_DELAY) {
          delayCounter := delayCounter + 1
          when(delayCounter === U(INSTRUCTION(15 downto 8))) {
            delayCounter := 0
          } otherwise {
            execute.haltIt()
          }
        }
      }
    }

    Builder(fetch, decode, execute, f2d, d2e)
  }

  SpinalVerilog(new Cpu())

}

object Ex6_ModularPipelineCpuDemo extends App {
  case class Stages() extends Area {
    val fetch, decode, execute = CtrlLink()
  }

  case class Payloads() extends Area {
    val PC = Payload(UInt(8 bits))
    val INSTRUCTION = Payload(Bits(16 bits))
  }

  case class Fetcher(stages: Stages, payloads: Payloads) extends Area {
    import stages._
    import payloads._
    val logic = new fetch.Area{
      val pcReg = Reg(PC) init (0)
      up(PC) := pcReg
      up.valid := True
      when(up.isFiring) {
        pcReg := PC + 1
      }

      val mem = Mem.fill(256)(INSTRUCTION).simPublic
      INSTRUCTION := mem.readAsync(PC)
    }
  }

  case class Decoder(stages: Stages, payloads: Payloads) extends Area {
    import stages._
    import payloads._
    val logic = new decode.Area {
      val opcode = INSTRUCTION(7 downto 0)
      val IS_ADD = insert(opcode === 0x1)
      val IS_JUMP = insert(opcode === 0x2)
      val IS_LED = insert(opcode === 0x3)
      val IS_DELAY = insert(opcode === 0x4)
    }
  }

  case class Adder(stages: Stages, payloads: Payloads, decoder: Decoder, regfile: UInt) extends Area {
    import stages._
    import payloads._
    import decoder.logic._
    val logic = new execute.Area {
        when(IS_ADD) {
          regfile := regfile + U(INSTRUCTION(15 downto 8))
        }
    }
  }

  class Cpu extends Component {
    val stages = Stages()
    val payloads = Payloads()

    import stages._
    val f2d = StageLink(fetch.down, decode.up)
    val d2e = StageLink(decode.down, execute.up)


    val led = out(Reg(Bits(8 bits))) init (0)
    val fetcher = Fetcher(stages, payloads)
    val decoder = Decoder(stages, payloads)
    val regfile = Reg(UInt(8 bits)) init (0)
    val adder = Adder(stages, payloads, decoder, regfile)
    
    // implement other logic in this way
    // write some tests

    Builder(fetch, decode, execute, f2d, d2e)
  }

  SpinalVerilog(new Cpu())

}
