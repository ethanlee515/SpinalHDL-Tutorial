package basic

import spinal.core._
import spinal.core.sim._
import spinal.lib._

object OneClient extends App {
  case class Client() extends Component {
    val o = new Bundle {
      val data = out(Vec.fill(16)(SInt(10 bits)))
      val valid = out(Bool())
    }

    // some logic to generate the output
  }

  case class Accumulator() extends Component {
    val i = new Bundle {
      val data = in(Vec.fill(16)(SInt(10 bits)))
      val valid = in(Bool())
    }

    val r = Reg(SInt(10 bits))
    val v = Reg(Bool())

    // when(i.valid) { // we don't need this
    //   r := i.data.reduce(_ + _)
    // }
    r := i.data.reduce(_ + _)
    v := i.valid

    val o = new Bundle {
      val data = out(SInt(10 bits))
      val valid = out(Bool())
    }

    o.data := r
    o.valid := v
  }

  case class Server() extends Component {
    val i = new Bundle {
      val data = in(SInt(10 bits))
      val valid = in(Bool())
    }
    // logic to print the data
  }

  case class Top() extends Component {
    val c = Client()
    val acc = Accumulator()
    val s = Server()

    acc.i.assignAllByName(c.o)
    s.i.assignAllByName(acc.o)
  }
}

object OneClientFlow extends App {
  case class Client() extends Component {
    val o = out port Flow(Vec.fill(16)(SInt(10 bits)))

    // some logic to generate the output
  }

  case class Accumulator() extends Component {
    val i = in port Flow(Vec.fill(16)(SInt(10 bits)))

    val r = Reg(Flow(SInt(10 bits)))

    // when(i.valid) { // we don't need this
    //   r := i.data.reduce(_ + _)
    // }
    r.payload := i.payload.reduce(_ + _)
    r.valid := i.valid

    val o = out port Flow(SInt(10 bits))
    o := r
  }

  case class Server() extends Component {
    val i = in port Flow(SInt(10 bits))
    // logic to print the data
  }

  case class Top() extends Component {
    val c = Client()
    val acc = Accumulator()
    val s = Server()

    acc.i := c.o
    s.i := acc.o
  }
}

object OneClientStream extends App {
  case class Client() extends Component {
    val o = master port Stream(Vec.fill(16)(SInt(10 bits)))

    // some logic to generate the output
  }

  case class Accumulator() extends Component {
    val i = slave port Stream(Vec.fill(16)(SInt(10 bits)))

    
    // below is a bad hardware implementation, we just use it to demonstrate that modules can be busy
    val iBuffer = Reg(i.payload)
    val counter = UInt(5 bits) init U(16)
    val outputFired = Bool() init True
    val busy = counter =/= U(16)
    val sum = SInt(10 bits)

    when(i.valid & i.ready) {
      iBuffer := i.payload
      counter := U(0)
      sum := S(0)
      outputFired := False
    }
    when(busy) {
      sum := sum + iBuffer(counter.resized)
      counter := counter + U(1)
    }
    
    val fireOutput = (~outputFired) && (~busy)
    when(fireOutput) {
      outputFired := True
    }

    val o = out port Flow(SInt(10 bits))
    o.payload := sum
    o.valid := fireOutput
  }

  case class Server() extends Component {
    val i = in port Flow(SInt(10 bits))
    // logic to print the data
  }

  case class Top() extends Component {
    val c = Client()
    val acc = Accumulator()
    val s = Server()

    acc.i << c.o
    s.i := acc.o
  }
}

object Ex3 extends App {
  case class SimpleFifo(depth: Int) extends Component {
    val dataType = HardType(SInt(10 bits))
    val push = slave port Stream(dataType())
    val pop = master port Stream(dataType())

    // implement a fifo
    // after implementation, see StreamFifo(dataType, depth = 10)
  }

  // implement test
}

object MultiClient extends App {
  case class Client() extends Component {
    val o = master port Stream(Vec.fill(16)(SInt(10 bits)))

    // some logic to generate the output
  }

  case class Accumulator(n: Int) extends Component {
    val inputType = HardType(Vec.fill(16)(SInt(10 bits)))
    val i = Vec.fill(n)(slave port Stream(inputType()))

    val inValids = i.map(_.valid).asBits()
    val sel = OHMasking.firstV2(inValids)
    (i, sel.asBools).zipped.foreach{ case (i, s) => i.ready := s}

    val inBuffer = Reg(inputType())
    inBuffer := OHMux.or(sel, i.map(_.payload))
    val valid = Reg(Bool())
    valid := inValids.orR

    val o = out port Flow(SInt(10 bits))
    o.payload := inBuffer.reduce(_ + _)
    o.valid := valid
  }

  case class Server() extends Component {
    val i = in port Flow(SInt(10 bits))
    // logic to print the data
  }

  case class Top(n: Int) extends Component {
    val c = List.fill(n)(Client())
    val acc = Accumulator(n)
    val s = Server()

    (acc.i, c).zipped.foreach{ case (i, c) => i << c.o}
    s.i := acc.o
  }
}

object MemDemo extends App {
  case class MemDemo() extends Component {
    val mem = Mem.fill(1024)(SInt(16 bits))

    val memPort = slave port mem.readWriteSyncPort()

    // val address = in port UInt(memPort.addressWidth bits)
    // memPort.address := address
    // val rdata = out port cloneOf(memPort.dataType)
    // rdata := memPort.rdata
    // val write = in port Bool()
    // memPort.write := write
    // val wdata = in port cloneOf(memPort.dataType)
    // memPort.wdata := wdata
    // val enable = in port Bool()
    // memPort.enable := enable
  }

  SimConfig.compile{
    val dut = MemDemo()
    dut
  }.doSim{ dut =>
    val cd = dut.clockDomain
    cd.forkStimulus(10)

    dut.memPort.enable #= true
    dut.memPort.write #= false
    dut.memPort.address #= 5

    cd.waitRisingEdge()
    sleep(1)
    println(s"init value at addr 5: ${dut.memPort.rdata.toInt}")

    dut.memPort.write #= true
    dut.memPort.wdata #= 123

    cd.waitRisingEdge()
    sleep(1)
    println(s"value at addr 5 right after write: ${dut.memPort.rdata.toInt}")

    dut.memPort.write #= false
    cd.waitRisingEdge()
    println(s"value at addr 5 after 1 cycle: ${dut.memPort.rdata.toInt}")
    sleep(1)
    println(s"value at addr 5 after 1 cycle + 1 unit time: ${dut.memPort.rdata.toInt}") // rdata is usable in this cycle
  }

}


// used in following exercise
case class Complex(n: Int) extends Bundle {
  val r = SInt(n bits)
  val i = SInt(n bits)
}

// used in following exercise 
case class SimpleComplexMul(width: Int) extends Component {
  val io = new Bundle {
    val c0 = in port Complex(width)
    val c1 = in port Complex(width)
    val c = out port Complex(width)
  }

  val dataType = HardType(SInt(width bits))

  val XR, YR, XI, YI = Reg(dataType())
  val XRYR, XRYI, XIYR, XIYI = Reg(dataType())
  val REAL, IMAG = Reg(dataType())

  XR := io.c0.r
  YR := io.c1.r
  XI := io.c0.i
  YI := io.c1.i

  XRYR := (XR * YR)(width - 1, width bits)
  XRYI := (XR * YI)(width - 1, width bits)
  XIYR := (XI * YR)(width - 1, width bits)
  XIYI := (XI * YI)(width - 1, width bits)

  REAL := XRYR - XIYI
  IMAG := XRYI + XIYR

  io.c.r := REAL
  io.c.i := IMAG
}

object Ex4 extends App {

  case class PulseSpec(
    addrWidth: Int,
    durWidth: Int,
    dataWidth: Int
  )

  case class PulseCmd(spec: PulseSpec) extends Bundle {
    val address = UInt(spec.addrWidth bits)
    val duration = UInt(spec.durWidth bits)
    val phase = Complex(spec.dataWidth)
  }

  case class SimplePulseGenerator(spec: PulseSpec) extends Component {
    val cmd = slave port Stream(PulseCmd(spec))
    val rsp = master port Flow(Complex(spec.dataWidth))

    val mem = Mem.fill(1 << spec.addrWidth)(Complex(spec.dataWidth))
    val memPort = slave port mem.readWriteSyncPort() // this port is used by other modules to write data in the pulse memory

    val address = Reg(UInt(spec.addrWidth bits))
    val pulseData = mem.readSync(address) // create a new port that is only used for read data

    object State extends SpinalEnum {
      val IDLE, LOAD, RUN = newElement()
    }
    import State._

    val state = Reg(State()) init IDLE


    val duration = Reg(UInt(spec.durWidth bits))
    val phase = Reg(Complex(spec.dataWidth))
    val loadTimer = Reg(UInt(4 bits))

    val complexMulDelay = 4

    val complexMul = SimpleComplexMul(spec.dataWidth)
    complexMul.io.c0 := phase
    complexMul.io.c1 := pulseData
    rsp.payload := complexMul.io.c
    rsp.valid := False
    cmd.ready := state === IDLE

    address := address + U(1)
    when(state === IDLE) {
      when(cmd.fire) {
        address := cmd.address
        duration := cmd.duration
        phase := cmd.phase
        state := LOAD
        loadTimer := U(complexMulDelay)
      }
    }

    when(state === LOAD) {
      loadTimer := loadTimer - U(1)
      when(loadTimer === U(1)) {
        state := RUN
      }
    }

    when(state === RUN) {
      rsp.valid := True
      duration := duration - U(1)
      when(duration === U(1)) {
        state := IDLE
      }
    }
  }

  // SimConfig.compile{
  //   val dut = SimpleComplexMul(16)
  //   dut.XR.simPublic
  //   dut
  // }.doSim{ dut =>
  //   val cd = dut.clockDomain
  //   cd.forkStimulus(10)

  //   val maxValue = (1 << 15) - 1
  //   dut.io.c0.r #= (maxValue * 0.707).round
  //   dut.io.c0.i #= (maxValue * 0.707).round
  //   dut.io.c1.r #= (maxValue * 0.707).round
  //   dut.io.c1.i #= (maxValue * 0.707).round
  //   for(i <- 0 until 10) {
  //     cd.waitRisingEdge
  //     println(s"${dut.XR.toInt}")
  //     println(s"${dut.io.c.r.toInt}, ${dut.io.c.i.toInt}")
  //   }
  // }

  val spec = PulseSpec(
    addrWidth = 10,
    durWidth = 10,
    dataWidth = 16
  )
  SimConfig.compile{
    val dut = SimplePulseGenerator(spec)
    dut.pulseData.simPublic()
    dut.address.simPublic()
    dut.state.simPublic()
    dut.phase.simPublic()
    dut
  }.doSim{ dut =>
    val cd = dut.clockDomain
    cd.forkStimulus(10)

    val maxValue = dut.rsp.payload.r.maxValue.toLong

    dut.memPort.enable #= true
    dut.memPort.write #= true
    dut.cmd.valid #= false

    val n = 10
    val addr = 123
    for(i <- 0 until n) {
      import scala.math
      val real = math.cos(i * 2 * math.Pi / n)
      val imag = math.sin(i * 2 * math.Pi / n)
      dut.memPort.wdata.r #= (real * maxValue).round
      dut.memPort.wdata.i #= (imag * maxValue).round
      dut.memPort.address #= addr + i
      cd.waitRisingEdge()
    }
    dut.memPort.write #= false

    dut.cmd.valid #= true
    dut.cmd.address #= addr
    dut.cmd.duration #= n
    dut.cmd.phase.r #= maxValue
    dut.cmd.phase.i #= 0

    cd.waitRisingEdge()
    dut.cmd.valid #= false

    for(i <- 0 until 20) {
      println(s"valid: ${dut.rsp.valid.toBoolean}, r: ${1.0 * dut.rsp.r.toLong / maxValue}, i: ${1.0 * dut.rsp.i.toLong / maxValue}")
      println(s"pd: ${dut.pulseData.r.toLong}")
      // println(s"ph: ${dut.phase.r.toLong}")
      // println(s"addr: ${dut.address.toLong}")
      // println(s"state: ${dut.state.toEnum}")
      cd.waitRisingEdge()
    }
  }
}

object Ex4_2 extends App {
    case class PulseSpec(
      addrWidth: Int,
      durWidth: Int,
      dataWidth: Int,
      startWidth: Int // width of the start time
    )
    case class PulseCmd(spec: PulseSpec) extends Bundle {
      val address = UInt(spec.addrWidth bits)
      val duration = UInt(spec.durWidth bits)
      val phase = Complex(spec.dataWidth)
      val start = UInt(spec.startWidth bits)
    }
    case class SimplePulseGenerator(spec: PulseSpec) extends Component {
      val time = in port UInt(spec.startWidth bits)
      val cmd = slave port Stream(PulseCmd(spec))
      val rsp = master port Flow(Complex(spec.dataWidth))

    // implement a pulse generator that send a pulse when cmd.start === time. 
    // use a fifo to store pending pulses
    }
}