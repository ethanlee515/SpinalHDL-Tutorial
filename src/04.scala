package plugin

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import spinal.core.fiber._

import scala.collection.mutable.{ArrayBuffer, LinkedHashMap}

object AreaDemo extends App {
  case class Multiplier() extends Component {
    val x = in SInt (16 bits)
    val y = in SInt (16 bits)
    val z = out SInt (32 bits)
    z := x * y
  }

  case class AreaDemo() extends Component {
    val i = in SInt (16 bits)
    val o = out SInt (16 bits)

    val multByFive = new Area {
      val multiplier = Multiplier()
      multiplier.x := i
      multiplier.y := S(5, 16 bits)
      val res = Reg(SInt(16 bits))
      res := multiplier.z.resized
    }

    o := multByFive.res
  }

  SpinalVerilog(AreaDemo())
}

object SimplePlugin extends App {
  case class InPlugin() extends FiberPlugin {
    val logic = during setup new Area {
      val width = 16
      awaitBuild()
      val i = in(UInt(16 bits))
    }
  }
  case class OutPlugin() extends FiberPlugin {
    val logic = during setup new Area {
      awaitBuild()
      val inPlugin = host[InPlugin]
      val width = inPlugin.logic.width
      val o = out(UInt(width bits))

      o := inPlugin.logic.i
    }
  }
  case class SimplePlugin(plugins: Seq[FiberPlugin]) extends Component {
    val host = new PluginHost()
    host.asHostOf(plugins)
  }

  def getPlugins() = List(InPlugin(), OutPlugin())
  SpinalVerilog {
    SimplePlugin(getPlugins())
  }

  // equivalent to
  case class SimplePlugin1() extends Component {
    val inPlugin = new Area {
      val logic = new Area {
        val i = in(UInt(16 bits))
      }
    }
    val outPlugin = new Area {
      val logic = new Area {
        val o = out(UInt(16 bits))
        o := inPlugin.logic.i
      }
    }
  }
  SpinalVerilog(SimplePlugin1())

}

object RetainerDemo extends App {
  case class StatePlugin() extends FiberPlugin {
    val logic = during setup new Area {
      awaitBuild()
      val signal = Reg(UInt(32 bits))
    }
  }

  case class DriverPlugin() extends FiberPlugin {
    // incrementBy will be set by others plugin at elaboration time
    var incrementBy = 0
    // retainer allows other plugins to create locks, on which this plugin will wait before using incrementBy
    val retainer = Retainer()

    val logic = during setup new Area {
      awaitBuild()
      val sp = host[StatePlugin].logic.get
      retainer.await()

      // Generate the incrementer hardware
      sp.signal := sp.signal + U(incrementBy)
    }
  }

  // Let's define a plugin which will modify the DriverPlugin.incrementBy variable because letting it elaborate its hardware
  case class SetupPlugin() extends FiberPlugin {
    // during setup { body } will spawn the body of code in the Fiber setup phase (it is before the Fiber build phase)
    val logic = during setup new Area {
      // *** Setup phase code ***
      val dp = host[DriverPlugin]

      // Prevent the DriverPlugin from executing its build's body (until release() is called)
      val lock = dp.retainer()
      // Wait until the fiber phase reached build phase
      awaitBuild()

      // *** Build phase code ***
      // Let's mutate DriverPlugin.incrementBy
      dp.incrementBy += 1

      // Allows the DriverPlugin to execute its build's body
      lock.release()
    }
  }

  case class RetainerTop(plugins: Seq[FiberPlugin]) extends Component {
    val host = new PluginHost()
    host.asHostOf(plugins)
  }

  def getPlugins() = List(StatePlugin(), DriverPlugin(), SetupPlugin())
  SpinalVerilog { RetainerTop(getPlugins()) }
}

object CpuDemo extends App {
  val INSTRUCTION = Payload(Bits(32 bits))
  val PC = Payload(UInt(32 bits))

  case class PipelinePlugin() extends FiberPlugin {
    val fetch, decode, execute, write = CtrlLink()
    val lock = Retainer()
    val logic = during setup new Area {
      awaitBuild()
      val flush = False

      lock.await()
      for (stage <- List(fetch, decode)) {
        stage.throwWhen(flush, usingReady = true)
      }
      val links = List(fetch, decode, execute, StageLink(fetch.down, decode.up), StageLink(decode.down, execute.up))
      Builder(links)
    }
  }

  case class FetchPlugin() extends FiberPlugin {
    val logic = during setup new Area {
      val pp = host[PipelinePlugin]
      val buildBefore = retains(pp.lock)

      awaitBuild()
      val fetchLogic = new pp.fetch.Area {
        val pcReg = Reg(PC) init (0)
        up(PC) := pcReg
        up.valid := True
        when(up.isFiring) {
          pcReg := PC + U(4)
        }

        val mem = Mem.fill(256)(INSTRUCTION)
        INSTRUCTION := mem.readAsync((PC >> 2).resized)
      }
      buildBefore.release()
    }
  }

  case class Opcode(code: MaskedLiteral) {}

  case class DecoderPlugin() extends FiberPlugin {
    val defaultList = ArrayBuffer[(Payload[_ <: Data], Data)]()
    def addDefault(payload: Payload[_ <: Data], value: Data) = {
      val pair = (payload, value)
      defaultList += pair
    }

    val decodingList = LinkedHashMap[Opcode, ArrayBuffer[(Payload[_ <: Data], Data)]]()
    def addDecoding(op: Opcode, payload: Payload[_ <: Data], value: Data) = {
      val opList = decodingList.getOrElseUpdate(op, ArrayBuffer())
      val pair = (payload, value)
      opList += pair
    }

    val lock = Retainer()
    val logic = during setup new Area {
      val pp = host[PipelinePlugin]
      val buildBefore = retains(pp.lock)

      awaitBuild()

      lock.await()
      val decLogic = new pp.decode.Area {
        for ((payload, value) <- defaultList) {
          payload.assignFrom(value) // payload := valid
        }
        val defaultPayloads = (for ((payload, _) <- defaultList) yield payload).toList
        val decodePayloads = (for {
          (op, opList) <- decodingList
          (payload, _) <- opList
        } yield payload).toList
        val payloads = (defaultPayloads ++ decodePayloads).distinct
        for (payload <- payloads) {
          if (!defaultPayloads.contains(payload)) {
            // payload.assignDontCare()
          }
        }

        for ((op, list) <- decodingList) {
          for ((payload, value) <- list) {
            when(INSTRUCTION === op.code) {
              payload.assignFrom(value)
            }
          }
        }
      }
      buildBefore.release()
    }
  }
  case class RegfilePlugin() extends FiberPlugin {
    val logic = during setup new Area {
      val regfile = Mem.fill(32)(Bits(32 bits))
      val readPort1 = regfile.readAsyncPort()
      val readPort2 = regfile.readAsyncPort()
      val writePort = regfile.writePort()
    }
  }

  case class WriteBackPlugin() extends FiberPlugin {
    val SEL = Payload(Bool())
    val WRITE_DATA = Payload(Bits(32 bits))
    val logic = during setup new Area {
      val pp = host[PipelinePlugin]
      val dp = host[DecoderPlugin]
      val rp = host[RegfilePlugin]
      val buildBefore = retains(pp.lock, dp.lock)
      awaitBuild()
      dp.addDefault(SEL, False)
      val writeLogic = new pp.write.Area {
        rp.logic.writePort.valid := SEL
        rp.logic.writePort.address := INSTRUCTION(11 downto 7).asUInt
        rp.logic.writePort.data := WRITE_DATA
      }
      buildBefore.release()
    }
  }

  case class AluPlugin() extends FiberPlugin {
    val logic = during setup new Area {
      val addOp = Opcode(M"0000000----------000-----0110011")
      val addiOp = Opcode(M"-----------------000-----0010011")
      val SEL = Payload(Bool())
      val SRC2_CTRL = Payload(Bool())

      val pp = host[PipelinePlugin]
      val rp = host[RegfilePlugin]
      val dp = host[DecoderPlugin]
      val wp = host[WriteBackPlugin]
      val buildBefore = retains(dp.lock)

      awaitBuild()

      dp.addDefault(SEL, False)
      dp.addDecoding(addOp, SEL, True)
      dp.addDecoding(addOp, SRC2_CTRL, False)
      dp.addDecoding(addOp, wp.SEL, True)
      dp.addDecoding(addiOp, SEL, True)
      dp.addDecoding(addiOp, SRC2_CTRL, True)
      dp.addDecoding(addiOp, wp.SEL, True)

      val aluLogic = new pp.execute.Area {
        val rs1Addr = INSTRUCTION(19 downto 15).asUInt
        val rs2Addr = INSTRUCTION(24 downto 20).asUInt
        rp.logic.readPort1.address := rs1Addr
        rp.logic.readPort2.address := rs2Addr
        val rs1 = rp.logic.readPort1.data.asSInt
        val src1 = rs1
        val rs2 = rp.logic.readPort2.data.asSInt
        val imm = INSTRUCTION(31 downto 20).asSInt.resized
        val src2 = SRC2_CTRL.mux(imm, rs2)
        wp.WRITE_DATA := (src1 + src2).asBits
        when(SEL) {
          val writeRd = pp.write(INSTRUCTION)(11 downto 7).asUInt
          when(pp.write.isValid & pp.write(wp.SEL) & (rs1Addr === writeRd | rs2Addr === writeRd)) {
            haltIt() // data hazard
          }
        }
      }

      buildBefore.release()
    }
  }

  case class Cpu(plugins: Seq[FiberPlugin]) extends Component {
    val host = new PluginHost()
    host.asHostOf(plugins)
  }

  def getPlugins() = {
    val plugins = ArrayBuffer[FiberPlugin]()
    plugins += PipelinePlugin()
    plugins += FetchPlugin()
    plugins += DecoderPlugin()
    plugins += RegfilePlugin()
    plugins += WriteBackPlugin()
    plugins += AluPlugin()
  }

  SpinalVerilog(Cpu(getPlugins()))
}

object Ex7 extends App {

}
