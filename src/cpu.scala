package riscv

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import spinal.core.fiber._

import scala.collection.mutable.{ArrayBuffer, LinkedHashMap}
import scala.io

class Cpu extends Component {
  val INSTRUCTION = Payload(Bits(32 bits))
  val PC = Payload(UInt(32 bits))

  case class PipelinePlugin() extends FiberPlugin {
    val fetch, decode, execute, write = CtrlLink()
    // TODO maybe `flush` should be in an area?
    // Gives errors about circular stuff though.
    val flush = False
    val lock = Retainer()
    val logic = during setup new Area {
      awaitBuild()
      lock.await()
      for (stage <- List(fetch, decode)) {
        stage.throwWhen(flush, usingReady = true)
      }
      val links = List(fetch, decode, execute, write,
        StageLink(fetch.down, decode.up), StageLink(decode.down, execute.up), StageLink(execute.down, write.up))
      Builder(links)
    }
  }

  case class MemPlugin() extends FiberPlugin {
    val logic = during setup new Area {
      val mem = Mem.fill(256)(INSTRUCTION)
      val iBusPort = mem.readAsyncPort() // port for fetching instructions
    }
  }

  case class FetchPlugin() extends FiberPlugin {
    val logic = during setup new Area {
      val pp = host[PipelinePlugin]
      val mp = host[MemPlugin]
      val buildBefore = retains(pp.lock)
      awaitBuild()
      val fetchLogic = new pp.fetch.Area {
        val pcReg = Reg(PC) init (0)
        up(PC) := pcReg
        up.valid := True
        when(up.isFiring) {
          pcReg := PC + U(4)
       }
        val memPort = mp.logic.iBusPort
        val addr = (PC >> 2)
        memPort.address := addr.resized
        INSTRUCTION := memPort.data
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
          payload.assignFrom(value)
        }
        val defaultPayloads = (for ((payload, _) <- defaultList) yield payload).toList
        val decodePayloads = (for {
          (op, opList) <- decodingList
          (payload, _) <- opList
        } yield payload).toList
        val payloads = (defaultPayloads ++ decodePayloads).distinct
        for (payload <- payloads) {
          if (!defaultPayloads.contains(payload)) {
            payload.assignDontCare()
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
    val ALU_WRITE = Payload(Bool())
    val ALU_WRITE_DATA = Payload(Bits(32 bits))
    val LW_WRITE = Payload(Bool())
    val LW_WRITE_DATA = Payload(Bits(32 bits))
    val logic = during setup new Area {
      val pp = host[PipelinePlugin]
      val dp = host[DecoderPlugin]
      val rp = host[RegfilePlugin]
      val buildBefore = retains(pp.lock, dp.lock)
      awaitBuild()
      dp.addDefault(SEL, False)
      val writeLogic = new pp.write.Area {
        rp.logic.writePort.valid := SEL & isValid
        rp.logic.writePort.address := INSTRUCTION(11 downto 7).asUInt
        when(ALU_WRITE) {
          rp.logic.writePort.data := ALU_WRITE_DATA
        } otherwise {
          rp.logic.writePort.data := LW_WRITE_DATA
        }
      }
      buildBefore.release()
    }
  }

  // Maybe need this?
  // Otherwise AluPlugin and BeqHandler both reads...
  case class RegistersReader() extends FiberPlugin {
    val logic = during build new Area {
      val pp = host[PipelinePlugin]
      val rp = host[RegfilePlugin]
      val reader = new pp.execute.Area {
        val rs1Addr = INSTRUCTION(19 downto 15).asUInt
        val rs2Addr = INSTRUCTION(24 downto 20).asUInt
        rp.logic.readPort1.address := rs1Addr
        rp.logic.readPort2.address := rs2Addr
        val rs1 = rp.logic.readPort1.data.asSInt
        val rs2 = rp.logic.readPort2.data.asSInt
      }
    }
  }

  case class Ram() extends FiberPlugin {
    val logic = during setup new Area {
      val mem = Mem.fill(32)(Bits(32 bits))
      val readPort = mem.readAsyncPort()
      val writePort = mem.writePort()
    }
  }

  case class SwHandler() extends FiberPlugin {
    val logic = during setup new Area {
      val swOp = Opcode(M"-----------------010-----0100011")
      val SEL = Payload(Bool())
      val dp = host[DecoderPlugin]
      val pp = host[PipelinePlugin]
      val regs = host[RegistersReader]
      val wp = host[WriteBackPlugin]
      val ram = host[Ram]
      val buildBefore = retains(dp.lock)
      val address = UInt(5 bits)
      awaitBuild()
      dp.addDefault(SEL, False)
      dp.addDecoding(swOp, SEL, True)
      new pp.execute.Area {
        val rs1 = regs.logic.reader.rs1
        val rs2 = regs.logic.reader.rs2
        val o11_5 = INSTRUCTION(31 downto 25).asUInt
        val o4_0 = INSTRUCTION(11 downto 7).asUInt
        val offset = (o4_0 + (o11_5 << 5)).asSInt
        address := (rs1 + offset).asUInt.resized
        ram.logic.writePort.address := address
        ram.logic.writePort.data := rs2.asBits
        when(pp.write.isValid & pp.write(wp.SEL)) {
          ram.logic.writePort.valid := False
          haltIt() // data hazard
        } otherwise {
          ram.logic.writePort.valid := SEL & isValid
        }
      }
      buildBefore.release()
    }
  }

  case class LwHandler() extends FiberPlugin {
    val logic = during setup new Area {
      val lwOp = Opcode(M"-----------------010-----0000011")
      val SEL = Payload(Bool())
      val dp = host[DecoderPlugin]
      val pp = host[PipelinePlugin]
      val wp = host[WriteBackPlugin]
      val regs = host[RegistersReader]
      val ram = host[Ram]
      val buildBefore = retains(dp.lock)
      awaitBuild()
      dp.addDefault(SEL, False)
      dp.addDecoding(lwOp, SEL, True)
      dp.addDecoding(lwOp, wp.SEL, True)
      new pp.execute.Area {
        // read from ram
        val rs1 = regs.logic.reader.rs1
        val offset = INSTRUCTION(31 downto 20).asSInt
        ram.logic.readPort.address := (rs1 + offset).asUInt.resized
        val value = ram.logic.readPort.data.asSInt
        // write onto register
        wp.LW_WRITE := SEL
        wp.LW_WRITE_DATA := value.asBits
      }
      buildBefore.release()
    }
  }

  case class AluPlugin() extends FiberPlugin {
    val logic = during setup new Area {
      val addOp = Opcode(M"0000000----------000-----0110011")
      val subOp = Opcode(M"0100000----------000-----0110011")
      val addiOp = Opcode(M"-----------------000-----0010011")
      val SEL = Payload(Bool())
      val SRC2_CTRL = Payload(Bool())
      val IS_SUB = Payload(Bool())

      val pp = host[PipelinePlugin]
      val rp = host[RegfilePlugin]
      val dp = host[DecoderPlugin]
      val wp = host[WriteBackPlugin]
      val regs = host[RegistersReader]
      val buildBefore = retains(dp.lock)

      awaitBuild()

      dp.addDefault(SEL, False)
      dp.addDecoding(addOp, SEL, True)
      dp.addDecoding(addOp, SRC2_CTRL, False)
      dp.addDecoding(addOp, IS_SUB, False)
      dp.addDecoding(addOp, wp.SEL, True)

      dp.addDecoding(addiOp, SEL, True)
      dp.addDecoding(addiOp, SRC2_CTRL, True)
      dp.addDecoding(addiOp, IS_SUB, False)
      dp.addDecoding(addiOp, wp.SEL, True)

      dp.addDecoding(subOp, SEL, True)
      dp.addDecoding(subOp, SRC2_CTRL, False)
      dp.addDecoding(subOp, IS_SUB, True)
      dp.addDecoding(subOp, wp.SEL, True)

      val aluLogic = new pp.execute.Area {
        val rs1 = regs.logic.reader.rs1
        val rs2 = regs.logic.reader.rs2
        val src1 = rs1
        val imm = INSTRUCTION(31 downto 20).asSInt.resized
        val src2 = SRC2_CTRL.mux(imm, rs2)
        wp.ALU_WRITE := SEL
        wp.ALU_WRITE_DATA := IS_SUB ? (src1 - src2).asBits | (src1 + src2).asBits
        when(SEL & isValid) {
          val writeRd = pp.write(INSTRUCTION)(11 downto 7).asUInt
          val rs1Addr = INSTRUCTION(19 downto 15).asUInt
          val rs2Addr = INSTRUCTION(24 downto 20).asUInt
          when(pp.write.isValid & pp.write(wp.SEL) & (rs1Addr === writeRd | rs2Addr === writeRd)) {
            haltIt() // data hazard
          }
        }
      }

      buildBefore.release()
    }
  }

  case class BeqHandler() extends FiberPlugin {
    val logic = during setup new Area {
      val opcode = Opcode(M"-----------------000-----1100011")
      val SEL = Payload(Bool())
      val dp = host[DecoderPlugin]
      val pp = host[PipelinePlugin]
      val fp = host[FetchPlugin]
      val rp = host[RegfilePlugin]
      val wp = host[WriteBackPlugin]
      val regs = host[RegistersReader]
      val buildBefore = retains(dp.lock)
      awaitBuild()
      dp.addDefault(SEL, False)
      dp.addDecoding(opcode, SEL, True)
      val beqLogic = new pp.execute.Area {
        val rs1 = regs.logic.reader.rs1
        val rs2 = regs.logic.reader.rs2
        val regs_eq = (rs1 === rs2)
        val o12 = INSTRUCTION(31).asUInt
        val o11 = INSTRUCTION(7).asUInt
        val o10_5 = INSTRUCTION(30 downto 25).asUInt
        val o4_1 = INSTRUCTION(11 downto 8).asUInt
        val offset = ((o4_1 << 1) + (o10_5 << 5) + (o11 << 11) + (o12 << 12)).asSInt
        when(SEL & regs_eq & isValid) {
          pp.flush := True
          fp.logic.fetchLogic.pcReg := (PC.asSInt + offset).asUInt
        }
      }
      buildBefore.release()
    }
  }

  case class OutputPlugin() extends FiberPlugin {
    val logic = during setup new Area {
      val done = out(Bool())
      done := False
      val ecallOp = Opcode(M"00000000000000000000000001110011")
      val SEL = Payload(Bool())
      val dp = host[DecoderPlugin]
      val pp = host[PipelinePlugin]
      val wp = host[WriteBackPlugin]
      val buildBefore = retains(dp.lock)
      awaitBuild()
      dp.addDefault(SEL, False)
      dp.addDecoding(ecallOp, SEL, True)
      val outputLogic = new pp.execute.Area {
        when(SEL & isValid) {
          when(pp.write.isValid & pp.write(wp.SEL)) {
            haltIt()
          } otherwise {
            done := True
          }
        }
      }
      buildBefore.release()
    }
  }

  case class WhiteboxerPlugin() extends FiberPlugin {
    val logic = during build new Area {
      val mp = host[MemPlugin]
      val mem = mp.logic.mem
      mem.simPublic()
      val op = host[OutputPlugin]
      val done = op.logic.done
      done.simPublic()
      val rp = host[RegfilePlugin]
      val regfile = rp.logic.regfile
      regfile.simPublic()

      val pp = host[PipelinePlugin]
      val fetchPc = UInt(32 bits)
      val fetchValid = Bool()
      new pp.fetch.Area {
        fetchPc := PC
        fetchValid := isValid
      }
      fetchPc.simPublic()
      fetchValid.simPublic()
      val decodePc = UInt(32 bits)
      val decodeValid = Bool()
      new pp.decode.Area {
        decodePc := PC
        decodeValid := isValid
      }
      decodePc.simPublic()
      decodeValid.simPublic()
      val executePc = UInt(32 bits)
      val executeValid = Bool()
      new pp.execute.Area {
        executePc := PC
        executeValid := isValid
      }
      executePc.simPublic()
      executeValid.simPublic()
      val writePc = UInt(32 bits)
      val writeValid = Bool()
      new pp.write.Area {
        writePc := PC
        writeValid := isValid
      }
      writePc.simPublic()
      writeValid.simPublic()

      val ram_plugin = host[Ram]
      val ram = ram_plugin.logic.mem
      ram.simPublic()

      val sw_plugin = host[SwHandler]
      val swSEL = Bool()
      val swAddr = UInt(5 bits)
      swAddr.simPublic()
      swSEL.simPublic()
      new pp.execute.Area {
        swSEL := sw_plugin.logic.SEL
        swAddr := sw_plugin.logic.address
      }
    }
  }

  val whitebox = WhiteboxerPlugin()
  val plugins = List(PipelinePlugin(),
    MemPlugin(),
    FetchPlugin(),
    DecoderPlugin(),
    RegfilePlugin(),
    WriteBackPlugin(),
    RegistersReader(),
    AluPlugin(),
    BeqHandler(),
    SwHandler(),
    LwHandler(),
    OutputPlugin(),
    Ram(),
    whitebox)

  val host = new PluginHost()
  host.asHostOf(plugins)
}

object Simulate extends App {
  if(!List(1, 2).contains(args.length)) {
    println("usage: mill t.runMain riscv.Simulate tests/test_beq.s")
    System.exit(0)
  }
  val program = io.Source.fromFile(args(0)).mkString
  val tokenizer = new Tokenizer(program)
  val parser = new Parser(tokenizer.tokens)
  val assembler = new Assembler(parser.instructions)

  SimConfig.compile { new Cpu }.doSim { dut =>
    /* -- set up program -- */
    for(i <- 0 until assembler.binary.length) {
      dut.whitebox.logic.mem.setBigInt(i, assembler.binary(i))
    }
    val x = if (args.length == 2) args(1).toInt else 0
    dut.whitebox.logic.regfile.setBigInt(0, 0)
    dut.whitebox.logic.regfile.setBigInt(10, BigInt(x))
    /* -- clock boilerplate -- */
    val cd = dut.clockDomain
    cd.forkStimulus(10)
    cd.waitSampling()
    cd.assertReset()
    cd.waitRisingEdge()
    cd.deassertReset()
    cd.waitSampling()
    // sleep(10)
    /* -- run -- */ 
    val initialFuel = 120
    var fuel = initialFuel
    while(!dut.whitebox.logic.done.toBoolean && fuel > 0) {
      fuel -= 1
      sleep(10)
      /*
      println(s"PCs = ${dut.whitebox.logic.fetchPc.toBigInt}, ${dut.whitebox.logic.decodePc.toBigInt}, ${dut.whitebox.logic.executePc.toBigInt}, ${dut.whitebox.logic.writePc.toBigInt}")
      println(s"valids = ${dut.whitebox.logic.fetchValid.toBoolean}, ${dut.whitebox.logic.decodeValid.toBoolean}, ${dut.whitebox.logic.executeValid.toBoolean}, ${dut.whitebox.logic.writeValid.toBoolean}")
      val regs = List(10, 11, 12, 13, 14, 15).map(i => dut.whitebox.logic.regfile.getBigInt(i))
      println(s"regs = $regs")
      */
     /*
      val ram = (0 to 6).map(i => dut.whitebox.logic.ram.getBigInt(4 * i))
      println(s"ram = $ram")
      println(s"sw: SEL = ${dut.whitebox.logic.swSEL.toBoolean}, addr = ${dut.whitebox.logic.swAddr.toInt}")
      println("")
      */
    }
    if(fuel == 0) {
      println("out of fuel?")
    } else {
      println(s"output ${dut.whitebox.logic.regfile.getBigInt(10)} after ${initialFuel - fuel} cycles")
      // println(s"time = ${initialFuel - fuel} cycles")
    }
  }
}
