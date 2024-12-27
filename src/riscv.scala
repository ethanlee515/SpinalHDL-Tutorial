package riscv

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.misc.pipeline._
import scala.io

object RiscV {
  val instructions = List("add", "addi", "sub", "lw", "sw", "beq", "ecall")
  val register_names = List("zero", "ra", "sp", "gp", "tp",
    "t0", "t1", "t2", "fp", "s1",
    "a0", "a1","a2", "a3", "a4", "a5", "a6", "a7",
    "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11",
    "t3", "t4", "t5", "t6")
  val operators = List("(", ")", ",", "-")
}

case class Token(content: String) {
  import RiscV._
  def containsRegister = register_names.contains(content)

  def containsNaturalNumber : Boolean = {
    try {
      content.toInt
      return true
    } catch {
      case e: java.lang.NumberFormatException => {
        return false
      }
    }
  }
  
  def getNaturalNumber = content.toInt

  def getRegisterNumber : Integer = {
    val n = register_names.indexOf(content)
    if(n == -1) {
      throw new RuntimeException(s"register name expected; seen $content")
    }
    return n
  }
}

class Instruction
class Add(val rd : Integer, val rs1 : Integer, val rs2: Integer) extends Instruction
class Addi(val rd : Integer, val rs1 : Integer, val imm: Integer) extends Instruction
class Sub(val rd : Integer, val rs1 : Integer, val rs2 : Integer) extends Instruction
class Lw(val rd : Integer, val offset : Integer, val rs1 : Integer) extends Instruction
class Sw(val rs2 : Integer, val offset : Integer, val rs1 : Integer) extends Instruction
class Beq(val rs1 : Integer, val rs2 : Integer, val offset : Integer) extends Instruction
class Ecall extends Instruction

class Tokenizer(program : String) {
  import RiscV._
  var tokens = List[Token]()
  private var s = ""

  def pushToken() = {
    val allowed_tokens = instructions ++ operators ++ register_names
    if ((!allowed_tokens.contains(s)) && (!s.matches("^\\d+$"))) {
      throw new RuntimeException(s"Bad token: ${s}")
    }
    tokens = tokens :+ Token(s)
    s = ""
  }

  for(c <- program) {
    if(c.isWhitespace) {
      if(s != "") {
        pushToken()
      }
    } else if (c.isLetter) {
      if(s != "" && s(0).isDigit) {
        pushToken()
      }
      s += c
    } else if (c.isDigit) {
      s += c
    } else if (operators.contains(c.toString)) {
      if(s != "") {
        pushToken()
      }
      tokens = tokens :+ Token(c.toString)
    } else {
      throw new RuntimeException(s"Tokenizer input contains weird character: $c")
    }
  }
  if(s != "") {
    pushToken()
  }
}

class Parser(tokens: List[Token]) {
  var loc = 0

  def matchToken(expected: String) = {
    if(tokens(loc).content != expected) {
      throw new RuntimeException(s"Seen ${tokens(loc).content}, expected $expected")
    }
    loc += 1
  }

  def matchNaturalNumber() : Integer = {
    val result = tokens(loc).getNaturalNumber
    loc += 1
    return result
  }

  def matchConstant() : Integer = {
    if(tokens(loc).content == "-") {
      loc += 1
      val n = matchNaturalNumber()
      return -n
    } else if(tokens(loc).containsNaturalNumber) {
      return matchNaturalNumber()
    } else {
      throw new RuntimeException(s"Seen ${tokens(loc).content}, expected constant number")
    }
  }

  def matchRegister() : Integer = {
    val result = tokens(loc).getRegisterNumber
    loc += 1
    return result
  }

  def matchAdd() : Add = {
    matchToken("add")
    val rd = matchRegister()
    matchToken(",")
    val rs1 = matchRegister()
    matchToken(",")
    val rs2 = matchRegister()
    return new Add(rd, rs1, rs2)
  }

  def matchAddi() : Addi = {
    matchToken("addi")
    val rd = matchRegister()
    matchToken(",")
    val rs1 = matchRegister()
    matchToken(",")
    val imm = matchConstant()
    return new Addi(rd, rs1, imm)
  }

  def matchSub() : Sub = {
    matchToken("sub")
    val rd = matchRegister()
    matchToken(",")
    val rs1 = matchRegister()
    matchToken(",")
    val rs2 = matchRegister()
    return new Sub(rd, rs1, rs2)
  }

  def matchLw() : Lw = {
    matchToken("lw")
    val rd = matchRegister()
    matchToken(",")
    val offset = matchConstant()
    matchToken("(")
    val rs1 = matchRegister()
    matchToken(")")
    return new Lw(rd, offset, rs1)
  }

  def matchSw() : Sw = {
    matchToken("sw")
    val rs2 = matchRegister()
    matchToken(",")
    val offset = matchConstant()
    matchToken("(")
    val rs1 = matchRegister()
    matchToken(")")
    return new Sw(rs2, offset, rs1)
  }

  def matchBeq() : Beq = {
    matchToken("beq")
    val rs1 = matchRegister()
    matchToken(",")
    val rs2 = matchRegister()
    matchToken(",")
    val offset = matchConstant()
    return new Beq(rs1, rs2, offset)
  }

  def matchEcall() : Ecall = {
    matchToken("ecall")
    return new Ecall()
  }

  def matchInstruction() : Instruction = {
    val token = tokens(loc)
    token.content match {
      case "addi" => {
        return matchAddi()
      }
      case "add" => {
        return matchAdd()
      }
      case "sub" => {
        return matchSub()
      }
      case "lw" => {
        return matchLw()
      }
      case "sw" => {
        return matchSw()
      }
      case "beq" => {
        return matchBeq()
      }
      case "ecall" => {
        return matchEcall()
      }
      case _ => {
        throw new RuntimeException(s"instruction expected, seen ${token.content}.")
      }
    }
  }

  var instructions = List[Instruction]()
  while(loc < tokens.length) {
    val instruction = matchInstruction()
    instructions = instructions :+ instruction
  }
}

class Interpreter (instructions : List[Instruction], x : Integer) {
  val regs = Array.fill(32)(0)
  val ram = Array.fill(256)(0)
  var pc = 0
  var done = false

  regs(10) = x

  def interpret_add(add : Add) {
    if(add.rd != 0) {
      regs(add.rd) = regs(add.rs1) + regs(add.rs2)
    }
  }
  def interpret_addi(addi : Addi) {
    if(addi.rd != 0) {
      regs(addi.rd) = regs(addi.rs1) + addi.imm
    }
  }
  def interpret_sub(sub : Sub) {
    if(sub.rd != 0) {
      regs(sub.rd) = regs(sub.rs1) - regs(sub.rs2)
    }
  }

  def interpret_lw(lw : Lw) {
    regs(lw.rd) = ram(regs(lw.rs1) + lw.offset)
  }

  def interpret_sw(sw : Sw) {
    ram(regs(sw.rs1) + sw.offset) = regs(sw.rs2)
  }

  def interpret_beq(beq : Beq) {
    if(regs(beq.rs1) == regs(beq.rs2)) {
      pc += beq.offset
    }
  }

  def interpret_ecall() {
    done = true
  }

  // maybe more prudent to use `fuel` too
  // but I am not prudent
  while(!done) {
    val instruction = instructions(pc)
    pc += 1
    instruction match {
      case add: Add => interpret_add(add)
      case addi: Addi => interpret_addi(addi)
      case sub: Sub => interpret_sub(sub)
      case ecall: Ecall => interpret_ecall()
      case beq: Beq => interpret_beq(beq)
      case _ => {
        throw new RuntimeException(s"Interpreter encountered unknown instruction.")
      }
    }
  }

  def getOutput() : Integer = {
    return regs(10)
  }
}

class Assembler (instructions : List[Instruction]) {

  def assemble_add(add : Add) : Integer = {
    return (b"110011") + (add.rd << 7) + (add.rs1 << 15) + (add.rs2 << 20)
  }

  def assemble_addi(addi : Addi) : Integer = {
    return b"10011" + (addi.rd << 7) + (addi.rs1 << 15) + ((addi.imm & 0xFFF) << 20)
  }

  def assemble_sub(sub : Sub) : Integer = {
    // TODO
    return 0
  }

  def assemble_ecall() : Integer = {
    return b"1110011"
  }

  def assemble_beq(beq : Beq) : Integer = {
    // TODO
    return 0
  }

  def assemble_instruction(instruction : Instruction) : Integer = {
    instruction match {
      case add: Add => {
        return assemble_add(add)
      }
      case addi: Addi => {
        return assemble_addi(addi)
      }
      // case sub: Sub => interpret_sub(sub)
      case ecall: Ecall => {
        return assemble_ecall()
      }
      // case beq: Beq => interpret_beq(beq)
      case _ => {
        throw new RuntimeException(s"Assembler encountered unknown instruction.")
      }
    }
  }

  val binary = instructions.map(assemble_instruction)
}

object Interpret extends App {
  if(args.length != 1 && args.length != 2) {
    println("usage: mill t.runMain riscv.Main filename.s x")
    System.exit(0)
  }
  val program = io.Source.fromFile(args(0)).mkString
  val tokenizer = new Tokenizer(program)
  val parser = new Parser(tokenizer.tokens)
  val x = if (args.length == 1) 0 else args(1).toInt
  val interpreter = new Interpreter(parser.instructions, x)
  println(interpreter.getOutput())
}

object Assemble extends App {
  val program = io.Source.fromFile(args(0)).mkString
  val tokenizer = new Tokenizer(program)
  val parser = new Parser(tokenizer.tokens)
  val assembler = new Assembler(parser.instructions)
  println(assembler.binary)
}

