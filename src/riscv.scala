package riscv

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

object Main extends App {
  if(args.length != 1) {
    println("usage: mill t.runMain riscv.Main filename.s")
    System.exit(0)
  }
  val program = io.Source.fromFile(args(0)).mkString
  val tokenizer = new Tokenizer(program)
  println(tokenizer.tokens)
  val parser = new Parser(tokenizer.tokens)
  println(parser.instructions)

}
