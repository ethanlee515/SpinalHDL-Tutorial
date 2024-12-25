package riscv

object RiscV {
  val instructions = List("add", "addi", "sub", "lw", "sw", "beq")
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

  def matchAdd() = {
    
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

  def matchRegister() : Integer = {
    val result = tokens(loc).getRegisterNumber
    loc += 1
    return result
  }

  def match_sub() = {

  }

  def match_lw() = {

  }

  def match_sw() = {

  }

  def match_beq() = {

  }

  def matchInstruction() : Instruction = {
    val token = tokens(loc)
    token.content match {
      case "addi" => {
        return matchAddi()
      }
      case _ => {
        throw new RuntimeException("NYI")
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
  val tokenizer = new Tokenizer("addi ra, t0, 15 addi t1, t2, -19")
  println(tokenizer.tokens)
  val parser = new Parser(tokenizer.tokens)
  println(parser.instructions)

}
