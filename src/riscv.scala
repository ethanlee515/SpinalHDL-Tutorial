package riscv

case class Token(content: String)

class Instruction
class Add(val rd : Integer, val rs1 : Integer, val rs2: Integer) extends Instruction
class Addi(val rd : Integer, val rs1 : Integer, val imm: Integer) extends Instruction
class Sub(val rd : Integer, val rs1 : Integer, val rs2 : Integer) extends Instruction
class Lw(val rd : Integer, val offset : Integer, val rs1 : Integer) extends Instruction
class Sw(val rs2 : Integer, val offset : Integer, val rs1 : Integer) extends Instruction
class Beq(val rs1 : Integer, val rs2 : Integer, val offset : Integer) extends Instruction

class Tokenizer(program : String) {
  val instructions = List("add", "addi", "sub", "lw", "sw", "beq")
  val register_names = List("zero", "ra", "sp", "gp", "tp",
    "t0", "t1", "t2", "fp", "s1",
    "a0", "a1","a2", "a3", "a4", "a5", "a6", "a7",
    "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11",
    "t3", "t4", "t5", "t6")
  val operators = List("(", ")", ",", "-")

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

  def match_next() = {
    val token = tokens(loc)
    token.content match {
      case "addi" => {
        println("addi")
      }
      case _ => {
        println("default")
      }
    }
  }
}

object Main extends App {
  val tokenizer = new Tokenizer("addi ra(3, 5) lw s1 s5, ra")
  val parser = new Parser(tokenizer.tokens)

}
