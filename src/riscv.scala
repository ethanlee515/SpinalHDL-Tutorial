package riscv

case class Token(content: String) {
  val keywords = List[String]("add", "addi", "(", ")", ",", ":")

  def isKeyword : Boolean = {
    return keywords.contains(content) 
  }

  def isIdentifier : Boolean = {
    return (!isKeyword) && content(0).isLetter
  }

  def isNumber : Boolean = {
    return content(0).isDigit
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
  val operators = List('(', ')', ',', ':')
  var tokens = List[Token]()
  var s = ""

  def pushToken() = {
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
    } else if (operators.contains(c)) {
      if(s != "") {
        pushToken()
      }
      tokens = tokens :+ Token(c.toString)
    } else {
      throw new RuntimeException("Tokenizer input contains weird characters?")
    }
  }
  if(s != "") {
    pushToken()
  }
}

class Parser(tokens: List[Token]) {

}

object Main extends App {
  val tokenizer = new Tokenizer("hello world ra(3, 5) label:")
  println(s"tokens = ${tokenizer.tokens}")

}
