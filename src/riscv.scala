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

object RiscV {
  def tokenize(program : String) : List[Token] = {
    val operators = List('(', ')', ',', ':')
    var tokens = List[Token]()
    var s = ""
    for(c <- program) {
      if(c.isWhitespace) {
        if(s != "") {
          tokens = tokens :+ Token(s)
          s = ""
        }
      } else if (c.isLetter) {
        if(s != "" && s(0).isDigit) {
          tokens = tokens :+ Token(s)
          s = ""
        }
        s += c
      } else if (c.isDigit) {
        s += c
      } else if (operators.contains(c)) {
        if(s != "") {
          tokens = tokens :+ Token(s)
          s = ""
        }
        tokens = tokens :+ Token(c.toString)
      } else {
        throw new RuntimeException("tokenizer input is nonsense?")
      }
    }
    if(s != "") {
      tokens = tokens :+ Token(s)
    }
    return tokens
  }
}

object Main extends App {
  val tokens = RiscV.tokenize("hello world ra(3, 5) label:")
  println(s"tokens = $tokens")

}
