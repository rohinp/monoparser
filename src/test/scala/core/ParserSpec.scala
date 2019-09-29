package core

import org.scalatest._
import org.scalatest.Matchers._

class ParserSpec extends WordSpec {

  "Parser" can {

    "primitive parser" should {
      import Primitive._

      "how to lift a value in a parser" in {
        assert(result("a")("") == List(("a","")))
      }

      "How to create unit/zero value for parser" in {
        assert(zero("a") == List())
      }

      "how to parse a char" in {
        assert(item("data") == List(('d',"ata")))
      }

      "how to parse a specific char" in {
        assert(sat(_ == 'd')("data") == List(('d',"ata")))
        assert(sat(_ == 'x')("data") == List())
      }

      "how to create a choice parser" in {
        val choiceParser1 = sat(_ == 'a') || sat(_ == 'd')
        val choiceParser2 = item || sat(_ == 'd')
        
        assert(choiceParser1("data") == List(('d',"ata")))
        assert(choiceParser2("data") == List(('d',"ata"),('d',"ata")))
      }

    }

    "derived parser" should {
      import Derived._

      "how to parse a given char" in {
        val charParser = char('t')
        assert(charParser("test") == List(('t',"est")))
        assert(charParser("nomatch") == List())
      }

      "how to parse a digit" in {
        assert(digit("12.3") == List(('1',"2.3")))
        assert(digit("nomatch") == List())
      }

      "how to parse a lower char" in {
        assert(lower("testing") == List(('t',"esting")))
        assert(lower("Test") == List())
      }

      "how to parse a upper char" in {
        assert(upper("Testing") == List(('T',"esting")))
        assert(upper("test") == List())
      }

      "how to parse a upper or lower char (any letter)" in {
        assert(letter("Testing") == List(('T',"esting")))
        assert(letter("testing") == List(('t',"esting")))
        assert(letter("1test") == List())
      }

      "how to parse an alphanum" in {
        assert(alphanum("Testing") == List(('T',"esting")))
        assert(alphanum("testing") == List(('t',"esting")))
        assert(alphanum("1test") == List(('1',"test")))
        assert(alphanum(",test") == List())
      }
    }
  }
}
