package core

import org.scalatest._
import org.scalatest.Matchers._


class ParserSpec extends WordSpec {

  "Parser" can {

    "primitive parsing" should {
      import Primitives._

      "lift a value to parser using result" in {
        assert(result("a")("") == List(("a","")))
      }
    }
  }
}
