package se.citerus.pnehm.version2

import org.junit.Test
import collection.mutable.Stack
import actors.Actor._
import net.liftweb.json._

/**
 * Trait supporting (de)serialization of a case class hierarchy with customized type field name.
 */
trait SerializationSupport {
  implicit val formats = new DefaultFormats {
    override val typeHints = ShortTypeHints(List(classOf[Login], classOf[Logout]))
    override val typeHintFieldName = "type"
  }
}

sealed trait Message

case class Login(userName: String) extends Message

case class Logout(userName: String) extends Message

class ExampleTest extends SerializationSupport {

  @Test
  def testScenario() {
    val incomingMessages = Stack(
      "{\"type\":\"Login\",\"userName\":\"alice\"}", // <= Serialization.write(Login("alice"))
      "{\"type\":\"Logout\",\"userName\":\"alice\"}" // <= Serialization.write(Logout("alice"))
    )

    val messageProcessor = actor {
      var done = false
      loopWhile(!done) {
        receive {
          case Login(userName) => println("User [%s] logged in" format userName)
          case Logout(userName) => println("User [%s] logged out" format userName)
          case "DONE" => done = true; reply()
          case other => throw new UnsupportedOperationException("Unexpected message: " + other)
        }
      }
    }

    incomingMessages.foreach(messageString => {
      println("Deserializing json string: " + messageString)
      val message = Serialization.read[Message](messageString)
      messageProcessor ! message
    })

    messageProcessor !? "DONE"

  }

}
