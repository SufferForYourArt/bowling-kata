import org.scalacheck.Gen._
import org.scalacheck.Properties
import org.scalacheck.Prop._

class BowlingSpec extends Properties("Bowling") {
  val validThrow = choose(0, 10)
  val nonStrikeThrow = choose(0, 9)
  val strike = const(10)

  val spare = for {
    shot1 <- nonStrikeThrow
    shot2 <- const(10 - shot1)
  } yield (shot1, shot2)

  val openFrame = for {
    shot1 <- nonStrikeThrow
    shot2 <- choose(0, 9 - shot1)
  } yield (shot1, shot2)

  val validFrame = for {
    shot1 <- nonStrikeThrow
    shot2 <- choose(0, 10 - shot1)
  } yield (shot1, shot2)
}