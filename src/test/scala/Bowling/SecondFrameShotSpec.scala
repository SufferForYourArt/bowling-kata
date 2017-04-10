import app.Bowling
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

class SecondFrameShotSpec extends Properties("SecondFrameShot") {
  val validScore = choose(0, 300)
  val validThrow = choose(0, 10)

  val openFrame = for {
    shot1 <- choose(0, 9)
    shot2 <- choose(0, 9 - shot1)
  } yield (shot1, shot2)

  val spare = for {
    shot1 <- choose(0, 9)
    shot2 <- const(10 - shot1)
  } yield (shot1, shot2)

  def bowling() = new Bowling {
  }

  property("should return totalScore when passed an empty Seq()") = {
    forAll { score: Int =>
      val totalScore = bowling.secondFrameShot(0)(score)(Seq())
      totalScore == score
    }
  }

  property("should return number of pins knocked down for an open frame") = {
    forAll(validScore, openFrame) { (score, openFrame) =>
      val totalScore = bowling.secondFrameShot(openFrame._1)(score)(Seq(openFrame._2))
      totalScore == score + openFrame._2
    }
  }

  property("spare should return the following throw for the score") = {
    forAll(validScore, spare, validThrow) { (score, spare, nextShot) =>
      val totalScore = bowling.secondFrameShot(spare._1)(score)(Seq(spare._2, nextShot))
      totalScore == score + spare._2 + nextShot
    }
  }
}