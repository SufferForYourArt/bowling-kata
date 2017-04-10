import app.Bowling
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

class FirstFrameShotSpec extends Properties("OpenFrame") {
  val validScore = choose(0, 300)
  val validThrow = choose(0, 10)
  val strike = const(10)

  def bowling() = new Bowling {
  }

  property("should return totalScore when passed an empty Seq()") = {
    forAll { score: Int =>
      val totalScore = bowling.firstFrameShot()(score)(Seq())
      totalScore == score
    }
  }

  property("should return number of pins knocked down for a single throw") = {
    forAll(validScore, validThrow) { (score, shot) =>
      val totalScore = bowling.firstFrameShot()(score)(Seq(shot))
      totalScore == score + shot
    }
  }

  property("strike should return the following throw for the score") = {
    forAll(validScore, strike, validThrow) { (score, strike, shot) =>
      val totalScore = bowling.firstFrameShot()(score)(Seq(strike, shot))
      totalScore == score + strike + shot
    }
  }

  property("strike should return the following 2 throws for the score") = {
    forAll(validScore, strike, validThrow, validThrow) { (score, strike, shot1, shot2) =>
      val totalScore = bowling.firstFrameShot()(score)(Seq(strike, shot1, shot2))
      totalScore == score + strike + shot1 + shot2
    }
  }
}