package test

import app.Bowling
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties

class BowlingSpec extends Properties("Bowling") {
  val strike = const(10)
  val validBowl = choose(0, 10)
  val nonStrikeBowl = choose(0, 9)

  val spare = for {
    bowl1 <- nonStrikeBowl
    bowl2 <- const(10 - bowl1)
  } yield (bowl1, bowl2)

  val openFrame = for {
    bowl1 <- nonStrikeBowl
    bowl2 <- const(9 - bowl1)
  } yield (bowl1, bowl2)

  def sum(frames: (Int, Int)*): Int = {
    frames.foldLeft(0)((acc, frame) => acc + frame._1 + frame._2)
  }

  def sumOpeningBowl(frames: (Int, Int)*): Int = {
    frames.foldLeft(0)((acc, frame) => acc + frame._1)
  }

  def sumClosingBowl(frames: (Int, Int)*): Int = {
    frames.foldLeft(0)((acc, frame) => acc + frame._2)
  }

  property("8 open frames in a row will result in a score equal to the number of pins knocked down") = {
    forAll(openFrame, openFrame, openFrame, openFrame, openFrame, openFrame, openFrame, openFrame) {
      (frame1, frame2, frame3, frame4, frame5, frame6, frame7, frame8) =>
        val score = Bowling.scorePins(
          frame1._1, frame1._2,
          frame2._1, frame2._2,
          frame3._1, frame3._2,
          frame4._1, frame4._2,
          frame5._1, frame5._2,
          frame6._1, frame6._2,
          frame7._1, frame7._2,
          frame8._1, frame8._2
        )

        score == sum(frame1, frame2, frame3, frame4, frame5, frame6, frame7, frame8)
    }
  }

  property("alternating strike and open frames for 10 frames results in a score of 50 + openFrames * 2") = {
    forAll(strike, openFrame, openFrame, openFrame, openFrame, openFrame) {
      (strike, frame2, frame4, frame6, frame8, frame10) =>
        val score = Bowling.scorePins(
          strike,
          frame2._1, frame2._2,
          strike,
          frame4._1, frame4._2,
          strike,
          frame6._1, frame6._2,
          strike,
          frame8._1, frame8._2,
          strike,
          frame10._1, frame10._2
        )

        score == 50 + sum(frame2, frame4, frame6, frame8, frame10) * 2
    }
  }

  property("alternating spares and open frames for 10 frames result in a score of 50 + open frames score + the "
    + "value of the opening shot of each open frame") = {
    forAll(spare, openFrame, openFrame, openFrame, openFrame, openFrame) {
      (spare, frame2, frame4, frame6, frame8, frame10) =>
        val score = Bowling.scorePins(
          spare._1, spare._2,
          frame2._1, frame2._2,
          spare._1, spare._2,
          frame4._1, frame4._2,
          spare._1, spare._2,
          frame6._1, frame6._2,
          spare._1, spare._2,
          frame8._1, frame8._2,
          spare._1, spare._2,
          frame10._1, frame10._2
        )

        val baseSpareScore = 50
        val openFrameScore = sum(frame2, frame4, frame6, frame8, frame10)
        val openFrameOpeningBowlScore = sumOpeningBowl(frame2, frame4, frame6, frame8, frame10)

        score == baseSpareScore + openFrameScore + openFrameOpeningBowlScore
    }
  }

  property("alternating strikes and spares for 10 frames results in a score of 190") = {
    forAll(strike, spare, spare, spare, spare, spare) {
      (strike, frame2, frame4, frame6, frame8, frame10) =>
        val score = Bowling.scorePins(
          strike,
          frame2._1, frame2._2,
          strike,
          frame4._1, frame4._2,
          strike,
          frame6._1, frame6._2,
          strike,
          frame8._1, frame8._2,
          strike,
          frame10._1, frame10._2
      )

      score == 190
    }
  }

  property("12 or more strikes in a row results in a score of 300") = {
    val strike = 10

    val score = Bowling.scorePins(
      strike,
      strike,
      strike,
      strike,
      strike,
      strike,
      strike,
      strike,
      strike,
      strike,
      strike,
      strike
    )

    score == 300
  }

  property("9 stikes in a row followed by a spare and" +
  " a valid bowl will result in 270 + the value of the" +
  " first shot of the spare + the value of the valid bowl") = {
    forAll(strike, spare, validBowl) {
      (strike, spare, validBowl) =>
        val score = Bowling.scorePins(
          strike,
          strike,
          strike,
          strike,
          strike,
          strike,
          strike,
          strike,
          strike,
          spare._1, spare._2,
          validBowl
        )
        
        score == 260 + spare._1 + validBowl
    }
  }

  property("9 strikes in a row followed by an open frame results in 240 + open frame x2 + opening bowl of open frame") = {
    forAll(strike, openFrame) {
      (strike, openFrame) =>
        val score = Bowling.scorePins(
          strike,
          strike,
          strike,
          strike,
          strike,
          strike,
          strike,
          strike,
          strike,
          openFrame._1, openFrame._2
        )

        score == 240 + sum(openFrame) * 2 + sumOpeningBowl(openFrame)
    }
  }
}