package com.me

import org.scalacheck.Gen.choose
import org.scalatest.PropSpec

object ScorePinsSpec extends PropSpec {
  val validThrow = choose(0, 10)
  val nonStrikeThrow = choose(0, 9)
  val spare = for {
    shotA <- nonStrikeThrow
    shotB <- choose(10 - shotA, 10 - shotA)
  } yield (shotA, shotB)

  property("a single shot should return the amount scored for the number of pins knocked down") {
    println("n\n\n\n\n\nhere\n\n\n\n\n\n\n")

    fail("here")
  }

}