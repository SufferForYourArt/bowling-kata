package app

trait Bowling {
//  def scorePins(pinsKnockedDown: Int*): Int = {
//    var strike = false
//    var pStrike = false
//    var spare = false
//    var total = 0
//    var previousBowl = 0
//    var bowlFrameNumber = 1
//
//    for( a <- pinsKnockedDown){
//      if(a==10 && bowlFrameNumber == 1){
//        strike=true
//        total += 10
//      } else {
//        total += (if(pStrike||strike||spare){a*2}else{a})
//        pStrike = strike
//        strike = false
//        bowlFrameNumber += 1
//      }
//
//      if(bowlFrameNumber == 3 && (previousBowl + a) == 10) {
//        spare = true
//      }else{
//        spare = false
//      }
//
//      if (bowlFrameNumber == 3) {
//        bowlFrameNumber = 1
//      }
//
//      previousBowl = a
//    }
//
//    total
//  }

  def scorePins(pinsKnockedDown: Int*): Int = {
    scoreFirstBowlOfFrame(0, pinsKnockedDown)
  }

  private def scoreFirstBowlOfFrame(totalScore: Int, pinsKnockedDown: Seq[Int]): Int = {
    pinsKnockedDown match {
      case Seq() => totalScore
      case _ => pinsKnockedDown.head match {
        case 10 => scoreFirstBowlOfFrame(totalScore + scoreStrike(pinsKnockedDown), pinsKnockedDown.drop(1))
        case _ => scoreSecondBowlOfFrame(totalScore + pinsKnockedDown.head, 10 - pinsKnockedDown.head, pinsKnockedDown.drop(1))
      }
    }
  }

  private def scoreSecondBowlOfFrame(totalScore: Int, pinsRemaining: Int, pinsKnockedDown: Seq[Int]): Int = {
    pinsKnockedDown match {
      case Seq() => totalScore
      case _ => pinsKnockedDown.head match {
        case `pinsRemaining` => scoreFirstBowlOfFrame(totalScore + scoreSpare(pinsKnockedDown), pinsKnockedDown.drop(1))
        case _ => scoreFirstBowlOfFrame(totalScore + pinsKnockedDown.head, pinsKnockedDown.drop(1))
      }
    }
  }

  private def scoreStrike(pinsKnockedDown: Seq[Int]): Int = {
    val nextBowl = if (pinsKnockedDown.length > 1) pinsKnockedDown(1) else 0
    val bowlAfterNext = if (pinsKnockedDown.length > 2) pinsKnockedDown(2) else 0

    pinsKnockedDown.head + nextBowl + bowlAfterNext
  }

  private def scoreSpare(pinsKnockedDown: Seq[Int]): Int = {
    val nextBowl = if (pinsKnockedDown.length > 1) pinsKnockedDown(1) else 0

    pinsKnockedDown.head + nextBowl
  }
}

object Bowling extends Bowling {

}