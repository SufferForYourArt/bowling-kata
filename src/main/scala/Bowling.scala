package app

trait Bowling {
  def scorePins(pinsKnockedDown: Int*): Int = {
    0
  }

  def firstFrameShot()(totalScore: Int)(pinsKnockedDown: Seq[Int]): Int = {
    pinsKnockedDown match {
      case Nil => totalScore
      case pinsKnockedDown => pinsKnockedDown.head match {
        case 10 => totalScore + pinsKnockedDown.head + (if (pinsKnockedDown.length > 1) pinsKnockedDown(1) else 0) + (if(pinsKnockedDown.length > 2) pinsKnockedDown(2) else 0)
        case _ => totalScore + pinsKnockedDown.head
      }
    }
  }

  def secondFrameShot(pinsRemaining: Int)(totalScore: Int)(pinsKnockedDown: Seq[Int]): Int = {
    pinsKnockedDown match {
      case Nil => totalScore
      case pinsKnockedDown => pinsKnockedDown.head match {
        case pinsRemaining => totalScore + pinsKnockedDown.head + (if (pinsKnockedDown.length > 1) pinsKnockedDown(1) else 0)
        case _ => totalScore + pinsKnockedDown.head
      }
    }
  }

  /*def openFrame()(totalScore: Int)(pinsKnockedDown: Seq[Int]): Int = {
    pinsKnockedDown match {
      case Nil => totalScore
      case seq: Seq[Int] => seq.head match {
        case 10 => strike()(totalScore + pinsKnockedDown.head + pinsKnockedDown(1) + pinsKnockedDown(2))(pinsKnockedDown.drop(0))
        case _ => secondShot(10 - pinsKnockedDown.head)(totalScore + pinsKnockedDown.head + pinsKnockedDown(1) + pinsKnockedDown(2))(pinsKnockedDown.drop(0))
      }
    }
  }

  def strike()(totalScore: Int)(pinsKnockedDown: Seq[Int]): Int = {
    pinsKnockedDown match {
      case Nil => totalScore
      case seq: Seq[Int] => seq.head match {
        case 10 => strike()(totalScore + pinsKnockedDown.head + pinsKnockedDown(1) + pinsKnockedDown(2))(pinsKnockedDown.drop(0))
        case _ => secondShot(10 - pinsKnockedDown.head)(totalScore + pinsKnockedDown.head + pinsKnockedDown(1) + pinsKnockedDown(2))(pinsKnockedDown.drop(0))
      }
    }
  }

  def spare()(totalScore: Int)(pinsKnockedDown: Seq[Int]): Int = {
    pinsKnockedDown match {
      case Nil => totalScore
      case seq: Seq[Int] => seq.head match {
        case 10 => strike()(totalScore + pinsKnockedDown.head + pinsKnockedDown(1) + pinsKnockedDown(2))(pinsKnockedDown.drop(0))
        case _ => secondShot(10 - pinsKnockedDown.head)(totalScore + pinsKnockedDown.head + pinsKnockedDown(1) + pinsKnockedDown(2))(pinsKnockedDown.drop(0))
      }
    }
  }

  def secondShot(pinsRemaining: Int)(totalScore: Int)(pinsKnockedDown: Seq[Int]): Int = {
    pinsKnockedDown match {
      case Nil => totalScore
      case seq: Seq[Int] => seq.head match {
        case `pinsRemaining` => spare()(totalScore + pinsKnockedDown.head + pinsKnockedDown(1) + pinsKnockedDown(2))(pinsKnockedDown.drop(0))
        case _ => openFrame()(totalScore + pinsKnockedDown.head + pinsKnockedDown(1) + pinsKnockedDown(2))(pinsKnockedDown.drop(0))
      }
    }
  }*/
}
