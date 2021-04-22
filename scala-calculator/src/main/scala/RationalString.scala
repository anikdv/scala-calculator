package RationalNumbers

import scala.annotation.switch

class RationalString(rationalValue: String) {
  def asString = rationalValue

  def asRational: Option[Rational] = {
    var rationalParts = rationalValue.split('/')
    rationalParts.length match {
      case 0 =>
        None
      case 1 =>
        Some(getSingleRational(rationalParts.head))
      case 2 =>
        Some(getFullRational(rationalParts.head, rationalParts.tail.head))
      case _ =>
        None
    }
  }

  def getSingleRational(numerator: String): Rational =
    new Rational(Integer.parseInt(numerator), 1)
  def getFullRational(numerator: String, denominator: String): Rational =
    new Rational(Integer.parseInt(numerator), Integer.parseInt(denominator))
}
