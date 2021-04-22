package RationalCalculator

import RationalNumbers.Rational
import RationalNumbers.RationalString

object Calculator {
  def calculate(expression: Array[String], numbers: Array[Rational]): Rational = {
    var resultNumbers = expression.head match {
      case "+" =>
        Array(
          numbers.head.add(numbers.tail.head)
        ) ++ numbers.tail.tail
      case "-" =>
        Array(
          numbers.head.negate().add(numbers.tail.head)
        ) ++ numbers.tail.tail
      case "*" =>
        Array(
          numbers.head.multiply(numbers.tail.head)
        ) ++ numbers.tail.tail
      case "/" =>
        Array(
          numbers.head.reciprocal().multiply(numbers.tail.head)
        ) ++ numbers.tail.tail
      case _ =>
        Array(
          new RationalString(expression.head).asRational.get
        ) ++ numbers
    }
    expression.tail.length match {
        case 0 => resultNumbers.head
        case _ => calculate(expression.tail, resultNumbers)
    }
  }
}
