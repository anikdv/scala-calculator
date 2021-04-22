import RationalNumbers.Rational
import RationalNumbers.RationalString
import RationalCalculator.Calculator

object Main extends App {
  println("Calculation started")
  print("Expression is: ")
  args.foreach(arg => print(arg + " "))
  println()
  print("Result is: ")
  println(Calculator.calculate(args, Array()))
}