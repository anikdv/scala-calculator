package RationalNumbers

import java.math
//base java code is taken from: https://introcs.cs.princeton.edu/java/92symbolic/Rational.java.html

class Rational (num: Int, denom: Int) extends Comparable[Rational] {
    def divisor: Int = gcd(num, denom)
    def numerator = (if (denom < 0) -num else num) / divisor
    def denominator = (if (denom < 0) -denom else denom) / divisor

    def gcd(x: Int, y: Int): Int = greatestCommonDivisor(x.abs, y.abs)
    private def greatestCommonDivisor(x: Int, y: Int): Int = y match {
        case 0 => x
        case _ => greatestCommonDivisor(y, x % y)
    }

    def lcm(x: Int, y: Int) = leastCommonMultiple(x.abs, y.abs)
    private def leastCommonMultiple(x: Int, y: Int) = x * (y / gcd(x, y))

    override def toString(): String = denominator match {
        case 1 => numerator.toString
        case _ => numerator + "/" + denominator
    }  

    def compareTo(other: Rational) =
        numerator * other.denominator - denominator * other.numerator

    override def equals(other: Any): Boolean =
        if (other == null) false
        else if (getClass() != other.getClass()) false
        else compareTo(other.asInstanceOf[Rational]) == 0

    override def hashCode(): Int = toString().hashCode()

    def mediant(other: Rational) =
        new Rational(numerator + other.numerator, denominator + other.denominator)
    
    def negate() = new Rational(-numerator, denominator)

    def abs() = if (numerator >= 0) this else negate()

    def reciprocal() = new Rational(denominator, numerator)

    def add(other: Rational) = new Rational(
        numerator * other.denominator + denominator * other.numerator,
        denominator * other.denominator)
    def subtract(other: Rational) = add(other.negate())

    def multiply(other: Rational) =
        new Rational(numerator * other.numerator, denominator * other.denominator)
    def devide(other: Rational) = multiply(other.reciprocal())
}