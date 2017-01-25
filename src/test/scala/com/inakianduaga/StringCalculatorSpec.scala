package com.inakianduaga

import org.scalatest._
import StringCalculator.add

class StringCalculatorSpec extends FlatSpec with Matchers {
  "A StringCalculator" should "return zero when string is empty" in {
    add("") should === (0)
  }

  "A StringCalculator" should "handle adding a variable number of numbers" in {
    add("1,2,3,4") should === (10)
    add("99,1,1,1,0,1") should === (103)
  }

  "A StringCalculator" should "handle newlines between numbers" in {
    add("1\n2,3\n4") should === (10)
  }

  "A StringCalculator" should "support custom delimiters through first line config" in {
    add("//;\n2;3;4") should === (9)
  }

  "A StringCalculator" should "throw an exception when numbers are negative" in {

    val exception = intercept[IllegalArgumentException] {
      add("-1,-2,-3")
    }

    assert(exception.getMessage == "negative not allowed: -1,-2,-3")

  }

  "A StringCalculator" should "ignore numbers greater than 1000" in {
    add("//;\n1000;1001;1;50000") should === (1001)
  }


}
