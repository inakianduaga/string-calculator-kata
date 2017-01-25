package com.inakianduaga
import scala.util.{Success, Failure, Try}

object StringCalculator {

  val defaultDelimiter: Char = ','
  val cutoff = 1000

  def add(numbers: String): Try[Int] =
    numbers.toString.split(Array(delimiter(numbers),'\n'))
      .map(numberString => Try(numberString.toInt))
      .collect { case Success(n) => n }
      .filter(_ <= 1000)
      .map {
          case number if number >= 0 => Success(number)
          case number if number < 0 => Failure(new IllegalArgumentException(number.toString))
      }
      .sum

  private def delimiter(numbers: String): Char =
    if(numbers.startsWith("//"))
      numbers
        .split("\n")
        .headOption.map(delimiterPart => delimiterPart.substring(2))
        .map(_.toList.head)
        .getOrElse(defaultDelimiter)
    else
      defaultDelimiter
}