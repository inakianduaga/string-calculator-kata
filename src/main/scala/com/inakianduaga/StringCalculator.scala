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
      .map (number =>
          if (number >= 0) Right(number)
          else Left(number)
      )
      .foldLeft[Either[List[Int], Int]](Right(0))((sumOrExceptions, wrappedInt) =>
        if (wrappedInt.isLeft) Left(wrappedInt.left.get :: sumOrExceptions.left.getOrElse(Nil))
        else Right(sumOrExceptions.right.get + wrappedInt.right.get)
      )
      .left.map(_.map(int => int.toString))
      .left.map("negative not allowed: " + _.mkString(","))
      .fold[Try[Int]](exceptionMsg => Failure(new IllegalArgumentException(exceptionMsg)), Success(_))

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