package com.algo.micro

object ValidTimes extends App {

  Solution.solution(1, 8, 3, 2)
//  Solution.solution(9, 9, 9, 9)

//    Solution.solution(2, 3, 3, 2)

  object Solution {
    def solution(a: Int, b: Int, c: Int, d: Int): Int = {
      val digits = Seq(a, b, c, d)
      find(possibleMHourDigits(digits), possibleLHourDigits(digits), possibleMMinDigits(digits), possibleLMinDigits(digits))
    }

    private def possibleMHourDigits(digits: Seq[Int]): Seq[Int] =
      digits.filter(_ <= 2)

    private def possibleLHourDigits(digits: Seq[Int]): Seq[Int] =
      digits

    private def possibleMMinDigits(digits: Seq[Int]): Seq[Int] =
      digits.filter(_ <= 5)

    private def possibleLMinDigits(digits: Seq[Int]): Seq[Int] =
      digits

    private def find(mHourDigits: Seq[Int], lHourDigits: Seq[Int], mMinDigits: Seq[Int], lMinDigits: Seq[Int]): Int = {
      val times: Seq[Seq[Seq[Seq[String]]]] =
        for (mHourDigit <- mHourDigits) yield {
          val possibleLHourDigits: Seq[Int] = findPossibleLHourDigits(mHourDigit, lHourDigits)
          for (lHourDigit <- possibleLHourDigits) yield {
            val possibleMMinDigits = findPossibleMMinDigits(mHourDigit, lHourDigit, mMinDigits)
            for (mMinDigit <- possibleMMinDigits) yield {
              val possibleLMinDigits = findPossibleLMinDigits(mHourDigit, lHourDigit, mMinDigit, lMinDigits)
              for (lMinDigit <- possibleLMinDigits) yield {
                val time = formTime(mHourDigit, lHourDigit, mMinDigit, lMinDigit)
//                println(time)
                time
              }
            }
          }
        }
      val uniqueTimes = times.flatten.flatten.flatten.distinct
      println(uniqueTimes)
      println(uniqueTimes.size)
      uniqueTimes.size
    }

    private def formTime(mHour: Int, lHour: Int, mMin: Int, lMin: Int): String =
      s"$mHour$lHour:$mMin$lMin"


    private def findPossibleLHourDigits(mHourDigit: Int, lHourDigits: Seq[Int]) = {
      val filteredLHourDigits = lHourDigits.diff(Seq(mHourDigit))
      if (mHourDigit <= 1) filteredLHourDigits else filteredLHourDigits.filter(_ <= 3)
    }

    private def findPossibleMMinDigits(mHourDigit: Int, lHourDigit: Int, mMinDigits: Seq[Int]) = {
      mMinDigits.diff(Seq(mHourDigit, lHourDigit))
    }

    private def findPossibleLMinDigits(mHourDigit: Int, lHourDigit: Int, mMinDigit: Int, lMinDigits: Seq[Int]) = {
      lMinDigits.diff(Seq(mHourDigit, lHourDigit, mMinDigit))
    }
  }

}
