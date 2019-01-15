package com.algo

object KaratsubaMultiplication extends App {
  val num1 = 1234567890
  val num2 = 1234567890
  val expectedResult = num1 * num2

  println(s"Expected result = $expectedResult")

  val num1Str = if (num1.toString.length % 2 == 1) s"0${num1.toString}" else num1.toString
  val num2Str = if (num2.toString.length % 2 == 1) s"0${num2.toString}" else num2.toString


  val result = multiply(num1Str, num2Str)

  println(s"$num1 * $num2 = $result")

  private def parts(numStr: String): (String, String) = {
    val halfNumLen: Double = numStr.length / 2.0
    val part1Len: Int = halfNumLen.toInt
    (numStr.substring(0, part1Len), numStr.substring(part1Len))
  }

  private def multiply(num1: String, num2: String): Long = {
    println("-----------------------------")
    // println(s"num1 $num1 num2 $num2")
    val result: Long = if (num1.toInt < 10 && num2.toInt < 10) {
      num1.toLong * num2.toLong
    } else {
      val n1 = num1.toString.length
      val n2 = num2.toString.length

      val tempN = if (n1 > n2) n1 else n2
      val n = if (tempN % 2 == 1) tempN + 1 else tempN


      val num1Str = if (n > n1) s"%0${n}d".format(num1.toInt) else num1.toString
      val num2Str = if (n > n2) s"%0${n}d".format(num2.toInt) else num2.toString

      val (a, b) = splitNum(num1Str)
      val (c, d) = splitNum(num2Str)

      //println(s"a = $a b = $b c = $c d = $d")

      val ac: Long = multiply(a, c)
      val bd: Long = multiply(b, d)
      val adPlusBc: Long = calcAdPlusBc(a, b, c, d, ac, bd)

      println(s"num1 = $num1Str num2 = $num2Str a = $a c = $c ac = $ac b = $b d = $d bd = $bd adPlusBc = $adPlusBc")

      println(s"n1 = $n1 n2 = $n2 n = $n")

      (Math.pow(10, n) * ac + Math.pow(10, Math.ceil(n / 2.0)) * adPlusBc + bd).toLong
    }

    println(s"num1 = $num1Str num2 = $num2Str result = $result expectedResult = ${num1.toLong * num2.toLong}")
    println("-----------------------------")

    result
  }

  private def calcAdPlusBc(a: String, b: String, c: String, d: String, ac: Long, bd: Long): Long = {
    val aPlusbCPlusd: Long = multiply((a.toInt + b.toInt).toString, (c.toInt + d.toInt).toString)
//    println(s"ac = $ac bd = $bd aPlusbCPlusd = $aPlusbCPlusd")
    aPlusbCPlusd - ac - bd
  }

  private def splitNum(num: String): (String, String) = {
    if (num.length == 1) ("0", num) else parts(num)
  }
}
