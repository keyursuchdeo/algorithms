package com.algo.arrays

object SumTwoIntegers extends App {

  val res = Solution.getSum(-2, 3)
  println(res)

  object Solution {
    def getSum(a: Int, b: Int): Int = {
      val aBin = a.toBinaryString.toCharArray.toList.reverse
      val bBin = b.toBinaryString.toCharArray.toList.reverse

      @scala.annotation.tailrec
      def sum(currA: List[Char], currB: List[Char], carry: Char, out: List[Char]): List[Char] = {
        (currA, currB) match {
          case (headA :: tailA, headB :: tailB) if headA == headB && headA == '1' =>
            sum(tailA, tailB, '1', carry +: out)
          case (headA :: tailA, headB :: tailB) if headA == headB && headA == '0' =>
            sum(tailA, tailB, '0', carry +: out)
          case (_ :: tailA, _ :: tailB) if carry == '0' =>
            sum(tailA, tailB, '0', '1' +: out)
          case (_ :: tailA, _ :: tailB) if carry == '1' =>
            sum(tailA, tailB, '1', '0' +: out)
          case (Nil, headB :: tailB) if headB == '0' =>
            tailB ++ (carry +: out)
          case (Nil, headB :: tailB) if carry == '0' =>
            tailB ++ (headB +: out)
          case (Nil, _ :: tailB) =>
            sum(Nil, tailB, '1', '0' +: out)
          case (headA :: tailA, Nil) if headA == '0' =>
            tailA ++ (carry +: out)
          case (headA :: tailA, Nil) if carry == '0' =>
            tailA ++ (headA +: out)
          case (_ :: tailA, Nil) =>
            sum(tailA, Nil, '1', '0' +: out)
          case (Nil, Nil) if carry == '1' =>
            '1' +: out
          case (Nil, Nil) =>
            out
        }
      }

      def flipBit(bit: Char): Char = if (bit == '0') '1' else '0'

      val out: List[Char] = sum(aBin, bBin, '0', Nil)
      if (a > 0 && b > 0) {
        Integer.parseInt(out.mkString(""), 2)
      } else if (a < 0 && b < 0) {
        val twoCOut = sum(out.map(flipBit).dropWhile(_ == '0').reverse, List('1'), '0', Nil)
        -Integer.parseInt(twoCOut.mkString(""), 2)
      } else if (a < 0) {
        val twoCOut = sum(out.map(flipBit).dropWhile(_ == '0').reverse, List('1'), '0', Nil)
        println(out)
        println(twoCOut)
        if(a == Int.MinValue || Math.abs(a) == b) {
          0
        } else if(Math.abs(a) > b) {
          -Integer.parseInt(twoCOut.mkString(""), 2)
        } else {
          Integer.parseInt(twoCOut.mkString(""), 2)
        }
      } else {
        val twoCOut = sum(out.map(flipBit).dropWhile(_ == '0').reverse, List('1'), '0', Nil)
        if(Math.abs(b) == a) {
          0
        } else if(b == Int.MinValue || Math.abs(b) > a) {
          -Integer.parseInt(twoCOut.mkString(""), 2)
        } else {
          Integer.parseInt(twoCOut.mkString(""), 2)
        }
      }

    }
  }
}
