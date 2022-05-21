package com.algo.arrays

object IntervalListIntersection extends App {

  val la: Array[Array[Int]] = Array(Array(0, 2), Array(5, 10), Array(13, 23), Array(24, 25))
  val lb: Array[Array[Int]] = Array(Array(1, 5), Array(8, 12), Array(15, 24), Array(25, 26))

//  val la: Array[Array[Int]] = Array(Array(13, 23), Array(24, 25))
//  val lb: Array[Array[Int]] = Array(Array(8, 12), Array(15, 24), Array(25, 26))

  val res: Array[Array[Int]] = Solution.intervalIntersection(la, lb)
  println(res.map(_.mkString(",")).mkString("|"))

  object Solution {
    def intervalIntersection(A: Array[Array[Int]], B: Array[Array[Int]]): Array[Array[Int]] = {

      @scala.annotation.tailrec
      def find(a: Seq[Array[Int]], b: Seq[Array[Int]], output: Seq[Array[Int]]): Seq[Array[Int]] = {
        (a, b)  match {
          case ((Array(_, a2) :: xa), (Array(b1, _) :: _)) if (a2 < b1) =>
            find(xa, b, output)
          case ((Array(a1, _) :: _), (Array(_, b2) :: xb)) if (a1 > b2) =>
            find(a, xb, output)
          case (Array(a1, a2) :: _, Array(b1, b2) :: xb) if (a1 <= b1 && b2 <= a2) =>
            find(a, xb, Array(b1, b2) +: output)
          case ((Array(a1, a2) :: _), (Array(b1, b2) :: xb)) if (a1 >= b1 && b2 <= a2) =>
            find(a, xb, Array(a1, b2) +: output)
          case ((Array(a1, a2) :: xa), (Array(b1, b2) :: _)) if (a1 <= b1 && b2 >= a2) =>
            find(xa, b, Array(b1, a2) +: output)
          case ((Array(a1, a2) :: xa), (Array(b1, b2) :: _)) if (a1 >= b1 && b2 >= a2) =>
            find(xa, b, Array(a1, a2) +: output)
          case (_, Nil) =>
            output
          case (Nil, _) =>
            output
        }
      }

      find(A.toList, B.toList, Nil).toArray
    }
  }
}
