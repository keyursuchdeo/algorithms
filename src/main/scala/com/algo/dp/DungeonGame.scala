package com.algo.dp

object DungeonGame extends App {
  //  val d: Array[Array[Int]] = Array.ofDim[Int](3, 3)
  //  d(0) = Array(-2, -3, -3)
  //  d(1) = Array(-5, -10, 1)
  //  d(2) = Array(10, 30, -5)
//  val d: Array[Array[Int]] = Array.ofDim[Int](3, 2)
//  d(0) = Array(-3, 3)
//  d(1) = Array(-10, 1)
//  d(2) = Array(30, -5)
  val d = Array.ofDim[Int](1, 3)
  d(0) = Array(2, 0, -1)
  val res = Solution.calculateMinimumHP(d)
  println(res)

  object Solution {
    def calculateMinimumHP(dungeon: Array[Array[Int]]): Int = {
      val maxRows = dungeon.length
      val maxCols = dungeon(0).length
      val healthNeeded: Array[Array[Option[Int]]] = Array.fill[Option[Int]](maxRows, maxCols)(None)

      def calculate(row: Int, col: Int): Int = {
        if (row == maxRows - 1 && col == maxCols - 1) {
          val a = if (dungeon(row)(col) > 0) 1 else (-dungeon(row)(col) + 1)
          healthNeeded(row)(col) = Option(a)
          a
        } else {
          healthNeeded(row)(col) match {
            case Some(value) => value
            case None =>
              if (row == maxRows - 1) {
                val desiredPath = calculate(row, col + 1)
                val a = if (dungeon(row)(col) > 0) {
                  val b = dungeon(row)(col) - desiredPath
                  if (b >= 0) 1 else -b
                } else {
                  if(desiredPath == 0) (-dungeon(row)(col) + 1) else (-dungeon(row)(col)) + desiredPath
                }
                healthNeeded(row)(col) = Option(a)
                a
              } else if (col == maxCols - 1) {
                val desiredPath = calculate(row + 1, col)
                val a = if (dungeon(row)(col) > 0) {
                  val b = dungeon(row)(col) - desiredPath
                  if (b >= 0) 1 else -b
                } else {
                  if(desiredPath == 0) (-dungeon(row)(col) + 1) else (-dungeon(row)(col)) + desiredPath
                }
                healthNeeded(row)(col) = Option(a)
                a
              } else {
                val desiredPath = Math.min(calculate(row + 1, col), calculate(row, col + 1))
                val a =
                  if (dungeon(row)(col) > 0) {
                    val b = dungeon(row)(col) - desiredPath
                    if (b >= 0) 1 else -b
                  } else {
                    if(desiredPath == 0) (-dungeon(row)(col) + 1) else (-dungeon(row)(col)) + desiredPath
                  }
                healthNeeded(row)(col) = Option(a)
                a
              }
          }
        }
      }

      val minHealthNeeded = calculate(0, 0)
//      println(minHealthNeeded)
      val a: Array[String] = healthNeeded.map(_.mkString(","))
      println(a.mkString("|"))
      minHealthNeeded
    }
  }

}
