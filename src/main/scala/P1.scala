import scala.collection.immutable
import scala.io.StdIn

object P1 extends App {
  val numOfTestCases = StdIn.readInt()
  for(x <- 1 to numOfTestCases) {
    val size = StdIn.readInt()
    val matrix = Array.ofDim[Int](size, size)
    for(y <- 0 until size) {
      val row = StdIn.readLine()
      val rowValues: Array[Int] = row.split(" ").map(_.toInt)
      for(z <- 0 until size) {
        matrix(y)(z) = rowValues(z)
      }
    }
    val k = findTrace(matrix, size)
    val r = findCountWithRepeatingElements(matrix, size)
    val c = findColCountWithRepeatingElements(matrix, size)

    println(s"Case #$x: $k $r $c")
  }

  private def findTrace(mat: Array[Array[Int]], size: Int): Int = {
    val diagonals: immutable.Seq[Int] = for {
      i <- 0 until size
      j <- 0 until size
      if i == j
    } yield {
      mat(i)(j)
    }
    diagonals.sum
  }

  private def findCountWithRepeatingElements(mat: Array[Array[Int]], size: Int): Int = {
    val repeatingElements: immutable.Seq[Boolean] = for {
      i <- 0 until size
    } yield {
      val set = mat(i).toSet
      set.size == mat(i).length
    }
    repeatingElements.count(_ == false)
  }

  private def findColCountWithRepeatingElements(mat: Array[Array[Int]], size: Int): Int = {
    val columnElements: immutable.Seq[Int] = for {
      i <- 0 until size
      j <- 0 until size
    } yield {
      mat(j)(i)
    }

    val columns: Array[immutable.Seq[Int]] = columnElements.grouped(size).toArray
    val repeatingElements: immutable.Seq[Boolean] = for {
      i <- 0 until size
    } yield {
      val set = columns(i).toSet
      set.size == columns(i).length
    }
    repeatingElements.count(_ == false)
  }

}
