package gcj2019

import scala.io.StdIn

object GoOwnWay extends App {

  val numOfTestCases = StdIn.readInt()
  for(x <- 1 until numOfTestCases + 1) {
    val gridSize =  StdIn.readInt()
    val lyndiasPath = StdIn.readLine()
    val myPath = path(lyndiasPath)
    println(s"Case #$x: $myPath")
  }

  def path(existingPath: String): String = {
    existingPath.map(step => {
      if (step == 'E') 'S' else 'E'
    }).mkString
  }
}
