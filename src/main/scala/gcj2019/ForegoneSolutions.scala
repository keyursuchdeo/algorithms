package gcj2019
import scala.collection.immutable
import scala.io.StdIn

object ForegoneSolutions extends App {

  val numOfTestCases = StdIn.readInt()
  for(x <- 1 until numOfTestCases + 1) {
    val num =  StdIn.readInt()
    val (num1, num2) = calcValue(num)
    println(s"Case #$x: $num1 $num2")
  }

  private def calcValue(input: Long): (Long, Long) = {
    var sb1 = new StringBuilder()
    var sb2 = new StringBuilder()
    val inputStr = input.toString
    inputStr.map(char => {
      if (char == '4') {
        ('3', '1')
      } else {
        (char, '0')
      }
    }).foreach(digits => {
      val (digitNum1, digitNum2) = digits
      (sb1.append(digitNum1), sb2.append(digitNum2))
    })

    (sb1.toLong, sb2.toLong)
  }
}
