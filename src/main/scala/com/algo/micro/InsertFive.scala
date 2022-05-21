package com.algo.micro

object InsertFive extends App {

  Solution.solution(-111)

  object Solution {
    def solution(n: Int): Int = {

      def insert(index: Int, stringBuilder: StringBuilder, n: Int): String = {
        if(n >= 0) {
          insertInPositiveNumber(index, n.toString, stringBuilder)
        } else {
          '-' +insertInNegativeNumber(index, Math.abs(n).toString, stringBuilder)
        }
      }

      @scala.annotation.tailrec
      def insertInPositiveNumber(index: Int, numSeq: String, stringBuilder: StringBuilder): String = {
        if(index == numSeq.length) {
          stringBuilder.append('5').toString()
        } else if(numSeq(index).toString.toInt < 5){
          stringBuilder.append('5')
          appendRest(index, numSeq, stringBuilder)
        } else {
          insertInPositiveNumber(index + 1, numSeq, stringBuilder.append(numSeq(index)))
        }
      }

      @scala.annotation.tailrec
      def insertInNegativeNumber(index: Int, numSeq: String, stringBuilder: StringBuilder): String = {
        if(index == numSeq.length) {
          stringBuilder.append('5').toString()
        } else if(numSeq(index).toString.toInt > 5){
          stringBuilder.append('5')
          appendRest(index, numSeq, stringBuilder)
        } else {
          insertInNegativeNumber(index + 1, numSeq, stringBuilder.append(numSeq(index)))
        }
      }

      def appendRest(index: Int, numSeq: String, stringBuilder: StringBuilder): String= {
        (index until numSeq.length).foreach(i => stringBuilder.append(numSeq(i)))
        stringBuilder.toString()
      }

      insert(0, new StringBuilder(), n).toInt
    }
  }

}
