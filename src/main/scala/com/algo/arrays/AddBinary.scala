package com.algo.arrays

object AddBinary extends App {
  object Solution {
    def addBinary(a: String, b: String): String = {
      @scala.annotation.tailrec
      def add(currA: String, currB: String, carry: Char, currSum: String): String = {
        if(currA.isEmpty && currB.isEmpty) {
          if(carry == '0') {
            currSum
          } else {
            carry +: currSum
          }
        } else {
          val charA = currA.headOption.getOrElse('0')
          val charB = currB.headOption.getOrElse('0')
          if(charA == '0' && charB == '0') {
            add(currA.tail, currB.tail, '0', carry +: currSum)
          } else if (charA == '0') {
            if(carry == '0') {
              add(currA.tail, currB.tail, '0', charB +: currSum)
            } else {
              add(currA.tail, currB.tail, '1', '0' +: currSum)
            }
          } else if (charB == '0') {
            if(carry == '0') {
              add(currA.tail, currB.tail, '0', charA +: currSum)
            } else {
              add(currA.tail, currB.tail, '1', '0' +: currSum)
            }
          } else {
            if(carry == '0') {
              add(currA.tail, currB.tail, '1', '0' +: currSum)
            } else {
              add(currA.tail, currB.tail, '1', '1' +: currSum)
            }
          }
        }
      }

      add(a.reverse, b.reverse, '0', "")
    }
  }
}
