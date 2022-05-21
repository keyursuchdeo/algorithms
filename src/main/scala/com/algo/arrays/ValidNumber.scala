package com.algo.arrays

object ValidNumber extends App {
  object Solution {
    def isNumber(s: String): Boolean = {
      val chars = s.toCharArray

      def isValidInteger(startingIndex: Int) = {
        @scala.annotation.tailrec
        def check(index: Int, digitFound: Boolean): Boolean = {
          if (index == chars.length) {
            digitFound
          } else {
            if (chars(index).isDigit) {
              check(index + 1, digitFound = true)
            } else {
              false
            }
          }
        }

        if(startingIndex < chars.length) {
          if(chars(startingIndex) == '+' || chars(startingIndex) == '-') {
            check(startingIndex + 1, digitFound = false)
          } else {
            check(startingIndex, digitFound = false)
          }
        } else {
          false
        }
      }

      def isExponential: Boolean = {
        s.contains('e') || s.contains('E')
      }

      def indexOfExponent(): Int = {
        val indexOfLowerE = s.indexOf('e')
        if(indexOfLowerE == -1) {
          s.indexOf('E')
        } else {
          indexOfLowerE
        }
      }

      def isValidDecimalSansExp(endIndex: Int) = {
        @scala.annotation.tailrec
        def check(index: Int, decimalFound: Boolean, digitFound: Boolean): Boolean = {
          if(index == endIndex) {
            digitFound
          } else {
            if(chars(index) == '.') {
              if(!decimalFound) {
                check(index + 1, decimalFound = true, digitFound)
              } else {
                false
              }
            } else {
              if (chars(index).isDigit) {
                check(index + 1, decimalFound, digitFound = true)
              } else {
                false
              }
            }
          }
        }
        if(chars(0) == '+' || chars(0) == '-') {
          check(1, decimalFound = false, digitFound = false)
        } else {
          check(0, decimalFound = false, digitFound = false)
        }
      }

      def isValidDecimal: Boolean = {
        if(isExponential) {
          val indexOfE = indexOfExponent()
          if(indexOfE == 0 || (indexOfE == 1 && (chars(0) == '+' || chars(0) == '-'))) {
            false
          } else {
            if(!isValidInteger(indexOfE + 1)) {
              false
            } else {
              isValidDecimalSansExp(indexOfE)
            }
          }
        } else {
          isValidDecimalSansExp(chars.length)
        }
      }

      isValidDecimal
    }
  }
}
