package com.algo.dp

object RegularExpressionMatching extends App {
  object Solution {
    def isMatch(s: String, p: String): Boolean = {
      val sChars = s.toCharArray
      val pChars = p.toCharArray
      val matchStatuses = Array.fill[Option[Boolean]](sChars.length, pChars.length)(None)

      def check(sIndex: Int, pIndex: Int): Boolean = {
        if (sIndex == sChars.length && pIndex == pChars.length) {
          true
        } else if (pIndex == pChars.length) {
          false
        } else if (sIndex == sChars.length) {
          if (pChars(pIndex) == '*') {
            check(sIndex, pIndex + 1)
          } else  {
            if(pIndex + 1 < pChars.length && pChars(pIndex + 1) == '*') {
              check(sIndex, pIndex + 2)
            } else {
              false
            }
          }
        } else {
          matchStatuses(sIndex)(pIndex) match {
            case Some(value) => value
            case None =>
              val value =
                if (pChars(pIndex) == '*') {
                  if(pIndex == 0) {
                    check(sIndex, pIndex + 1)
                  } else {
                    if(pChars(pIndex - 1) == '.') {
                      (sIndex to sChars.length).exists(index => {
                        check(index, pIndex + 1)
                      })
                    } else {
                      if(pChars(pIndex - 1) == sChars(sIndex)) {
                        check(sIndex + 1, pIndex) || check(sIndex + 1, pIndex + 1) || check(sIndex, pIndex + 1)
                      } else {
                        check(sIndex, pIndex + 1)
                      }
                    }
                  }
                } else if (pChars(pIndex) == '.' || pChars(pIndex) == sChars(sIndex)) {
                  if(pIndex + 1 < pChars.length && pChars(pIndex + 1) == '*') {
                    check(sIndex + 1, pIndex + 1) || check(sIndex, pIndex + 2)
                  } else {
                    check(sIndex + 1, pIndex + 1)
                  }
                } else {
                  if(pIndex + 1 < pChars.length && pChars(pIndex + 1) == '*') {
                    check(sIndex, pIndex + 2)
                  } else {
                    false
                  }

                }
              matchStatuses(sIndex)(pIndex) = Option(value)
              value
          }
        }
      }

      check(0, 0)
    }
  }
}
