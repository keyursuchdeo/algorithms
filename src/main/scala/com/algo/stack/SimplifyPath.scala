package com.algo.stack

object SimplifyPath extends App {
  object Solution {
    def simplifyPath(path: String): String = {
      val pathChars = path.toCharArray

      @scala.annotation.tailrec
      def simplify(index: Int, simplifiedPathChars: Seq[String]): String = {
        if(index == pathChars.length) {
          if(simplifiedPathChars.head == "/" && simplifiedPathChars.tail.nonEmpty) {
            simplifiedPathChars.tail.reverse.mkString("")
          } else if (simplifiedPathChars.head == ".") {
            simplify(index, simplifiedPathChars.tail)
          } else if (simplifiedPathChars.head == "..") {
            if(simplifiedPathChars.tail.tail.isEmpty) {
              simplifiedPathChars.tail.reverse.mkString("")
            } else {
              simplify(index,  simplifiedPathChars.tail.tail.tail)
            }
          } else {
            simplifiedPathChars.reverse.mkString("")
          }
        } else {
          if(simplifiedPathChars.isEmpty) {
            simplify(index + 1, pathChars(index).toString +: simplifiedPathChars)
          } else {
            if(pathChars(index) == '/') {
              if(simplifiedPathChars.head == "/") {
                simplify(index + 1, simplifiedPathChars)
              } else if (simplifiedPathChars.head == ".") {
                simplify(index, simplifiedPathChars.tail)
              } else if (simplifiedPathChars.head == "..") {
                if(simplifiedPathChars.tail.tail.isEmpty) {
                  simplify(index, simplifiedPathChars.tail)
                } else {
                  simplify(index, simplifiedPathChars.tail.tail.tail)
                }
              } else {
                simplify(index + 1, pathChars(index).toString +: simplifiedPathChars)
              }
            } else {
              if(simplifiedPathChars.head == "/") {
                simplify(index + 1, pathChars(index).toString +: simplifiedPathChars)
              } else {
                simplify(index + 1, (simplifiedPathChars.head + pathChars(index).toString) +: simplifiedPathChars.tail)
              }
            }
          }
        }
      }

      simplify(0, Nil)
    }
  }
}
