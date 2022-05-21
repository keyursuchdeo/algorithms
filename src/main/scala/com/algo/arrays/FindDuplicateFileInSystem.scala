package com.algo.arrays

object FindDuplicateFileInSystem extends App {

  object Solution {
    def findDuplicate(paths: Array[String]): List[List[String]] = {

      var map: Map[String, List[String]] = Map()

      def splitFileNameAndContent(file: String): (String, String) = {
        val Array(name, content) = file.split('(')
        (name, content.dropRight(1))
      }

      @scala.annotation.tailrec
      def mapFilesByContent(index: Int): Unit = {
        if (index == paths.length) {
          ()
        } else {
          val path = paths(index)
          val pathComponents = path.split(" ")
          val dir = pathComponents.head
          pathComponents.tail.foreach(file => {
            val (name, content) = splitFileNameAndContent(file)
            val currFilesByContent = map.getOrElse(content, Nil)
            map = map + (content -> (s"$dir/$name" +: currFilesByContent))
          })
          mapFilesByContent(index + 1)
        }
      }

      mapFilesByContent(0)
      map.values.filter(_.size > 1).toList
    }
  }

}
