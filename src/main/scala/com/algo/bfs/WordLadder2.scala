package com.algo.bfs

import scala.collection.mutable

object WordLadder2 extends App {

  object Solution {
    def ladderLength(beginWord: String, endWord: String, wordList: List[String]): Int = {

      val length = beginWord.length
      var visited = Set[Int]()
      var adjList: Map[String, Seq[Int]] = Map[String, Seq[Int]]()

      def maskChar(charIndex: Int, word: String): String = {
        val (before, after) = word.splitAt(charIndex)
        s"$before*${after.tail}"
      }

      @scala.annotation.tailrec
      def prepAdjList(currIndex: Int = 0): Unit = {
        if (currIndex == wordList.length) {
          ()
        } else {
          (0 until length).foreach(index => {
            val maskedWord = maskChar(index, wordList(currIndex))
            adjList = adjList + (maskedWord -> (currIndex +: adjList.getOrElse(maskedWord, Nil)))
          })
          prepAdjList(currIndex + 1)
        }
      }

      def adjIndicesToWord(word: String): Seq[Int] = {
        (0 until length).flatMap(index => {
          val maskedWord = maskChar(index, word)
          adjList.getOrElse(maskedWord, Nil)
        })
      }

      @scala.annotation.tailrec
      def calculateLength(queue: mutable.Queue[(Int, Int)]): Int = {
        if (queue.isEmpty) {
          0
        } else {
          val (wordIndex, currLen) = queue.dequeue()
          if (wordList(wordIndex) == endWord) {
            currLen + 1
          } else {
            visited = visited + wordIndex
            val neighbours: Seq[Int] = adjIndicesToWord(wordList(wordIndex))
            neighbours.foreach(n => {
              if (!visited.contains(n)) {
                visited = visited + n
                queue.enqueue((n, currLen + 1))
              }
            })
            calculateLength(queue)
          }
        }
      }

      prepAdjList()
      val neighboursOfBeginWord: Seq[Int] = adjIndicesToWord(beginWord)
      val queue: mutable.Queue[(Int, Int)] = new mutable.Queue[(Int, Int)]()
      neighboursOfBeginWord.foreach(n => queue.enqueue((n, 1)))
      calculateLength(queue)
    }
  }

}
