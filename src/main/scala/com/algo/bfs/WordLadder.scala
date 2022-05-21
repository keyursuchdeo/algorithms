package com.algo.bfs
import scala.collection.mutable

object WordLadder extends App {
  object Solution {
    def ladderLength(beginWord: String, endWord: String, wordList: List[String]): Int = {

      val adjList: Array[Set[Int]] = Array.fill[Set[Int]](wordList.length)(Set())

      def isAdjacent(word1: String, word2: String): Boolean = {
        @scala.annotation.tailrec
        def check(currWord1: String, currWord2: String): Boolean = {
          if(currWord1.isEmpty && currWord2.isEmpty) {
            false
          } else {
            if(currWord1.head == currWord2.head) {
              check(currWord1.tail, currWord2.tail)
            } else {
              currWord1.tail == currWord2.tail
            }
          }
        }
        check(word1, word2)
      }

      @scala.annotation.tailrec
      def fillAdjList(currIndex: Int): Unit = {
        if(currIndex == wordList.length) {
          ()
        } else {
          (currIndex + 1 until wordList.length).foreach(index => {
            if(isAdjacent(wordList(currIndex), wordList(index))) {
              adjList(currIndex) = adjList(currIndex) + index
              adjList(index) = adjList(index) + currIndex
            }
          })
          fillAdjList(currIndex)
        }
      }

      def adjIndicesToBeginWord(): Seq[Int] = {
        wordList.indices.filter(index => {
          isAdjacent(beginWord, wordList(index))
        })
      }

      @scala.annotation.tailrec
      def calculateLength(queue: mutable.Queue[(Int, Int)], visited: Set[Int]): Int = {
        if(queue.isEmpty) {
          0
        } else {
          val (wordIndex, currLen) = queue.dequeue()
          if(wordList(wordIndex) == endWord) {
            currLen + 1
          } else {
            val unVisitedNeighbours: Set[Int] = adjList(wordIndex) -- visited
            unVisitedNeighbours.foreach(n => queue.enqueue((n, currLen + 1)))
            calculateLength(queue, visited + wordIndex)
          }
        }
      }

      fillAdjList(0)
      val neighboursOfBeginWord: Seq[Int] = adjIndicesToBeginWord()
      val queue: mutable.Queue[(Int, Int)] = new mutable.Queue[(Int, Int)]()
      neighboursOfBeginWord.foreach(n => queue.enqueue((n, 0)))
      calculateLength(queue, Set())
    }
  }
}
