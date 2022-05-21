package com.algo.design

object StreamOfCharacters2 extends App {

  class StreamChecker(_words: Array[String]) {

    /** Initialize your data structure here. */
    private case class TrieNode(endOfWord: Boolean, childCharNodes: Map[Char, TrieNode])

    private var root: Option[TrieNode] = None

    _words.map(_.reverse).map(insert)

    /** Inserts a word into the trie. */
    private def insert(word: String): Unit = {
      def buildTrieNode(index: Int, existingChild: Option[TrieNode]): TrieNode = {
        if (index == word.length) {
          existingChild match {
            case Some(c) => c.copy(endOfWord = true)
            case None => TrieNode(endOfWord = true, Map.empty)
          }
        } else if (index == 0) {
          root match {
            case Some(r) =>
              val node = r.childCharNodes.get(word(index))
              r.copy(childCharNodes = r.childCharNodes + (word(index) -> buildTrieNode(index + 1, node)))
            case None =>
              TrieNode(endOfWord = false, Map(word(index) -> buildTrieNode(index + 1, existingChild)))
          }
        } else {
          val node = existingChild.flatMap(_.childCharNodes.get(word(index)))
          existingChild match {
            case Some(c) =>
              c.copy(childCharNodes = c.childCharNodes + (word(index) -> buildTrieNode(index + 1, node)))
            case None =>
              TrieNode(endOfWord = false, Map(word(index) -> buildTrieNode(index + 1, node)))
          }
        }
      }

      root = Option(buildTrieNode(0, None))
    }

    var letters: Seq[Char] = Seq[Char]()

    def query(letter: Char): Boolean = {
      @scala.annotation.tailrec
      def sr(currLetters: Seq[Char], trieNode: TrieNode): Boolean = {
        if(currLetters.isEmpty) {
          false
        } else {
          trieNode.childCharNodes.get(currLetters.head) match {
            case Some(childNode) if childNode.endOfWord => true
            case Some(childNode) => sr(currLetters.tail, childNode)
            case None => false
          }
        }
      }

      root match {
        case Some(r) =>
          letters = letter +: letters
          sr(letters, r)
        case None => false
      }

    }

  }

}
