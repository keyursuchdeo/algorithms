package com.algo.design

object StreamOfCharacters extends App {
  class StreamChecker(_words: Array[String]) {

    /** Initialize your data structure here. */
    private case class TrieNode(endOfWord: Boolean, childCharNodes: Map[Char, TrieNode])
    private var root: Option[TrieNode] = None

    _words.map(insert)

    /** Inserts a word into the trie. */
    private def insert(word: String): Unit = {
      def buildTrieNode(index: Int, existingChild: Option[TrieNode]): TrieNode = {
        if(index == word.length) {
          existingChild match {
            case Some(c) =>  c.copy(endOfWord = true)
            case None =>  TrieNode(endOfWord = true, Map.empty)
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

    private var triePointers: Set[Map[Char, TrieNode]] = Set[Map[Char, TrieNode]]()

    def query(letter: Char): Boolean = {

      def findInRoot(): Boolean = {
        root match {
          case Some(rootNode) =>
            rootNode.childCharNodes.get(letter) match {
              case Some(node) if node.endOfWord  && node.childCharNodes.nonEmpty =>
                triePointers = triePointers + node.childCharNodes
                true
              case Some(node) if node.endOfWord =>
                true
              case Some(node) if node.childCharNodes.nonEmpty =>
                triePointers = triePointers + node.childCharNodes
                false
              case Some(_) =>
                false
              case _ =>
                false
            }
          case _ =>
            false
        }
      }

      def findInPointers(): Boolean = {
        triePointers.exists(pointer => {
          pointer.get(letter) match {
            case Some(node) if node.endOfWord && node.childCharNodes.nonEmpty =>
              triePointers = triePointers - pointer + node.childCharNodes
              true
            case Some(node) if node.endOfWord =>
              triePointers = triePointers - pointer
              true
            case Some(node) if node.childCharNodes.nonEmpty =>
              triePointers = triePointers - pointer + node.childCharNodes
              false
            case Some(node) =>
              triePointers = triePointers - pointer
              false
            case _ => false
          }
        })
      }

      findInRoot() || findInPointers()
    }

  }

}
