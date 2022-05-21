package com.algo.arrays

object VowelSpellchecker extends App {
  object Solution {
    def spellchecker(wordlist: Array[String], queries: Array[String]): Array[String] = {

      def isVowel(char: Char): Boolean =
        char == 'a' || char == 'e' ||  char == 'i' ||  char == 'o' ||  char == 'u'

      def maskVowel(word: String) = {
        word.map(char => {
          if(isVowel(char)) '_' else char
        })
      }

      @scala.annotation.tailrec
      def prepMaps(index: Int,
                    perfectMatch: Map[String, String],
                    lowerCaseMatch: Map[String, String],
                    maskedVowelMatch: Map[String, String]): (Map[String, String], Map[String, String], Map[String, String]) = {
        if(index == wordlist.length) {
          (perfectMatch, lowerCaseMatch, maskedVowelMatch)
        } else {
          val updatedPerfectMatch = perfectMatch + (wordlist(index) -> wordlist(index))
          val lowerCaseWord = wordlist(index).toLowerCase()
          val updatedLowerCaseMatch =
            lowerCaseMatch.get(lowerCaseWord) match {
              case Some(_) => lowerCaseMatch
              case _ => lowerCaseMatch + (lowerCaseWord -> wordlist(index))
            }
          val maskedVowelWord = maskVowel(lowerCaseWord)
          val updatedMaskedVowelMatch =
            maskedVowelMatch.get(maskedVowelWord) match {
              case Some(_) => maskedVowelMatch
              case _ => maskedVowelMatch + (maskedVowelWord -> wordlist(index))
            }
          prepMaps(index + 1, updatedPerfectMatch, updatedLowerCaseMatch, updatedMaskedVowelMatch)
        }
      }

      @scala.annotation.tailrec
      def solveQueries(index: Int,
                       perfectMatch: Map[String, String],
                       lowerCaseMatch: Map[String, String],
                       maskedVowelMatch: Map[String, String], solution: Array[String]): Array[String] = {
        if(index == queries.length) {
          solution
        } else {
          perfectMatch.get(queries(index)) match {
            case Some(word) =>
              solution(index) = word
              solveQueries(index + 1, perfectMatch, lowerCaseMatch, maskedVowelMatch, solution)
            case None =>
              lowerCaseMatch.get(queries(index).toLowerCase()) match {
                case Some(word) =>
                  solution(index) = word
                  solveQueries(index + 1, perfectMatch, lowerCaseMatch, maskedVowelMatch, solution)
                case None =>
                  maskedVowelMatch.get(maskVowel(queries(index).toLowerCase())) match {
                    case Some(word) =>
                      solution(index) = word
                      solveQueries(index + 1, perfectMatch, lowerCaseMatch, maskedVowelMatch, solution)
                    case None =>
                      solution(index) = ""
                      solveQueries(index + 1, perfectMatch, lowerCaseMatch, maskedVowelMatch, solution)
                  }
              }

          }
        }
      }

      val (p, l, m) = prepMaps(0, Map(), Map(), Map())
      solveQueries(0, p, l, m, new Array[String](queries.length))
    }
  }
}
