package com.algo.dp

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object Sticker2 extends App {

  object Solution {
    def minStickers(stickers: Array[String], target: String): Int = {

      val stickerCharCount = Array.ofDim[Int](stickers.length, 26)
      var minCount: mutable.Map[String, Int] = mutable.Map[String, Int]()

      @scala.annotation.tailrec
      def fillCharCountBySticker(stickerIndex: Int, sticker: String): Unit = {
        if (sticker.isEmpty) {
          ()
        } else {
          val charIndex = sticker.head - 'a'
          stickerCharCount(stickerIndex)(charIndex) = stickerCharCount(stickerIndex)(charIndex) + 1
          fillCharCountBySticker(stickerIndex, sticker.tail)
        }
      }

      @scala.annotation.tailrec
      def fillCharCount(index: Int): Unit = {
        if (index == stickers.length) {
          ()
        } else {
          fillCharCountBySticker(index, stickers(index))
          fillCharCount(index + 1)
        }
      }

      def addToRemainingCharCount(remainingCharCount: Map[Int, Int], sticker: Array[Int], charIndex: Int) = {
        @scala.annotation.tailrec
        def add(index: Int, map: Map[Int, Int]): Map[Int, Int] = {
          if (index == sticker.length) {
            map
          } else if (sticker(index) == 0) {
            add(index + 1, map)
          } else {
            remainingCharCount.get(index) match {
              case Some(value) if index == charIndex =>
                add(index + 1, map + (index -> (value + sticker(index) - 1)))
              case Some(value) =>
                add(index + 1, map + (index -> (value + sticker(index))))
              case _ =>
                add(index + 1, map + (index -> 1))
            }
          }
        }

        add(0, remainingCharCount)
      }

      def calculate(currTarget: String, remainingCharCount: Map[Int, Int]): Int = {
        if (currTarget.isEmpty) {
          0
        } else if (minCount(currTarget) != -1) {
          minCount(currTarget)
        } else {
          minCount.get(currTarget) match {
            case Some(count) => count
            case _ =>
              val head = currTarget.head
              val output =
                remainingCharCount.get(head - 'a') match {
                  case Some(value) if value > 1 =>
                    calculate(currTarget.tail, remainingCharCount + (head - 'a' -> (value - 1)))
                  case Some(value) if value == 1 =>
                    calculate(currTarget.tail, remainingCharCount - (head - 'a'))
                  case _ =>
                    Try(
                      stickerCharCount.collect {
                        case sticker if sticker(head - 'a') > 0 =>
                          1 + calculate(currTarget.tail, addToRemainingCharCount(remainingCharCount, sticker, head - 'a'))
                      }.min) match {
                      case Success(value) => value
                      case Failure(_) => -1
                    }
                }
              minCount = minCount + (currTarget -> output)
              output
          }
        }
      }

      fillCharCount(0)
      println(stickerCharCount.map(_.mkString(",")).mkString("|"))
      val count = calculate(target, Map())
      if(count == 0) -1 else count
    }
  }

}
