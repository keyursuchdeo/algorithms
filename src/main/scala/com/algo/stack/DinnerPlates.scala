package com.algo.stack

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class DinnerPlates(_capacity: Int) {

  object FreeStackMinOrdering extends Ordering[(Int, Int)] {
    override def compare(x: (Int, Int), y: (Int, Int)): Int = {
      val (xIndex, xCurrCap) = x
      val (yIndex, yCurrCap) = y
      if(yCurrCap == xCurrCap) {
        yIndex compare xIndex
      } else  {
        yCurrCap compare xCurrCap
      }
    }
  }

  var freeStackQueue: mutable.PriorityQueue[(Int, Int)] = mutable.PriorityQueue.empty[(Int, Int)](FreeStackMinOrdering)
  var stacks: ArrayBuffer[mutable.Seq[Int]] = new ArrayBuffer[mutable.Seq[Int]]()
  var stackCaps: ArrayBuffer[Int] = new ArrayBuffer[Int]()
  var firstFreeStack: Int = 0
  var lastFullStack: Int = -1

  def push(`val`: Int) {
    val stack: mutable.Seq[Int] = stacks(firstFreeStack)
    stacks(firstFreeStack) = `val` +: stack
    stackCaps(firstFreeStack) = 1 + stackCaps(firstFreeStack)
    if(stackCaps(firstFreeStack) == _capacity) {
      firstFreeStack = firstFreeStack + 1
      lastFullStack = lastFullStack + 1
    }
  }

  def pop(): Int = {
    if(lastFullStack == -1) {
      -1
    } else {
      val stack: mutable.Seq[Int] = stacks(lastFullStack)
      val head = stack.head
      stacks(lastFullStack) = stack.tail
      stackCaps(lastFullStack) = 1 - stackCaps(lastFullStack)
      if(stack.tail.isEmpty) {
        if(firstFreeStack == lastFullStack - 1) {
          firstFreeStack = firstFreeStack - 1
        }
        lastFullStack = lastFullStack - 1
      }
      head
    }
  }

  def popAtStack(index: Int): Int = {
    if(index == lastFullStack) {
      pop()
    } else {
      val stack: mutable.Seq[Int] = stacks(index)
      stack.headOption match {
        case None => -1
        case Some(head) =>
          stacks(index) = stack.tail
          stackCaps(index) = 1 - stackCaps(index)
          if(index < firstFreeStack) {

          }
          head
      }
    }
  }

}
