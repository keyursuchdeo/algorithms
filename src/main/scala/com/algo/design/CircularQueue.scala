package com.algo.design

object CircularQueue extends App {
  class MyCircularQueue(_k: Int) {

    val length = _k
    val queue: Array[Int] = Array.fill[Int](length)(-1)
    var headPos: Int = -1
    var lastPos: Int = -1

    def enQueue(value: Int): Boolean = {
      if(isFull()) {
        false
      } else {
        if(isEmpty()) {
          queue(0) = value
          headPos = 0
          lastPos = 0
        } else {
          queue((lastPos + 1) % length) = value
          lastPos = (lastPos + 1) % length
        }
        true
      }
    }

    def deQueue(): Boolean = {
      if(isEmpty()) {
        false
      } else {
        queue(headPos) = -1
        headPos = (headPos + 1) % length
        true
      }
    }

    def Front(): Int = {
      queue(headPos)
    }

    def Rear(): Int = {
      queue(lastPos)
    }

    def isEmpty(): Boolean = {
      headPos == -1 || queue(headPos) == -1
    }

    def isFull(): Boolean = {
      if(headPos < lastPos) {
        headPos == 0 && lastPos == length - 1
      } else if (headPos > lastPos) {
        headPos - lastPos == 1
      } else {
        false
      }
    }

  }

  /**
   * Your MyCircularQueue object will be instantiated and called as such:
   * var obj = new MyCircularQueue(k)
   * var param_1 = obj.enQueue(value)
   * var param_2 = obj.deQueue()
   * var param_3 = obj.Front()
   * var param_4 = obj.Rear()
   * var param_5 = obj.isEmpty()
   * var param_6 = obj.isFull()
   */
}
