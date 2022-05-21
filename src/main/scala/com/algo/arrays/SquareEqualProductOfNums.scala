package com.algo.arrays

object SquareEqualProductOfNums extends App {

  object Solution {
    def numTriplets(nums1: Array[Int], nums2: Array[Int]): Int = {
      val num1Squares = nums1.map(num => num * num)
      val num2Squares = nums2.map(num => num * num)

      def fillProducts(array: Array[Int]): Array[Array[Int]] = {
        @scala.annotation.tailrec
        def fill(row: Int, col: Int, products: Array[Array[Int]]): Array[Array[Int]] = {
          if (row == array.length) {
            products
          } else if (col == array.length) {
            fill(row + 1, row + 1, products)
          } else {
            products(row)(col) = array(row) * array(col)
            fill(row + 1, col + 1, products)
          }
        }

        fill(0, 0, Array.ofDim[Int](array.length, array.length))
      }

      val num1Products = fillProducts(nums1)
      val num2Products: Array[Array[Int]] = fillProducts(nums2)

      num1Squares.map(sq => {
        num2Products.flatten.count(_ == sq)
      }).sum +
        num2Squares.map(sq => {
          num1Products.flatten.count(_ == sq)
        }).sum
    }
  }

}
