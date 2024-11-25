//> using scala 3.5.0

import scala.util.boundary
import scala.util.boundary.break

object Solution:
  def twoSum(nums: Array[Int], target: Int): Array[Int] =
    boundary:
      val ns = nums.zipWithIndex.sortInPlaceWith { case (a, b) => a._1 < b._1 }
      var left = 0
      var right = ns.length - 1
      while left < right do
        val x = ns(left)._1 + ns(right)._1
        if x < target then left += 1
        else if x > target then right -= 1
        else break(Array(ns(left)._2, ns(right)._2).sorted)
      Array.empty

@main def twosum =
  println(Solution.twoSum(Array(2, 7, 11, 15), 9).toList)
  println(Solution.twoSum(Array(3, 2, 4), 6).toList)
