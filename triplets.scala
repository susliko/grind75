object TripletsSolution:
  object Solution:
    def threeSum(nums: Array[Int]): List[List[Int]] =
      val sorted = nums.sorted
      var triplets = List.empty[List[Int]]
      for i <- 0 to sorted.size - 3 do
        var l = i + 1
        var r = nums.size - 1
        while l < r do
          (sorted(l) + sorted(r) + sorted(i)).sign match
            case -1 => l += 1
            case 1  => r -= 1
            case _ =>
              triplets = List(sorted(i), sorted(l), sorted(r)) :: triplets
              l += 1
              r -= 1
      triplets.toSet.toList

@main def tripletsMain =
  import TripletsSolution.*
  println(Solution.threeSum(Array(-1, 0, 1, 2, -1, -4)))
  println(Solution.threeSum(Array(0, 1, 1)))
  println(Solution.threeSum(Array(0, 0, 0)))
  println(Solution.threeSum(Array()))
  println(Solution.threeSum(Array(-5, 2, 3, -4, 1)))
