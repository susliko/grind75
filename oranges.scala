import scala.util.boundary
object OrangesSolution:
  /*
  0 - empty,
  1 - fresh,
  2 - rotten
   */
  import scala.util.boundary, boundary.break
  object Solution:
    def orangesRotting(grid: Array[Array[Int]]): Int =
      var roots = List.empty[(Int, Int)]
      val aux = Array.fill(grid.length)(Array.fill(grid(0).length)(-1))
      for i <- 0 until grid.length do
        for j <- 0 until grid(i).length do
          grid(i)(j) match
            case 1 => aux(i)(j) = Int.MaxValue
            case 2 =>
              roots = (i, j) :: roots
              aux(i)(j) = 0
            case _ => ()
      roots.foreach: (ri, rj) =>
        def go(i: Int, j: Int, d: Int): Unit =
          val c = aux(i)(j)
          if c >= 0 && d <= c then
            aux(i)(j) = d
            if i > 0 then go(i - 1, j, d + 1)
            if i < grid.length - 1 then go(i + 1, j, d + 1)
            if j > 0 then go(i, j - 1, d + 1)
            if j < grid(0).length - 1 then go(i, j + 1, d + 1)
        go(ri, rj, 0)

      boundary:
        var outcome = 0
        for i <- 0 until aux.length do
          for j <- 0 until aux(i).length do
            val c = aux(i)(j)
            if c == Int.MaxValue then break(-1)
            else if c > outcome then outcome = c
        outcome

@main def orangesMain =
  import OrangesSolution.*
  println(
    Solution.orangesRotting(
      Array(Array(2, 1, 1), Array(1, 1, 0), Array(0, 1, 1))
    )
  )
  println(
    Solution.orangesRotting(
      Array(Array(2, 1, 1), Array(0, 1, 1), Array(1, 0, 1))
    )
  )
  println(Solution.orangesRotting(Array(Array(0, 2))))
  println(Solution.orangesRotting(Array.empty))
  println(Solution.orangesRotting(Array(Array(0))))
