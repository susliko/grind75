class Node(var _value: Int) {
  var value: Int = _value
  var neighbors: List[Node] = List()
}

def dump(n: Node): Unit =
  val visited: scala.collection.mutable.Set[Int] =
    scala.collection.mutable.Set.empty
  def go(n: Node): Unit =
    if visited.contains(n.value) then return
    println(
      s"Node ${n.value}. Neighbors: ${n.neighbors.map(_.value).mkString("[", ",", "]")}"
    )
    visited += n.value
    n.neighbors.foreach(go)
  go(n)

object CloneGraph:
  object Solution {
    def cloneGraph(graph: Node): Node = 
      if graph == null then null
      else clone(graph, Map.empty)._1

    def clone(original: Node, nodes: Map[Int, Node]): (Node, Map[Int, Node]) =
      val n = new Node(original.value)
      val done = nodes + (original.value -> n)
      val newDone = original.neighbors.foldLeft(done): (done, neigh) =>
        val (neighClone, newDone) =
          done.get(neigh.value) match
            case Some(neighClone) => (neighClone, done)
            case None             => clone(neigh, done)
        n.neighbors = neighClone :: n.neighbors
        newDone
      (n, newDone)
  }

def makeGraph(input: List[List[Int]]): Node =
  assert(input.length > 0)
  val nodes = input.toArray.zipWithIndex.map: (n, i) =>
    new Node(i + 1)

  nodes
    .zip(input)
    .foreach: (n, l) =>
      n.neighbors = l.map(i => nodes(i - 1))
  nodes.head

@main def cloneGraphMain =
  import CloneGraph.*
  dump(makeGraph(List(List())))
  println("")

  // val g2 = makeGraph(List(List(2, 4), List(1, 3), List(2, 4), List(1, 3)))
  // dump(g2)
  // dump(Solution.cloneGraph(g2))
  // println("")
  
  // val g3 = makeGraph(List(List(2, 4), List(1, 3, 4), List(2, 4), List(1, 3, 4)))
  // dump(g3)
  // dump(Solution.cloneGraph(g3))
  // println("")
