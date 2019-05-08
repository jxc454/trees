package com.jcc

/**
 * Hello world!
 *
 */
object App {
  def main(args: Array[String]): Unit = {
    case class Node[T](value: T, nodes: Option[Seq[Node[T]]])

    object Node {
      def apply[T](value: T): Node[T] = Node(value, None)
      def apply[T](value: T, nodes: Seq[Node[T]]): Node[T] = Node(value, Option(nodes))
    }

    def traverse[T](node: Node[T], fn: Node[T] => Unit): Unit = {
      val q: scala.collection.mutable.Queue[Node[T]] = scala.collection.mutable.Queue()

      q += node

      // track nodes that have been touched
      val visited: scala.collection.mutable.Map[Node[T], Boolean] = scala.collection.mutable.Map()

      while (q.nonEmpty) {
        // dequeue
        val n: Node[T] = q.dequeue()

        // add child nodes
        q ++= n.nodes.getOrElse(Seq())

        if (!visited.contains(n)) {
          fn(n)
          visited += (n -> true)
        }
      }
    }

    val u = Node("u")
    val s = Node("s")
    val t = Node("t", Seq(u))
    val r = Node("r", Seq(u))
    val q = Node("q", Seq(t, r))
    val p = Node("p", Seq(q, s, t))

    val fn: Node[String] => Unit = (n: Node[String]) => println(n.value)
    traverse(p, fn)
  }
}
