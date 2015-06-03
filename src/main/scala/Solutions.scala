
object Solutions extends App {

  case class Tree(value: Int, left: Option[Tree], right: Option[Tree])

  val myTree: Tree =
  Tree(1,
    Some(Tree(3,
      Some(Tree(2,
        Some(Tree(9, None, None)),
        Some(Tree(6, None, None))
      )),
      Some(Tree(4,
        None,
        Some(Tree(8, None, None))
      ))
    )),
    Some(Tree(5,
      None,
      Some(Tree(7, None, None))
    ))
  )

  def TreePrinter(rootNode: Tree): Unit = {

    var temp     = List.empty[Tree]
    var list     = List.empty[Tree]
    var children = List.empty[Tree]
    var line     = ""

    list = list.+:(rootNode)

    while(!list.isEmpty) {

      while(!list.isEmpty) {
        val node = list.head

        list = list.drop(1)

        line = line + node.value + " "

        node.left .map ( node =>
          children = children.:+(node)
        )

        node.right.map ( node =>
          children = children.:+(node)
        )
      }

      println(line)

      line = ""

      temp = children

      children = list

      list = temp
    }
  }

  TreePrinter(myTree)

}
