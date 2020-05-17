sealed trait TextModel {
  def text: String

  val children: ElaborationList = ElaborationList()

  def addChild(child: TextNode): Unit = {
    child.elaborationNode = children.add(child)
  }

  def addChildren(children: TextNode*): Unit = {
    children.foreach { child =>
      addChild(child)
    }
  }

  def elaborate(elaboration: String): TextNode = {
    val child = TextNode(elaboration, this)
    addChild(child)

    child
  }

  override def toString: String = {
    this match {
      case root: TextRoot => s"Root($text, ${root.children})"
      case node: TextNode => s"Node($text, ${node.children})"
    }
  }
}

case class TextRoot(title: String) extends TextModel {
  override def text: String = title
}

case class TextNode(text: String, parent: TextModel) extends TextModel {
  var elaborationNode: ElaborationNode = _

  def elaborate(elaboration: String, from: Int, to: Int): (TextNode, TextNode, TextNode) = {
    val (before, elaboratedWithAfter) = text.splitAt(from)
    val (elaborated, after) = elaboratedWithAfter.splitAt(to - from)

    val newNode = TextNode(elaborated.trim, parent)
    val beforeNode = TextNode(before.trim, parent)
    val afterNode = TextNode(after.trim, parent)

    val newElaborationNode = elaborationNode.replace(newNode)
    val beforeElaborationNode = newElaborationNode.addBefore(beforeNode)
    val afterElaborationNode = newElaborationNode.addAfter(afterNode)

    newNode.elaborationNode = newElaborationNode
    beforeNode.elaborationNode = beforeElaborationNode
    afterNode.elaborationNode = afterElaborationNode

    newNode.elaborate(elaboration)

    (beforeNode, newNode, afterNode)
  }
}

object TextModel {
  val empty: TextModel = TextRoot("")

  val example: TextModel = {
    val root = TextRoot("Rabbit")
    val child = root.elaborate("A Furry Creature")
    val _ = child.elaborate("Adjective: covered in hair.", from = 2, to = 7)

    root
  }
}
