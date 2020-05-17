import scala.collection.mutable

sealed trait TextModel {
  def text: String

  def untrimmedText: String

  var children: mutable.ArrayBuffer[TextNode] = mutable.ArrayBuffer.empty

  def addChild(child: TextNode): Unit = {
    children.append(child)
  }

  def addChildren(children: TextNode*): Unit = {
    children.foreach { child =>
      addChild(child)
    }
  }

  def replaceChild(child: TextNode, replacements: Vector[TextNode]): Unit = {
    val index = children.indexOf(child)
    children.remove(index)
    children.insertAll(index, replacements)
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
  override def untrimmedText: String = title
}

case class TextNode(text: String, parent: TextModel, beforeSpacing: String = "", afterSpacing: String = "") extends TextModel {
  override def untrimmedText: String = beforeSpacing ++ text ++ afterSpacing

  def elaborate(elaboration: String, from: Int, to: Int): (TextNode, TextNode, TextNode) = {
    val (before, elaboratedWithAfter) = text.splitAt(from)
    val (elaborated, after) = elaboratedWithAfter.splitAt(to - from)

    val newNode = TextNode.fromUntrimmed(elaborated, parent)
    val beforeNode = TextNode.fromUntrimmed(before, parent)
    val afterNode = TextNode.fromUntrimmed(after, parent)

    parent.replaceChild(this, Vector(beforeNode, newNode, afterNode))

    newNode.elaborate(elaboration)

    (beforeNode, newNode, afterNode)
  }
}

object TextNode {
  def fromUntrimmed(text: String, parent: TextModel): TextNode = {
    val trimmedText = text.trim
    val beforeSpacing = text.takeWhile(_ != trimmedText.head)
    val afterSpacing = text.reverse.takeWhile(_ != trimmedText.last).reverse

    TextNode(trimmedText, parent, beforeSpacing, afterSpacing)
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
