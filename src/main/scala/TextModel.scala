import java.io.{File, PrintWriter}

import scala.annotation.tailrec
import scala.collection.mutable

sealed trait TextModel {
  def text: String

  def generation: Int

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

  @tailrec
  final def getRoot: TextRoot = {
    this match {
      case root: TextRoot => root
      case TextNode(_, parent, _, _) => parent.getRoot
    }
  }

  @tailrec
  final def isAncestor(text: TextModel): Boolean = {
    this match {
      case _: TextRoot => false
      case node: TextNode =>
        if (node.parent == text) {
          true
        } else if (generation <= text.generation) {
          false
        } else {
          node.parent.isAncestor(text)
        }
    }
  }

  def save(file: File): Unit = {
    val writer = new PrintWriter(file)

    writer.write(getRoot.toSaveFormat)

    writer.close()
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
  override def generation: Int = 0

  def toSaveFormat: String = {
    ???
  }
}

case class TextNode(text: String, parent: TextModel, beforeSpacing: String = "", afterSpacing: String = "") extends TextModel {
  override def generation: Int = parent.generation + 1

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
