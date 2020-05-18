import scalafx.beans.property.StringProperty

import scala.collection.mutable

case class ElaborationState(buffer: mutable.ArrayBuffer[TextModel]) {
  val text: StringProperty = StringProperty("")

  def computeCurrentText(): String = {
    val builder = new StringBuilder()

    buffer.foreach(node => builder.append(node.untrimmedText))

    val result = builder.result()

    text.value = result

    result
  }

  locally {
    val _ = computeCurrentText()
  }

  def getNodeAtCharIndex(index: Int): (TextModel, Int, Int) = {
    var accum: Int = 0
    var indexInBuffer: Int = 0
    val nodeToExpand = buffer.dropWhile { node =>
      val nodeLen = node.untrimmedText.length
      if (index <= accum + nodeLen) {
        false
      } else {
        accum += nodeLen
        indexInBuffer += 1
        true
      }
    }.head

    (nodeToExpand, indexInBuffer, index - accum)
  }

  def expand(index: Int): Unit = {
    val (nodeToExpand, indexInBuffer, _) = getNodeAtCharIndex(index)

    if (nodeToExpand.children.isEmpty) {
      throw new IllegalArgumentException(s"Cannot expand un-elaborated node: ${nodeToExpand.text}")
    }

    buffer.remove(indexInBuffer)
    buffer.insertAll(indexInBuffer, nodeToExpand.children)

    computeCurrentText()
  }

  def compress(index: Int): Unit = {
    val (nodeToCompressFrom, _, _) = getNodeAtCharIndex(index)
    val parent = nodeToCompressFrom match {
      case TextRoot(title) => throw new IllegalArgumentException(s"Cannot compress from root: $title")
      case TextNode(_, parent, _, _) => parent
    }
    val (_, startIndex) = buffer.toVector.zipWithIndex.find(_._1.isAncestor(parent)).get
    val (_, endIndex) = buffer.toVector.zipWithIndex.findLast(_._1.isAncestor(parent)).get

    buffer.remove(startIndex, endIndex - startIndex + 1)
    buffer.insert(startIndex, parent)

    computeCurrentText()
  }

  def select(index: Int): (Int, Int) = {
    val (nodeSelected, _, indexDepthInNode) = getNodeAtCharIndex(index)
    val nodeStartIndex = index - indexDepthInNode

    (nodeStartIndex, nodeSelected.untrimmedText.length)
  }
}

object ElaborationState {
  def apply(texts: TextModel*): ElaborationState = {
    ElaborationState(mutable.ArrayBuffer(texts:_*))
  }
}