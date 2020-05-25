import java.io.File

import scalafx.beans.property.StringProperty

import scala.collection.mutable

case class ElaborationState(buffer: mutable.ArrayBuffer[TextModel]) {
  val text: StringProperty = StringProperty("")

  var nodeBeingEdited: Option[(TextModel, Int)] = None

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
    val nodeAtIndex = buffer.dropWhile { node =>
      val nodeLen = node.untrimmedText.length
      if (index <= accum + nodeLen) {
        false
      } else {
        accum += nodeLen
        indexInBuffer += 1
        true
      }
    }.head

    (nodeAtIndex, indexInBuffer, index - accum)
  }

  def expand(index: Int): Unit = {
    val (nodeToExpand, indexInBuffer, _) = getNodeAtCharIndex(index)

    expand(nodeToExpand, indexInBuffer)
  }

  def expand(nodeToExpand: TextModel, indexInBuffer: Int): Unit = {
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
      case TextNode(_, parent, _, _, _) => parent
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

  def getParentStr(index: Int): String = {
    getNodeAtCharIndex(index) match {
      case (TextRoot(_), _, _) => ""
      case (TextNode(_, parent, _, _, _), _, _) => parent.text
    }
  }

  def selectParentalRelatives(index: Int): (Int, Int) = {
    val (nodeSelected, _, _) = getNodeAtCharIndex(index)
    nodeSelected match {
      case TextRoot(_) => (0, text().length)
      case TextNode(_, parent, _, _, _) =>
        var startIndex = 0
        val firstRelative = buffer.toVector.find { text =>
          startIndex += text.untrimmedText.length
          text.isAncestor(parent)
        }.get
        startIndex -= firstRelative.untrimmedText.length

        var endIndex = 0
        val lastRelative = buffer.reverseIterator.toVector.find { text =>
          endIndex += text.untrimmedText.length
          text.isAncestor(parent)
        }.get
        endIndex -= lastRelative.untrimmedText.length
        endIndex = text().length - endIndex

        (startIndex, endIndex - startIndex)
    }
  }

  def toggleEditingForNode(index: Int, currentTextFunc: () => String, selectFunc: (Int, Int) => Unit): Boolean = {
    nodeBeingEdited match {
      case Some((node, index)) =>
        val expectedPreText = buffer.take(index).map(_.untrimmedText).mkString("")
        val expectedPostText = buffer.drop(index + 1).map(_.untrimmedText).mkString("")

        val currentText = currentTextFunc()

        lazy val err = new RuntimeException("You have edited outside of bounds. Revert illegal changes and try again (illegal change should be highlighted).")

        val relevantText = if (currentText.startsWith(expectedPreText)) {
          if (currentText.endsWith(expectedPostText)) {
            currentText.drop(expectedPreText.length).dropRight(expectedPostText.length).trim
          } else {
            val beforeEnd = currentText.zipWithIndex.reverseIterator.zip(expectedPostText.reverseIterator).find { case ((a, _), b) => a != b }.get._1._2
            val backWord = currentText.reverseIterator.drop(currentText.length - beforeEnd - 1).takeWhile(!_.isWhitespace)
            selectFunc(beforeEnd - backWord.length + 1, beforeEnd + 1)
            throw err
          }
        } else {
          val start = currentText.zip(expectedPreText).zipWithIndex.find { case ((a, b), _) => a != b }.get._2
          val word = currentText.drop(start).takeWhile(!_.isWhitespace)
          selectFunc(start, start + word.length)
          throw err
        }

        node match {
          case node: TextNode =>
            if (relevantText.startsWith(TextNode.editBegin)) {
              if (relevantText.endsWith(TextNode.editEnd)) {
                node.text = relevantText
                  .drop("--- EDIT BEGIN ---".length)
                  .dropRight("--- EDIT END ---".length)
                  .trim()

                node.beingEdited = false
              } else {
                val iter = currentText.zipWithIndex.reverse.drop(expectedPostText.length).dropWhile(_._1.isWhitespace)
                val beforeEnd = iter.head._2
                val backWord = iter.takeWhile(!_._1.isWhitespace)
                selectFunc(beforeEnd - backWord.length, beforeEnd + 1)
                throw err
              }
            } else {
              val iter = currentText.zipWithIndex.drop(expectedPreText.length).dropWhile(_._1.isWhitespace)
              val start = iter.head._2
              val word = iter.takeWhile(!_._1.isWhitespace)
              selectFunc(start, start + word.length)
              throw err
            }
          case root: TextRoot => root.title = relevantText
        }
        nodeBeingEdited = None
        computeCurrentText()
        true
      case None =>
        val (nodeSelected, indexInBuffer, _) = getNodeAtCharIndex(index)
        nodeSelected match {
          case root: TextRoot =>
            nodeBeingEdited = Some((root, indexInBuffer))
            true
          case node: TextNode =>
            node.beingEdited = true
            nodeBeingEdited = Some((node, indexInBuffer))
            computeCurrentText()
            true
        }
    }
  }

  def elaborate(start: Int, end: Int): Boolean = {
    val (nodeSelected, indexInBuffer, depthInNode) = getNodeAtCharIndex(start)
    nodeSelected match {
      case _: TextRoot => false
      case node: TextNode =>
        val selectionLength = end - start
        if (selectionLength <= nodeSelected.untrimmedText.length - depthInNode) {
          val (beforeNode, newNode, afterNode) = node.elaborate("Elaborate Here (replace this)", depthInNode, depthInNode + selectionLength)
          buffer.remove(indexInBuffer)
          buffer.insertAll(indexInBuffer, Vector(beforeNode, newNode, afterNode))
          expand(newNode, indexInBuffer + 1)
          true
        } else false
    }
  }

  def getRoot: TextRoot = {
    buffer.head.getRoot
  }

  def save(file: File): Unit = {
    getRoot.save(file)
  }

  def reset(root: TextRoot): Unit = {
    buffer.clearAndShrink()
    buffer.addOne(root)
    computeCurrentText()
  }

  def resetAndOpen(file: File): Unit = {
    reset(TextRoot.fromFile(file))
  }
}

object ElaborationState {
  def apply(texts: TextModel*): ElaborationState = {
    ElaborationState(mutable.ArrayBuffer(texts:_*))
  }
}