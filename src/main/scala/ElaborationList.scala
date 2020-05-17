import scalafx.beans.property.StringProperty

class ElaborationList extends IterableOnce[ElaborationNode] {
  val beforeHead: ElaborationNode = ElaborationNode(TextModel.empty)
  val afterLast: ElaborationNode = beforeHead.addAfter(TextModel.empty)

  override def iterator: Iterator[ElaborationNode] = ElaborationListIterator(beforeHead, afterLast)

  var length: Int = 0

  def headOption: Option[ElaborationNode] = {
    if (length == 0) {
      None
    } else {
      beforeHead.next
    }
  }

  def head: ElaborationNode = headOption.get

  def lastOption: Option[ElaborationNode] = {
    if (length == 0) {
      None
    } else {
      afterLast.prev
    }
  }

  def last: ElaborationNode = lastOption.get

  def getNode(index: Int): ElaborationNode = {
    iterator.drop(index).next()
  }

  def add(index: Int, text: TextModel): ElaborationNode = {
    require(index >= 0 && index <= length, s"Index $index was out of bounds for list of length $length")
    
    if (index == length) {
      add(text)
    } else {
      val nodeAfter = getNode(index)
      val newNode = nodeAfter.addBefore(text)
      length += 1
      
      newNode
    }
  }
  
  def add(text: TextModel): ElaborationNode = {
    length += 1

    afterLast.addBefore(text)
  }

  def addAll(texts: Seq[TextModel]): Vector[ElaborationNode] = {
    texts.map(add).toVector
  }

  def addAllAfterNode(nodeBefore: ElaborationNode, subElaboration: ElaborationList): Unit = {
    val nodeAfter = nodeBefore.next.get

    val subHead = subElaboration.head
    val subLast = subElaboration.last

    nodeBefore.next = Some(subHead)
    subHead.prev = Some(nodeBefore)

    nodeAfter.prev = Some(subLast)
    subLast.next = Some(nodeAfter)

    length += subElaboration.length
  }

  def remove(node: ElaborationNode): Unit = {
    length -= 1
    node.remove()
  }

  def remove(index: Int): ElaborationNode = {
    val node = getNode(index)
    remove(node)
    node
  }

  def isEmpty: Boolean = {
    length == 0
  }

  override def toString: String = {
    s"Elaboration(${iterator.map(_.text).mkString(", ")})"
  }
}

object ElaborationList {
  def apply(texts: TextModel*): ElaborationList = {
    val list = new ElaborationList()
    val _ = list.addAll(texts)

    list
  }
}

case class ElaborationListIterator(private var curr: ElaborationNode, afterLast: ElaborationNode) extends Iterator[ElaborationNode] {
  override def hasNext: Boolean = {
    !curr.next.contains(afterLast)
  }

  override def next(): ElaborationNode = {
    if (hasNext) {
      curr = curr.next.get
      curr
    } else {
      throw new IllegalStateException("Cannot call next() when hasNext is false.")
    }
  }
}

case class ElaborationNode(text: TextModel, var prev: Option[ElaborationNode] = None, var next: Option[ElaborationNode] = None) {
  def addAfter(text: TextModel): ElaborationNode = {
    val newNode = ElaborationNode(text, Some(this), next)
    next.foreach(_.prev = Some(newNode))
    next = Some(newNode)
    
    newNode
  }
  
  def addBefore(text: TextModel): ElaborationNode = {
    val newNode = ElaborationNode(text, prev, Some(this))
    prev.foreach(_.next = Some(newNode))
    prev = Some(newNode)
    
    newNode
  }
  
  def replace(text: TextModel): ElaborationNode = {
    val newNode = ElaborationNode(text, prev, next)
    prev.foreach(_.next = Some(newNode))
    next.foreach(_.prev = Some(newNode))
    
    newNode
  }

  def remove(): ElaborationNode = {
    prev.foreach(_.next = next)
    next.foreach(_.prev = prev)

    this
  }

  override def toString: String = {
    s"ElaborationNode($text)"
  }
}

case class ElaborationState(list: ElaborationList) {
  val text: StringProperty = StringProperty("")

  def computeCurrentText: String = {
    val builder = new StringBuilder()

    list.iterator.foreach(node => builder.append(node.text.untrimmedText))

    val result = builder.result()

    text.value = result

    result
  }

  val _ = computeCurrentText

  def getNodeAtCharIndex(index: Int): (ElaborationNode, Int) = {
    var accum: Int = 0
    val nodeToExpand = list.iterator.dropWhile { node =>
      val nodeLen = node.text.untrimmedText.length
      if (index <= accum + nodeLen) {
        false
      } else {
        accum += nodeLen
        true
      }
    }.next()

    (nodeToExpand, index - accum)
  }

  def expand(index: Int): Unit = {
    val (nodeToExpand, _) = getNodeAtCharIndex(index)

    list.addAllAfterNode(nodeToExpand, nodeToExpand.text.children)
    list.remove(nodeToExpand)

    computeCurrentText
  }

  def select(index: Int): (Int, Int) = {
    val (nodeSelected, indexDepthInNode) = getNodeAtCharIndex(index)
    val nodeStart = index - indexDepthInNode

    (nodeStart, nodeSelected.text.untrimmedText.length)
  }
}