import scala.annotation.tailrec

class ElaborationList {
  val beforeHead: ElaborationNode = ElaborationNode(TextModel.empty)
  val afterLast: ElaborationNode = beforeHead.addAfter(TextModel.empty)

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

  def add(index: Int, text: TextModel): ElaborationNode = {
    require(index >= 0 && index <= length, s"Index $index was out of bounds for list of length $length")
    
    if (index == length) {
      add(text)
    } else {
      @tailrec
      def findNth(node: ElaborationNode, hopsLeft: Int): ElaborationNode = {
        if (hopsLeft <= 0) {
          node
        } else {
          findNth(node.next.get, hopsLeft - 1)
        }
      }

      val newNode = findNth(beforeHead, index).addAfter(text)
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

  def isEmpty: Boolean = {
    length == 0
  }

  override def toString: String = {
    if (isEmpty) {
      "Elaboration()"
    } else {
      val builder = new StringBuilder(s"Elaboration(${head.text}")

      @tailrec
      def loop(node: ElaborationNode): Unit = {
        if (node == afterLast) {
          builder.append(")")
        } else {
          builder.append(", ")
          builder.append(node.text)
          loop(node.next.get)
        }
      }

      loop(head.next.get)

      builder.result()
    }
  }
}

object ElaborationList {
  def apply(texts: TextModel*): ElaborationList = {
    val list = new ElaborationList()
    val _ = list.addAll(texts)

    list
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

  override def toString: String = {
    s"ElaborationNode($text)"
  }
}
