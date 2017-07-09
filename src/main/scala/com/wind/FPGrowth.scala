package com.wind

import scala.collection.mutable
import scala.io.Source

/**
  * Created by root on 7/3/17.
  */
object FPGrowth extends App{

  val is = ClassLoader.getSystemResourceAsStream("data1.csv")
  var lines = Source.fromInputStream(is).getLines.toArray
  for (line <- lines) println(line)

  // support
  val support = 2
  // frequency of each item
  val itemCount = mutable.Map[String, Int]()
  lines.foreach( line => {
    line.split(",").foreach( item => {
      if (itemCount.contains(item)) itemCount(item) +=1 else itemCount(item) = 1
    })
  })

  // remove item whose count < support
  val filted_itemCount = itemCount.filter(v => v._2 >= support).toSeq.sortBy(v => -v._2)
  // ordered items
  val filted_items = filted_itemCount.map(v => v._1)
  lines = lines.map(line => line.split(",").filter(item => filted_items.contains(item)).mkString(","))

  // FPgrowth build first tree
  val node = buildTree(lines)
  // start mining....

  val itemPathPairs = filted_items.reverse.map(item => {
    // mining one by one
    val frequentPath = fpgrowthMining(node, item)
    (item -> frequentPath)
  })
  itemPathPairs foreach { fp =>
    println("---------------------" + fp._1)
    fp._2.foreach(path => println(path.mkString(",")))
    println("---------------------")
  }

  def fpgrowthMining(node: FPNode, item: String): Array[Array[String]] = {
    val frequentItems = mutable.ArrayBuffer.empty[Array[String]]
    var tmpNode = cutTreeUnderSupport(item, node)
    while (!tmpNode.isMaxFrequent) {
      val itemsCnt = getItemsCount(tmpNode)
      val cutItem = itemsCnt.toSeq.sortBy(v => v._2).head._1
      tmpNode = cutTreeUnderSupport(cutItem, node)
      frequentItems.append(Array(cutItem))
    }
    frequentItems.append(tmpNode.maxFrequentPath)
    frequentItems.toArray
  }

  def cutTreeUnderSupport(item: String, node: FPNode): FPNode = {
    var n = cutTree(item, node)
    var itemsCnt = getItemsCount(n)
    while (itemsCnt.values.toSeq.exists(_ < support)) {
      val unsupportItem = itemsCnt.filter(m => m._2 < support).keys.head
      n = cutTree(unsupportItem, n, false)
      itemsCnt = getItemsCount(n)
    }
    n
    // choose
  }

  // choose an item to do mining
  def cutTree(item: String, node: FPNode, remove: Boolean = true): FPNode = {
    val children = node.child.map(child => {
      if (child.name equals item) FPNode("target", 0, Array())
      else if (child.child.length == 0 && remove) FPNode("remove", 0, Array())
      else if (child.child.length == 0 && !remove) child
      else cutTree(item, child, remove)
    })
    if (children.filter(_.name equals "remove").length == children.length && remove) FPNode("remove", 0, Array())
    else FPNode(node.name, node.cnt, children.filter(node => !Seq("remove","target").contains(node.name)))
  }

  def getItemsCount(node: FPNode): Map[String, Int] = {
    val itemCnt = if (node.name equals "root") Map[String, Int]() else Map(node.name -> node.cnt)
    val childCnt = (node.child.map(n => {
      getItemsCount(n)
    }) :+ itemCnt).reduce( (a,b) => a ++ b.map{ case (k,v) => k -> (v + a.getOrElse(k, 0))})
    childCnt
  }

  println("Finish!")

  def buildTree(lines: Array[String]): FPNode = {
    val root: FPNode = lines.foldLeft[FPNode](FPNode("root", 0, Array()))((node, line)=>{
      val items = line.split(",")
      recursiveInsert(node, items)
    })
    root
  }

  def recursiveInsert(node: FPNode, items: Array[String]): FPNode = {
    if(items.length > 0){
      val repeat = node.child.map(_.name).intersect(items)
      val (rChildNode,rItems) = if (repeat.length > 0) {
        (node.child.find(_.name.equals(repeat.head)).get.addCount(), items.filter(!_.equals(repeat.head)))
      } else {
        val max_frequent_item = filted_items.intersect(items).head
        (FPNode(max_frequent_item, 1, Array()), items.filter(!_.equals(max_frequent_item)))
      }
      val childNode = recursiveInsert(rChildNode, rItems)
      if (node.child.exists(_.name.equals(childNode.name))){
        val index = node.child.indexOf(node.child.find(_.name.equals(childNode.name)).get)
        node.child.update(index, childNode)
        node
      } else {
        node.addChild(childNode)
      }
    } else {
      node
    }
  }
}

case class FPNode(name: String, cnt: Int, child: Array[FPNode]) {

  def addCount() : FPNode = {
    FPNode(name, cnt+1, child)
  }

  def addChild(node: FPNode) : FPNode = {
    FPNode(name, cnt, child :+ node)
  }

  def isMaxFrequent(): Boolean = {
    var tmpNode = this
    while (tmpNode.child.length == 1) tmpNode = tmpNode.child(0)
    if (tmpNode.child.length == 0 ) true else false
  }

  def maxFrequentPath(): Array[String] = {
    val a = mutable.ArrayBuffer.empty[String]
    var tmpNode = this
    while (tmpNode.child.length == 1) {
      tmpNode = tmpNode.child(0)
      a.append(tmpNode.name)
    }
    a.toArray

  }

}