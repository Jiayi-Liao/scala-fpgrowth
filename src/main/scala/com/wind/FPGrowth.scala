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
  val filted_itemCount = itemCount.filter(v => v._2 > support).toSeq.sortBy(v => -v._2)
  // ordered items
  val filted_items = filted_itemCount.map(v => v._1)
  lines = lines.map(line => line.split(",").filter(item => filted_items.contains(item)).mkString(","))

  // FPgrowth
  val node = buildTree(lines)
  // start mining....
  filted_items.reverse.foreach(item => {
    // mining one by one
    var tmpNode = node
    val frequent_path = Array[Array[String]]()
    while(!isMaxFrequent(tmpNode)){
      val cut_nodes = cutTree(item, tmpNode)
      val cut_lines = flatNodes(cut_nodes)
      tmpNode = buildTree(cut_lines.map(_.mkString(",")))
    }

  })

  def cutTree(item: String, node: FPNode): Array[Array[FPNode]] = {
    val paths = mutable.ArrayBuffer.empty[Array[FPNode]]
    if (node.name.equals(item)) {
      Array(Array(node))
    } else {
      node.child.foreach(nd => {
        val result = cutTree(item, nd)
        if (result.nonEmpty) {
          result.foreach(path => {
            val append_path = path ++ Array(node)
            paths += append_path
          })
        }
      })
      paths.toArray
    }
  }



  println("Finish!")

  def isMaxFrequent(node: FPNode) : Boolean = {
    var n = node
    while(n.child.length == 1)
      n = n.child(0)
    if(n.child.length > 0) false else true
  }

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
}