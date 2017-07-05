package com.wind

import scala.collection.mutable
import scala.io.Source

/**
  * Created by root on 7/3/17.
  */
object FPGrowth extends App{

  val is = ClassLoader.getSystemResourceAsStream("data1.csv")
  val lines = Source.fromInputStream(is).getLines.toArray
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
  val filted_itemCount = itemCount.filter(v => v._2 > support)

  // FPgrowth
  val node = buildTree(lines.toArray)
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
        (FPNode(items.head, 1, Array()), items.tail)
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