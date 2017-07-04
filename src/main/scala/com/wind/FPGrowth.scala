package com.wind

import scala.collection.mutable
import scala.io.Source

/**
  * Created by root on 7/3/17.
  */
object FPGrowth extends App{

  val is = ClassLoader.getSystemResourceAsStream("data1.csv")
  val lines = Source.fromInputStream(is).getLines
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

  // remove item whose count < 2
  val filted_itemCount = itemCount.filter(v => v._2 > 2)

  // FPgrowth



  def buildTree(lines: Array[String]): FPNode = {
    val root = FPNode("root", 0, Array())
    lines.foreach( line => {
      var tmpNode = root
      var items = line.split(",")
      while (items.length > 0){
        val repeat = tmpNode.child.map(_.name).intersect(items)
        items = if (repeat.length > 0) {
          tmpNode.child.find(_.name.equals(repeat.head))
          items.filter(_.equals(repeat.head))
        } else {
          items.tail
        }
      }
    })
  }
}

case class FPNode(name: String, cnt: Int, child: Array[FPNode])