package com.wind

import scala.io.Source

/**
  * Created by root on 7/3/17.
  */
object FPGrowth extends App{

  val lines = Source.fromResource("data1.csv").getLines
  for (line <- lines) println(line)




}



case class FPNode(name: String, cnt: Int, child: FPNode)