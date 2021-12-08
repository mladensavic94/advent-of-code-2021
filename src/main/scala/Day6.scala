package io.github.mladensavic94

import java.util
import scala.collection.mutable.ArrayBuffer


object Day6 {
  def main(args: Array[String]): Unit = {
//    task1()
    task2()
  }

  def task1(): Unit = {
    val list = Util.readFile("src/main/resources/day6.txt").head.split(",")
    var fish = list.map(_.toInt)

    for (i <- 1 to 80) {
      var counter = 0
      for (j <- fish.indices) {
        if (fish(j) == 0) {
          fish(j) = 6
          counter += 1
        } else {
          fish(j) -= 1
        }
      }
      fish = fish.appendedAll(Array.fill(counter)(8))
    }
    println(fish.length)
  }

  def task2(): Unit = {
    val list = Util.readFile("src/main/resources/dummy.txt").head.split(",").map(_.toInt)
    var fish = Array.fill(Int.MaxValue/2)(0)
    var arrayCounter = 0
    for(l <- list){
      fish(arrayCounter) = l
      arrayCounter += 1
    }
    for (i <- 1 to 256) {
      var counter = 0
      for (j <- fish.indices) {
        if (fish(j) == 0) {
          fish(j) = 6
          counter += 1
        } else {
          fish(j) -= 1
        }
      }
      fish = fish.appendedAll(Array.fill(counter)(8))
    }
    println(fish.length)
  }
}
