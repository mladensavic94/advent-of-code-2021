package io.github.mladensavic94
package completed

object Day6 {
  def main(args: Array[String]): Unit = {
    task1()
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
    val list = Util.readFile("src/main/resources/day6.txt").head
    val fish = list.split(",").groupBy(identity).map(m => (m._1.toInt, m._2.length.toLong)).to(scala.collection.mutable.Map)
    fish.put(6, 0L)
    fish.put(7, 0L)
    fish.put(8, 0L)
    fish.put(0, 0L)

    for (i <- 1 to 256) {
      val temp = fish(0)
      fish.put(0, fish(1))
      fish.put(1, fish(2))
      fish.put(2, fish(3))
      fish.put(3, fish(4))
      fish.put(4, fish(5))
      fish.put(5, fish(6))
      fish.put(6, fish(7) + temp)
      fish.put(7, fish(8))
      fish.put(8, temp)
    }
    println(fish.values.sum)
  }
}
