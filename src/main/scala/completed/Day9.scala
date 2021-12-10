package io.github.mladensavic94
package completed

object Day9 {
  def main(args: Array[String]): Unit = {
    task1()
    task2()
  }

  def task1(): Unit = {
    val list = Util.readFile("src/main/resources/day9.txt")
    val matrix = list.map(_.split("").map(_.toInt)).toArray
    println(findBasin(matrix).map(xy => matrix(xy._1)(xy._2) + 1).sum)
  }

  def task2(): Unit = {
    val list = Util.readFile("src/main/resources/day9.txt")
    val matrix = list.map(_.split("").map(_.toInt)).toArray
    val basins = findBasin(matrix)
    var sizes = Array[Int]()
    for (basin <- basins) {
      var init = expandInAllDirs(matrix, basin._1, basin._2)
      var len = 0
      while (len < init.length) {
        val n = expandInAllDirs(matrix, init(len)._1, init(len)._2)
        init = init.appendedAll(n).distinct
        len += 1
      }
      sizes = sizes.appended(init.length)
    }
    println(sizes.sorted.takeRight(3).product)

  }

  def printMatrix(matrix: Array[Array[Int]]): Unit = {
    for (row <- matrix) {
      for (cell <- row) {
        print(cell)
        print(" ")
      }
      println()
    }
    println("***")
  }

  def findBasin(matrix: Array[Array[Int]]): Array[(Int, Int)] = {
    var risk = Array[(Int, Int)]()
    for (i <- matrix.indices) {
      val row = matrix(i)
      for (j <- row.indices) {
        val left = j - 1 >= 0
        val right = j + 1 < row.length
        val up = i - 1 >= 0
        val down = i + 1 < matrix.length
        if ((left && row(j - 1) > row(j)) || !left)
          if ((right && row(j + 1) > row(j)) || !right) {
            if ((up && matrix(i - 1)(j) > row(j)) || !up) {
              if ((down && matrix(i + 1)(j) > row(j)) || !down) {
                risk = risk.appended((i, j))
              }
            }
          }
      }
    }
    risk
  }

  def expandInAllDirs(matrix: Array[Array[Int]], i: Int, j: Int): Array[(Int, Int)] = {
    var newCords = Array[(Int, Int)]()
    if (j - 1 >= 0 && matrix(i)(j - 1) != 9) {
      newCords = newCords.appended((i, j - 1))
    }
    if (j + 1 < matrix(i).length && matrix(i)(j + 1) != 9) {
      newCords = newCords.appended((i, j + 1))
    }
    if (i - 1 >= 0 && matrix(i - 1)(j) != 9) {
      newCords = newCords.appended((i - 1, j))
    }
    if (i + 1 < matrix.length && matrix(i + 1)(j) != 9) {
      newCords = newCords.appended((i + 1, j))
    }
    newCords
  }
}
