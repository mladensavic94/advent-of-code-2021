package io.github.mladensavic94
package completed

object Day11 {
  def main(args: Array[String]): Unit = {
    task1()
    task2()
  }

  def task1(): Unit = {
    val list = Util.readFile("src/main/resources/day11.txt")
    val matrix = list.map(_.split("").map(_.toInt)).toArray
    printMatrix(matrix)
    var flashes = 0
    for (k <- 1 to 100) {
      increaseEnergy(matrix)
      while (matrix.flatten.exists(_ > 9)) {
        for (i <- matrix.indices) {
          for (j <- matrix.indices) {
            if (matrix(i)(j) > 9) {
              flash(matrix, (i, j))
              flashes += 1
            }
          }
        }
      }
    }
    printMatrix(matrix)
    println(flashes)
  }

  def task2(): Unit = {
    val list = Util.readFile("src/main/resources/day11.txt")
    val matrix = list.map(_.split("").map(_.toInt)).toArray
    var flashes = 0
    var iterations = 0
    while (matrix.flatten.exists(_ != 0)) {
      iterations += 1
      increaseEnergy(matrix)
      while (matrix.flatten.exists(_ > 9)) {
        for (i <- matrix.indices) {
          for (j <- matrix.indices) {
            if (matrix(i)(j) > 9) {
              flash(matrix, (i, j))
              flashes += 1
            }
          }
        }
      }
    }
    printMatrix(matrix)
    println(flashes)
    println(iterations)
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

  def increaseEnergy(matrix: Array[Array[Int]]): Unit = {
    for (i <- matrix.indices) {
      for (j <- matrix(i).indices) {
        matrix(i)(j) += 1
      }
    }
  }

  def flash(matrix: Array[Array[Int]], pos: (Int, Int)): Unit = {
    matrix(pos._1)(pos._2) = 0
    if (pos._1 - 1 >= 0) {
      if (matrix(pos._1 - 1)(pos._2) != 0)
        matrix(pos._1 - 1)(pos._2) += 1
      if (pos._2 - 1 >= 0 && matrix(pos._1 - 1)(pos._2 - 1) != 0)
        matrix(pos._1 - 1)(pos._2 - 1) += 1
      if (pos._2 + 1 < matrix.length && matrix(pos._1 - 1)(pos._2 + 1) != 0)
        matrix(pos._1 - 1)(pos._2 + 1) += 1
    }
    if (pos._1 + 1 < matrix.length) {
      if (matrix(pos._1 + 1)(pos._2) != 0)
        matrix(pos._1 + 1)(pos._2) += 1
      if (pos._2 - 1 >= 0 && matrix(pos._1 + 1)(pos._2 - 1) != 0)
        matrix(pos._1 + 1)(pos._2 - 1) += 1
      if (pos._2 + 1 < matrix.length && matrix(pos._1 + 1)(pos._2 + 1) != 0)
        matrix(pos._1 + 1)(pos._2 + 1) += 1
    }
    if (pos._2 - 1 >= 0 && matrix(pos._1)(pos._2 - 1) != 0) {
      matrix(pos._1)(pos._2 - 1) += 1
    }
    if (pos._2 + 1 < matrix.length && matrix(pos._1)(pos._2 + 1) != 0) {
      matrix(pos._1)(pos._2 + 1) += 1
    }
  }

}
