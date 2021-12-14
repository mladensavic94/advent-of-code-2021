package io.github.mladensavic94
package completed

object Day13 {
  def main(args: Array[String]): Unit = {
    task1()
    task2()
  }

  def task1(): Unit = {
    val list = Util.readFile("src/main/resources/day13.txt")
    val dots = list.filter(!_.startsWith("fold")).map(_.split(",")).map(arr => (arr.head.toInt, arr.last.toInt))
    val maxX = dots.map(_._1).max
    val maxY = dots.map(_._2).max
    val matrix = Array.fill(maxY + 1)(Array.fill(maxX + 1)("."))
    for (dot <- dots) {
      matrix(dot._2)(dot._1) = "#"
    }
    foldVertical(matrix, 655, maxX)

    val newMatrix = matrix.map(_.take(655))
    println(newMatrix.flatten.count(_.equals("#")))
  }


  def task2(): Unit = {
    val list = Util.readFile("src/main/resources/day13.txt")
    val dots = list.filter(!_.startsWith("fold")).map(_.split(",")).map(arr => (arr.head.toInt, arr.last.toInt))
    var maxX = dots.map(_._1).max
    var maxY = dots.map(_._2).max
    val matrix = Array.fill(maxY + 1)(Array.fill(maxX + 1)("."))
    for (dot <- dots) {
      matrix(dot._2)(dot._1) = "#"
    }
    val folds = list.filter(_.startsWith("fold"))
      .map(str => str.replace("fold along ", ""))
      .map(_.split("="))
      .map(arr => (arr.head, arr.last.toInt))

    for (fold <- folds) {
      if (fold._1 == "y") {
        foldHorizontal(matrix, fold._2, maxY)
        maxY = fold._2
      } else {
        foldVertical(matrix, fold._2, maxX)
        maxX = fold._2
      }
    }

    val end = matrix.take(maxY + 1).map(_.take(maxX + 1))
    printMatrix(end)
  }

  def printMatrix(matrix: Array[Array[String]]): Unit = {
    for (row <- matrix) {
      for (cell <- row) {
        print(cell)
        print(" ")
      }
      println()
    }
    println("***")
  }

  def foldVertical(matrix: Array[Array[String]], x: Int, maxX: Int): Unit = {
    for (i <- matrix.indices) {
      var foldX = x - 1
      for (j <- x + 1 to maxX) {
        if (matrix(i)(j) == "#")
          matrix(i)(foldX) = matrix(i)(j)
        foldX -= 1
      }

    }
  }

  def foldHorizontal(matrix: Array[Array[String]], y: Int, maxY: Int): Unit = {
    var foldY = y - 1
    for (i <- y + 1 to maxY) {
      for (j <- matrix(i).indices) {
        if (matrix(i)(j) == "#")
          matrix(foldY)(j) = matrix(i)(j)
      }
      foldY -= 1
    }
  }
}
