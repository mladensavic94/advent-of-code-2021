package io.github.mladensavic94
package completed

object Day5 {

  def main(args: Array[String]): Unit = {
    //    task1()
    task2()
  }

  def task1(): Unit = {
    val list = Util.readFile("src/main/resources/day5.txt")
    val lines = list.map(str => str.replace(" -> ", ","))
      .map(str => str.split(","))
      .map(arr => Line((arr(0).toInt, arr(1).toInt), (arr(2).toInt, arr(3).toInt)))
    var matrix = Array.ofDim[Int](1000, 1000)
    //    printMatrix(matrix)
    lines.foreach(markLine(_, matrix))
    //    printMatrix(matrix)
    println(matrix.flatMap(_.toList).count(_.>(1)))

  }

  def task2(): Unit = {
    val list = Util.readFile("src/main/resources/day5.txt")
    val lines = list.map(str => str.replace(" -> ", ","))
      .map(str => str.split(","))
      .map(arr => Line((arr(0).toInt, arr(1).toInt), (arr(2).toInt, arr(3).toInt)))
    var matrix = Array.ofDim[Int](1000, 1000)
//    printMatrix(matrix)
    lines.foreach(markLine2(_, matrix))
//    printMatrix(matrix)
    println(matrix.flatMap(_.toList).count(_.>(1)))
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

  def markLine(line: Line, matrix: Array[Array[Int]]): Unit = {
    if (line.point1._2 == line.point2._2) {
      if (line.point1._1 > line.point2._1) {
        for (x <- line.point2._1 to line.point1._1)
          matrix(line.point2._2)(x) += 1
      } else if (line.point1._1 < line.point2._1) {
        for (x <- line.point1._1 to line.point2._1)
          matrix(line.point2._2)(x) += 1
      } else {
        matrix(line.point1._1)(line.point1._2) += 1
      }
    }
    if (line.point1._1 == line.point2._1) {
      if (line.point1._2 > line.point2._2) {
        for (y <- line.point2._2 to line.point1._2)
          matrix(y)(line.point1._1) += 1
      } else if (line.point1._2 < line.point2._2) {
        for (y <- line.point1._2 to line.point2._2)
          matrix(y)(line.point1._1) += 1
      } else {
        matrix(line.point1._1)(line.point1._2) += 1
      }
    }
  }

  def markLine2(line: Line, matrix: Array[Array[Int]]): Unit = {
    val xMin = Math.min(line.point1._1, line.point2._1)
    val xMax = Math.max(line.point1._1, line.point2._1)
    val yMin = Math.min(line.point1._2, line.point2._2)
    val yMax = Math.max(line.point1._2, line.point2._2)
    if (line.point1._1 == line.point2._1) {
      for (y <- yMin to yMax)
        matrix(y)(line.point1._1) += 1
    }
    else if (line.point1._2 == line.point2._2) {
      for (x <- xMin to xMax)
        matrix(line.point1._2)(x) += 1
    } else {
      if (isDiagonal(line)) {
        var xRange = Range.inclusive(line.point1._1, line.point2._1)
        var yRange = Range.inclusive(line.point1._2, line.point2._2)
        if (line.point1._1 > line.point2._1) {
          xRange = Range.inclusive(line.point1._1, line.point2._1, -1)
        }
        if (line.point1._2 > line.point2._2) {
          yRange = Range.inclusive(line.point1._2, line.point2._2, -1)
        }
        for ((x, y) <- xRange.zip(yRange))
          matrix(y)(x) += 1
      }
    }
  }

  def isDiagonal(line: Line): Boolean = {
    val xLen = Math.abs(line.point1._1 - line.point2._1)
    val yLen = Math.abs(line.point1._2 - line.point2._2)
    val z = Math.sqrt(Math.pow(xLen, 2) + Math.pow(yLen, 2))
    Math.cos(xLen / z) == Math.cos(yLen / z)
  }

}

case class Line(point1: (Int, Int), point2: (Int, Int))
