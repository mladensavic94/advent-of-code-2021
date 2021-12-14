package io.github.mladensavic94
package completed

import scala.collection.mutable

object Day12 {
  def main(args: Array[String]): Unit = {
    task1()
    task2()
  }

  def task1(): Unit = {
    val list = Util.readFile("src/main/resources/day12.txt")
    val graph = createGraph(list)
    println(graph)
    val paths = mutable.Stack[Array[String]]()
    paths.push(Array("start"))
    var counter = 0
    while (paths.nonEmpty) {
      val path = paths.pop()
      val branches = graph(path.last)
      for (branch <- branches) {
        if (branch.equals("end")) {
          //          println(path.appended(branch).mkString("Array(", ", ", ")"))
          counter += 1
        } else {
          if (path.filter(str => str.matches("[a-z]*") || str.equals("start")).count(_ == branch) < 1) {
            paths.push(path.appended(branch))
          }
        }
      }
    }
    println(counter)
  }

  def task2(): Unit = {
    val list = Util.readFile("src/main/resources/day12.txt")
    val graph = createGraph(list)
    println(graph)
    val paths = mutable.Stack[Array[String]]()
    paths.push(Array("start"))
    var counter = 0
    while (paths.nonEmpty) {
      val path = paths.pop()
      val branches = graph(path.last)
      for (branch <- branches) {
        if (branch.equals("end")) {
          //          println(path.appended(branch).mkString("Array(", ", ", ")"))
          counter += 1
        } else {
          if (isValid(path, branch)) {
            paths.push(path.appended(branch))
          }
        }
      }
    }
    println(counter)
  }

  def createGraph(list: List[String]): scala.collection.mutable.Map[String, List[String]] = {
    val map = scala.collection.mutable.Map[String, List[String]]().withDefaultValue(List())
    for (line <- list) {
      val arr = line.split("-")
      map.put(arr.head, map(arr.head).appended(arr.last))
      map.put(arr.last, map(arr.last).appended(arr.head))
    }
    map
  }

  def isValid(strings: Array[String], str: String): Boolean = {
    if (str.equals("start"))
      return false
    val map = strings.filter(str => str.matches("[a-z]*")).groupBy(identity).view.mapValues(_.length).toMap.withDefaultValue(0)
    if (map.exists((k, v) => v == 2)) {
      val instances = map(str)
      instances < 1
    } else {
      true
    }
  }

}
