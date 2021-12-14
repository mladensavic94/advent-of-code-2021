package io.github.mladensavic94

object Day14 {
  def main(args: Array[String]): Unit = {
    task1()
    task2()
  }

  def task1(): Unit = {
    val list = Util.readFile("src/main/resources/day14.txt")
    var template = list.head
    val codes = list.filter(_.contains("->")).map(_.split(" -> ")).map(arr => (arr.head, arr.last)).toMap.withDefault(k => "")
    for (i <- 0 until 10) {
      val pairs = template.split("").sliding(2).map(_.mkString).toList
      template = pairs.map(str => {
        val arr = str.split("")
        arr.mkString(codes(str))
      }).reduce((s1, s2) => s1.concat(s2.substring(1)))
    }
    val counter = template.split("").groupBy(identity).view.mapValues(_.length).values
    println(counter.max - counter.min)
  }

  def task2(): Unit = {
    val list = Util.readFile("src/main/resources/dummy.txt")
    var template = list.head.sliding(2).toList.groupBy(identity).view.mapValues(_.size).toMap
    val codes = list.filter(_.contains("->")).map(_.split(" -> ")).map(arr => (arr.head, arr.last)).toMap.withDefault(k => "")
    for (i <- 0 until 40) {

    }
    println(template)
//    val counter = template.split("").groupBy(identity).view.mapValues(_.length).values
//    println(counter.max - counter.min)
  }
}
