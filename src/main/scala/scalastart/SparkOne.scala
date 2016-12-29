package scalastart

import org.apache.spark._

/**
  * Created by prashant.s on 27/12/16.
  */
object SparkOne {
  val conf = new SparkConf().setAppName("myapp").setMaster("local")
  val sc = new SparkContext(conf)


  def main(args:Array[String]) = {
    println("Inside")
    val textFile = sc.textFile("/Users/prashant.s/add.txt")
    val counts = textFile.flatMap(line => line.split(" "))
      .map(word => (word, 1))
      .reduceByKey(_ + _)
    counts.foreach(println)

    counts.saveAsTextFile("Users/prashant.s/add1")

  }
}