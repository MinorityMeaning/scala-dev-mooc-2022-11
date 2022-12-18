import module1.{functions, opt, type_system}

import module1.list.List

object Main{


  def main(args: Array[String]): Unit = {

    import module1.opt.Option

    val x = Option(4)

    println(x.filter(_ != 4))

    val inputList = List(1, 2, 4)

    val newList = inputList.::(0)

    println(newList.mkString())
    println(newList.map(_ + 1).mkString())
    println(newList.filter(_ > 1).mkString())


  }

}