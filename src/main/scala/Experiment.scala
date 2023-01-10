object Experiment extends App {

  val urnList = Range(0, 100000).map(_ => new Urn())

  val eventList = urnList.map(_.hasTakeWhiteBall)

  println("Количество проведённых экспериментов: " + eventList.length)


  /* Ожидаемый процент удачных событий 4/5 */
  println("Процент удачных событий в экспериментах: " + eventList.count(_ == true).toDouble / eventList.length)

}
