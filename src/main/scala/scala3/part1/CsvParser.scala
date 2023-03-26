package part1.CsvParser



//1. исполользовать given, как написано в комментариях и в почеченных местах ниже
//2. использовать новый "тихий синтаксис", где сочтете приемлемым, тут на ваше усмотрение
//https://docs.scala-lang.org/scala3/new-in-scala3.html  глава New & Shiny: The Syntax
//главное это разобраться с given


class MonadParser[T, Src](private val p: Src => (T, Src)):
  def flatMap[M](f: T => MonadParser[M, Src]): MonadParser[M, Src] =
    MonadParser { src =>
      val (word, rest) = p(src)
      val mn = f(word)
      val res = mn.p(rest)

      //с помощью функции — аргумента метода добавляем его в контекст, видимый всем последующим парсерам по цепочке.
      res
    }
  def map[M](f: T => M): MonadParser[M, Src] =
    MonadParser { src =>
      val (word, rest) = p(src)
      (f(word), rest)
    }
  def parse(src: Src): T = p(src)._1

end MonadParser


object MonadParser:
  def apply[T, Src](f: Src => (T, Src)) =
    new MonadParser[T, Src](f)


trait FieldConversion[A,B]:
  def convert(x: A): B


given intFieldConversion: FieldConversion[String, Int] with
  def convert(x: String): Int = x.toInt

given floatFieldConversion: FieldConversion[String, Float] with
  def convert(x: String): Float = x.toFloat
given doubleFieldConversion: FieldConversion[String, Double] with
  def convert(x: String): Double = x.toDouble
// сделать given instance для типов Int Float Double
// в функции просто сконвертнуть строку в нужный тип


object TestExecution:

  //здесь написать функцию, которая будет применять given определенные выше
  // использовать using fieldConversion c первым параметром String, а второй будет вариативны параметр B

  def parse[String,B](x:String)(using fieldConversion: FieldConversion[String, B]) : B =
    fieldConversion.convert(x)


  @main def runParser(): Unit =

    def StringField =
      MonadParser[String, String] { str =>
        val idx = str.indexOf(";")
        if (idx > -1)
          (str.substring(0, idx), str.substring(idx + 1))
        else
          (str, "")
      }



    def IntField: MonadParser[Int, String] =  StringField.map(parse) //StringField.map(...здесь применить parse который подхватит нужный given автоматически ...)
    def FloatField: MonadParser[Float, String] = StringField.map(parse)

    case class Car(year: Int, mark: String, model: String, comment: String, price: Float)

    val str = "1997;Ford;E350;ac, abs, moon;3000\n1996; Jeep; Grand Cherokee; MUST SELL! air, moon roof, loaded; 4799"

    val parser =
      for
        year <- IntField
        mark <- StringField
        model <- StringField
        comment <- StringField
        price <- FloatField
      yield
        Car(year, mark, model, comment, price)


    val result = str.split('\n').map(parser.parse)

    println(result.map(x=>s"${x.model},${x.mark},${x.year}").mkString(";"))

end TestExecution