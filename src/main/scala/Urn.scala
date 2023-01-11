import scala.util.Random

class Urn {
  val ballList: List[Int] = Random.shuffle(List(1, 1, 1, 0, 0, 0))

  def hasTakeWhiteBall: Boolean = {
    val n1 = Random.nextInt(ballList.length)
    val firstBall = ballList(n1)

    val n2 = Random.nextInt(ballList.length - 1)
    val secondBall = ballList.patch(n1, Nil, 1)(n2)

    firstBall == 1 || secondBall == 1
  }
}
