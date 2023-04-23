package module3.cats_effect_homework

import cats.effect.{IO, IOApp, Spawn}
import cats.implicits._

import scala.concurrent.duration.DurationLong

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())
object WalletFibersApp extends IOApp.Simple {

  def printBalance(w: Wallet[IO], nameWallet: String): IO[Unit] = for {
    balance <- w.balance
    _ <- IO.println(s"$nameWallet: $balance")
  } yield ()

  def processTopUpAndSleep(wallet: Wallet[IO], amount: BigDecimal, duration: Long): IO[Unit] =
    (wallet.topup(amount) *> IO.sleep(duration.millis))
      .flatMap(_ => processTopUpAndSleep(wallet, amount, duration))

  def printBalances(w1: Wallet[IO], w2: Wallet[IO], w3: Wallet[IO], duration: Long): IO[Unit] =
    for {
      _ <- printBalance(w1, "wallet1")
      _ <- printBalance(w2, "wallet2")
      _ <- printBalance(w3, "wallet3")
      _ <- IO.sleep(duration.millis)
      _ <- printBalances(w1, w2, w3, duration)
    } yield ()

  def run: IO[Unit] =
    for {
      _ <- IO.println("Press any key to stop...")
      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")
      // todo: запустить все файберы и ждать ввода от пользователя чтобы завершить работу
      _ <- Spawn[IO].start(processTopUpAndSleep(wallet1, 100, 100))
      _ <- Spawn[IO].start(processTopUpAndSleep(wallet2, 100, 500))
      _ <- Spawn[IO].start(processTopUpAndSleep(wallet3, 100, 2000))
      _ <- Spawn[IO].start(printBalances(wallet1, wallet2, wallet3, 1000))
      _ <- IO.readLine.iterateWhile(_.isEmpty)
    } yield ()

}