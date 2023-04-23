package module3.cats_effect_homework

import cats.effect.Sync
import cats.implicits._
import Wallet._

// DSL управления электронным кошельком
trait Wallet[F[_]] {
  // возвращает текущий баланс
  def balance: F[BigDecimal]
  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]
  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]
}

// Игрушечный кошелек который сохраняет свой баланс в файл
// todo: реализовать используя java.nio.file._
// Насчёт безопасного конкуррентного доступа и производительности не заморачиваемся, делаем максимально простую рабочую имплементацию. (Подсказка - можно читать и сохранять файл на каждую операцию).
// Важно аккуратно и правильно завернуть в IO все возможные побочные эффекты.
//
// функции которые пригодятся:
// - java.nio.file.Files.write
// - java.nio.file.Files.readString
// - java.nio.file.Files.exists
// - java.nio.file.Paths.get
final class FileWallet[F[_]: Sync](id: WalletId) extends Wallet[F] {
  def balance: F[BigDecimal] = for {
    path <- Sync[F].delay(java.nio.file.Paths.get(id))
    lines <- Sync[F].delay(java.nio.file.Files.readAllLines(path))
    strBalance <- Sync[F].pure(lines.get(0))
    balance <- Sync[F].delay(BigDecimal(strBalance))
  } yield balance

  def topup(amount: BigDecimal): F[Unit] = for {
    balanceCurrent <- balance
    balanceNew <- Sync[F].pure(balanceCurrent + amount)
    path <- Sync[F].delay(java.nio.file.Paths.get(id))
    _ <- Sync[F].delay(java.nio.file.Files.write(path, balanceNew.toString().getBytes()))
  } yield ()

  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] = for {
    balanceCurrent <- balance
    balanceNew <- Sync[F].pure(Either.cond(balanceCurrent >= amount, balanceCurrent - amount, BalanceTooLow))
    path <- Sync[F].delay(java.nio.file.Paths.get(id))
    result <- Sync[F].delay( balanceNew match {
      case Left(error) => Left(error)
      case Right(_) =>
        java.nio.file.Files.write(path, balanceNew.toOption.get.toString().getBytes())
        Right(())
    }
    )

  } yield result
}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов
  def fileWallet[F[_]: Sync](id: WalletId): F[Wallet[F]] = for {
    _ <- Sync[F].delay(
      java.nio.file.Files.write(
        java.nio.file.Paths.get(id),
        "0.0".getBytes()
      )
    )
  } yield new FileWallet[F](id)

  type WalletId = String

  sealed trait WalletError
  case object BalanceTooLow extends WalletError
}
