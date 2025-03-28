package machinespir.it.jig

/** Represents the result of reading a config value */
sealed trait ReadResult[+A]:
  def isSuccess: Boolean = this match
    case ReadSucceeded(_) => true
    case ReadFailed(_)    => false

  def map[B](f: A => B): ReadResult[B] = this match
    case ReadSucceeded(a)  => ReadSucceeded(f(a))
    case f @ ReadFailed(_) => f

  def flatMap[B](f: A => ReadResult[B]): ReadResult[B] = this match
    case ReadSucceeded(a)  => f(a)
    case f @ ReadFailed(_) => f

  def fold[B](f: NonEmptyList[ConfigError] => B, g: A => B): B = this match
    case ReadSucceeded(a) => g(a)
    case ReadFailed(es)   => f(es)

  def toEither: Either[NonEmptyList[ConfigError], A] = this match
    case ReadSucceeded(a) => Right(a)
    case ReadFailed(es)   => Left(es)

case class ReadSucceeded[+A](value: A) extends ReadResult[A]
case class ReadFailed(errors: NonEmptyList[ConfigError]) extends ReadResult[Nothing]

object ReadResult:
  def success[A](a: A): ReadResult[A] = ReadSucceeded(a)
  def failure(error: ConfigError): ReadResult[Nothing] = ReadFailed(NonEmptyList.one(error))
  def failures(head: ConfigError, tail: ConfigError*): ReadResult[Nothing] =
    ReadFailed(NonEmptyList.fromHeadTail(head, tail.toList))

  def map2[A, B, C](fa: ReadResult[A], fb: ReadResult[B])(f: (A, B) => C): ReadResult[C] =
    (fa, fb) match
      case (ReadSucceeded(a), ReadSucceeded(b)) => ReadSucceeded(f(a, b))
      case (ReadFailed(e1), ReadFailed(e2))     => ReadFailed(e1 ++ e2)
      case (ReadFailed(e), _)                   => ReadFailed(e)
      case (_, ReadFailed(e))                   => ReadFailed(e)

  def sequence[A](fas: List[ReadResult[A]]): ReadResult[List[A]] =
    fas.foldRight[ReadResult[List[A]]](ReadSucceeded(Nil)) { (fa, acc) =>
      map2(fa, acc)(_ :: _)
    }

extension [A](nel: NonEmptyList[A]) def mkString(sep: String = ", "): String = nel.toList.mkString(sep)
