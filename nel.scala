package machinespir.it.jig

/** A simple non-empty list implementation */
sealed trait NonEmptyList[+A]:
  def head: A
  def tail: List[A]
  def toList: List[A] = head :: tail
  def ::[B >: A](b: B): NonEmptyList[B] = NonEmptyList.cons(b, this)
  def ++[B >: A](other: NonEmptyList[B]): NonEmptyList[B] =
    NonEmptyList.fromHeadTail(head, tail ++ other.toList)

object NonEmptyList:
  def one[A](a: A): NonEmptyList[A] = NEL(a, Nil)
  def fromHeadTail[A](h: A, t: List[A]): NonEmptyList[A] = NEL(h, t)
  def cons[A](h: A, t: NonEmptyList[A]): NonEmptyList[A] = NEL(h, t.toList)

  private case class NEL[+A](head: A, tail: List[A]) extends NonEmptyList[A]
