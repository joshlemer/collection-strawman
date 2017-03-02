package strawman.collection.immutable

import scala.annotation.unchecked.uncheckedVariance
import scala.Nothing
import scala.Predef.???
import strawman.collection
import strawman.collection.{IterableFactories, IterableOnce, LinearSeq, SeqLike, View}
import strawman.collection.mutable.{Buildable, Builder, ListBuffer}
import scala.{Unit, Option, Int, None, Some}


/** Concrete collection type: List */
sealed trait List[+A]
  extends Seq[A]
     with SeqLike[A, List]
     with LinearSeq[A]
     with Buildable[A, List[A]] {

  def fromIterable[B](c: collection.Iterable[B]): List[B] = List.fromIterable(c)

  protected[this] def newBuilder = List.newBuilder

  /** Prepend element */
  def :: [B >: A](elem: B): ::[B] =  new ::(elem, this)

  /** Prepend operation that avoids copying this list */
  def ++:[B >: A](prefix: List[B]): List[B] =
    if (prefix.isEmpty) this
    else prefix.head :: prefix.tail ++: this

  /** When concatenating with another list `xs`, avoid copying `xs` */
  override def ++[B >: A](xs: IterableOnce[B]): List[B] = xs match {
    case xs: List[B] => this ++: xs
    case _ => super.++(xs)
  }

  override def className = "List"
}

case class :: [+A](x: A, private[collection] var next: List[A @uncheckedVariance]) // sound because `next` is used only locally
  extends List[A] {
  override def isEmpty = false
  override def nonEmpty = true
  override def head = x
  override def tail = next

  /** headOption not in strawman library, but worth pointing this out anyways */
  /* override */ def headOption: Some[A] = Some(x)

  override def ++:[B >: A](prefix: List[B]): ::[B] =
    if (prefix.isEmpty) this
    else prefix.head :: (prefix.tail ++: this)

  override def ++[B >: A](xs: IterableOnce[B]): ::[B] = xs match {
    case Nil => this
    case xs: List[B] => head :: (tail ++: xs)
    case xs => head :: (tail ++ xs)
  }

  override def map[B](f: A => B): ::[B] = f(head) :: tail.map(f)

  override def reverse: ::[A] = tail.foldLeft(head :: Nil)((as, a) => a :: as)

  override def className = "::"

  def zip[B](xs: ::[B]): ::[(A @uncheckedVariance, B)] = (head, xs.head) :: tail.zip(xs.tail)
}

case object Nil extends List[Nothing] {
  override def isEmpty = true
  override def nonEmpty = false
  override def head = ???
  override def tail = ???

  // Similarly if we wanted to, we could override many methods here, keeping type information
  // For example:
  // def ++(nil: Nil.type): Nil.type = this
  // def map[B](f: _ => B): Nil.type = Nil
  // def filter(f: _ => Boolean): Nil.type = Nil
  // etc..

}

object List extends IterableFactories[List] {

  def fromIterable[B](coll: collection.Iterable[B]): List[B] = coll match {
    case coll: List[B] => coll
    case _ => ListBuffer.fromIterable(coll).toList
  }

  def newBuilder[A]: Builder[A, List[A]] = new ListBuffer[A].mapResult(_.toList)


}

// Can't actually extend IterableFactories[::], since it's impossible to create a builder
object :: /* extends IterableFactories[::] */{
  def apply[A](x: A, xs: A*): ::[A] = new ::(x, List(xs: _*))
  def fill[A](n: Int)(elem: => A): ::[A] = if(n > 0) elem :: List.fill(n - 1)(elem) else ???

//  Not sure what to do with these methods...
  def newBuilder[A]: Builder[A, ::[A]] = ???
}


// Possible alternative...

// Doesn't actually compile because NonEmptyListOption doesn't extend Iterable,
// but maybe if Iterable were a typeclass this could work..

//package object immutable {
//  type NonEmptyListOption[+A] = Option[::[A]]
//}

//object :: extends IterableFactories[NonEmptyListOption] {
//  override def empty[A]: Option[::[A]] = None
//
//  override def apply[A](xs: A*): Option[::[A]] = xs match {
//    case xs if xs.nonEmpty => Option(new ::(xs.head, List(xs.tail: _*)))
//    case _ => None
//  }
//
//  override def fill[A](n: Int)(elem: => A): Option[::[A]] =
//    if(n > 0) Some(elem :: List.fill(n - 1)(elem)) else None
//
//  def newBuilder[A]: Builder[A, Option[::[A]]] = new Builder[A, Option[::[A]]] {
//    /** build NonEmptyList in reverse */
//    var tempReverse: Option[::[A]] = None
//
//    override def +=(x: A): Unit = tempReverse match {
//      case Some(nonEmpty) => Some(x :: nonEmpty)
//      case None => Some(x :: List.empty)
//    }
//
//    override def clear(): Unit = {
//      tempReverse = None
//    }
//
//    override def result: Option[::[A]] = tempReverse.map(_.reverse)
//  }
//
//  def fromIterable[B](coll: collection.Iterable[B]): Option[::[B]] = {
//    val iterator = coll.iterator()
//    if (iterator.hasNext) Option(iterator.next() :: iterator.foldRight(List.empty[B])((b, bs) => b :: bs))
//    else None
//  }
//}



