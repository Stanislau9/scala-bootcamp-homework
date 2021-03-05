package implicits

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object ImplicitsHomework {

  object SuperVipCollections4s {
    type SizeScore = Int

    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object syntax {
      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
        def sizeScore: SizeScore = implicitly[GetSizeScore[T]].apply(inner)
      }
    }

    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {
      import syntax._
      private val map = mutable.LinkedHashMap.empty[K, V]
      private implicit def sizeScore(map: mutable.LinkedHashMap[K, V]): Int =
        map.map { case (k, v) => k.sizeScore + v.sizeScore }.sum

      def put(key: K, value: V): Unit = {
        map.put(key, value)
        while (sizeScore(map) > maxSizeScore) map.remove(map.head._1)
      }

      def get(key: K): Option[V] = map.get(key)

    }

    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])
    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()
      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] =
        PackedMultiMap(inner = ArraySeq(values: _*))
    }

    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }

    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]
      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object instances {
      import syntax._

      implicit val iterableOnceIterate: Iterate[Iterable] =
        new Iterate[Iterable] {
          override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
        }

      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }

      implicit val mapIterate: Iterate2[Map] = new Iterate2[Map] {
        override def iterator1[T, S](f: Map[T, S]): Iterator[T] = f.keysIterator
        override def iterator2[T, S](f: Map[T, S]): Iterator[S] = f.valuesIterator
      }

      implicit val packedMultiMapIterate: Iterate2[PackedMultiMap] =
        new Iterate2[PackedMultiMap] {
          override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] =
            f.inner.toMap.keysIterator
          override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] =
            f.inner.toMap.valuesIterator
        }

      implicit val byteScore: GetSizeScore[Byte]     = _ => 1
      implicit val charScore: GetSizeScore[Char]     = _ => 2
      implicit val intScore: GetSizeScore[Int]       = _ => 4
      implicit val longScore: GetSizeScore[Long]     = _ => 8
      implicit val stringScore: GetSizeScore[String] = 12 + _.map(_.sizeScore).sum

      implicit def iteratorSizeScore[T: GetSizeScore]: GetSizeScore[Iterator[T]] =
        _.map(_.sizeScore).sum

      implicit def classScore[T: GetSizeScore]: GetSizeScore[T] = _.sizeScore

      implicit def iterateScore[T: GetSizeScore, F[_]: Iterate]: GetSizeScore[F[T]] =
        12 + implicitly[Iterate[F]].iterator(_).sizeScore

      implicit def iterate2Score[K: GetSizeScore, V: GetSizeScore, F[_, _]: Iterate2]
          : GetSizeScore[F[K, V]] =
        (map: F[K, V]) =>
          12 + implicitly[Iterate2[F]]
            .iterator1(map)
            .sizeScore + implicitly[Iterate2[F]]
            .iterator2(map)
            .sizeScore
    }
  }

  object MyTwitter {
    import SuperVipCollections4s._
    import SuperVipCollections4s.instances._
    import SuperVipCollections4s.syntax._

    final case class Twit(
        id: Long,
        userId: Int,
        hashTags: Vector[String],
        attributes: PackedMultiMap[String, String],
        fbiNotes: List[FbiNote]
    )

    final case class FbiNote(
        month: String,
        favouriteChar: Char,
        watchedPewDiePieTimes: Long
    )

    trait TwitCache {
      def put(twit: Twit): Unit
      def get(id: Long): Option[Twit]
    }

    implicit val fbiNoteGetSizeScore: GetSizeScore[FbiNote] =
      note =>
        12 +
          note.month.sizeScore +
          note.favouriteChar.sizeScore +
          note.watchedPewDiePieTimes.sizeScore
    implicit val twitGetSizeScore: GetSizeScore[Twit] =
      twit =>
        12 +
          twit.id.sizeScore +
          twit.userId.sizeScore +
          twit.hashTags.size +
          twit.attributes.sizeScore +
          twit.fbiNotes.sizeScore

    def createTwitCache(maxSizeScore: SizeScore): TwitCache =
      new TwitCache {
        val mutableBoundedCache: MutableBoundedCache[Long, Twit] =
          new MutableBoundedCache[Long, Twit](maxSizeScore)
        override def put(twit: Twit): Unit       = mutableBoundedCache.put(twit.id, twit)
        override def get(id: Long): Option[Twit] = mutableBoundedCache.get(id)
      }

  }
}
