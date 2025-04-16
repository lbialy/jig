package machinespir.it.jig

import org.ekrich.config.*
import org.ekrich.config.impl.Origin

import scala.util.control.NoStackTrace
import scala.deriving.Mirror
import scala.quoted.*
import scala.jdk.CollectionConverters.*
import scala.annotation.implicitNotFound

/** Typeclass for writing an `A` value to a `ConfigValue`. */
@implicitNotFound(
  "No ConfigWriter found for type ${A}. Try to derive it using `derives ConfigWriter` clause or `ConfigWriter.derived[A]`. You might also want to look at `ConfigCodec` which offers both reading and writing."
)
trait ConfigWriter[A]:
  def write(a: A, includeComments: Boolean = false): ConfigValue

  /** Contramaps on the input type. */
  def contramap[B](f: B => A): ConfigWriter[B] = new ConfigWriter[B]:
    def write(b: B, includeComments: Boolean = false): ConfigValue =
      ConfigWriter.this.write(f(b), includeComments)

/** Derivation helpers for writing. */
object ConfigWriter:

  class ConfigSumWriter[A](sum: Mirror.SumOf[A], labelsWithInstances: => Vector[(String, ConfigWriter[?])])
      extends ConfigWriter[A]:
    lazy val labeledWriters = labelsWithInstances
    def write(a: A, includeComments: Boolean = false): ConfigValue =
      // Identify which subtype (ordinal) we have
      val idx = sum.ordinal(a)
      val (label, subtypeWriter) = labeledWriters(idx)
      val writer = subtypeWriter.asInstanceOf[ConfigWriter[A]]
      val value = writer.write(a, includeComments)

      // If the value is an empty object, just return the type string
      if value.valueType == ConfigValueType.OBJECT && value.asInstanceOf[ConfigObject].isEmpty then
        ConfigValueFactory.fromAnyRef(label)
      else
        ConfigValueFactory.fromMap(
          Map("type" -> label, "value" -> value).asJava
        )
  end ConfigSumWriter

  class ConfigProductWriter[A](
      product: Mirror.ProductOf[A],
      instances: => Vector[(String, ConfigWriter[?])],
      commentAnnotationsByField: Map[String, Vector[comment]] = Map.empty
  ) extends ConfigWriter[A]:
    lazy val labelsWithInstances = instances
    def write(a: A, includeComments: Boolean = false): ConfigValue =
      val product = a.asInstanceOf[Product]

      // For each field label, pick the correct writer, produce (label -> ConfigValue)
      val kvPairs =
        labelsWithInstances
          .zip(product.productIterator)
          .map { case ((label, w: ConfigWriter[?]), fieldValue) =>
            val fieldCfg = w.asInstanceOf[ConfigWriter[Any]].write(fieldValue, includeComments)
            val withComment =
              if includeComments && commentAnnotationsByField.contains(label) then
                val comments = commentAnnotationsByField(label).map(_.text)
                fieldCfg.withOrigin(Origin.withComments(comments))
              else fieldCfg
            label -> withComment
          }
          .filterNot { case (_, cfg) => cfg.unwrapped == null }
          .toMap

      ConfigValueFactory.fromMap(kvPairs.asJava)
  end ConfigProductWriter

  /** A few base instances. Add as many as you need. */
  given ConfigWriter[String] with
    def write(a: String, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a)

  given ConfigWriter[Int] with
    def write(a: Int, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.asInstanceOf[AnyRef])

  given ConfigWriter[Boolean] with
    def write(a: Boolean, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.asInstanceOf[AnyRef])

  given ConfigWriter[Byte] with
    def write(a: Byte, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.asInstanceOf[AnyRef])

  given ConfigWriter[Short] with
    def write(a: Short, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.asInstanceOf[AnyRef])

  given ConfigWriter[Long] with
    def write(a: Long, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.asInstanceOf[AnyRef])

  given ConfigWriter[Float] with
    def write(a: Float, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.asInstanceOf[AnyRef])

  given ConfigWriter[Double] with
    def write(a: Double, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.asInstanceOf[AnyRef])

  given ConfigWriter[Char] with
    def write(a: Char, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.toString)

  given ConfigWriter[BigInt] with
    def write(a: BigInt, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.toString)

  given ConfigWriter[BigDecimal] with
    def write(a: BigDecimal, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.toString)

  given ConfigWriter[scala.concurrent.duration.Duration] with
    def write(a: scala.concurrent.duration.Duration, includeComments: Boolean = false): ConfigValue =
      val nanos = a.toNanos
      val (value, unit) = nanos match
        case n if n >= 86400000000000L => (n.toDouble / 86400000000000L, "d")
        case n if n >= 3600000000000L  => (n.toDouble / 3600000000000L, "h")
        case n if n >= 60000000000L    => (n.toDouble / 60000000000L, "m")
        case n if n >= 1000000000L     => (n.toDouble / 1000000000L, "s")
        case n if n >= 1000000L        => (n.toDouble / 1000000L, "ms")
        case n if n >= 1000L           => (n.toDouble / 1000L, "us")
        case n                         => (n.toDouble, "ns")
      val formattedValue = if value % 1 == 0 then value.toLong.toString else value.toString
      ConfigValueFactory.fromAnyRef(s"$formattedValue$unit")

  given ConfigWriter[scala.concurrent.duration.FiniteDuration] with
    def write(a: scala.concurrent.duration.FiniteDuration, includeComments: Boolean = false): ConfigValue =
      ConfigWriter[scala.concurrent.duration.Duration].write(a, includeComments)

  given javaLongWriter: ConfigWriter[java.lang.Long] with
    def write(a: java.lang.Long, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a)

  given javaDoubleWriter: ConfigWriter[java.lang.Double] with
    def write(a: java.lang.Double, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a)

  given javaFloatWriter: ConfigWriter[java.lang.Float] with
    def write(a: java.lang.Float, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a)

  given javaShortWriter: ConfigWriter[java.lang.Short] with
    def write(a: java.lang.Short, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a)

  given javaByteWriter: ConfigWriter[java.lang.Byte] with
    def write(a: java.lang.Byte, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a)

  given javaCharacterWriter: ConfigWriter[java.lang.Character] with
    def write(a: java.lang.Character, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.toString)

  given [A](using w: ConfigWriter[A]): ConfigWriter[Option[A]] = new ConfigWriter[Option[A]]:
    def write(a: Option[A], includeComments: Boolean = false): ConfigValue =
      a match
        case Some(value) => w.write(value, includeComments)
        case None        => ConfigValueFactory.fromAnyRef(null)

  // Ty, s
  given [A, B](using wa: ConfigWriter[A], wb: ConfigWriter[B]): ConfigWriter[Either[A, B]] =
    new ConfigWriter[Either[A, B]]:
      def write(a: Either[A, B], includeComments: Boolean = false): ConfigValue =
        a match
          case Left(value) =>
            ConfigValueFactory.fromMap(Map("type" -> "left", "value" -> wa.write(value, includeComments)).asJava)
          case Right(value) =>
            ConfigValueFactory.fromMap(Map("type" -> "right", "value" -> wb.write(value, includeComments)).asJava)

  given [A](using w: ConfigWriter[A]): ConfigWriter[List[A]] = new ConfigWriter[List[A]]:
    def write(as: List[A], includeComments: Boolean = false): ConfigValue =
      val values = as.map(a => w.write(a, includeComments))
      ConfigValueFactory.fromIterable(values.asJava)

  /** Summon or derive a ConfigWriter[A]. */
  inline def apply[A](using cw: ConfigWriter[A]): ConfigWriter[A] = cw

  inline def derived[A]: ConfigWriter[A] = ${ derivedMacro[A] }

  inline given configWriterFromCodec[A](using cc: ConfigCodec[A]): ConfigWriter[A] = cc.writer

  /** Macro entry point for deriving a writer from a `Mirror`. */
  def derivedMacro[A: Type](using Quotes): Expr[ConfigWriter[A]] =
    import quotes.reflect.*

    def prepareWriterInstances(
        elemLabels: Type[?],
        elemTypes: Type[?],
        tryDerive: Boolean = false
    ): List[Expr[(String, ConfigWriter[?])]] =
      (elemLabels, elemTypes) match
        case ('[EmptyTuple], '[EmptyTuple]) => Nil
        case ('[label *: labelsTail], '[tpe *: tpesTail]) =>
          val label = Type.valueOfConstant[label].get.asInstanceOf[String]
          val fieldName = Expr(label)
          val fieldWriter = Expr.summon[ConfigWriter[tpe]].getOrElse {
            if tryDerive then '{ ConfigWriter.derived[tpe] }
            else report.errorAndAbort(s"Missing ConfigWriter for type ${Type.show[tpe]}")
          }
          val namedInstance = '{ ($fieldName, $fieldWriter) }
          namedInstance :: prepareWriterInstances(Type.of[labelsTail], Type.of[tpesTail], tryDerive)

    def annotationTree(tree: Tree): Option[Expr[comment]] =
      Option.when(tree.isExpr)(tree.asExpr).filter(_.isExprOf[comment]).map(_.asExprOf[comment])

    def fieldAnnotations(s: Symbol): Expr[(String, Vector[comment])] =
      val annots = Varargs(s.annotations.reverse.flatMap(annotationTree))
      val name = Expr(s.name)

      '{ $name -> Vector($annots: _*) }
    end fieldAnnotations

    def extractComments(sym: Symbol): Expr[Map[String, Vector[comment]]] =
      val caseParams = sym.primaryConstructor.paramSymss.take(1).flatten
      val fieldAnns = Varargs(caseParams.map(fieldAnnotations))

      '{ Map($fieldAnns: _*) }

    Expr.summon[Mirror.Of[A]].get match
      case '{
            $m: Mirror.ProductOf[A] { type MirroredElemLabels = labels; type MirroredElemTypes = types }
          } =>
        val instancesExpr = Expr.ofList(prepareWriterInstances(Type.of[labels], Type.of[types]))
        val comments = extractComments(TypeRepr.of[A].typeSymbol)
        '{ ConfigProductWriter($m, $instancesExpr.toVector, $comments) }

      case '{
            $m: Mirror.SumOf[A] { type MirroredElemLabels = labels; type MirroredElemTypes = types }
          } =>
        val writers = prepareWriterInstances(Type.of[labels], Type.of[types], tryDerive = true)
        val writersExpr = Expr.ofList(writers)
        '{ ConfigSumWriter($m, $writersExpr.toVector) }

  given ConfigWriter[java.time.Instant] with
    def write(a: java.time.Instant, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.toString)

  given ConfigWriter[java.time.LocalDate] with
    def write(a: java.time.LocalDate, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.toString)

  given ConfigWriter[java.time.LocalTime] with
    def write(a: java.time.LocalTime, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.toString)

  given ConfigWriter[java.time.LocalDateTime] with
    def write(a: java.time.LocalDateTime, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.toString)

  given ConfigWriter[java.time.ZonedDateTime] with
    def write(a: java.time.ZonedDateTime, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.toString)

  given ConfigWriter[java.time.OffsetDateTime] with
    def write(a: java.time.OffsetDateTime, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.toString)

  given javaTimeDurationConfigWriter: ConfigWriter[java.time.Duration] with
    def write(a: java.time.Duration, includeComments: Boolean = false): ConfigValue =
      ConfigWriter[scala.concurrent.duration.Duration]
        .write(scala.concurrent.duration.Duration.fromNanos(a.toNanos), includeComments)

  given ConfigWriter[java.time.Period] with
    def write(a: java.time.Period, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.toString)

  given ConfigWriter[java.time.Year] with
    def write(a: java.time.Year, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.getValue.asInstanceOf[AnyRef])

  given ConfigWriter[java.time.YearMonth] with
    def write(a: java.time.YearMonth, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.toString)

  given ConfigWriter[java.time.MonthDay] with
    def write(a: java.time.MonthDay, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.toString)

  given ConfigWriter[java.time.DayOfWeek] with
    def write(a: java.time.DayOfWeek, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.name)

  given ConfigWriter[java.time.Month] with
    def write(a: java.time.Month, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.name)

  given ConfigWriter[java.time.ZoneId] with
    def write(a: java.time.ZoneId, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.getId)

  given ConfigWriter[java.time.ZoneOffset] with
    def write(a: java.time.ZoneOffset, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.getId)

  // java.util instances
  given ConfigWriter[java.util.UUID] with
    def write(a: java.util.UUID, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.toString)

  given ConfigWriter[java.util.Locale] with
    def write(a: java.util.Locale, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.toLanguageTag)

  given ConfigWriter[java.util.Currency] with
    def write(a: java.util.Currency, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.getCurrencyCode)

  // java.net instances
  given ConfigWriter[java.net.URI] with
    def write(a: java.net.URI, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.toString)

  given ConfigWriter[java.net.InetAddress] with
    def write(a: java.net.InetAddress, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.getHostAddress)

  given ConfigWriter[java.net.InetSocketAddress] with
    def write(a: java.net.InetSocketAddress, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(s"${a.getHostString}:${a.getPort}")

  // java.nio.file instances
  given ConfigWriter[java.nio.file.Path] with
    def write(a: java.nio.file.Path, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.toString)

  // java.util.regex instances
  given ConfigWriter[java.util.regex.Pattern] with
    def write(a: java.util.regex.Pattern, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.pattern)

  // java.math instances
  given javaMathBigIntegerWriter: ConfigWriter[java.math.BigInteger] with
    def write(a: java.math.BigInteger, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.toString)

  given javaMathBigDecimalWriter: ConfigWriter[java.math.BigDecimal] with
    def write(a: java.math.BigDecimal, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.toString)

  // Scala collection instances
  given [A](using w: ConfigWriter[A]): ConfigWriter[Set[A]] = new ConfigWriter[Set[A]]:
    def write(as: Set[A], includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromIterable(as.map(a => w.write(a, includeComments)).toList.asJava)

  given [A](using w: ConfigWriter[A]): ConfigWriter[scala.collection.immutable.TreeSet[A]] =
    new ConfigWriter[scala.collection.immutable.TreeSet[A]]:
      def write(as: scala.collection.immutable.TreeSet[A], includeComments: Boolean = false): ConfigValue =
        ConfigValueFactory.fromIterable(as.toList.map(a => w.write(a, includeComments)).asJava)

  given [A](using w: ConfigWriter[A]): ConfigWriter[Vector[A]] = new ConfigWriter[Vector[A]]:
    def write(as: Vector[A], includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromIterable(as.map(a => w.write(a, includeComments)).asJava)

  given [K, V](using wk: ConfigWriter[K], wv: ConfigWriter[V]): ConfigWriter[Map[K, V]] = new ConfigWriter[Map[K, V]]:
    def write(map: Map[K, V], includeComments: Boolean = false): ConfigValue =
      val entries = map.map { case (k, v) =>
        k.toString -> wv.write(v, includeComments)
      }
      ConfigValueFactory.fromMap(entries.asJava)

  given [K, V](using wk: ConfigWriter[K], wv: ConfigWriter[V]): ConfigWriter[scala.collection.immutable.TreeMap[K, V]] =
    new ConfigWriter[scala.collection.immutable.TreeMap[K, V]]:
      def write(map: scala.collection.immutable.TreeMap[K, V], includeComments: Boolean = false): ConfigValue =
        val entries = map.map { case (k, v) =>
          k.toString -> wv.write(v, includeComments)
        }
        ConfigValueFactory.fromMap(entries.asJava)
