package machinespir.it.jig

import org.ekrich.config.*
import org.ekrich.config.impl.Origin

import scala.util.control.NoStackTrace
import scala.deriving.Mirror
import scala.quoted.*
import scala.jdk.CollectionConverters.*

/** Typeclass for writing an `A` value to a `ConfigValue`. */
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
      ConfigValueFactory.fromAnyRef(a.toNanos.asInstanceOf[AnyRef])

  given ConfigWriter[scala.concurrent.duration.FiniteDuration] with
    def write(a: scala.concurrent.duration.FiniteDuration, includeComments: Boolean = false): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.toNanos.asInstanceOf[AnyRef])

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
