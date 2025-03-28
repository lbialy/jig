package machinespir.it.jig

import org.ekrich.config.*
import org.ekrich.config.impl.Origin

import scala.util.control.NoStackTrace
import scala.deriving.Mirror
import scala.quoted.*
import scala.jdk.CollectionConverters.*
import scala.annotation.StaticAnnotation

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

case class comment(text: String) extends StaticAnnotation

/** Represents a path element in the config structure */
enum ConfigPath:
  case Root
  case Field(name: String)
  case Index(idx: Int)

  def render: String = this match
    case Root        => "root"
    case Field(name) => name
    case Index(idx)  => s"($idx)"

object ConfigPath:
  def renderPath(path: List[ConfigPath]): String =
    path.reverse.foldLeft("root")((acc, p) =>
      p match
        case Root        => acc
        case Field(name) => s"$acc.$name"
        case Index(idx)  => s"$acc($idx)"
    )

/** A config error that never captures stack traces. */
case class ConfigError(msg: String, path: List[ConfigPath] = List(ConfigPath.Root))
    extends Exception(
      s"$msg (at ${ConfigPath.renderPath(path)})"
    )
    with NoStackTrace

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

/** Typeclass for reading an `A` from a `ConfigValue`, returning `ReadResult[A]`.
  */
trait ConfigReader[A]:
  def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[A]

  /** Maps an `A` to a `B` in `ReadResult` fashion (no exceptions). */
  def emap[B](f: A => Either[String, B]): ConfigReader[B] = new ConfigReader[B]:
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[B] =
      ConfigReader.this.read(config, path).flatMap { a =>
        f(a).fold(
          msg => ReadResult.failure(ConfigError(msg, path)),
          ReadResult.success
        )
      }

/** Derivation helpers for reading. */
object ConfigReader:

  class ConfigSumReader[A](labelsWithInstances: => Vector[(String, ConfigReader[?])]) extends ConfigReader[A]:
    val readersMap: Map[String, ConfigReader[?]] = labelsWithInstances.toMap

    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[A] =
      config.valueType match
        case ConfigValueType.OBJECT =>
          val obj = config.asInstanceOf[ConfigObject]

          (Option(obj.get("type")), Option(obj.get("value"))) match
            case (None, _) | (_, None) =>
              ReadResult.failure(
                ConfigError(
                  "Expected an object with 'type' (string) and 'value' fields.",
                  path
                )
              )

            case (Some(tpeObj), Some(valueObj)) if tpeObj.valueType != ConfigValueType.STRING =>
              ReadResult.failure(
                ConfigError(
                  "Expected 'type' field to be a string.",
                  ConfigPath.Field("type") :: path
                )
              )

            case (Some(tpeObj), Some(valueObj)) =>
              val tpe = tpeObj.unwrapped.asInstanceOf[String]
              readersMap.get(tpe) match
                case Some(reader) =>
                  reader.read(valueObj, ConfigPath.Field("value") :: path).map(_.asInstanceOf[A])
                case None =>
                  ReadResult.failure(ConfigError(s"Unknown subtype $tpe", ConfigPath.Field("type") :: path))

        case other =>
          ReadResult.failure(ConfigError(s"Expected OBJECT for sum type, got $other", path))

  end ConfigSumReader

  class ConfigProductReader[A](
      product: Mirror.ProductOf[A],
      instances: => Vector[(String, ConfigReader[?])],
      defaultParams: Map[String, Any] = Map.empty
  ) extends ConfigReader[A]:

    lazy val readersMap = instances.toMap

    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[A] =
      config.valueType match
        case ConfigValueType.OBJECT =>
          val obj = config.asInstanceOf[ConfigObject]
          val data = obj.unwrapped.asScala

          // We read each field and accumulate errors or results in a list
          val fieldsResults =
            instances.map { case (label, reader) =>
              data.get(label) match
                case None =>
                  // If field is missing, try to use default value
                  defaultParams.get(label) match
                    case Some(defaultValue) =>
                      ReadResult.success(defaultValue)
                    case None =>
                      ReadResult.failure(
                        ConfigError(s"Missing field '$label' for product type.", path)
                      )
                case Some(rawValue) =>
                  val cfgVal = ConfigValueFactory.fromAnyRef(rawValue)
                  reader.read(cfgVal, ConfigPath.Field(label) :: path)
            }

          // Combine them into one ReadResult
          val allFields = fieldsResults.foldRight[ReadResult[List[Any]]](ReadResult.success(Nil)) { (fieldRes, acc) =>
            ReadResult.map2(fieldRes, acc)(_ :: _)
          }

          allFields.map { fieldValues =>
            // Convert list -> Tuple -> product A
            product.fromProduct(Tuple.fromArray(fieldValues.toArray))
          }

        case other =>
          ReadResult.failure(ConfigError(s"Expected OBJECT for product type, got $other", path))

  /** A few base instances. Add as many as you need. */
  given ConfigReader[String] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[String] =
      config.valueType match
        case ConfigValueType.STRING =>
          ReadResult.success(config.unwrapped.asInstanceOf[String])
        case other =>
          ReadResult.failure(ConfigError(s"Expected STRING, got $other", path))

  given ConfigReader[Int] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[Int] =
      config.valueType match
        case ConfigValueType.NUMBER =>
          ReadResult.success(config.unwrapped.asInstanceOf[Number].intValue)
        case other =>
          ReadResult.failure(ConfigError(s"Expected NUMBER, got $other", path))

  given ConfigReader[Boolean] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[Boolean] =
      config.valueType match
        case ConfigValueType.BOOLEAN =>
          ReadResult.success(config.unwrapped.asInstanceOf[Boolean])
        case other =>
          ReadResult.failure(ConfigError(s"Expected BOOLEAN, got $other", path))

  given [A](using r: ConfigReader[A]): ConfigReader[List[A]] = new ConfigReader[List[A]]:
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[List[A]] =
      config.valueType match
        case ConfigValueType.LIST =>
          val list = config.asInstanceOf[ConfigList]
          val results = list.asScala.toList.zipWithIndex.map { case (elem, idx) =>
            r.read(elem, ConfigPath.Index(idx) :: path)
          }
          ReadResult.sequence(results)
        case other =>
          ReadResult.failure(ConfigError(s"Expected LIST, got $other", path))

  /** Summon or derive a ConfigReader[A]. */
  inline def apply[A](using cr: ConfigReader[A]): ConfigReader[A] = cr

  inline def derived[A]: ConfigReader[A] = ${ derivedMacro[A] }

  inline given configReaderFromCodec[A](using cc: ConfigCodec[A]): ConfigReader[A] = cc.reader

  /** Macro entry point for deriving a reader from a `Mirror`. */
  def derivedMacro[A: Type](using Quotes): Expr[ConfigReader[A]] =
    import quotes.reflect.*

    def findDefaultParams: Expr[Map[String, Any]] =
      TypeRepr.of[A].classSymbol match
        case None => '{ Map.empty[String, Any] }
        case Some(sym) =>
          val comp = sym.companionClass
          try
            val mod = Ref(sym.companionModule)
            val names =
              for p <- sym.caseFields if p.flags.is(Flags.HasDefault)
              yield p.name
            val namesExpr: Expr[List[String]] =
              Expr.ofList(names.map(Expr(_)))

            val body = comp.tree.asInstanceOf[ClassDef].body
            val idents: List[Ref] =
              for
                case deff @ DefDef(name, _, _, _) <- body
                if name.startsWith("$lessinit$greater$default")
              yield mod.select(deff.symbol)
            val typeArgs = TypeRepr.of[A].typeArgs
            val identsExpr: Expr[List[Any]] =
              if typeArgs.isEmpty then Expr.ofList(idents.map(_.asExpr))
              else Expr.ofList(idents.map(_.appliedToTypes(typeArgs).asExpr))

            '{ $namesExpr.zip($identsExpr).toMap }
          catch case _: ClassCastException => '{ Map.empty[String, Any] }

    def prepareReaderInstances(
        elemLabels: Type[?],
        elemTypes: Type[?],
        tryDerive: Boolean = false
    ): List[Expr[(String, ConfigReader[?])]] =
      (elemLabels, elemTypes) match
        case ('[EmptyTuple], '[EmptyTuple]) => Nil
        case ('[label *: labelsTail], '[tpe *: tpesTail]) =>
          val label = Type.valueOfConstant[label].get.asInstanceOf[String]
          val fieldName = Expr(label)
          val fieldReader = Expr.summon[ConfigReader[tpe]].getOrElse {
            if tryDerive then '{ ConfigReader.derived[tpe] }
            else report.errorAndAbort(s"Missing ConfigReader for type ${Type.show[tpe]}")
          }
          val namedInstance = '{ ($fieldName, $fieldReader) }
          namedInstance :: prepareReaderInstances(Type.of[labelsTail], Type.of[tpesTail], tryDerive)

    Expr.summon[Mirror.Of[A]].get match
      case '{
            $m: Mirror.ProductOf[A] { type MirroredElemLabels = labels; type MirroredElemTypes = types }
          } =>
        val instancesExpr = Expr.ofList(prepareReaderInstances(Type.of[labels], Type.of[types]))
        val defaultParams = findDefaultParams
        '{ ConfigProductReader($m, $instancesExpr.toVector, $defaultParams) }

      case '{
            $m: Mirror.SumOf[A] { type MirroredElemLabels = labels; type MirroredElemTypes = types }
          } =>
        val readers = prepareReaderInstances(Type.of[labels], Type.of[types], tryDerive = true)
        val readersExpr = Expr.ofList(readers)
        '{ ConfigSumReader($readersExpr.toVector) }

/** Typeclass that combines both reading and writing capabilities for a type `A`. */
trait ConfigCodec[A]:
  def reader: ConfigReader[A]
  def writer: ConfigWriter[A]

  def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[A] =
    reader.read(config, path)

  def write(a: A, includeComments: Boolean = false): ConfigValue =
    writer.write(a, includeComments)

object ConfigCodec:
  class ConfigCodecImpl[A](val reader: ConfigReader[A], val writer: ConfigWriter[A]) extends ConfigCodec[A]

  /** Summon or derive a ConfigCodec[A]. */
  inline def apply[A](using cc: ConfigCodec[A]): ConfigCodec[A] = cc

  /** Derive a ConfigCodec by combining derived ConfigReader and ConfigWriter instances. */
  inline def derived[A]: ConfigCodec[A] = ConfigCodecImpl(ConfigReader.derived[A], ConfigWriter.derived[A])

  /** Base instances that combine existing ConfigReader and ConfigWriter instances */
  inline given configCodecFromReaderAndWriter[A](using r: ConfigReader[A], w: ConfigWriter[A]): ConfigCodec[A] =
    ConfigCodecImpl(r, w)
