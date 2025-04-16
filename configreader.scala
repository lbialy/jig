package machinespir.it.jig

import org.ekrich.config.*
import scala.deriving.Mirror
import scala.quoted.*
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}
import scala.annotation.implicitNotFound

/** Typeclass for reading an `A` from a `ConfigValue`, returning `ReadResult[A]`.
  */
@implicitNotFound(
  "No ConfigReader found for type ${A}. Try to derive it using `derives ConfigReader` clause or `ConfigReader.derived[A]`. You might also want to look at `ConfigCodec` which offers both reading and writing."
)
trait ConfigReader[A]:
  def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[A]

  /** Maps an `A` to a `B` in `ReadResult` fashion (no exceptions). */
  def emap[B](f: A => Either[String, B]): ConfigReader[B] = new ConfigReader[B]:
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[B] =
      ConfigReader.this.read(config, path).flatMap { a =>
        f(a).fold(
          msg => ReadResult.failure(ConfigEntryError(msg, path)),
          ReadResult.success
        )
      }

/** Derivation helpers for reading. */
object ConfigReader:

  class ConfigSumReader[A](labelsWithInstances: => Vector[(String, ConfigReader[?])]) extends ConfigReader[A]:
    val readersMap: Map[String, ConfigReader[?]] = labelsWithInstances.toMap

    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[A] =
      config.valueType match
        case ConfigValueType.STRING =>
          // Handle case where config is just a string (empty enum member)
          val tpe = config.unwrapped.asInstanceOf[String]
          readersMap.get(tpe) match
            case Some(reader) =>
              // For empty enum members, we can use any empty object as the value
              reader.read(ConfigValueFactory.fromMap(Map.empty.asJava), path).map(_.asInstanceOf[A])
            case None =>
              ReadResult.failure(ConfigEntryError(s"Unknown subtype $tpe", path))

        case ConfigValueType.OBJECT =>
          val obj = config.asInstanceOf[ConfigObject]

          (Option(obj.get("type")), Option(obj.get("value"))) match
            case (None, _) | (_, None) =>
              ReadResult.failure(
                ConfigEntryError(
                  "Expected an object with 'type' (string) and 'value' fields.",
                  path
                )
              )

            case (Some(tpeObj), Some(valueObj)) if tpeObj.valueType != ConfigValueType.STRING =>
              ReadResult.failure(
                ConfigEntryError(
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
                  ReadResult.failure(ConfigEntryError(s"Unknown subtype $tpe", ConfigPath.Field("type") :: path))

        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected STRING or OBJECT for sum type, got $other", path))

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
                        ConfigEntryError(s"Missing field '$label' for product type.", path)
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
          ReadResult.failure(ConfigEntryError(s"Expected OBJECT for product type, got $other", path))

  /** A few base instances. Add as many as you need. */
  given ConfigReader[String] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[String] =
      config.valueType match
        case ConfigValueType.STRING =>
          ReadResult.success(config.unwrapped.asInstanceOf[String])
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given ConfigReader[Int] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[Int] =
      config.valueType match
        case ConfigValueType.NUMBER =>
          ReadResult.success(config.unwrapped.asInstanceOf[Number].intValue)
        case ConfigValueType.STRING =>
          try ReadResult.success(config.unwrapped.asInstanceOf[String].toInt)
          catch
            case e: NumberFormatException =>
              ReadResult.failure(ConfigEntryError(s"Invalid Int format: ${e.getMessage}", path))
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected NUMBER or STRING, got $other", path))

  given ConfigReader[Boolean] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[Boolean] =
      config.valueType match
        case ConfigValueType.BOOLEAN =>
          ReadResult.success(config.unwrapped.asInstanceOf[Boolean])
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected BOOLEAN, got $other", path))

  given ConfigReader[Byte] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[Byte] =
      config.valueType match
        case ConfigValueType.NUMBER =>
          ReadResult.success(config.unwrapped.asInstanceOf[Number].byteValue)
        case ConfigValueType.STRING =>
          try ReadResult.success(config.unwrapped.asInstanceOf[String].toByte)
          catch
            case e: NumberFormatException =>
              ReadResult.failure(ConfigEntryError(s"Invalid Byte format: ${e.getMessage}", path))
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected NUMBER or STRING, got $other", path))

  given ConfigReader[Short] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[Short] =
      config.valueType match
        case ConfigValueType.NUMBER =>
          ReadResult.success(config.unwrapped.asInstanceOf[Number].shortValue)
        case ConfigValueType.STRING =>
          try ReadResult.success(config.unwrapped.asInstanceOf[String].toShort)
          catch
            case e: NumberFormatException =>
              ReadResult.failure(ConfigEntryError(s"Invalid Short format: ${e.getMessage}", path))
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected NUMBER or STRING, got $other", path))

  given ConfigReader[Long] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[Long] =
      config.valueType match
        case ConfigValueType.NUMBER =>
          ReadResult.success(config.unwrapped.asInstanceOf[Number].longValue)
        case ConfigValueType.STRING =>
          try ReadResult.success(config.unwrapped.asInstanceOf[String].toLong)
          catch
            case e: NumberFormatException =>
              ReadResult.failure(ConfigEntryError(s"Invalid Long format: ${e.getMessage}", path))
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected NUMBER or STRING, got $other", path))

  given ConfigReader[Float] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[Float] =
      config.valueType match
        case ConfigValueType.NUMBER =>
          ReadResult.success(config.unwrapped.asInstanceOf[Number].floatValue)
        case ConfigValueType.STRING =>
          try ReadResult.success(config.unwrapped.asInstanceOf[String].toFloat)
          catch
            case e: NumberFormatException =>
              ReadResult.failure(ConfigEntryError(s"Invalid Float format: ${e.getMessage}", path))
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected NUMBER or STRING, got $other", path))

  given ConfigReader[Double] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[Double] =
      config.valueType match
        case ConfigValueType.NUMBER =>
          ReadResult.success(config.unwrapped.asInstanceOf[Number].doubleValue)
        case ConfigValueType.STRING =>
          try ReadResult.success(config.unwrapped.asInstanceOf[String].toDouble)
          catch
            case e: NumberFormatException =>
              ReadResult.failure(ConfigEntryError(s"Invalid Double format: ${e.getMessage}", path))
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected NUMBER or STRING, got $other", path))

  given ConfigReader[Char] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[Char] =
      config.valueType match
        case ConfigValueType.STRING =>
          val str = config.unwrapped.asInstanceOf[String]
          if str.length == 1 then ReadResult.success(str.charAt(0))
          else ReadResult.failure(ConfigEntryError("Expected single character string", path))
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given ConfigReader[BigInt] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[BigInt] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(BigInt(config.unwrapped.asInstanceOf[String]))
          catch
            case e: NumberFormatException =>
              ReadResult.failure(ConfigEntryError(s"Invalid BigInt format: ${e.getMessage}", path))
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given ConfigReader[BigDecimal] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[BigDecimal] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(BigDecimal(config.unwrapped.asInstanceOf[String]))
          catch
            case e: NumberFormatException =>
              ReadResult.failure(ConfigEntryError(s"Invalid BigDecimal format: ${e.getMessage}", path))
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given ConfigReader[scala.concurrent.duration.Duration] with
    def read(
        config: ConfigValue,
        path: List[ConfigPath] = List(ConfigPath.Root)
    ): ReadResult[scala.concurrent.duration.Duration] =
      config.valueType match
        case ConfigValueType.STRING =>
          Try(config.atPath("here").getDuration("here")) match
            case Success(javaDuration) =>
              ReadResult.success(scala.concurrent.duration.Duration.fromNanos(javaDuration.toNanos))
            case Failure(e) =>
              ReadResult.failure(ConfigEntryError(s"Invalid duration format: ${e.getMessage}", path))
        case ConfigValueType.NUMBER =>
          ReadResult.success(
            scala.concurrent.duration.Duration.fromNanos(config.unwrapped.asInstanceOf[Number].longValue)
          )
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected STRING or NUMBER, got $other", path))

  given ConfigReader[scala.concurrent.duration.FiniteDuration] with
    def read(
        config: ConfigValue,
        path: List[ConfigPath] = List(ConfigPath.Root)
    ): ReadResult[scala.concurrent.duration.FiniteDuration] =
      config.valueType match
        case ConfigValueType.STRING =>
          Try(config.atPath("here").getDuration("here")) match
            case Success(javaDuration) =>
              ReadResult.success(scala.concurrent.duration.Duration.fromNanos(javaDuration.toNanos))
            case Failure(e) =>
              ReadResult.failure(ConfigEntryError(s"Invalid duration format: ${e.getMessage}", path))
        case ConfigValueType.NUMBER =>
          ReadResult.success(
            scala.concurrent.duration.FiniteDuration(
              config.unwrapped.asInstanceOf[Number].longValue,
              scala.concurrent.duration.NANOSECONDS
            )
          )
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected STRING or NUMBER, got $other", path))

  given javaLongReader: ConfigReader[java.lang.Long] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.lang.Long] =
      config.valueType match
        case ConfigValueType.NUMBER =>
          ReadResult.success(config.unwrapped.asInstanceOf[Number].longValue)
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected NUMBER, got $other", path))

  given javaDoubleReader: ConfigReader[java.lang.Double] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.lang.Double] =
      config.valueType match
        case ConfigValueType.NUMBER =>
          ReadResult.success(config.unwrapped.asInstanceOf[Number].doubleValue)
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected NUMBER, got $other", path))

  given javaFloatReader: ConfigReader[java.lang.Float] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.lang.Float] =
      config.valueType match
        case ConfigValueType.NUMBER =>
          ReadResult.success(config.unwrapped.asInstanceOf[Number].floatValue)
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected NUMBER, got $other", path))

  given javaShortReader: ConfigReader[java.lang.Short] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.lang.Short] =
      config.valueType match
        case ConfigValueType.NUMBER =>
          ReadResult.success(config.unwrapped.asInstanceOf[Number].shortValue)
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected NUMBER, got $other", path))

  given javaByteReader: ConfigReader[java.lang.Byte] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.lang.Byte] =
      config.valueType match
        case ConfigValueType.NUMBER =>
          ReadResult.success(config.unwrapped.asInstanceOf[Number].byteValue)
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected NUMBER, got $other", path))

  given javaCharacterReader: ConfigReader[java.lang.Character] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.lang.Character] =
      config.valueType match
        case ConfigValueType.STRING =>
          val str = config.unwrapped.asInstanceOf[String]
          if str.length == 1 then ReadResult.success(str.charAt(0))
          else ReadResult.failure(ConfigEntryError("Expected single character string", path))
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given [A](using r: ConfigReader[A]): ConfigReader[Option[A]] = new ConfigReader[Option[A]]:
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[Option[A]] =
      if config.valueType == ConfigValueType.NULL then ReadResult.success(None)
      else r.read(config, path).map(Some(_))

  given [A, B](using ra: ConfigReader[A], rb: ConfigReader[B]): ConfigReader[Either[A, B]] =
    new ConfigReader[Either[A, B]]:
      def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[Either[A, B]] =
        config.valueType match
          case ConfigValueType.OBJECT =>
            val obj = config.asInstanceOf[ConfigObject]
            (Option(obj.get("type")), Option(obj.get("value"))) match
              case (Some(tpe), Some(value)) if tpe.unwrapped == "left" =>
                ra.read(value, ConfigPath.Field("value") :: path).map(Left(_))
              case (Some(tpe), Some(value)) if tpe.unwrapped == "right" =>
                rb.read(value, ConfigPath.Field("value") :: path).map(Right(_))
              case _ =>
                ReadResult.failure(
                  ConfigEntryError("Expected object with 'type' ('left' or 'right') and 'value' fields", path)
                )
          case other =>
            ReadResult.failure(ConfigEntryError(s"Expected OBJECT, got $other", path))

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
          ReadResult.failure(ConfigEntryError(s"Expected LIST, got $other", path))

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

  given ConfigReader[java.time.Instant] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.time.Instant] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(java.time.Instant.parse(config.unwrapped.asInstanceOf[String]))
          catch
            case e: Exception => ReadResult.failure(ConfigEntryError(s"Invalid Instant format: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given ConfigReader[java.time.LocalDate] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.time.LocalDate] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(java.time.LocalDate.parse(config.unwrapped.asInstanceOf[String]))
          catch
            case e: Exception =>
              ReadResult.failure(ConfigEntryError(s"Invalid LocalDate format: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given ConfigReader[java.time.LocalTime] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.time.LocalTime] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(java.time.LocalTime.parse(config.unwrapped.asInstanceOf[String]))
          catch
            case e: Exception =>
              ReadResult.failure(ConfigEntryError(s"Invalid LocalTime format: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given ConfigReader[java.time.LocalDateTime] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.time.LocalDateTime] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(java.time.LocalDateTime.parse(config.unwrapped.asInstanceOf[String]))
          catch
            case e: Exception =>
              ReadResult.failure(ConfigEntryError(s"Invalid LocalDateTime format: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given ConfigReader[java.time.ZonedDateTime] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.time.ZonedDateTime] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(java.time.ZonedDateTime.parse(config.unwrapped.asInstanceOf[String]))
          catch
            case e: Exception =>
              ReadResult.failure(ConfigEntryError(s"Invalid ZonedDateTime format: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given ConfigReader[java.time.OffsetDateTime] with
    def read(
        config: ConfigValue,
        path: List[ConfigPath] = List(ConfigPath.Root)
    ): ReadResult[java.time.OffsetDateTime] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(java.time.OffsetDateTime.parse(config.unwrapped.asInstanceOf[String]))
          catch
            case e: Exception =>
              ReadResult.failure(ConfigEntryError(s"Invalid OffsetDateTime format: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given javaTimeDurationReader: ConfigReader[java.time.Duration] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.time.Duration] =
      config.valueType match
        case ConfigValueType.STRING =>
          val key = path.head match
            case ConfigPath.Root        => "root"
            case ConfigPath.Field(name) => name
            case ConfigPath.Index(idx)  => idx.toString

          Try(config.atPath(key).getDuration(key)) match
            case Success(javaDuration) => ReadResult.success(javaDuration)
            case Failure(e) => ReadResult.failure(ConfigEntryError(s"Invalid duration format: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given ConfigReader[java.time.Period] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.time.Period] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(java.time.Period.parse(config.unwrapped.asInstanceOf[String]))
          catch
            case e: Exception => ReadResult.failure(ConfigEntryError(s"Invalid Period format: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given ConfigReader[java.time.Year] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.time.Year] =
      config.valueType match
        case ConfigValueType.NUMBER =>
          try ReadResult.success(java.time.Year.of(config.unwrapped.asInstanceOf[Number].intValue))
          catch case e: Exception => ReadResult.failure(ConfigEntryError(s"Invalid Year value: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected NUMBER, got $other", path))

  given ConfigReader[java.time.YearMonth] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.time.YearMonth] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(java.time.YearMonth.parse(config.unwrapped.asInstanceOf[String]))
          catch
            case e: Exception =>
              ReadResult.failure(ConfigEntryError(s"Invalid YearMonth format: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given ConfigReader[java.time.MonthDay] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.time.MonthDay] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(java.time.MonthDay.parse(config.unwrapped.asInstanceOf[String]))
          catch
            case e: Exception => ReadResult.failure(ConfigEntryError(s"Invalid MonthDay format: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given ConfigReader[java.time.DayOfWeek] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.time.DayOfWeek] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(java.time.DayOfWeek.valueOf(config.unwrapped.asInstanceOf[String].toUpperCase))
          catch
            case e: Exception => ReadResult.failure(ConfigEntryError(s"Invalid DayOfWeek value: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given ConfigReader[java.time.Month] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.time.Month] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(java.time.Month.valueOf(config.unwrapped.asInstanceOf[String].toUpperCase))
          catch case e: Exception => ReadResult.failure(ConfigEntryError(s"Invalid Month value: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given ConfigReader[java.time.ZoneId] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.time.ZoneId] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(java.time.ZoneId.of(config.unwrapped.asInstanceOf[String]))
          catch
            case e: Exception => ReadResult.failure(ConfigEntryError(s"Invalid ZoneId format: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given ConfigReader[java.time.ZoneOffset] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.time.ZoneOffset] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(java.time.ZoneOffset.of(config.unwrapped.asInstanceOf[String]))
          catch
            case e: Exception =>
              ReadResult.failure(ConfigEntryError(s"Invalid ZoneOffset format: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  // java.util instances
  given ConfigReader[java.util.UUID] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.util.UUID] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(java.util.UUID.fromString(config.unwrapped.asInstanceOf[String]))
          catch case e: Exception => ReadResult.failure(ConfigEntryError(s"Invalid UUID format: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given ConfigReader[java.util.Locale] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.util.Locale] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(java.util.Locale.forLanguageTag(config.unwrapped.asInstanceOf[String]))
          catch
            case e: Exception => ReadResult.failure(ConfigEntryError(s"Invalid Locale format: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given ConfigReader[java.util.Currency] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.util.Currency] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(java.util.Currency.getInstance(config.unwrapped.asInstanceOf[String]))
          catch
            case e: Exception => ReadResult.failure(ConfigEntryError(s"Invalid Currency code: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  // java.net instances
  given ConfigReader[java.net.URI] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.net.URI] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(java.net.URI.create(config.unwrapped.asInstanceOf[String]))
          catch case e: Exception => ReadResult.failure(ConfigEntryError(s"Invalid URI format: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given ConfigReader[java.net.InetAddress] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.net.InetAddress] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(java.net.InetAddress.getByName(config.unwrapped.asInstanceOf[String]))
          catch
            case e: Exception =>
              ReadResult.failure(ConfigEntryError(s"Invalid InetAddress format: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given ConfigReader[java.net.InetSocketAddress] with
    def read(
        config: ConfigValue,
        path: List[ConfigPath] = List(ConfigPath.Root)
    ): ReadResult[java.net.InetSocketAddress] =
      config.valueType match
        case ConfigValueType.STRING =>
          try
            val str = config.unwrapped.asInstanceOf[String]
            val parts = str.split(":")
            if parts.length != 2 then throw new IllegalArgumentException("Expected format: host:port")
            val host = parts(0)
            val port = parts(1).toInt
            ReadResult.success(java.net.InetSocketAddress.createUnresolved(host, port))
          catch
            case e: Exception =>
              ReadResult.failure(
                ConfigEntryError(s"Invalid InetSocketAddress format (expected host:port): ${e.getMessage}", path)
              )
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  // java.nio.file instances
  given ConfigReader[java.nio.file.Path] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.nio.file.Path] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(java.nio.file.Path.of(config.unwrapped.asInstanceOf[String]))
          catch case e: Exception => ReadResult.failure(ConfigEntryError(s"Invalid Path format: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  // java.util.regex instances
  given ConfigReader[java.util.regex.Pattern] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.util.regex.Pattern] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(java.util.regex.Pattern.compile(config.unwrapped.asInstanceOf[String]))
          catch
            case e: Exception => ReadResult.failure(ConfigEntryError(s"Invalid regex pattern: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  // java.math instances (if not already present)
  given javaMathBigIntegerReader: ConfigReader[java.math.BigInteger] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.math.BigInteger] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(new java.math.BigInteger(config.unwrapped.asInstanceOf[String]))
          catch
            case e: Exception =>
              ReadResult.failure(ConfigEntryError(s"Invalid BigInteger format: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  given javaMathBigDecimalReader: ConfigReader[java.math.BigDecimal] with
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[java.math.BigDecimal] =
      config.valueType match
        case ConfigValueType.STRING =>
          try ReadResult.success(new java.math.BigDecimal(config.unwrapped.asInstanceOf[String]))
          catch
            case e: Exception =>
              ReadResult.failure(ConfigEntryError(s"Invalid BigDecimal format: ${e.getMessage}", path))
        case other => ReadResult.failure(ConfigEntryError(s"Expected STRING, got $other", path))

  // Scala collection instances
  given [A](using r: ConfigReader[A]): ConfigReader[Set[A]] = new ConfigReader[Set[A]]:
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[Set[A]] =
      config.valueType match
        case ConfigValueType.LIST =>
          val list = config.asInstanceOf[ConfigList]
          val results = list.asScala.toList.zipWithIndex.map { case (elem, idx) =>
            r.read(elem, ConfigPath.Index(idx) :: path)
          }
          ReadResult.sequence(results).map(_.toSet)
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected LIST, got $other", path))

  given [A: Ordering](using r: ConfigReader[A]): ConfigReader[scala.collection.immutable.TreeSet[A]] =
    new ConfigReader[scala.collection.immutable.TreeSet[A]]:
      def read(
          config: ConfigValue,
          path: List[ConfigPath] = List(ConfigPath.Root)
      ): ReadResult[scala.collection.immutable.TreeSet[A]] =
        config.valueType match
          case ConfigValueType.LIST =>
            val list = config.asInstanceOf[ConfigList]
            val results = list.asScala.toList.zipWithIndex.map { case (elem, idx) =>
              r.read(elem, ConfigPath.Index(idx) :: path)
            }
            ReadResult.sequence(results).map(scala.collection.immutable.TreeSet.from(_))
          case other =>
            ReadResult.failure(ConfigEntryError(s"Expected LIST, got $other", path))

  given [A](using r: ConfigReader[A]): ConfigReader[Vector[A]] = new ConfigReader[Vector[A]]:
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[Vector[A]] =
      config.valueType match
        case ConfigValueType.LIST =>
          val list = config.asInstanceOf[ConfigList]
          val results = list.asScala.toList.zipWithIndex.map { case (elem, idx) =>
            r.read(elem, ConfigPath.Index(idx) :: path)
          }
          ReadResult.sequence(results).map(_.toVector)
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected LIST, got $other", path))

  given [K, V](using rk: ConfigReader[K], rv: ConfigReader[V]): ConfigReader[Map[K, V]] = new ConfigReader[Map[K, V]]:
    def read(config: ConfigValue, path: List[ConfigPath] = List(ConfigPath.Root)): ReadResult[Map[K, V]] =
      config.valueType match
        case ConfigValueType.OBJECT =>
          val obj = config.asInstanceOf[ConfigObject]
          val results = obj.entrySet.asScala.map { entry =>
            val keyStr = entry.getKey
            val value = entry.getValue
            for
              key <- rk.read(ConfigValueFactory.fromAnyRef(keyStr), ConfigPath.Field(keyStr) :: path)
              value <- rv.read(value, ConfigPath.Field(keyStr) :: path)
            yield key -> value
          }
          ReadResult.sequence(results.toList).map(_.toMap)
        case other =>
          ReadResult.failure(ConfigEntryError(s"Expected OBJECT, got $other", path))

  given [K: Ordering, V](using
      rk: ConfigReader[K],
      rv: ConfigReader[V]
  ): ConfigReader[scala.collection.immutable.TreeMap[K, V]] =
    new ConfigReader[scala.collection.immutable.TreeMap[K, V]]:
      def read(
          config: ConfigValue,
          path: List[ConfigPath] = List(ConfigPath.Root)
      ): ReadResult[scala.collection.immutable.TreeMap[K, V]] =
        config.valueType match
          case ConfigValueType.OBJECT =>
            val obj = config.asInstanceOf[ConfigObject]
            val results = obj.entrySet.asScala.map { entry =>
              val keyStr = entry.getKey
              val value = entry.getValue
              for
                key <- rk.read(ConfigValueFactory.fromAnyRef(keyStr), ConfigPath.Field(keyStr) :: path)
                value <- rv.read(value, ConfigPath.Field(keyStr) :: path)
              yield key -> value
            }
            ReadResult.sequence(results.toList).map(pairs => scala.collection.immutable.TreeMap.from(pairs))
          case other =>
            ReadResult.failure(ConfigEntryError(s"Expected OBJECT, got $other", path))
