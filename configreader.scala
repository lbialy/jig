package machinespir.it.jig

import org.ekrich.config.*
import scala.deriving.Mirror
import scala.quoted.*
import scala.jdk.CollectionConverters.*

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
