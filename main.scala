//> using scala 3.3.5
//> using dep org.ekrich::sconfig:1.9.0

package example

import org.ekrich.config._
import scala.util.control.NoStackTrace
import scala.deriving.Mirror
import scala.compiletime._
import scala.jdk.CollectionConverters._

/** A config error that never captures stack traces. */
case class ConfigError(msg: String) extends Exception(msg) with NoStackTrace

/** Typeclass for writing an `A` value to a `ConfigValue`. */
trait ConfigWriter[A]:
  def write(a: A): ConfigValue

  /** Contramaps on the input type. */
  def contramap[B](f: B => A): ConfigWriter[B] = new ConfigWriter[B]:
    def write(b: B): ConfigValue = ConfigWriter.this.write(f(b))

/** Typeclass for reading an `A` from a `ConfigValue`, returning `Either[ConfigError, A]`.
  */
trait ConfigReader[A]:
  def read(config: ConfigValue): Either[ConfigError, A]

  /** Maps an `A` to a `B` in `Either` fashion (no exceptions). */
  def emap[B](f: A => Either[String, B]): ConfigReader[B] = new ConfigReader[B]:
    def read(config: ConfigValue): Either[ConfigError, B] =
      ConfigReader.this.read(config).flatMap { a =>
        f(a).left.map(msg => ConfigError(msg))
      }

/** Derivation helpers for writing. */
object ConfigWriter:

  class ConfigSumWriter[A](sum: Mirror.SumOf[A], labels: Tuple, instances: => Vector[ConfigWriter[?]])
      extends ConfigWriter[A]:
    lazy val writers = instances
    def write(a: A): ConfigValue =
      // Identify which subtype (ordinal) we have
      val idx = sum.ordinal(a)
      // Get that subtype's writer
      val subtypeWriter = writers(idx).asInstanceOf[ConfigWriter[A]]
      // Name of the subtype (from its label in the ADT)
      val label = labels.productElement(idx).asInstanceOf[String]

      ConfigValueFactory.fromMap(Map("type" -> label, "value" -> subtypeWriter.write(a)).asJava)
  end ConfigSumWriter

  class ConfigProductWriter[A](product: Mirror.ProductOf[A], instances: => Vector[(String, ConfigWriter[?])])
      extends ConfigWriter[A]:
    lazy val labelsWithInstances = instances
    def write(a: A): ConfigValue =
      val product = a.asInstanceOf[Product]

      // For each field label, pick the correct writer, produce (label -> ConfigValue)
      val kvPairs =
        labelsWithInstances
          .zip(product.productIterator)
          .map { case ((label, w: ConfigWriter[?]), fieldValue) =>
            val fieldCfg = w.asInstanceOf[ConfigWriter[Any]].write(fieldValue)
            label -> fieldCfg
          }
          .toMap

      ConfigValueFactory.fromMap(kvPairs.asJava)
  end ConfigProductWriter

  /** A few base instances. Add as many as you need. */
  given ConfigWriter[String] with
    def write(a: String): ConfigValue =
      ConfigValueFactory.fromAnyRef(a)

  given ConfigWriter[Int] with
    def write(a: Int): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.asInstanceOf[AnyRef])

  given ConfigWriter[Boolean] with
    def write(a: Boolean): ConfigValue =
      ConfigValueFactory.fromAnyRef(a.asInstanceOf[AnyRef])

  /** Summon or derive a ConfigWriter[A]. */
  inline def apply[A](using cw: ConfigWriter[A]): ConfigWriter[A] = cw

  /** Macro entry point for deriving a writer from a `Mirror`. */
  inline def derived[A](using m: Mirror.Of[A]): ConfigWriter[A] =
    lazy val instances = summonWriterInstances[A, m.MirroredElemTypes]
    lazy val labels = constValueTuple[m.MirroredElemLabels].toArray.toVector.map(_.asInstanceOf[String])
    lazy val instancesWithLabels = labels.zip(instances)
    inline m match
      case s: Mirror.SumOf[A]     => sumWriter(s, instances)
      case p: Mirror.ProductOf[A] => productWriter(p, instancesWithLabels)

  inline def summonWriterInstances[T, Elems <: Tuple]: Vector[ConfigWriter[?]] =
    inline erasedValue[Elems] match
      case _: (elem *: elems) => deriveOrSummonWriter[T, elem] +: summonWriterInstances[T, elems]
      case _: EmptyTuple      => Vector.empty

  inline def deriveOrSummonWriter[T, Elem]: ConfigWriter[Elem] =
    inline erasedValue[Elem] match
      case _: T => deriveRecWriter[T, Elem]
      case _    => summonInline[ConfigWriter[Elem]]

  inline def deriveRecWriter[T, Elem]: ConfigWriter[Elem] =
    inline erasedValue[T] match
      case _: Elem => error("infinite recursive derivation")
      case _       => ConfigWriter.derived[Elem](using summonInline[Mirror.Of[Elem]]) // recursive derivation

  /** Derives a writer for a sum type (sealed trait / abstract class + case subtypes).
    */
  private inline def sumWriter[A](sum: Mirror.SumOf[A], instances: => Vector[ConfigWriter[?]]): ConfigWriter[A] =
    ConfigSumWriter(sum, constValueTuple[sum.MirroredElemLabels], instances)

  /** Derives a writer for a product type (case class). */
  private inline def productWriter[A](
      prod: Mirror.ProductOf[A],
      labelsWithInstances: => Vector[(String, ConfigWriter[?])]
  ): ConfigWriter[A] = ConfigProductWriter(prod, labelsWithInstances)

/** Derivation helpers for reading. */
object ConfigReader:

  class ConfigSumReader[A](sum: Mirror.SumOf[A], labelsWithInstances: => Vector[(String, ConfigReader[?])])
      extends ConfigReader[A]:
    val readersMap: Map[String, ConfigReader[?]] = labelsWithInstances.toMap

    def read(config: ConfigValue): Either[ConfigError, A] =
      config.valueType match
        case ConfigValueType.OBJECT =>
          val obj = config.asInstanceOf[ConfigObject]

          (obj.get("type"), obj.get("value")) match
            case (tpeObj, value) if tpeObj.valueType == ConfigValueType.STRING =>
              val tpe = tpeObj.unwrapped.asInstanceOf[String]
              readersMap.get(tpe) match
                case Some(reader: ConfigReader[?]) =>
                  reader.read(value).map(_.asInstanceOf[A])
                case None =>
                  Left(ConfigError(s"Unknown subtype $tpe"))
            case _ =>
              Left(
                ConfigError(
                  "Expected an object with 'type' (string) and 'value' fields."
                )
              )

        case other =>
          Left(ConfigError(s"Expected OBJECT for sum type, got $other"))

  end ConfigSumReader

  class ConfigProductReader[A](product: Mirror.ProductOf[A], instances: => Vector[(String, ConfigReader[?])])
      extends ConfigReader[A]:

    lazy val readersMap = instances.toMap

    def read(config: ConfigValue): Either[ConfigError, A] =
      config.valueType match
        case ConfigValueType.OBJECT =>
          val obj = config.asInstanceOf[ConfigObject]
          val data = obj.unwrapped.asScala

          // We read each field and accumulate errors or results in a list
          val fieldsOrErr =
            instances.map { case (label, reader) =>
              data.get(label) match
                case None =>
                  Left(
                    ConfigError(s"Missing field '$label' for product type.")
                  )
                case Some(rawValue) =>
                  val cfgVal = ConfigValueFactory.fromAnyRef(rawValue)
                  reader.read(cfgVal)
            }

          // Combine them into one Either
          sequence(fieldsOrErr).map { fieldValues =>
            // Convert list -> Tuple -> product A
            product.fromProduct(Tuple.fromArray(fieldValues.toArray))
          }

        case other =>
          Left(ConfigError(s"Expected OBJECT for product type, got $other"))

    private def sequence[A](
        xs: Vector[Either[ConfigError, A]]
    ): Either[ConfigError, Vector[A]] =
      xs.foldLeft[Either[ConfigError, Vector[A]]](Right(Vector.empty)) { case (accOrErr, elemOrErr) =>
        for
          elem <- elemOrErr
          acc <- accOrErr
        yield acc :+ elem
      }

  /** A few base instances. Add as many as you need. */
  given ConfigReader[String] with
    def read(config: ConfigValue): Either[ConfigError, String] =
      config.valueType match
        case ConfigValueType.STRING =>
          Right(config.unwrapped.asInstanceOf[String])
        case other =>
          Left(ConfigError(s"Expected STRING, got $other"))

  given ConfigReader[Int] with
    def read(config: ConfigValue): Either[ConfigError, Int] =
      config.valueType match
        case ConfigValueType.NUMBER =>
          Right(config.unwrapped.asInstanceOf[Number].intValue)
        case other =>
          Left(ConfigError(s"Expected NUMBER, got $other"))

  given ConfigReader[Boolean] with
    def read(config: ConfigValue): Either[ConfigError, Boolean] =
      config.valueType match
        case ConfigValueType.BOOLEAN =>
          Right(config.unwrapped.asInstanceOf[Boolean])
        case other =>
          Left(ConfigError(s"Expected BOOLEAN, got $other"))

  /** Summon or derive a ConfigReader[A]. */
  inline def apply[A](using cr: ConfigReader[A]): ConfigReader[A] = cr

  /** Macro entry point for deriving a reader from a `Mirror`. */
  inline def derived[A](using m: Mirror.Of[A]): ConfigReader[A] =
    lazy val instances = summonReaderInstances[A, m.MirroredElemTypes]
    lazy val labels = constValueTuple[m.MirroredElemLabels].toArray.toVector.map(_.asInstanceOf[String])
    lazy val labelsWithInstances = labels.zip(instances)
    inline m match
      case s: Mirror.SumOf[A]     => sumReader(s, labelsWithInstances)
      case p: Mirror.ProductOf[A] => productReader(p, labelsWithInstances)

  inline def summonReaderInstances[T, Elems <: Tuple]: Vector[ConfigReader[?]] =
    inline erasedValue[Elems] match
      case _: (elem *: elems) => deriveOrSummonReader[T, elem] +: summonReaderInstances[T, elems]
      case _: EmptyTuple      => Vector.empty

  inline def deriveOrSummonReader[T, Elem]: ConfigReader[Elem] =
    inline erasedValue[Elem] match
      case _: T => deriveRecReader[T, Elem]
      case _    => summonInline[ConfigReader[Elem]]

  inline def deriveRecReader[T, Elem]: ConfigReader[Elem] =
    inline erasedValue[T] match
      case _: Elem => error("infinite recursive derivation")
      case _       => ConfigReader.derived[Elem](using summonInline[Mirror.Of[Elem]]) // recursive derivation

  /** Derives a reader for a sum type (sealed trait / abstract class + case subtypes).
    */
  private inline def sumReader[A](
      sum: Mirror.SumOf[A],
      labelsWithInstances: => Vector[(String, ConfigReader[?])]
  ): ConfigReader[A] = ConfigSumReader(sum, labelsWithInstances)

  /** Derives a reader for a product type (case class). */
  private inline def productReader[A](
      p: Mirror.ProductOf[A],
      labelsWithInstances: Vector[(String, ConfigReader[?])]
  ): ConfigReader[A] = ConfigProductReader(p, labelsWithInstances)
