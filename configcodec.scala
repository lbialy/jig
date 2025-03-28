package machinespir.it.jig

import org.ekrich.config.*

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
