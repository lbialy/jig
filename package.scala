package machinespir.it.jig

import org.ekrich.config.*
import scala.util.{Try, Success, Failure}
import java.nio.file.{Path, Files}
import scala.jdk.CollectionConverters.*

export org.ekrich.config.{Config, ConfigFactory, ConfigRenderOptions}

val JigRenderOptions =
  ConfigRenderOptions.defaults.setComments(true).setJson(false).setOriginComments(false).setFormatted(true)

case class ConfigError(msg: String, errors: List[ConfigEntryError] = List.empty, cause: Throwable = null)
    extends Exception(msg, cause):
  override def toString: String =
    s"""ConfigError(
      |  msg = $msg,
      |  errors = $errors,
      |  cause = $cause
      |)""".stripMargin

  def render: String =
    s"""Configuration error: ${msg}
       |  ${errors.map(e => s" * ${e.render}").mkString("\n")}
       |${if cause != null then "Caused by: " else ""}
       |  ${Option(cause).map(c => s"  ${c.toString}").mkString("\n")}
       |""".stripMargin

object read:
  def apply[A](using reader: ConfigReader[A]): Either[ConfigError, A] =
    Try(ConfigFactory.load()) match
      case Success(config) =>
        reader.read(config.root).toEither.left.map { nel =>
          ConfigError(s"Failed to read config", nel.toList)
        }
      case Failure(e) =>
        Left(ConfigError(s"Failed to load config: ${e.getMessage}"))

  object config:
    def apply[A](config: Config)(using reader: ConfigReader[A]): Either[ConfigError, A] =
      reader.read(config.root).toEither.left.map { nel =>
        ConfigError(s"Failed to read config", nel.toList)
      }

  object file:
    def apply[A](path: Path)(using reader: ConfigReader[A]): Either[ConfigError, A] =
      Try(ConfigFactory.parseFile(path.toFile)) match
        case Success(config) =>
          reader.read(config.root).toEither.left.map { nel =>
            ConfigError(s"Failed to read config", nel.toList)
          }
        case Failure(e) =>
          Left(ConfigError(s"Failed to read config from path ${path}: ${e.getMessage}"))

object write:
  object file:
    def apply[A](
        a: A,
        path: Path,
        includeComments: Boolean = true,
        renderOptions: ConfigRenderOptions = JigRenderOptions
    )(using
        writer: ConfigWriter[A]
    ): Either[ConfigError, Unit] =
      Try {
        val configValue = writer.write(a, includeComments)
        val rendered = configValue.render(renderOptions)
        Files.writeString(path, rendered)

        ()
      }.toEither.left.map { e =>
        ConfigError(s"Failed to write config to path ${path}: ${e.getMessage}", cause = e)
      }

  def apply[A](a: A, includeComments: Boolean = true, renderOptions: ConfigRenderOptions = JigRenderOptions)(using
      writer: ConfigWriter[A]
  ): String =
    val configValue = writer.write(a, includeComments)
    configValue.render(renderOptions)
