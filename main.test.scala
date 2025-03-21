//> using scala 3.3.5
//> using dep org.ekrich::sconfig:1.9.0
//> using dep org.scalameta::munit::1.0.0-M10

package example

import org.ekrich.config.*
import munit.FunSuite
import scala.jdk.CollectionConverters.*

// Test ADTs
case class Person(name: String, age: Int) derives ConfigWriter, ConfigReader
case class Address(street: String, number: Int, isApartment: Boolean) derives ConfigWriter, ConfigReader

// Sum type example
enum Animal derives ConfigWriter, ConfigReader:
  case Dog(name: String, age: Int)
  case Cat(name: String, lives: Int)
  case Bird(species: String, canFly: Boolean)

// Complex nested type with List support
object ConfigInstances:
  given [A](using w: ConfigWriter[A]): ConfigWriter[List[A]] = new ConfigWriter[List[A]]:
    def write(as: List[A]): ConfigValue =
      val values = as.map(a => w.write(a))
      ConfigValueFactory.fromIterable(values.asJava)

  given [A](using r: ConfigReader[A]): ConfigReader[List[A]] = new ConfigReader[List[A]]:
    def read(config: ConfigValue): Either[ConfigError, List[A]] =
      config.valueType match
        case ConfigValueType.LIST =>
          val list = config.asInstanceOf[ConfigList]
          val results = list.asScala.toList.map(r.read)
          sequence(results)
        case other =>
          Left(ConfigError(s"Expected LIST, got $other"))

    private def sequence[A](xs: List[Either[ConfigError, A]]): Either[ConfigError, List[A]] =
      xs.foldLeft[Either[ConfigError, List[A]]](Right(Nil)) { case (accOrErr, elemOrErr) =>
        for
          acc <- accOrErr
          elem <- elemOrErr
        yield acc :+ elem
      }

import ConfigInstances.given

case class Zoo(
    name: String,
    animals: List[Animal],
    address: Address
) derives ConfigWriter,
      ConfigReader

class ConfigTests extends FunSuite:
  import ConfigInstances.given

  def assertIsomorphicConfig[A](configStr: String)(using reader: ConfigReader[A], writer: ConfigWriter[A]): Unit =
    val parsedConfig = ConfigFactory.parseString(configStr).root
    val readValue = reader.read(parsedConfig)
    assert(readValue.isRight, s"Failed to read value: ${readValue.left.getOrElse("")}")

    val value = readValue.toOption.get
    val writtenConfig = writer.write(value)
    val renderedConfig = writtenConfig.render(ConfigRenderOptions.concise.setJson(false))

    // Parse both strings to normalize formatting
    val normalizedOriginal =
      ConfigFactory.parseString(configStr).root.render(ConfigRenderOptions.concise.setJson(false))
    assertEquals(renderedConfig, normalizedOriginal)

  test("product type isomorphic serialization") {
    val personConfig = """
      |{
      |  name = "Alice"
      |  age = 30
      |}""".stripMargin

    assertIsomorphicConfig[Person](personConfig)

    val addressConfig = """
      |{
      |  street = "Main St"
      |  number = 123
      |  isApartment = true
      |}""".stripMargin

    assertIsomorphicConfig[Address](addressConfig)
  }

  test("sum type isomorphic serialization") {
    val dogConfig = """
      |{
      |  type = "Dog"
      |  value = {
      |    name = "Rex"
      |    age = 5
      |  }
      |}""".stripMargin

    assertIsomorphicConfig[Animal](dogConfig)

    val catConfig = """
      |{
      |  type = "Cat"
      |  value = {
      |    name = "Whiskers"
      |    lives = 9
      |  }
      |}""".stripMargin

    assertIsomorphicConfig[Animal](catConfig)

    val birdConfig = """
      |{
      |  type = "Bird"
      |  value = {
      |    species = "Eagle"
      |    canFly = true
      |  }
      |}""".stripMargin

    assertIsomorphicConfig[Animal](birdConfig)
  }

  test("complex nested type isomorphic serialization") {
    val zooConfig = """
      |{
      |  name = "City Zoo"
      |  animals = [
      |    {
      |      type = "Dog"
      |      value = {
      |        name = "Rex"
      |        age = 5
      |      }
      |    },
      |    {
      |      type = "Cat"
      |      value = {
      |        name = "Whiskers"
      |        lives = 9
      |      }
      |    },
      |    {
      |      type = "Bird"
      |      value = {
      |        species = "Eagle"
      |        canFly = true
      |      }
      |    }
      |  ]
      |  address = {
      |    street = "Zoo Road"
      |    number = 1
      |    isApartment = false
      |  }
      |}""".stripMargin

    assertIsomorphicConfig[Zoo](zooConfig)
  }

  test("reader handles invalid input") {
    val invalidConfig = """
      |{
      |  name = "Alice"
      |  age = "not a number"
      |}""".stripMargin

    val parsedConfig = ConfigFactory.parseString(invalidConfig).root
    assertEquals(
      ConfigReader[Person].read(parsedConfig).isLeft,
      true
    )
  }

  test("sum type reader handles invalid type") {
    val invalidConfig = """
      |{
      |  type = "InvalidAnimal"
      |  value = {
      |    name = "Rex"
      |    age = 5
      |  }
      |}""".stripMargin

    val parsedConfig = ConfigFactory.parseString(invalidConfig).root
    assertEquals(
      ConfigReader[Animal].read(parsedConfig),
      Left(ConfigError("Unknown subtype InvalidAnimal"))
    )
  }

  test("product type reader handles missing fields") {
    val invalidConfig = """
      |{
      |  name = "Alice"
      |}""".stripMargin

    val parsedConfig = ConfigFactory.parseString(invalidConfig).root
    assert(
      ConfigReader[Person].read(parsedConfig).isLeft
    )
  }
