package machinespir.it.jig

import org.ekrich.config.*
import munit.FunSuite
import scala.jdk.CollectionConverters.*

// Test ADTs
case class Person(
    @comment("The person's full name")
    @comment("Used for display purposes")
    name: String,
    @comment("The person's age in months")
    @comment("Must be non-negative")
    age: Int
) derives ConfigWriter,
      ConfigReader

case class Address(
    @comment("Street name without number")
    street: String,
    @comment("Building number")
    number: Int,
    @comment("Whether this is an apartment building")
    isApartment: Boolean
) derives ConfigWriter,
      ConfigReader

// Sum type example
enum Animal derives ConfigWriter, ConfigReader:
  case Dog(name: String, age: Int)
  case Cat(name: String, lives: Int)
  case Bird(species: String, canFly: Boolean)

case class Zoo(
    name: String,
    animals: List[Animal],
    address: Address
) derives ConfigWriter,
      ConfigReader

class IsoTests extends FunSuite:

  val renderOptions =
    ConfigRenderOptions.defaults.setComments(true).setJson(true).setOriginComments(false).setFormatted(true)

  def assertIsomorphicConfig[A](configStr: String)(using reader: ConfigReader[A], writer: ConfigWriter[A]): Unit =
    val parsedConfig = ConfigFactory.parseString(configStr).root
    val readValue = reader.read(parsedConfig, List(ConfigPath.Root))
    assert(readValue.isRight, s"Failed to read value: ${readValue.left.getOrElse("")}")

    val value = readValue.toOption.get
    val writtenConfig = writer.write(value)
    val renderedConfig = writtenConfig.render(renderOptions)

    // Parse both strings to normalize formatting
    val normalizedOriginal =
      ConfigFactory
        .parseString(configStr)
        .root
        .render(renderOptions)

    assertEquals(renderedConfig, normalizedOriginal)

  test("comments in product type serialization") {
    val person = Person("Alice", 30)

    // Test with comments
    val withComments = ConfigWriter[Person].write(person, includeComments = true)
    val renderedWithComments = withComments.render(renderOptions)

    assert(renderedWithComments.contains("The person's full name"))
    assert(renderedWithComments.contains("Used for display purposes"))
    assert(renderedWithComments.contains("The person's age in months"))
    assert(renderedWithComments.contains("Must be non-negative"))

    // Test without comments
    val withoutComments = ConfigWriter[Person].write(person, includeComments = false)
    val renderedWithoutComments = withoutComments.render(renderOptions)

    assert(!renderedWithoutComments.contains("The person's full name"))
    assert(!renderedWithoutComments.contains("Used for display purposes"))
    assert(!renderedWithoutComments.contains("The person's age in months"))
    assert(!renderedWithoutComments.contains("Must be non-negative"))
  }

  test("isomorphic serialization with comments") {
    val configWithComments = """
      |{
      |  # The person's full name
      |  # Used for display purposes
      |  name = "Alice"
      |  # The person's age in months
      |  # Must be non-negative
      |  age = 30
      |}""".stripMargin

    // First read from config with comments
    val parsedConfig = ConfigFactory.parseString(configWithComments).root
    val readValue = ConfigReader[Person].read(parsedConfig)
    assert(readValue.isRight)

    // Write with comments and verify
    val value = readValue.toOption.get
    val writtenWithComments = ConfigWriter[Person].write(value, includeComments = true)
    val renderedWithComments = writtenWithComments.render(renderOptions)
    assert(renderedWithComments.contains("The person's full name"))
    assert(renderedWithComments.contains("Used for display purposes"))
    assert(renderedWithComments.contains("The person's age in months"))
    assert(renderedWithComments.contains("Must be non-negative"))

    // Write without comments and verify it matches original data
    val writtenWithoutComments = ConfigWriter[Person].write(value, includeComments = false)
    val renderedWithoutComments = writtenWithoutComments.render(renderOptions)
    val normalizedOriginal = ConfigFactory
      .parseString("""
      |{
      |  name = "Alice"
      |  age = 30
      |}""".stripMargin)
      .root
      .render(renderOptions)
    assertEquals(renderedWithoutComments, normalizedOriginal)
  }

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
    val result = ConfigReader[Person].read(parsedConfig, List(ConfigPath.Root))
    assert(result.isLeft)
    assertEquals(
      result.left.map(_.getMessage),
      Left("Expected NUMBER, got STRING (at root.age)")
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
      ConfigReader[Animal].read(parsedConfig, List(ConfigPath.Root)),
      Left(
        ConfigError("Unknown subtype InvalidAnimal", List(ConfigPath.Field("type"), ConfigPath.Root))
      ) // path will render as "root.type"
    )
  }

  test("product type reader handles missing fields") {
    val invalidConfig = """
      |{
      |  name = "Alice"
      |}""".stripMargin

    val parsedConfig = ConfigFactory.parseString(invalidConfig).root
    val result = ConfigReader[Person].read(parsedConfig, List(ConfigPath.Root))
    assert(result.isLeft)
    assertEquals(
      result.left.map(_.getMessage),
      Left("Missing field 'age' for product type. (at root)")
    )
  }

  test("list reader provides index in error path") {
    val invalidConfig = """
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
      |        lives = "nine"
      |      }
      |    }
      |  ]
      |  address = {
      |    street = "Zoo Road"
      |    number = 1
      |    isApartment = false
      |  }
      |}""".stripMargin

    val parsedConfig = ConfigFactory.parseString(invalidConfig).root
    val result = ConfigReader[Zoo].read(parsedConfig, List(ConfigPath.Root))
    assert(result.isLeft)
    assertEquals(
      result.left.map(_.getMessage),
      Left("Expected NUMBER, got STRING (at root.animals(1).value.lives)")
    )
  }
