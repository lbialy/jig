package machinespir.it.jig

import org.ekrich.config.*
import munit.FunSuite
import scala.jdk.CollectionConverters.*

// Test case classes
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

// types deriving only ConfigCodec
enum WeatherType derives ConfigCodec:
  case Sunny(temperature: Int, humidity: Int)
  case Rainy(intensity: String, windSpeed: Int)
  case Cloudy(coverage: Int, chanceOfRain: Int)

case class WeatherReport(
    @comment("Location name")
    location: String,
    @comment("Current weather conditions")
    currentWeather: WeatherType,
    @comment("Forecast for next hours")
    forecast: List[WeatherType]
) derives ConfigCodec

class IsoTests extends FunSuite:

  val renderOptions =
    ConfigRenderOptions.defaults.setComments(true).setJson(false).setOriginComments(false).setFormatted(true)

  def assertIsomorphicConfig[A](configStr: String)(using reader: ConfigReader[A], writer: ConfigWriter[A]): Unit =
    val parsedConfig = ConfigFactory.parseString(configStr).root
    val readValue = reader.read(parsedConfig, List(ConfigPath.Root))
    assert(
      readValue.isSuccess,
      readValue match {
        case ReadFailed(errors) => s"Failed to read value: ${errors.head.getMessage}"
        case _                  => ""
      }
    )

    val value = readValue match
      case ReadSucceeded(v) => v
      case _                => fail("Expected ReadSucceeded")

    val writtenConfig = writer.write(value)
    val renderedConfig = writtenConfig.render(renderOptions)

    // Parse both strings to normalize formatting
    val normalizedOriginal =
      ConfigFactory
        .parseString(configStr)
        .root
        .render(renderOptions)

    assertEquals(renderedConfig, normalizedOriginal, "Config serialization is not isomorphic")

  def assertIsomorphicConfigCodec[A](configStr: String)(using codec: ConfigCodec[A]): Unit =
    val parsedConfig = ConfigFactory.parseString(configStr).root
    val readValue = codec.read(parsedConfig, List(ConfigPath.Root))
    assert(
      readValue.isSuccess,
      readValue match {
        case ReadFailed(errors) => s"Failed to read value: ${errors.head.getMessage}"
        case _                  => ""
      }
    )

    val value = readValue match
      case ReadSucceeded(v) => v
      case _                => fail("Expected ReadSucceeded")

    val writtenConfig = codec.write(value)
    val renderedConfig = writtenConfig.render(renderOptions)

    // Parse both strings to normalize formatting
    val normalizedOriginal =
      ConfigFactory
        .parseString(configStr)
        .root
        .render(renderOptions)

    assertEquals(renderedConfig, normalizedOriginal, "Config serialization is not isomorphic")

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
    val configWithComments =
      """# The person's age in months
        |# Must be non-negative
        |age=30
        |# The person's full name
        |# Used for display purposes
        |name=Alice
        |""".stripMargin

    // First read from config with comments
    val parsedConfig = ConfigFactory.parseString(configWithComments).root
    val readValue = ConfigReader[Person].read(parsedConfig)
    assert(readValue.isSuccess)

    // Write with comments and verify
    val value = readValue match
      case ReadSucceeded(v) => v
      case _                => fail("Expected ReadSucceeded")

    val writtenWithComments = ConfigWriter[Person].write(value, includeComments = true)
    val renderedWithComments = writtenWithComments.render(renderOptions)
    assert(renderedWithComments.contains("The person's full name"))
    assert(renderedWithComments.contains("Used for display purposes"))
    assert(renderedWithComments.contains("The person's age in months"))
    assert(renderedWithComments.contains("Must be non-negative"))

    assertEquals(renderedWithComments, configWithComments, "Config with comments does not match original")

    // Write without comments and verify it matches original data
    val writtenWithoutComments = ConfigWriter[Person].write(value, includeComments = false)
    val renderedWithoutComments = writtenWithoutComments.render(renderOptions)
    val normalizedOriginal =
      ConfigFactory.parseString(configWithComments).root.render(renderOptions.setComments(false))

    assertEquals(renderedWithoutComments, normalizedOriginal, "Config without comments does not match")
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
    assert(!result.isSuccess)
    result match
      case ReadFailed(errors) =>
        assertEquals(
          errors.head.getMessage,
          """Invalid Int format: For input string: "not a number" (at root.age)""",
          "Error message does not match"
        )
      case _ => fail("Expected ReadFailed")
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
    val result = ConfigReader[Animal].read(parsedConfig, List(ConfigPath.Root))
    assertEquals(
      result,
      ReadResult.failure(
        ConfigError("Unknown subtype InvalidAnimal", List(ConfigPath.Field("type"), ConfigPath.Root))
      ),
      "Error message does not match"
    )
  }

  test("product type reader handles missing fields") {
    val invalidConfig = """
      |{
      |  name = "Alice"
      |}""".stripMargin

    val parsedConfig = ConfigFactory.parseString(invalidConfig).root
    val result = ConfigReader[Person].read(parsedConfig, List(ConfigPath.Root))
    assert(!result.isSuccess)
    result match
      case ReadFailed(errors) =>
        assertEquals(
          errors.head.getMessage,
          "Missing field 'age' for product type. (at root)",
          "Error message does not match"
        )
      case _ => fail("Expected ReadFailed")
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
    assert(!result.isSuccess)
    result match
      case ReadFailed(errors) =>
        assertEquals(
          errors.head.getMessage,
          """Invalid Int format: For input string: "nine" (at root.animals(1).value.lives)""",
          "Error message does not match"
        )
      case _ => fail("Expected ReadFailed")
  }

  // tests for ConfigCodec types
  test("WeatherType sum type isomorphic serialization") {
    val sunnyConfig = """
      |{
      |  type = "Sunny"
      |  value = {
      |    temperature = 25
      |    humidity = 60
      |  }
      |}""".stripMargin

    assertIsomorphicConfigCodec[WeatherType](sunnyConfig)

    val rainyConfig = """
      |{
      |  type = "Rainy"
      |  value = {
      |    intensity = "heavy"
      |    windSpeed = 30
      |  }
      |}""".stripMargin

    assertIsomorphicConfigCodec[WeatherType](rainyConfig)

    val cloudyConfig = """
      |{
      |  type = "Cloudy"
      |  value = {
      |    coverage = 80
      |    chanceOfRain = 40
      |  }
      |}""".stripMargin

    assertIsomorphicConfigCodec[WeatherType](cloudyConfig)
  }

  test("WeatherReport complex type isomorphic serialization with comments") {
    val configWithComments = """
      |{
      |  # Location name
      |  location = "New York"
      |  # Current weather conditions
      |  currentWeather = {
      |    type = "Sunny"
      |    value = {
      |      temperature = 25
      |      humidity = 60
      |    }
      |  }
      |  # Forecast for next hours
      |  forecast = [
      |    {
      |      type = "Cloudy"
      |      value = {
      |        coverage = 70
      |        chanceOfRain = 30
      |      }
      |    },
      |    {
      |      type = "Rainy"
      |      value = {
      |        intensity = "light"
      |        windSpeed = 15
      |      }
      |    }
      |  ]
      |}""".stripMargin

    // First read from config with comments
    val parsedConfig = ConfigFactory.parseString(configWithComments).root
    val readValue = ConfigCodec[WeatherReport].read(parsedConfig)
    assert(readValue.isSuccess)

    // Write with comments and verify
    val value = readValue match
      case ReadSucceeded(v) => v
      case _                => fail("Expected ReadSucceeded")

    val writtenWithComments = ConfigCodec[WeatherReport].write(value, includeComments = true)
    val renderedWithComments = writtenWithComments.render(renderOptions)
    assert(renderedWithComments.contains("Location name"))
    assert(renderedWithComments.contains("Current weather conditions"))
    assert(renderedWithComments.contains("Forecast for next hours"))

    // Write without comments and verify it matches original data
    val writtenWithoutComments = ConfigCodec[WeatherReport].write(value, includeComments = false)
    val renderedWithoutComments = writtenWithoutComments.render(renderOptions)
    val normalizedOriginal = ConfigFactory
      .parseString("""
      |{
      |  location = "New York"
      |  currentWeather = {
      |    type = "Sunny"
      |    value = {
      |      temperature = 25
      |      humidity = 60
      |    }
      |  }
      |  forecast = [
      |    {
      |      type = "Cloudy"
      |      value = {
      |        coverage = 70
      |        chanceOfRain = 30
      |      }
      |    },
      |    {
      |      type = "Rainy"
      |      value = {
      |        intensity = "light"
      |        windSpeed = 15
      |      }
      |    }
      |  ]
      |}""".stripMargin)
      .root
      .render(renderOptions)
    assertEquals(renderedWithoutComments, normalizedOriginal, "Config without comments does not match")
  }

  test("WeatherReport handles invalid weather type") {
    val invalidConfig = """
      |{
      |  location = "New York"
      |  currentWeather = {
      |    type = "InvalidWeather"
      |    value = {
      |      temperature = 25
      |      humidity = 60
      |    }
      |  }
      |  forecast = []
      |}""".stripMargin

    val parsedConfig = ConfigFactory.parseString(invalidConfig).root
    val result = ConfigCodec[WeatherReport].read(parsedConfig, List(ConfigPath.Root))
    assert(!result.isSuccess)
    result match
      case ReadFailed(errors) =>
        assertEquals(
          errors.head.getMessage,
          "Unknown subtype InvalidWeather (at root.currentWeather.type)"
        )
      case _ => fail("Expected ReadFailed")
  }

  // Tests for ReadResult error accumulation
  test("accumulates multiple field errors in product type") {
    val invalidConfig = """
      |{
      |  name = 42
      |  age = "thirty"
      |}""".stripMargin

    val parsedConfig = ConfigFactory.parseString(invalidConfig).root
    val result = ConfigReader[Person].read(parsedConfig)

    assert(!result.isSuccess)
    result match
      case ReadFailed(errors) =>
        assertEquals(errors.toList.size, 2, "Should contain exactly 2 errors")
        assert(errors.toList.exists(_.getMessage.contains("Expected STRING, got NUMBER (at root.name)")))
        assert(
          errors.toList.exists(
            _.getMessage.contains("""Invalid Int format: For input string: "thirty" (at root.age)""")
          )
        )
      case _ => fail("Expected ReadFailed")
  }

  test("accumulates errors in nested structure") {
    val invalidConfig = """
      |{
      |  name = "City Zoo"
      |  animals = [
      |    {
      |      type = "Dog"
      |      value = {
      |        name = 123
      |        age = "five"
      |      }
      |    },
      |    {
      |      type = "Cat"
      |      value = {
      |        name = true
      |        lives = "nine"
      |      }
      |    }
      |  ]
      |  address = {
      |    street = 42
      |    number = "one hundred"
      |    isApartment = "yes"
      |  }
      |}""".stripMargin

    val parsedConfig = ConfigFactory.parseString(invalidConfig).root
    val result = ConfigReader[Zoo].read(parsedConfig)

    assert(!result.isSuccess)
    result match
      case ReadFailed(errors) =>
        // Should collect 7 errors: 2 in first animal, 2 in second animal, and 3 in address
        assertEquals(errors.toList.size, 7, "Should contain all 7 errors")

        // Check for animal errors
        assert(errors.toList.exists(_.getMessage.contains("root.animals(0).value.name")))
        assert(errors.toList.exists(_.getMessage.contains("root.animals(0).value.age")))
        assert(errors.toList.exists(_.getMessage.contains("root.animals(1).value.name")))
        assert(errors.toList.exists(_.getMessage.contains("root.animals(1).value.lives")))

        // Check for address errors
        assert(errors.toList.exists(_.getMessage.contains("root.address.street")))
        assert(errors.toList.exists(_.getMessage.contains("root.address.number")))
        assert(errors.toList.exists(_.getMessage.contains("root.address.isApartment")))
      case _ => fail("Expected ReadFailed")
  }

  test("accumulates errors in list of simple types") {
    val invalidConfig = """
      |{
      |  names = ["Alice", 42, true, "Bob", 99]
      |}""".stripMargin

    // Define a case class for this test
    case class Names(names: List[String]) derives ConfigReader

    val parsedConfig = ConfigFactory.parseString(invalidConfig).root
    val result = ConfigReader[Names].read(parsedConfig)

    assert(!result.isSuccess)
    result match
      case ReadFailed(errors) =>
        assertEquals(errors.toList.size, 3, "Should contain exactly 3 errors")
        assert(errors.toList.exists(_.getMessage.contains("root.names(1)")))
        assert(errors.toList.exists(_.getMessage.contains("root.names(2)")))
        assert(errors.toList.exists(_.getMessage.contains("root.names(4)")))
      case _ => fail("Expected ReadFailed")
  }

  test("accumulates mixed error types") {
    val invalidConfig = """
      |{
      |  name = "Weather Report"
      |  currentWeather = {
      |    type = "Sunny"
      |    value = {
      |      temperature = "hot"
      |      humidity = true
      |    }
      |  }
      |  forecast = [
      |    {
      |      type = "Unknown"
      |      value = {}
      |    },
      |    {
      |      type = "Rainy"
      |      value = {
      |        intensity = 42
      |        windSpeed = "strong"
      |      }
      |    },
      |    {
      |      value = {
      |        coverage = 80
      |        chanceOfRain = 40
      |      }
      |    }
      |  ]
      |}""".stripMargin

    val parsedConfig = ConfigFactory.parseString(invalidConfig).root
    val result = ConfigReader[WeatherReport].read(parsedConfig)

    assert(!result.isSuccess)
    result match
      case ReadFailed(errors) =>
        // Count expected errors: 2 in current weather, 1 unknown type, 2 in rainy weather, 1 missing type field
        assertEquals(errors.toList.size, 7, "Should contain all 7 errors")

        // Current weather errors
        assert(errors.toList.exists(_.getMessage.contains("root.currentWeather.value.temperature")))
        assert(errors.toList.exists(_.getMessage.contains("root.currentWeather.value.humidity")))

        // Forecast errors
        assert(errors.toList.exists(_.getMessage.contains("Unknown subtype Unknown")))
        assert(errors.toList.exists(_.getMessage.contains("root.forecast(1).value.intensity")))
        assert(errors.toList.exists(_.getMessage.contains("root.forecast(1).value.windSpeed")))
        assert(errors.toList.exists(_.getMessage.contains("Expected an object with 'type'")))

        // The index-specific error for the missing type
        assert(errors.toList.exists(e => e.getMessage.contains("type") && e.getMessage.contains("root.forecast(2)")))
      case _ => fail("Expected ReadFailed")
  }

  test("ReadResult map2 correctly accumulates errors") {
    // Create a few error results to combine
    val error1 = ReadResult.failure(ConfigError("Error 1"))
    val error2 = ReadResult.failure(ConfigError("Error 2"))
    val success = ReadResult.success(42)

    // Test combining two failures
    val combined1 = ReadResult.map2(error1, error2)((_, _) => "irrelevant")
    combined1 match
      case ReadFailed(errors) =>
        assertEquals(errors.toList.size, 2, "Should contain both errors")
        assertEquals(errors.toList.map(_.msg), List("Error 1", "Error 2"))
      case _ => fail("Expected ReadFailed")

    // Test combining failure and success
    val combined2 = ReadResult.map2(error1, success)((_, _) => "irrelevant")
    combined2 match
      case ReadFailed(errors) =>
        assertEquals(errors.toList.size, 1, "Should contain one error")
        assertEquals(errors.head.msg, "Error 1")
      case _ => fail("Expected ReadFailed")

    // Test combining success and failure
    val combined3 = ReadResult.map2(success, error2)((_, _) => "irrelevant")
    combined3 match
      case ReadFailed(errors) =>
        assertEquals(errors.toList.size, 1, "Should contain one error")
        assertEquals(errors.head.msg, "Error 2")
      case _ => fail("Expected ReadFailed")

    // Test combining two successes
    val combined4 = ReadResult.map2(success, ReadResult.success("hello"))((a, b) => s"$a-$b")
    combined4 match
      case ReadSucceeded(value) =>
        assertEquals(value, "42-hello", "Should contain combined value")
      case _ => fail("Expected ReadSucceeded")
  }

  test("ReadResult sequence accumulates all errors") {
    // Create a list with multiple error results and some successes
    val results = List(
      ReadResult.success(1),
      ReadResult.failure(ConfigError("Error A")),
      ReadResult.success(3),
      ReadResult.failure(ConfigError("Error B")),
      ReadResult.failure(ConfigError("Error C"))
    )

    val sequenced = ReadResult.sequence(results)

    sequenced match
      case ReadFailed(errors) =>
        assertEquals(errors.toList.size, 3, "Should contain all three errors")
        assert(errors.toList.exists(_.msg == "Error A"))
        assert(errors.toList.exists(_.msg == "Error B"))
        assert(errors.toList.exists(_.msg == "Error C"))
      case _ => fail("Expected ReadFailed")

    // Test with all successes
    val allSuccess = List(
      ReadResult.success(1),
      ReadResult.success(2),
      ReadResult.success(3)
    )

    val sequencedSuccess = ReadResult.sequence(allSuccess)
    sequencedSuccess match
      case ReadSucceeded(list) =>
        assertEquals(list, List(1, 2, 3), "Should contain all values")
      case _ => fail("Expected ReadSucceeded")
  }

  // Tests for default parameter support
  test("ConfigReader uses default values for missing fields") {
    // Define a case class with default values
    case class ConfigWithDefaults(
        name: String,
        age: Int = 42,
        enabled: Boolean = true,
        tags: List[String] = List("default")
    ) derives ConfigReader

    // Test with missing fields
    val configWithMissingFields = """
      |{
      |  name = "Alice"
      |}""".stripMargin

    val parsedConfig = ConfigFactory.parseString(configWithMissingFields).root
    val result = ConfigReader[ConfigWithDefaults].read(parsedConfig)

    assert(result.isSuccess, "Should succeed with default values")
    result match
      case ReadSucceeded(value) =>
        assertEquals(value.name, "Alice", "Non-default field should be read")
        assertEquals(value.age, 42, "Should use default age")
        assertEquals(value.enabled, true, "Should use default enabled")
        assertEquals(value.tags, List("default"), "Should use default tags")
      case _ => fail("Expected ReadSucceeded")
  }

  test("ConfigReader uses provided values over defaults") {
    case class ConfigWithDefaults(
        name: String,
        age: Int = 42,
        enabled: Boolean = true,
        tags: List[String] = List("default")
    ) derives ConfigReader

    // Test with all fields provided
    val configWithAllFields = """
      |{
      |  name = "Bob"
      |  age = 30
      |  enabled = false
      |  tags = ["custom", "tags"]
      |}""".stripMargin

    val parsedConfig = ConfigFactory.parseString(configWithAllFields).root
    val result = ConfigReader[ConfigWithDefaults].read(parsedConfig)

    assert(result.isSuccess, "Should succeed with provided values")
    result match
      case ReadSucceeded(value) =>
        assertEquals(value.name, "Bob", "Should use provided name")
        assertEquals(value.age, 30, "Should use provided age")
        assertEquals(value.enabled, false, "Should use provided enabled")
        assertEquals(value.tags, List("custom", "tags"), "Should use provided tags")
      case _ => fail("Expected ReadSucceeded")
  }

  test("ConfigReader fails for missing required fields") {
    case class ConfigWithRequiredAndDefaults(
        name: String, // required
        age: Int = 42, // optional
        enabled: Boolean = true // optional
    ) derives ConfigReader

    // Test with missing required field
    val configWithMissingRequired = """
      |{
      |  age = 30
      |  enabled = false
      |}""".stripMargin

    val parsedConfig = ConfigFactory.parseString(configWithMissingRequired).root
    val result = ConfigReader[ConfigWithRequiredAndDefaults].read(parsedConfig)

    assert(!result.isSuccess, "Should fail with missing required field")
    result match
      case ReadFailed(errors) =>
        assertEquals(errors.toList.size, 1, "Should have one error")
        assertEquals(errors.head.getMessage, "Missing field 'name' for product type. (at root)")
      case _ => fail("Expected ReadFailed")
  }

  test("ConfigReader handles nested types with defaults") {
    case class NestedConfig(
        name: String,
        address: Address = Address("Default St", 1, false),
        age: Int = 42
    ) derives ConfigReader

    // Test with missing nested field
    val configWithMissingNested = """
      |{
      |  name = "Charlie"
      |  age = 30
      |}""".stripMargin

    val parsedConfig = ConfigFactory.parseString(configWithMissingNested).root
    val result = ConfigReader[NestedConfig].read(parsedConfig)

    assert(result.isSuccess, "Should succeed with default nested value")
    result match
      case ReadSucceeded(value) =>
        assertEquals(value.name, "Charlie", "Should use provided name")
        assertEquals(value.address, Address("Default St", 1, false), "Should use default address")
        assertEquals(value.age, 30, "Should use provided age")
      case _ => fail("Expected ReadSucceeded")
  }

  test("ConfigReader accumulates errors for invalid fields even with defaults") {
    case class ConfigWithDefaults(
        name: String,
        age: Int = 42,
        enabled: Boolean = true
    ) derives ConfigReader

    // Test with invalid field values
    val configWithInvalidFields = """
      |{
      |  name = 123
      |  age = "not a number"
      |}""".stripMargin

    val parsedConfig = ConfigFactory.parseString(configWithInvalidFields).root
    val result = ConfigReader[ConfigWithDefaults].read(parsedConfig)

    assert(!result.isSuccess, "Should fail with invalid field values")
    result match
      case ReadFailed(errors) =>
        assertEquals(errors.toList.size, 2, "Should have two errors")
        assert(errors.toList.exists(_.getMessage.contains("Expected STRING, got NUMBER (at root.name)")))
        assert(
          errors.toList.exists(
            _.getMessage.contains("""Invalid Int format: For input string: "not a number" (at root.age)""")
          )
        )
      case _ => fail("Expected ReadFailed")
  }

  test("Empty enum members are serialized as strings") {
    case class Test(`enum`: SimpleEnum) derives ConfigCodec
    enum SimpleEnum derives ConfigCodec:
      case A, B, C

    val config = """
      |{
      |  enum = "A"
      |}""".stripMargin

    val parsedConfig = ConfigFactory.parseString(config).root
    val result = ConfigReader[Test].read(parsedConfig)
    result.fold(
      errors => throw new AssertionError(s"Failed to read value: ${errors.mkString(", ")}"),
      readValue => assertEquals(readValue, Test(`enum` = SimpleEnum.A))
    )

    // Test writing
    val written = ConfigWriter[SimpleEnum].write(SimpleEnum.A)
    assertEquals(written.valueType, ConfigValueType.STRING)
    assertEquals(written.unwrapped, "A")
  }
