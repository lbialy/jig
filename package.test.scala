package machinespir.it.jig

import org.ekrich.config.*
import java.nio.file.{Path, Files}
import scala.util.{Try, Success, Failure}
import scala.jdk.CollectionConverters.*

class PackageTest extends munit.FunSuite:
  case class TestConfig(name: String, age: Int) derives ConfigCodec

  test("read with default config") {
    val config = ConfigFactory.parseString("""
      |name = "John"
      |age = 30
      """.stripMargin)

    val defaultConfig = ConfigFactory
      .defaultOverrides()
      .withFallback(config)
      .withFallback(ConfigFactory.defaultApplication())
      .resolve()

    val tempFile = Files.createTempFile("application", ".conf")
    Files.writeString(tempFile, defaultConfig.root.render(ConfigRenderOptions.defaults))

    System.setProperty("config.file", tempFile.toFile.getAbsolutePath)
    try
      val result = read[TestConfig]
      assertEquals(result, Right(TestConfig("John", 30)))
    finally System.clearProperty("config.file")
  }

  test("read.config with explicit Config object") {
    val config = ConfigFactory.parseString("""
      |name = "Jane"
      |age = 25
      """.stripMargin)

    val result = read.config[TestConfig](config)
    assertEquals(result, Right(TestConfig("Jane", 25)))
  }

  test("read.file from Path") {
    val tempFile = Files.createTempFile("test-config", ".conf")
    try
      Files.writeString(
        tempFile,
        """
        |name = "Bob"
        |age = 40
        """.stripMargin
      )

      val result = read.file[TestConfig](tempFile)
      assertEquals(result, Right(TestConfig("Bob", 40)))
    finally Files.deleteIfExists(tempFile)
  }

  test("read handles invalid config") {
    val config = ConfigFactory.parseString("""
      |name = "Invalid"
      |age = "not a number"
      """.stripMargin)

    val result = read.config[TestConfig](config)
    assert(result.isLeft)
  }

  test("write.file to Path") {
    val tempFile = Files.createTempFile("test-config-write", ".conf")
    try
      val config = TestConfig("Alice", 35)
      val result = write.file(config, tempFile)
      assert(result.isRight)

      val readBack = read.file[TestConfig](tempFile)
      assertEquals(readBack, Right(config))
    finally Files.deleteIfExists(tempFile)
  }

  test("write to String") {
    val config = TestConfig("Charlie", 45)
    val result = write(config)

    val parsedBack = ConfigFactory.parseString(result)
    val readBack = read.config[TestConfig](parsedBack)
    assertEquals(readBack, Right(config))
  }

  test("write handles complex objects") {
    case class ComplexConfig(nested: TestConfig, enabled: Boolean) derives ConfigCodec

    val config = ComplexConfig(TestConfig("Dave", 50), true)
    val result = write(config)
    assert(result.nonEmpty)
  }
