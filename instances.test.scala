package machinespir.it.jig

import munit.FunSuite
import org.ekrich.config.{ConfigValue, ConfigValueFactory}
import scala.concurrent.duration.{Duration, FiniteDuration}
import java.util.concurrent.TimeUnit.*

class InstancesTest extends FunSuite:
  def testIsomorphic[A](name: String, value: A)(using writer: ConfigWriter[A], reader: ConfigReader[A]): Unit =
    test(name) {
      val written = writer.write(value)
      val read = reader.read(written)
      val result = read.fold(
        errors => throw new AssertionError(s"Failed to read value: ${errors.mkString(", ")}"),
        readValue => assertEquals(readValue, value)
      )
    }

  // Numeric types
  testIsomorphic("Byte", 42.toByte)
  testIsomorphic("Short", 42.toShort)
  testIsomorphic("Int", 42)
  testIsomorphic("Long", 42L)
  testIsomorphic("Float", 42.0f)
  testIsomorphic("Double", 42.0)
  testIsomorphic("Boolean", true)
  testIsomorphic("Char", 'A')

  // Duration types
  testIsomorphic("Duration", Duration(42, NANOSECONDS))
  testIsomorphic("FiniteDuration", FiniteDuration(42, NANOSECONDS))

  // Java primitive types
  testIsomorphic("java.lang.Long", java.lang.Long.valueOf(42L))
  testIsomorphic("java.lang.Double", java.lang.Double.valueOf(42.0))
  testIsomorphic("java.lang.Float", java.lang.Float.valueOf(42.0f))
  testIsomorphic("java.lang.Short", java.lang.Short.valueOf(42.toShort))
  testIsomorphic("java.lang.Byte", java.lang.Byte.valueOf(42.toByte))
  testIsomorphic("java.lang.Character", java.lang.Character.valueOf('A'))

  // Big numbers
  testIsomorphic("BigInt", BigInt("12345678901234567890"))
  testIsomorphic("BigDecimal", BigDecimal("123456789.0123456789"))

  // Option
  testIsomorphic("Option[String] - Some", Some("test"): Option[String])
  testIsomorphic("Option[String] - None", None: Option[String])
  testIsomorphic("Option[Int] - Some", Some(42): Option[Int])
  testIsomorphic("Option[Int] - None", None: Option[Int])

  // Either
  testIsomorphic("Either[String, Int] - Left", Left("error"): Either[String, Int])
  testIsomorphic("Either[String, Int] - Right", Right(42): Either[String, Int])
  testIsomorphic("Either[Int, String] - Left", Left(42): Either[Int, String])
  testIsomorphic("Either[Int, String] - Right", Right("success"): Either[Int, String])

  // List
  testIsomorphic("List[String]", List("a", "b", "c"))
  testIsomorphic("List[Int]", List(1, 2, 3))
  testIsomorphic("List[Option[String]]", List(Some("a"), None, Some("c")))
  testIsomorphic("List[Either[String, Int]]", List(Left("error"), Right(42), Left("oops")))

  // Nested types
  testIsomorphic("Option[List[Int]]", Some(List(1, 2, 3)): Option[List[Int]])
  testIsomorphic("List[Option[Int]]", List(Some(1), None, Some(3)))
  testIsomorphic("Either[List[Int], String]", Left(List(1, 2, 3)): Either[List[Int], String])
  testIsomorphic("List[Either[String, Int]]", List(Left("error"), Right(42), Left("oops")))

  // Java Time types
  testIsomorphic("Instant", java.time.Instant.parse("2024-03-14T12:34:56.789Z"))
  testIsomorphic("LocalDate", java.time.LocalDate.parse("2024-03-14"))
  testIsomorphic("LocalTime", java.time.LocalTime.parse("12:34:56.789"))
  testIsomorphic("LocalDateTime", java.time.LocalDateTime.parse("2024-03-14T12:34:56.789"))
  testIsomorphic("ZonedDateTime", java.time.ZonedDateTime.parse("2024-03-14T12:34:56.789+01:00[Europe/Paris]"))
  testIsomorphic("OffsetDateTime", java.time.OffsetDateTime.parse("2024-03-14T12:34:56.789+01:00"))
  testIsomorphic("Duration", java.time.Duration.parse("PT1H30M"))
  testIsomorphic("Period", java.time.Period.parse("P1Y2M3D"))
  testIsomorphic("Year", java.time.Year.of(2024))
  testIsomorphic("YearMonth", java.time.YearMonth.parse("2024-03"))
  testIsomorphic("MonthDay", java.time.MonthDay.parse("--03-14"))
  testIsomorphic("DayOfWeek", java.time.DayOfWeek.THURSDAY)
  testIsomorphic("Month", java.time.Month.MARCH)
  testIsomorphic("ZoneId", java.time.ZoneId.of("Europe/Paris"))
  testIsomorphic("ZoneOffset", java.time.ZoneOffset.ofHours(1))

  // Java Util types
  testIsomorphic("UUID", java.util.UUID.fromString("550e8400-e29b-41d4-a716-446655440000"))
  testIsomorphic("Locale", java.util.Locale.forLanguageTag("en-US"))
  testIsomorphic("Currency", java.util.Currency.getInstance("USD"))

  // Java Net types
  testIsomorphic("URI", java.net.URI("http://example.com"))
  testIsomorphic("InetAddress", java.net.InetAddress.getByName("127.0.0.1"))
  testIsomorphic("InetSocketAddress", java.net.InetSocketAddress.createUnresolved("localhost", 8080))

  // Java NIO types
  testIsomorphic("Path", java.nio.file.Path.of("/tmp/test.txt"))

  // Java Regex types
  test("Pattern") {
    val pattern = java.util.regex.Pattern.compile("\\d+")
    val written = summon[ConfigWriter[java.util.regex.Pattern]].write(pattern)
    val read = summon[ConfigReader[java.util.regex.Pattern]].read(written)
    read.fold(
      errors => throw new AssertionError(s"Failed to read value: ${errors.mkString(", ")}"),
      readValue => assertEquals(readValue.pattern(), pattern.pattern())
    )
  }

  // Java Math types
  testIsomorphic("java.math.BigInteger", new java.math.BigInteger("123456789"))
  testIsomorphic("java.math.BigDecimal", new java.math.BigDecimal("123.456"))

  // Scala Collection types
  testIsomorphic("Set[Int]", Set(1, 2, 3))
  testIsomorphic("TreeSet[Int]", scala.collection.immutable.TreeSet(3, 1, 2))
  testIsomorphic("Vector[String]", Vector("a", "b", "c"))
  testIsomorphic("Map[String, Int]", Map("one" -> 1, "two" -> 2))
  testIsomorphic("TreeMap[String, Int]", scala.collection.immutable.TreeMap("a" -> 1, "b" -> 2))

  // Nested collection tests
  testIsomorphic("Set[List[Int]]", Set(List(1, 2), List(3, 4)))
  testIsomorphic("Map[String, Vector[Int]]", Map("nums" -> Vector(1, 2, 3)))
  testIsomorphic("Vector[Map[String, Int]]", Vector(Map("a" -> 1), Map("b" -> 2)))
