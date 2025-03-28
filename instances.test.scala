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
