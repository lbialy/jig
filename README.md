<p align="center">
  <img src="jig.png" alt="Jig Logo" style="width: 200px;">
</p>
<br/>

A **minimal, type-safe configuration library for Scala 3** that focuses on **both reading and writing configuration files**. Built with modern Scala features and designed to be multiplatform.

## Why Another Config Library?

Most Scala configuration libraries focus primarily on reading configuration, often neglecting the equally important task of writing configuration files. Jig takes a different approach:

- **Minimal Core**: Built with Scala 3's new features, keeping the core functionality focused and maintainable
- **Bidirectional**: First-class support for both reading and writing configuration, making it perfect for generating default configs
- **Multiplatform**: No Java dependencies, making it compatible with Scala Native and Scala.js
- **Type-Safe**: Leverages Scala 3's type system for compile-time safety and derivation

## How to add it to your project?

### Scala (scala-cli): 

`//> using dep ma.chinespirit::jig:0.1.0`

or in REPL: `--dep ma.chinespirit::jig:0.1.0`

### sbt 

`"ma.chinespirit" %% "jig" % "0.1.0"`

### mill

`ivy"ma.chinespirit::jig:0.1.0"`

### Gradle

`implementation 'ma.chinespirit:jig_3:0.1.0'`

### Maven 
```
<dependency>
  <groupId>ma.chinespirit</groupId>
  <artifactId>jig_3</artifactId>
  <version>0.1.0</version>
  <scope>compile</scope>
</dependency>
```

## Features

### Reading and Writing Case Classes

```scala
//> using dep ma.chinespirit::jig:0.1.0

import machinespir.it.jig.*
import scala.concurrent.duration.*

case class AppConfig(
  host: String,
  port: Int,
  timeout: Duration
) derives ConfigCodec

// Reading from a file
val configFromApplicationConf = read[AppConfig] // Reads from application.conf
// or
val configFromConfigConf = read.file[AppConfig](java.nio.file.Path.of("config.conf"))

// Reading from a string
val configStr = """
  host = "localhost"
  port = 8080
  timeout = 5s
"""
val configFromStr = read.config[AppConfig](ConfigFactory.parseString(configStr))

println(configFromStr)

// Writing
val defaultConfig = AppConfig("localhost", 8080, 5.seconds)
write.file[AppConfig](defaultConfig, java.nio.file.Path.of("default.conf"))
```

### Configuration Comments

```scala
import machinespir.it.jig.*

case class DatabaseConfig(
  @comment("Database connection URL")
  url: String,
  @comment("Maximum number of connections in the pool")
  maxConnections: Int
) derives ConfigCodec

val dbConf = DatabaseConfig(
  url = "jdbc://localhost:5432/pg",
  maxConnections = 20
)

// write to string
val confStr = write(dbConf)

/* prints:
 * # Maximum number of connections in the pool
 * maxConnections=20
 * # Database connection URL
 * url="jdbc://localhost:5432/pg"
 */
println(confStr)
```

### Sum Types Support

Jig supports both enums and sealed traits for sum types:

#### Enums with Values

```scala
import machinespir.it.jig.*

enum DatabaseType derives ConfigCodec:
  case Postgres(version: String)
  case MySQL(version: String)
  case H2(version: String)

case class DbConfig(
  `type`: DatabaseType,
  url: String
) derives ConfigCodec

// Reading from a string
val configString = """
  type = {
    type = Postgres
    value = {
      version = "15.0"
    }
  }
  url = "jdbc:postgresql://localhost:5432/mydb"
"""
val configFromString = read.config[DbConfig](ConfigFactory.parseString(configString))
println(configFromString)
```

#### Sealed Traits

```scala
import machinespir.it.jig.*

sealed trait Storage derives ConfigCodec
case class FileSystem(path: String) extends Storage derives ConfigCodec
case class S3(bucket: String, region: String) extends Storage derives ConfigCodec

case class BackupConfig(
  storage: Storage,
  retention: Int
) derives ConfigCodec

// Reading from a string
val cfgString = """
  storage = {
    type = S3
    value = {
      bucket = "my-backups"
      region = "us-east-1"
    }
  }
  retention = 30
"""
val confFromStr = read.config[BackupConfig](ConfigFactory.parseString(cfgString))

println(confFromStr)
```

### Rich Standard Library Type Support

Jig supports a wide range of standard library types out of the box:

- **Basic Types**: `String`, `Int`, `Boolean`, `Byte`, `Short`, `Long`, `Float`, `Double`, `Char`, `BigInt`, `BigDecimal`
- **Collections**: `List`, `Set`, `Vector`, `Map`, `TreeSet`, `TreeMap`
- **Optional Values**: `Option`
- **Either**: `Either[A, B]`
- **Time Types**:
  - Java Time: `Instant`, `LocalDate`, `LocalTime`, `LocalDateTime`, `ZonedDateTime`, `OffsetDateTime`, `Duration`, `Period`, `Year`, `YearMonth`, `MonthDay`, `DayOfWeek`, `Month`, `ZoneId`, `ZoneOffset`
  - Scala Duration: `Duration`, `FiniteDuration`
- **Java Types**: `UUID`, `Locale`, `Currency`, `URI`, `InetAddress`, `InetSocketAddress`, `Path`, `Pattern`, `BigInteger`, `BigDecimal`

### Additional Features

- **Error Aggregation**: Instead of failing fast, Jig collects all configuration errors and reports them together
- **Default Parameters**: Support for case class default parameters
- **Path Tracking**: Configuration errors include the full path to the problematic value
- **Clean API**: Utility methods in the package object hide the underlying config library types
- **Flexible Rendering**: Customizable rendering options for written configuration

## Example Usage

```scala
import machinespir.it.jig.*
import scala.concurrent.duration.*

case class ServerConfig(
  @comment("Server hostname")
  host: String,
  @comment("Server port")
  port: Int,
  @comment("Request timeout")
  timeout: Duration,
  @comment("Database configuration")
  database: DatabaseConfiguration
) derives ConfigCodec

case class DatabaseConfiguration(
  @comment("Database type")
  variant: DatabaseVariant,
  @comment("Connection URL")
  url: String,
  @comment("Connection pool size")
  poolSize: Int = 10 // Default value
) derives ConfigCodec

enum DatabaseVariant derives ConfigCodec:
  case Postgres(version: String)
  case MySQL(version: String)
  case H2(version: String)

// Reading from a string
val confString = """
  host = "localhost"
  port = 8080
  timeout = 30s
  database = {
    variant = {
      type = Postgres
      value = {
        version = "15.0"
      }
    }
    url = "jdbc:postgresql://localhost:5432/mydb"
  }
"""
val readServerConfig = read.config[ServerConfig](ConfigFactory.parseString(confString))

println(readServerConfig)

// Writing default configuration
val defaultServerConfig = ServerConfig(
  host = "localhost",
  port = 8080,
  timeout = 30.seconds,
  database = DatabaseConfiguration(
    variant = DatabaseVariant.Postgres("15.0"),
    url = "jdbc:postgresql://localhost:5432/mydb"
  )
)

write.file[ServerConfig](defaultServerConfig, java.nio.file.Path.of("server-default.conf"))
```

## A Note About This Library

This library was "vibe coded" to demonstrate that AI can write idiomatic Scala code. It showcases modern Scala 3 features while maintaining a clean, type-safe API. 
The focus on both reading and writing configuration, along with multiplatform support, makes it a unique addition to the Scala ecosystem.

## License

MIT License

## TODO

- [X] ConfigReader, ConfigWriter typeclasses
- [X] complete derivation for sums and products
- [X] configuration errors with paths
- [X] isomorphic tests
- [X] writing field-level comments from annotations
- [X] ConfigCodec typeclass (dual read/write)
- [X] aggregating errors instead of fail-fast
- [X] default parameters support
- [X] clean up file structure
- [X] add a boatload of common types
- [X] utility methods in jig package object to hide org.ekrich.config types
- [X] basic docs
- [ ] integration with iron