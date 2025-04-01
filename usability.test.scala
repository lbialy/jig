package machinespir.it.jig

import org.ekrich.config.*
import scala.concurrent.duration.*
import java.time.{Duration as JavaDuration, *}
import java.util.{List as JavaList, Map as JavaMap, Set as JavaSet, Vector as JavaVector, *}
import java.net.*
import java.nio.file.Path
import java.util.regex.Pattern
import scala.collection.immutable.{TreeMap, TreeSet}
import difflicious.Differ
import difflicious.munit.MUnitDiff.*

given Differ[UUID] = summon[Differ[String]].contramap(_.toString)
given Differ[InetAddress] = summon[Differ[String]].contramap(_.getHostAddress)
given Differ[InetSocketAddress] = summon[Differ[String]].contramap(_.toString)
given Differ[FiniteDuration] = summon[Differ[Long]].contramap(_.toNanos)
given Differ[java.nio.file.Path] = summon[Differ[String]].contramap(_.toString)
given Differ[java.time.Period] = summon[Differ[String]].contramap(_.toString)
given javaDurationDiffer: Differ[java.time.Duration] = summon[Differ[String]].contramap(_.toString)
given Differ[java.time.LocalDate] = summon[Differ[String]].contramap(_.toString)
given Differ[java.time.LocalTime] = summon[Differ[String]].contramap(_.toString)
given Differ[java.time.ZoneId] = summon[Differ[String]].contramap(_.getId)
given Differ[java.util.regex.Pattern] = summon[Differ[String]].contramap(_.pattern)
given Differ[scala.Float] = summon[Differ[Double]].contramap(_.toDouble)
given Differ[java.net.URI] = summon[Differ[String]].contramap(_.toString)
given Differ[Duration] = summon[Differ[Long]].contramap(_.toNanos)
given Differ[java.util.Locale] = summon[Differ[String]].contramap(_.toString)

// Enums for our domain
enum CloudProvider derives ConfigCodec, Differ:
  case AWS, GCP, Azure

enum InstanceSize derives ConfigCodec, Differ:
  case Small, Medium, Large, XLarge

// Main configuration classes
case class CloudInfraConfig(
    @comment("Basic metadata about the deployment")
    metadata: DeploymentMetadata,
    @comment("Network configuration settings")
    network: NetworkConfig,
    @comment("Compute resources configuration")
    compute: ComputeConfig,
    @comment("Storage configuration settings")
    storage: StorageConfig,
    @comment("Security and access control settings")
    security: SecurityConfig,
    @comment("Monitoring and alerting configuration")
    monitoring: Option[MonitoringConfig] = None,
    @comment("Backup configuration and retention policies")
    backup: BackupConfig
) derives ConfigCodec,
      Differ

case class DeploymentMetadata(
    @comment("Unique identifier for the deployment")
    id: UUID,
    @comment("Human readable name of the deployment")
    name: String,
    @comment("Creation timestamp")
    createdAt: Instant,
    @comment("Last modification timestamp")
    lastModified: ZonedDateTime,
    @comment("Environment (e.g. prod, staging)")
    environment: String,
    @comment("Cloud provider to use")
    provider: CloudProvider,
    @comment("Geographic region for deployment")
    region: String,
    @comment("Deployment version")
    version: BigInt = BigInt("1"),
    @comment("Cost center for billing")
    costCenter: Option[String] = None
) derives ConfigCodec,
      Differ

case class NetworkConfig(
    @comment("VPC CIDR block")
    vpcCidr: String,
    @comment("List of subnet configurations")
    subnets: List[SubnetConfig],
    @comment("DNS configuration")
    dns: DnsConfig,
    @comment("Load balancer settings")
    loadBalancers: Set[LoadBalancerConfig],
    @comment("Network policies ordered by priority")
    networkPolicies: TreeMap[Int, NetworkPolicy]
) derives ConfigCodec,
      Differ

case class SubnetConfig(
    @comment("Subnet CIDR block")
    cidr: String,
    @comment("Availability zone")
    zone: String,
    @comment("Whether subnet is public")
    isPublic: Boolean
) derives ConfigCodec,
      Differ

case class DnsConfig(
    @comment("Primary DNS server")
    primaryDns: InetAddress,
    @comment("Backup DNS servers")
    backupDns: Vector[InetAddress],
    @comment("DNS search domains")
    searchDomains: List[String],
    @comment("TTL for DNS records in seconds")
    ttl: JavaDuration = JavaDuration.ofSeconds(300)
) derives ConfigCodec,
      Differ

case class LoadBalancerConfig(
    @comment("Load balancer address")
    address: InetSocketAddress,
    @comment("Health check path")
    healthCheckPath: String,
    @comment("Health check interval")
    healthCheckInterval: FiniteDuration = 30.seconds,
    @comment("SSL certificate path")
    sslCertPath: Option[Path] = None
) derives ConfigCodec,
      Differ

case class NetworkPolicy(
    @comment("Policy name")
    name: String,
    @comment("Source IP pattern")
    sourceIp: Pattern,
    @comment("Destination IP pattern")
    destIp: Pattern,
    @comment("Allowed ports")
    ports: TreeSet[Int]
) derives ConfigCodec,
      Differ

case class ComputeConfig(
    @comment("Instance size to use")
    size: InstanceSize,
    @comment("Number of instances")
    instanceCount: Int,
    @comment("CPU allocation")
    cpu: CpuConfig,
    @comment("Memory allocation")
    memory: MemoryConfig,
    @comment("Auto-scaling configuration")
    autoScaling: AutoScalingConfig
) derives ConfigCodec,
      Differ

case class CpuConfig(
    @comment("Number of CPU cores")
    cores: Short,
    @comment("CPU architecture")
    architecture: String,
    @comment("Base clock speed in GHz")
    clockSpeed: Float,
    @comment("CPU usage threshold for scaling")
    usageThreshold: Double = 80.0
) derives ConfigCodec,
      Differ

case class MemoryConfig(
    @comment("Total RAM in GB")
    totalGb: Long,
    @comment("Swap space in GB")
    swapGb: Int = 0,
    @comment("Memory reservation in GB")
    reservationGb: Option[Int] = None
) derives ConfigCodec,
      Differ

case class AutoScalingConfig(
    @comment("Minimum number of instances")
    minInstances: Byte,
    @comment("Maximum number of instances")
    maxInstances: Byte,
    @comment("Cool-down period between scaling actions")
    coolDown: java.time.Duration,
    @comment("Scaling schedule")
    schedule: Option[AutoScalingSchedule] = None
) derives ConfigCodec,
      Differ

case class AutoScalingSchedule(
    @comment("Schedule start date")
    startDate: LocalDate,
    @comment("Daily scale-up time")
    scaleUpTime: LocalTime,
    @comment("Daily scale-down time")
    scaleDownTime: LocalTime,
    @comment("Time zone for the schedule")
    timeZone: ZoneId = ZoneId.of("UTC")
) derives ConfigCodec,
      Differ

case class StorageConfig(
    @comment("Storage type (e.g. SSD, HDD)")
    storageType: String,
    @comment("Total storage size")
    totalSize: BigDecimal,
    @comment("Storage mount point")
    mountPoint: Path,
    @comment("IOPS limit")
    iopsLimit: Option[Int] = None,
    @comment("Storage encryption settings")
    encryption: StorageEncryption
) derives ConfigCodec,
      Differ

case class StorageEncryption(
    @comment("Encryption algorithm")
    algorithm: String,
    @comment("Key rotation period")
    keyRotationPeriod: Period,
    @comment("Key strength in bits")
    keyStrength: Short = 256
) derives ConfigCodec,
      Differ

case class SecurityConfig(
    @comment("Security compliance level")
    complianceLevel: String,
    @comment("Firewall rules")
    firewallRules: Map[String, String],
    @comment("Authentication settings")
    authentication: AuthConfig,
    @comment("Audit log settings")
    auditLog: AuditLogConfig
) derives ConfigCodec,
      Differ

case class AuthConfig(
    @comment("Authentication provider")
    provider: String,
    @comment("Authentication endpoint")
    endpoint: URI,
    @comment("Session timeout")
    sessionTimeout: FiniteDuration,
    @comment("Allowed authentication methods")
    allowedMethods: Set[String]
) derives ConfigCodec,
      Differ

case class AuditLogConfig(
    @comment("Log retention period")
    retentionPeriod: Period,
    @comment("Log level")
    logLevel: String,
    @comment("Log file location")
    location: Path,
    @comment("Log format pattern")
    formatPattern: Pattern
) derives ConfigCodec,
      Differ

case class MonitoringConfig(
    @comment("Monitoring service endpoint")
    endpoint: URI,
    @comment("Metrics collection interval")
    interval: Duration,
    @comment("Alert notification settings")
    alerting: AlertConfig,
    @comment("Metrics retention configuration")
    retention: MetricsRetentionConfig
) derives ConfigCodec,
      Differ

case class AlertConfig(
    @comment("Alert recipients")
    recipients: List[String],
    @comment("Alert severity levels")
    severityLevels: Set[String],
    @comment("Notification channels")
    channels: Map[String, URI]
) derives ConfigCodec,
      Differ

case class MetricsRetentionConfig(
    @comment("Raw metrics retention")
    rawRetention: Period,
    @comment("Aggregated metrics retention")
    aggregatedRetention: Period,
    @comment("Retention buckets configuration")
    buckets: Map[String, Duration]
) derives ConfigCodec,
      Differ

case class BackupConfig(
    @comment("Backup schedule")
    schedule: String,
    @comment("Backup retention period")
    retentionPeriod: Period,
    @comment("Backup location")
    location: URI,
    @comment("Backup encryption settings")
    encryption: Option[StorageEncryption] = None,
    @comment("Geographic region for backup")
    region: Option[String] = None,
    @comment("Backup format settings")
    format: BackupFormat
) derives ConfigCodec,
      Differ

case class BackupFormat(
    @comment("Compression algorithm")
    compression: String,
    @comment("Compression level")
    compressionLevel: Byte,
    @comment("Backup file format")
    fileFormat: String,
    @comment("Locale for backup metadata")
    locale: Locale = Locale.US
) derives ConfigCodec,
      Differ

class UsabilityTests extends munit.FunSuite:
  test("should support reading complex configuration"):
    // Sample configuration string
    val configStr = """
      |metadata {
      |  id = "550e8400-e29b-41d4-a716-446655440000"
      |  name = "prod-cluster-east"
      |  createdAt = "2024-01-01T00:00:00Z"
      |  lastModified = "2024-01-01T00:00:00Z[UTC]"
      |  environment = "production"
      |  provider = "AWS"
      |  region = "us-east-1"
      |}
      |network {
      |  vpcCidr = "10.0.0.0/16"
      |  subnets = [
      |    { cidr = "10.0.1.0/24", zone = "us-east-1a", isPublic = true },
      |    { cidr = "10.0.2.0/24", zone = "us-east-1b", isPublic = false }
      |  ]
      |  dns {
      |    primaryDns = "8.8.8.8"
      |    backupDns = ["8.8.4.4", "1.1.1.1"]
      |    searchDomains = ["example.com", "internal.example.com"]
      |  }
      |  loadBalancers = [
      |    { address = "lb-1.example.com:443", healthCheckPath = "/health" }
      |  ]
      |  networkPolicies {
      |    1 = { name = "allow-http", sourceIp = ".*", destIp = "10\\.0\\..*", ports = [80, 443] }
      |  }
      |}
      |compute {
      |  size = "Small"
      |  instanceCount = 3
      |  cpu {
      |    cores = 4
      |    architecture = "x86_64"
      |    clockSpeed = 2.4
      |  }
      |  memory {
      |    totalGb = 16
      |  }
      |  autoScaling {
      |    minInstances = 2
      |    maxInstances = 10
      |    coolDown = 5m
      |  }
      |}
      |storage {
      |  storageType = "SSD"
      |  totalSize = "1000.50"
      |  mountPoint = "/data"
      |  encryption {
      |    algorithm = "AES"
      |    keyRotationPeriod = "P90D"
      |  }
      |}
      |security {
      |  complianceLevel = "high"
      |  firewallRules = {
      |    "allow-ssh" = "tcp/22"
      |  }
      |  authentication {
      |    provider = "OIDC"
      |    endpoint = "https://auth.example.com"
      |    sessionTimeout = "60m"
      |    allowedMethods = ["password", "2fa"]
      |  }
      |  auditLog {
      |    retentionPeriod = "P1Y"
      |    logLevel = "INFO"
      |    location = "/var/log/audit"
      |    formatPattern = ".*"
      |  }
      |}
      |backup {
      |  schedule = "0 0 * * *"
      |  retentionPeriod = "P30D"
      |  location = "s3://backups"
      |  format {
      |    compression = "gzip"
      |    compressionLevel = 9
      |    fileFormat = "tar"
      |  }
      |}
      |""".stripMargin

    // Test reading
    val config = ConfigFactory.parseString(configStr).root
    val result = ConfigCodec[CloudInfraConfig].read(config)

    result.fold(
      errors => throw new AssertionError(s"Failed to read configuration: ${errors.mkString("\n  ", "\n  ", "\n")}"),
      infraConfig => {
        // Metadata assertions
        assertEquals(infraConfig.metadata.id.toString, "550e8400-e29b-41d4-a716-446655440000")
        assertEquals(infraConfig.metadata.name, "prod-cluster-east")
        assertEquals(infraConfig.metadata.environment, "production")
        assertEquals(infraConfig.metadata.provider, CloudProvider.AWS)
        assertEquals(infraConfig.metadata.region, "us-east-1")
        assertEquals(infraConfig.metadata.version, BigInt("1"))
        assertEquals(infraConfig.metadata.costCenter, None)

        // Network assertions
        assertEquals(infraConfig.network.vpcCidr, "10.0.0.0/16")
        assertEquals(infraConfig.network.subnets.size, 2)
        assertEquals(infraConfig.network.subnets(0).cidr, "10.0.1.0/24")
        assertEquals(infraConfig.network.subnets(0).zone, "us-east-1a")
        assertEquals(infraConfig.network.subnets(0).isPublic, true)
        assertEquals(infraConfig.network.subnets(1).cidr, "10.0.2.0/24")
        assertEquals(infraConfig.network.subnets(1).zone, "us-east-1b")
        assertEquals(infraConfig.network.subnets(1).isPublic, false)
        assertEquals(infraConfig.network.dns.primaryDns.getHostAddress, "8.8.8.8")
        assertEquals(infraConfig.network.dns.backupDns.map(_.getHostAddress), Vector("8.8.4.4", "1.1.1.1"))
        assertEquals(infraConfig.network.dns.searchDomains, List("example.com", "internal.example.com"))
        assertEquals(infraConfig.network.dns.ttl, JavaDuration.ofSeconds(300))
        assertEquals(infraConfig.network.loadBalancers.size, 1)
        assertEquals(infraConfig.network.loadBalancers.head.healthCheckPath, "/health")
        assertEquals(infraConfig.network.loadBalancers.head.healthCheckInterval, 30.seconds)
        assertEquals(infraConfig.network.loadBalancers.head.sslCertPath, None)
        assertEquals(infraConfig.network.networkPolicies.size, 1)
        assertEquals(infraConfig.network.networkPolicies(1).name, "allow-http")
        assertEquals(infraConfig.network.networkPolicies(1).ports, TreeSet(80, 443))

        // Compute assertions
        assertEquals(infraConfig.compute.size, InstanceSize.Small)
        assertEquals(infraConfig.compute.instanceCount, 3)
        assertEquals(infraConfig.compute.cpu.cores, 4.toShort)
        assertEquals(infraConfig.compute.cpu.architecture, "x86_64")
        assertEquals(infraConfig.compute.cpu.clockSpeed, 2.4f)
        assertEquals(infraConfig.compute.cpu.usageThreshold, 80.0)
        assertEquals(infraConfig.compute.memory.totalGb, 16L)
        assertEquals(infraConfig.compute.memory.swapGb, 0)
        assertEquals(infraConfig.compute.memory.reservationGb, None)
        assertEquals(infraConfig.compute.autoScaling.minInstances, 2.toByte)
        assertEquals(infraConfig.compute.autoScaling.maxInstances, 10.toByte)
        assertEquals(infraConfig.compute.autoScaling.coolDown, JavaDuration.parse("PT5M"))
        assertEquals(infraConfig.compute.autoScaling.schedule, None)

        // Storage assertions
        assertEquals(infraConfig.storage.storageType, "SSD")
        assertEquals(infraConfig.storage.totalSize, BigDecimal("1000.50"))
        assertEquals(infraConfig.storage.mountPoint.toString, "/data")
        assertEquals(infraConfig.storage.iopsLimit, None)
        assertEquals(infraConfig.storage.encryption.algorithm, "AES")
        assertEquals(infraConfig.storage.encryption.keyRotationPeriod, Period.parse("P90D"))
        assertEquals(infraConfig.storage.encryption.keyStrength, 256.toShort)

        // Security assertions
        assertEquals(infraConfig.security.complianceLevel, "high")
        assertEquals(infraConfig.security.firewallRules, Map("allow-ssh" -> "tcp/22"))
        assertEquals(infraConfig.security.authentication.provider, "OIDC")
        assertEquals(infraConfig.security.authentication.endpoint.toString, "https://auth.example.com")
        assertEquals(infraConfig.security.authentication.sessionTimeout, 60.minutes)
        assertEquals(infraConfig.security.authentication.allowedMethods, Set("password", "2fa"))
        assertEquals(infraConfig.security.auditLog.retentionPeriod, Period.parse("P1Y"))
        assertEquals(infraConfig.security.auditLog.logLevel, "INFO")
        assertEquals(infraConfig.security.auditLog.location.toString, "/var/log/audit")

        // Monitoring assertions (should be None)
        assertEquals(infraConfig.monitoring, None)

        // Backup assertions
        assertEquals(infraConfig.backup.schedule, "0 0 * * *")
        assertEquals(infraConfig.backup.retentionPeriod, Period.parse("P30D"))
        assertEquals(infraConfig.backup.location.toString, "s3://backups")
        assertEquals(infraConfig.backup.encryption, None)
        assertEquals(infraConfig.backup.region, None)
        assertEquals(infraConfig.backup.format.compression, "gzip")
        assertEquals(infraConfig.backup.format.compressionLevel, 9.toByte)
        assertEquals(infraConfig.backup.format.fileFormat, "tar")
        assertEquals(infraConfig.backup.format.locale, Locale.US)
      }
    )

  val testConfig = CloudInfraConfig(
    metadata = DeploymentMetadata(
      id = UUID.fromString("550e8400-e29b-41d4-a716-446655440000"),
      name = "test-cluster",
      createdAt = Instant.parse("2024-01-01T00:00:00Z"),
      lastModified = ZonedDateTime.parse("2024-01-01T00:00:00Z[UTC]"),
      environment = "test",
      provider = CloudProvider.GCP,
      region = "us-central1"
    ),
    network = NetworkConfig(
      vpcCidr = "10.0.0.0/16",
      subnets = List(
        SubnetConfig("10.0.1.0/24", "us-central1-a", true)
      ),
      dns = DnsConfig(
        primaryDns = InetAddress.getByName("8.8.8.8"),
        backupDns = Vector(InetAddress.getByName("8.8.4.4")),
        searchDomains = List("test.com")
      ),
      loadBalancers = Set(
        LoadBalancerConfig(
          InetSocketAddress.createUnresolved("lb.test.com", 443),
          "/health"
        )
      ),
      networkPolicies = TreeMap(
        1 -> NetworkPolicy(
          "allow-http",
          Pattern.compile(".*"),
          Pattern.compile("10\\.0\\..*"),
          TreeSet(80, 443)
        )
      )
    ),
    compute = ComputeConfig(
      size = InstanceSize.Small,
      instanceCount = 2,
      cpu = CpuConfig(
        cores = 2,
        architecture = "arm64",
        clockSpeed = 2.5f
      ),
      memory = MemoryConfig(
        totalGb = 8L
      ),
      autoScaling = AutoScalingConfig(
        minInstances = 1,
        maxInstances = 5,
        coolDown = JavaDuration.parse("PT5M")
      )
    ),
    storage = StorageConfig(
      storageType = "SSD",
      totalSize = BigDecimal("500.00"),
      mountPoint = Path.of("/data"),
      encryption = StorageEncryption(
        algorithm = "AES",
        keyRotationPeriod = Period.parse("P90D")
      )
    ),
    security = SecurityConfig(
      complianceLevel = "high",
      firewallRules = Map("allow-ssh" -> "tcp/22"),
      authentication = AuthConfig(
        provider = "OIDC",
        endpoint = new URI("https://auth.test.com"),
        sessionTimeout = 1.hour,
        allowedMethods = Set("password")
      ),
      auditLog = AuditLogConfig(
        retentionPeriod = Period.parse("P1Y"),
        logLevel = "INFO",
        location = Path.of("/var/log/audit"),
        formatPattern = Pattern.compile(".*")
      )
    ),
    backup = BackupConfig(
      schedule = "0 0 * * *",
      retentionPeriod = Period.parse("P30D"),
      location = new URI("gs://backups"),
      format = BackupFormat(
        compression = "gzip",
        compressionLevel = 9,
        fileFormat = "tar"
      )
    )
  )

  val renderOptions =
    ConfigRenderOptions.defaults.setComments(true).setJson(false).setOriginComments(false).setFormatted(true)

  test("should support writing complex configuration"):
    val renderedConfig = ConfigCodec[CloudInfraConfig].write(testConfig, includeComments = true).render(renderOptions)

    val expectedStr = """# Backup configuration and retention policies
                        |backup {
                        |    # Backup format settings
                        |    format {
                        |        # Compression algorithm
                        |        compression=gzip
                        |        # Compression level
                        |        compressionLevel=9
                        |        # Backup file format
                        |        fileFormat=tar
                        |        # Locale for backup metadata
                        |        locale="en-US"
                        |    }
                        |    # Backup location
                        |    location="gs://backups"
                        |    # Backup retention period
                        |    retentionPeriod="P30D"
                        |    # Backup schedule
                        |    schedule="0 0 * * *"
                        |}
                        |# Compute resources configuration
                        |compute {
                        |    # Auto-scaling configuration
                        |    autoScaling {
                        |        # Cool-down period between scaling actions
                        |        coolDown="5m"
                        |        # Maximum number of instances
                        |        maxInstances=5
                        |        # Minimum number of instances
                        |        minInstances=1
                        |    }
                        |    # CPU allocation
                        |    cpu {
                        |        # CPU architecture
                        |        architecture="arm64"
                        |        # Base clock speed in GHz
                        |        clockSpeed=2.5
                        |        # Number of CPU cores
                        |        cores=2
                        |        # CPU usage threshold for scaling
                        |        usageThreshold=80.0
                        |    }
                        |    # Number of instances
                        |    instanceCount=2
                        |    # Memory allocation
                        |    memory {
                        |        # Swap space in GB
                        |        swapGb=0
                        |        # Total RAM in GB
                        |        totalGb=8
                        |    }
                        |    # Instance size to use
                        |    size=Small
                        |}
                        |# Basic metadata about the deployment
                        |metadata {
                        |    # Creation timestamp
                        |    createdAt="2024-01-01T00:00:00Z"
                        |    # Environment (e.g. prod, staging)
                        |    environment=test
                        |    # Unique identifier for the deployment
                        |    id="550e8400-e29b-41d4-a716-446655440000"
                        |    # Last modification timestamp
                        |    lastModified="2024-01-01T00:00Z[UTC]"
                        |    # Human readable name of the deployment
                        |    name="test-cluster"
                        |    # Cloud provider to use
                        |    provider=GCP
                        |    # Geographic region for deployment
                        |    region="us-central1"
                        |    # Deployment version
                        |    version="1"
                        |}
                        |# Network configuration settings
                        |network {
                        |    # DNS configuration
                        |    dns {
                        |        # Backup DNS servers
                        |        backupDns=[
                        |            "8.8.4.4"
                        |        ]
                        |        # Primary DNS server
                        |        primaryDns="8.8.8.8"
                        |        # DNS search domains
                        |        searchDomains=[
                        |            "test.com"
                        |        ]
                        |        # TTL for DNS records in seconds
                        |        ttl="5m"
                        |    }
                        |    # Load balancer settings
                        |    loadBalancers=[
                        |        {
                        |            # Load balancer address
                        |            address="lb.test.com:443"
                        |            # Health check interval
                        |            healthCheckInterval="30s"
                        |            # Health check path
                        |            healthCheckPath="/health"
                        |        }
                        |    ]
                        |    # Network policies ordered by priority
                        |    networkPolicies {
                        |        "1" {
                        |            # Destination IP pattern
                        |            destIp="10\\.0\\..*"
                        |            # Policy name
                        |            name="allow-http"
                        |            # Allowed ports
                        |            ports=[
                        |                80,
                        |                443
                        |            ]
                        |            # Source IP pattern
                        |            sourceIp=".*"
                        |        }
                        |    }
                        |    # List of subnet configurations
                        |    subnets=[
                        |        {
                        |            # Subnet CIDR block
                        |            cidr="10.0.1.0/24"
                        |            # Whether subnet is public
                        |            isPublic=true
                        |            # Availability zone
                        |            zone="us-central1-a"
                        |        }
                        |    ]
                        |    # VPC CIDR block
                        |    vpcCidr="10.0.0.0/16"
                        |}
                        |# Security and access control settings
                        |security {
                        |    # Audit log settings
                        |    auditLog {
                        |        # Log format pattern
                        |        formatPattern=".*"
                        |        # Log file location
                        |        location="/var/log/audit"
                        |        # Log level
                        |        logLevel=INFO
                        |        # Log retention period
                        |        retentionPeriod="P1Y"
                        |    }
                        |    # Authentication settings
                        |    authentication {
                        |        # Allowed authentication methods
                        |        allowedMethods=[
                        |            password
                        |        ]
                        |        # Authentication endpoint
                        |        endpoint="https://auth.test.com"
                        |        # Authentication provider
                        |        provider=OIDC
                        |        # Session timeout
                        |        sessionTimeout="1h"
                        |    }
                        |    # Security compliance level
                        |    complianceLevel=high
                        |    # Firewall rules
                        |    firewallRules {
                        |        "allow-ssh"="tcp/22"
                        |    }
                        |}
                        |# Storage configuration settings
                        |storage {
                        |    # Storage encryption settings
                        |    encryption {
                        |        # Encryption algorithm
                        |        algorithm=AES
                        |        # Key rotation period
                        |        keyRotationPeriod="P90D"
                        |        # Key strength in bits
                        |        keyStrength=256
                        |    }
                        |    # Storage mount point
                        |    mountPoint="/data"
                        |    # Storage type (e.g. SSD, HDD)
                        |    storageType=SSD
                        |    # Total storage size
                        |    totalSize="500.00"
                        |}
                        |""".stripMargin

    // Verify all fields are present in the rendered output
    assert(renderedConfig.contains("id=\"550e8400-e29b-41d4-a716-446655440000\""))
    assert(renderedConfig.contains("name=\"test-cluster\""))
    assert(renderedConfig.contains("environment=test"))
    assert(renderedConfig.contains("provider=GCP"))
    assert(renderedConfig.contains("region=\"us-central1\""))
    assert(renderedConfig.contains("vpcCidr=\"10.0.0.0/16\""))
    assert(renderedConfig.contains("cidr=\"10.0.1.0/24\""))
    assert(renderedConfig.contains("zone=\"us-central1-a\""))
    assert(renderedConfig.contains("isPublic=true"))
    assert(renderedConfig.contains("primaryDns=\"8.8.8.8\""))
    assert(renderedConfig.contains("""backupDns=[
      |            "8.8.4.4"
      |        ]""".stripMargin))
    assert(renderedConfig.contains("""searchDomains=[
      |            "test.com"
      |        ]""".stripMargin))
    assert(renderedConfig.contains("address=\"lb.test.com:443\""))
    assert(renderedConfig.contains("healthCheckPath=\"/health\""))
    assert(renderedConfig.contains("name=\"allow-http\""))
    assert(renderedConfig.contains("""ports=[
      |                80,
      |                443
      |            ]""".stripMargin))
    assert(renderedConfig.contains("size=Small"))
    assert(renderedConfig.contains("instanceCount=2"))
    assert(renderedConfig.contains("cores=2"))
    assert(renderedConfig.contains("architecture=\"arm64\""))
    assert(renderedConfig.contains("clockSpeed=2.5"))
    assert(renderedConfig.contains("totalGb=8"))
    assert(renderedConfig.contains("minInstances=1"))
    assert(renderedConfig.contains("maxInstances=5"))
    assert(renderedConfig.contains("coolDown=\"5m\""))
    assert(renderedConfig.contains("storageType=SSD"))
    assert(renderedConfig.contains("totalSize=\"500.00\""))
    assert(renderedConfig.contains("mountPoint=\"/data\""))
    assert(renderedConfig.contains("algorithm=AES"))
    assert(renderedConfig.contains("keyRotationPeriod=\"P90D\""))
    assert(renderedConfig.contains("complianceLevel=high"))
    assert(renderedConfig.contains("\"allow-ssh\"=\"tcp/22\""))
    assert(renderedConfig.contains("provider=OIDC"))
    assert(renderedConfig.contains("endpoint=\"https://auth.test.com\""))
    assert(renderedConfig.contains("sessionTimeout=\"1h\""))
    assert(renderedConfig.contains("""allowedMethods=[
      |            password
      |        ]""".stripMargin))
    assert(renderedConfig.contains("retentionPeriod=\"P1Y\""))
    assert(renderedConfig.contains("logLevel=INFO"))
    assert(renderedConfig.contains("location=\"/var/log/audit\""))
    assert(renderedConfig.contains("schedule=\"0 0 * * *\""))
    assert(renderedConfig.contains("retentionPeriod=\"P30D\""))
    assert(renderedConfig.contains("location=\"gs://backups\""))
    assert(renderedConfig.contains("compression=gzip"))
    assert(renderedConfig.contains("compressionLevel=9"))
    assert(renderedConfig.contains("fileFormat=tar"))
    assert(renderedConfig.contains("locale=\"en-US\""))

    // Verify optional fields are not present
    assert(!renderedConfig.contains("costCenter"))
    assert(!renderedConfig.contains("iopsLimit"))
    assert(!renderedConfig.contains("monitoring"))

    // Verify that the rendered config matches the expected string
    assertEquals(renderedConfig, expectedStr)

  test("sanity check - round trip") {
    val renderedConfig = ConfigCodec[CloudInfraConfig].write(testConfig, includeComments = true).render(renderOptions)

    val readConfig = ConfigCodec[CloudInfraConfig].read(ConfigFactory.parseString(renderedConfig).root)

    readConfig match
      case ReadSucceeded(obtainedConfig) =>
        Differ[CloudInfraConfig].assertNoDiff(
          obtainedConfig,
          testConfig
        )
      case ReadFailed(errors) =>
        fail(s"Failed to read config: ${errors.mkString("\n")}")
  }

end UsabilityTests
