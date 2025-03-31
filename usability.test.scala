package machinespir.it.jig

import org.ekrich.config.*
import scala.concurrent.duration.*
import java.time.{Duration as JavaDuration, *}
import java.util.{List as JavaList, Map as JavaMap, Set as JavaSet, Vector as JavaVector, *}
import java.net.*
import java.nio.file.Path
import java.util.regex.Pattern
import scala.collection.immutable.{TreeMap, TreeSet}

// Enums for our domain
enum CloudProvider derives ConfigCodec:
  case AWS, GCP, Azure

enum InstanceSize derives ConfigCodec:
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
) derives ConfigCodec

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
) derives ConfigCodec

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
) derives ConfigCodec

case class SubnetConfig(
    @comment("Subnet CIDR block")
    cidr: String,
    @comment("Availability zone")
    zone: String,
    @comment("Whether subnet is public")
    isPublic: Boolean
) derives ConfigCodec

case class DnsConfig(
    @comment("Primary DNS server")
    primaryDns: InetAddress,
    @comment("Backup DNS servers")
    backupDns: Vector[InetAddress],
    @comment("DNS search domains")
    searchDomains: List[String],
    @comment("TTL for DNS records in seconds")
    ttl: JavaDuration = JavaDuration.ofSeconds(300)
) derives ConfigCodec

case class LoadBalancerConfig(
    @comment("Load balancer address")
    address: InetSocketAddress,
    @comment("Health check path")
    healthCheckPath: String,
    @comment("Health check interval")
    healthCheckInterval: FiniteDuration = 30.seconds,
    @comment("SSL certificate path")
    sslCertPath: Option[Path] = None
) derives ConfigCodec

case class NetworkPolicy(
    @comment("Policy name")
    name: String,
    @comment("Source IP pattern")
    sourceIp: Pattern,
    @comment("Destination IP pattern")
    destIp: Pattern,
    @comment("Allowed ports")
    ports: TreeSet[Int]
) derives ConfigCodec

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
) derives ConfigCodec

case class CpuConfig(
    @comment("Number of CPU cores")
    cores: Short,
    @comment("CPU architecture")
    architecture: String,
    @comment("Base clock speed in GHz")
    clockSpeed: Float,
    @comment("CPU usage threshold for scaling")
    usageThreshold: Double = 80.0
) derives ConfigCodec

case class MemoryConfig(
    @comment("Total RAM in GB")
    totalGb: Long,
    @comment("Swap space in GB")
    swapGb: Int = 0,
    @comment("Memory reservation in GB")
    reservationGb: Option[Int] = None
) derives ConfigCodec

case class AutoScalingConfig(
    @comment("Minimum number of instances")
    minInstances: Byte,
    @comment("Maximum number of instances")
    maxInstances: Byte,
    @comment("Cool-down period between scaling actions")
    coolDown: java.time.Duration,
    @comment("Scaling schedule")
    schedule: Option[AutoScalingSchedule] = None
) derives ConfigCodec

case class AutoScalingSchedule(
    @comment("Schedule start date")
    startDate: LocalDate,
    @comment("Daily scale-up time")
    scaleUpTime: LocalTime,
    @comment("Daily scale-down time")
    scaleDownTime: LocalTime,
    @comment("Time zone for the schedule")
    timeZone: ZoneId = ZoneId.of("UTC")
) derives ConfigCodec

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
) derives ConfigCodec

case class StorageEncryption(
    @comment("Encryption algorithm")
    algorithm: String,
    @comment("Key rotation period")
    keyRotationPeriod: Period,
    @comment("Key strength in bits")
    keyStrength: Short = 256
) derives ConfigCodec

case class SecurityConfig(
    @comment("Security compliance level")
    complianceLevel: String,
    @comment("Firewall rules")
    firewallRules: Map[String, String],
    @comment("Authentication settings")
    authentication: AuthConfig,
    @comment("Audit log settings")
    auditLog: AuditLogConfig
) derives ConfigCodec

case class AuthConfig(
    @comment("Authentication provider")
    provider: String,
    @comment("Authentication endpoint")
    endpoint: URI,
    @comment("Session timeout")
    sessionTimeout: FiniteDuration,
    @comment("Allowed authentication methods")
    allowedMethods: Set[String]
) derives ConfigCodec

case class AuditLogConfig(
    @comment("Log retention period")
    retentionPeriod: Period,
    @comment("Log level")
    logLevel: String,
    @comment("Log file location")
    location: Path,
    @comment("Log format pattern")
    formatPattern: Pattern
) derives ConfigCodec

case class MonitoringConfig(
    @comment("Monitoring service endpoint")
    endpoint: URI,
    @comment("Metrics collection interval")
    interval: Duration,
    @comment("Alert notification settings")
    alerting: AlertConfig,
    @comment("Metrics retention configuration")
    retention: MetricsRetentionConfig
) derives ConfigCodec

case class AlertConfig(
    @comment("Alert recipients")
    recipients: List[String],
    @comment("Alert severity levels")
    severityLevels: Set[String],
    @comment("Notification channels")
    channels: Map[String, URI]
) derives ConfigCodec

case class MetricsRetentionConfig(
    @comment("Raw metrics retention")
    rawRetention: Period,
    @comment("Aggregated metrics retention")
    aggregatedRetention: Period,
    @comment("Retention buckets configuration")
    buckets: Map[String, Duration]
) derives ConfigCodec

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
) derives ConfigCodec

case class BackupFormat(
    @comment("Compression algorithm")
    compression: String,
    @comment("Compression level")
    compressionLevel: Byte,
    @comment("Backup file format")
    fileFormat: String,
    @comment("Locale for backup metadata")
    locale: Locale = Locale.US
) derives ConfigCodec

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
      |    coolDown = "PT5M"
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
        assertEquals(infraConfig.metadata.provider, CloudProvider.AWS)
        assertEquals(infraConfig.compute.size, InstanceSize.Small)
        assertEquals(infraConfig.network.subnets.size, 2)
        assertEquals(infraConfig.storage.totalSize, BigDecimal("1000.50"))

        pprint.pprintln(infraConfig)
      }
    )

  test("should support writing complex configuration"):
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

    val renderedConfig = ConfigCodec[CloudInfraConfig].write(testConfig, includeComments = true).render(renderOptions)

    println(renderedConfig)

    // Verify key elements in the rendered output
    assert(renderedConfig.contains("provider"))
    assert(renderedConfig.contains("GCP"))
    assert(renderedConfig.contains("us-central1"))
    assert(renderedConfig.contains("10.0.0.0/16"))
    assert(renderedConfig.contains("gs://backups"))

end UsabilityTests
