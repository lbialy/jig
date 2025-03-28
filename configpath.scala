package machinespir.it.jig

/** Represents a path element in the config structure */
enum ConfigPath:
  case Root
  case Field(name: String)
  case Index(idx: Int)

  def render: String = this match
    case Root        => "root"
    case Field(name) => name
    case Index(idx)  => s"($idx)"

object ConfigPath:
  def renderPath(path: List[ConfigPath]): String =
    path.reverse.foldLeft("root")((acc, p) =>
      p match
        case Root        => acc
        case Field(name) => s"$acc.$name"
        case Index(idx)  => s"$acc($idx)"
    )
