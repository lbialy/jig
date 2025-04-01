package machinespir.it.jig

import scala.util.control.NoStackTrace

/** A config error that never captures stack traces. */
case class ConfigEntryError(msg: String, path: List[ConfigPath] = List(ConfigPath.Root))
    extends Exception(
      s"$msg (at ${ConfigPath.renderPath(path)})"
    )
    with NoStackTrace
