def log(level: String = "INFO", message: String): Unit = println(
  s"$level: $message"
)

@main def main =
  log(message = "no level specified")
  log("WARNING", "level specified")
