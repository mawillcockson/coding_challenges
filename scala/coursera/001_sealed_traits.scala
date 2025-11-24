@main
def main: Unit =
  val received: List[MessageAction] = List(
    Subscribe("pub.example.com"),
    Unsubscribe("pub.example.org"),
    Post(message = "hello!", channel = "pub.example.edu")
  )
  for msg <- received
  do
    val log_message = msg match
      case Subscribe(channel)     => s"Subscribe to $channel"
      case Unsubscribe(channel)   => s"Unsubscribe from $channel"
      case Post(channel, message) => s"To $channel: $message"
    println(log_message)

  val action: MessageAction = Subscribe(channel = "pub.example.com")
  action match
    case Subscribe(channel)   => println(action.channel)
    case Unsubscribe(channel) => println(action.channel)
    case Post(channel)        => println(action.channel)

sealed trait MessageAction

case class Subscribe(channel: String) extends MessageAction
case class Unsubscribe(channel: String) extends MessageAction
case class Post(channel: String, message: String) extends MessageAction
