case class Experience(duration: Time, definition: Double, network: Network)

enum Network:
  case Fixed, Mobile

sealed trait Time(val ratio: Double):
  val amount: Double
  def +(left: Time, right: Time): Time =
    Seconds(left.amount * left.ratio + right.amount * right.ratio)

case class Seconds(amount: Double) extends Time(1)
case class Minutes(amount: Double) extends Time(1 * 60)
case class Hours(amount: Double) extends Time(1 * 60 * 60)

@main
def main: Unit =
  println(Minutes(1) + Hours(1))
