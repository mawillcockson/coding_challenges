package cats_of_ctthulhu

case class Cat(
    name: String,
    role: Role,
    background: Background,
    description: String
)

enum Role(val name: String):
  case Catcrobat extends Role("Catcrobat")
  case Pussyfoot extends Role("Pussyfoot")
  case Scraper extends Role("Scraper")
  case TigerDreamer extends Role("Tiger Dreamer")
  case Twofootologist extends Role("Twofootologist")
end Role

case class Background(
    lifestyle: Lifestyle,
    hunting_experience: String,
    other: String
)

enum Lifestyle:
  case Feral, Housecat, Showcat

@main
def main: Unit =
  println(
    Cat(
      name = "Catticus",
      role = Role.TigerDreamer,
      background = Background(
        lifestyle = Lifestyle.Housecat,
        hunting_experience = "all the yard birds",
        other = "N/A"
      ),
      description = "free-form description"
    )
  )
