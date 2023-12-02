import scala.util.matching.Regex

case class Round(red : Int, green : Int, blue : Int)

object Round:
  def makeRegex(color : String) : Regex =
    s"""(\\d+) $color""".r

  val red   = makeRegex("red")
  val green = makeRegex("green")
  val blue  = makeRegex("blue")

  def parse(str : String) : Round =
    def toInt(regex : Regex) : Int =
      regex
        .findFirstMatchIn(str)
        .fold(0){ _.group(1).toInt }
    Round(toInt(red), toInt(green), toInt(blue))

  def max(a : Round, b : Round) : Round =
    Round(Math.max(a.red, b.red),
          Math.max(a.green, b.green),
          Math.max(a.blue, b.blue))

  def isLessOrEqual(a : Round, b : Round) : Boolean =
    a.red <= b.red && a.blue <= b.blue && a.green <= b.green

  def prettyPrint(a : Round) : String =
    s"red: ${a.red}, green: ${a.green}, blue: ${a.blue}"

case class Game(id : Int, rounds : Array[Round])

object Game:
  def parse (str : String) : Game =
    str.split(":") match {
      case Array(game, roundsString) =>
        val id = game match {
          case s"Game $n" => n.toInt
          case _ => throw Exception("Expected a string starting with \"Game [number]\"")
        }
        val rounds = roundsString
          .split(";")
          .map(Round.parse)
        Game(id, rounds)
      case _ => throw Exception("Illegal input.")
    }

  def prettyPrint(a : Game) : String =
    val rs = a.rounds.map(Round.prettyPrint).mkString("; ")
    s"Game ${a.id}: $rs"

def possibleGames (baseline: Round, games : Iterator[String]) : Int =
  games
    .map(Game.parse)
    .foldLeft(0) {
      (sum, game) =>
        if game
          .rounds
          .forall { round => Round.isLessOrEqual(round, baseline) }
        then
          sum + game.id
        else
          sum
      }

import scala.io.Source

@main
def main (file : String) =
  val input = Source.fromFile(file).getLines
  println(possibleGames(Round(12, 13, 14), input))
