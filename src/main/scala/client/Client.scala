package client


import scala.annotation.tailrec
import scala.io.StdIn

enum ClientModes:
  override def toString: String =
    this match
      case ClientModes.GameSkinEdit => "GameSkin"
      case ClientModes.Normal => "Normal"
      case ClientModes.Invalid => "Invalid"

  case GameSkinEdit, Normal, Invalid

object ClientModes:
  def fromString(name: String): ClientModes =
    name.toLowerCase() match
      case "gameskin" => GameSkinEdit
      case "normal" => Normal
      case _ => Invalid



object Client:
  private var mode = ClientModes.Normal
  private var saveFolder = ""
  private var targetSkin = ""
  private var primarySkin = ""

  def run(): Unit =
    readInput()

  def setFolder(path: String): Unit =
    saveFolder = path
    println(s"Target Folder Set To: $path")

  def getFolder: String =
    saveFolder

  def setPrimary(skin: String): Unit =
    primarySkin = skin

  def setTarget(skin: String): Unit =
    targetSkin = skin

  def getTarget: String = targetSkin

  def getPrimary: String = primarySkin

  def setMode(mode: ClientModes): Unit =
    this.mode = mode

  def setMode(mode: String): Unit =
    this.mode = ClientModes.fromString(mode)



  @tailrec
  private def readInput(): Unit =
    val line: String = StdIn.readLine(s"${mode.toString} ${if mode == ClientModes.GameSkinEdit && Client.getPrimary != "" then s"[${Client.getPrimary} : ${Client.getTarget}]" else ""}> ")
    CommandHandler.tryRun(
      mode match
        case ClientModes.Normal => line.split(" ")
        case ClientModes.GameSkinEdit => s"gameskin $line".split(" ")
        case _ => Seq()
    )
    readInput()

  private def handleInput(input: String): Unit =
    printf("Hi")
