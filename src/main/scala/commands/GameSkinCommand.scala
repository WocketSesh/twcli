package commands

import client.ClientModes.Normal
import client.{Client, Command}
import gameskin.{GameSkin, GameSkinAsset}

import java.io.File

// TODO: Finish error handling and shiznits dude
// why go oop and then not oop this shit yuhdig

def createGameSkinCommand(): Command =
  (Command("gameskin", "View and edit gameskins", 0, Seq("gs")) += gameSkinCommand)
    ++= (Command("load", "Load skin folder", 0, Seq("l")) += ((args: Seq[String], cmd: Command) => loadSkinFolder(if args.nonEmpty then args.head else Client.getFolder)))
    ++= (Command("exit", "Exit gameskin mode", 0, Seq("e")) += ((args: Seq[String], cmd: Command) => Client.setMode(Normal)))
    ++= (Command("set_folder", "Set skin folder", 0, Seq("sf")) += ((args: Seq[String], cmd: Command) => Client.setFolder(if args.nonEmpty then args.head else "")))
    ++= (Command("set_asset", "Set asset", 0, Seq("seta")) += ((args: Seq[String], cmd: Command) => setAsset(args)))
    ++= (Command("save_asset", "Save asset", 0, Seq("savea")) += ((args: Seq[String], cmd: Command) => saveAsset(args)))
    ++= (Command("list", "List skins", 0, Seq("ls")) += ((args: Seq[String], cmd: Command) => listSkins(args)))
    ++= (Command("save_gameskin", "Save gameskin", 0, Seq("sg")) += ((args: Seq[String], cmd: Command) => saveGameskin(args)))
    ++= (Command("set_target", "Set target skin", 0, Seq("st")) += ((args: Seq[String], cmd: Command) => setTarget(args, cmd)))
    ++= (Command("set_primary", "Set primary skin", 0, Seq("sp")) += ((args: Seq[String], cmd: Command) => setPrimary(args, cmd)))
    ++= (Command("set_guns", "Set guns", 0, Seq("setg")) += ((args: Seq[String], cmd: Command) => setGuns(args)))
    ++= (Command("set_particles", "Set particles", 0, Seq("setp")) += ((args: Seq[String], cmd: Command) => setParticles(args)))
    ++= (Command("set_cursors", "Set cursors", 0, Seq("setc")) += ((args: Seq[String], cmd: Command) => setCursors(args)))
    ++= (Command("set_assets", "Sets multiple assets", 0, Seq("setas")) += ((args: Seq[String], cmd: Command) => setLotsAssets(args)))

private def setGuns(args: Seq[String]): Unit =
  val (validArguments, usingPrimary) = sufficientArguments(args, 2)

  if !validArguments then
    println(s"Expected: set_guns <skin> <target>")
    return

  if !usingPrimary then setLotsAssets(Seq(args.head, GameSkinAsset.Guns.mkString(","), args(1))) else setLotsAssets(args)

private def setParticles(args: Seq[String]): Unit =
  val (validArguments, usingPrimary) = sufficientArguments(args, 2)

  if !validArguments then
    println(s"Expected: set_particles <skin> <target>")
    return

  if !usingPrimary then setLotsAssets(Seq(args.head, GameSkinAsset.OtherParticles.mkString(","), args(1))) else setLotsAssets(args)

private def setCursors(args: Seq[String]): Unit =
  val (validArguments, usingPrimary) = sufficientArguments(args, 2)

  if !validArguments then
    println(s"Expected: set_cursors <skin> <target>")
    return

  if !usingPrimary then setLotsAssets(Seq(args.head, GameSkinAsset.Cursors.mkString(","), args(1))) else setLotsAssets(args)

private def sufficientArguments(args: Seq[String], expectedCount: Int): (Boolean, Boolean) =
  val usingPrimary = Client.getPrimary != ""
  val usingTarget = Client.getTarget != ""
  val sufficientArguments =
    (args.nonEmpty && usingPrimary && usingTarget)
      || args.size >= expectedCount

  (sufficientArguments, usingPrimary && usingTarget)

private def setLotsAssets(args: Seq[String]): Unit =
  val (validArguments, usingPrimary) = sufficientArguments(args, 3)


  if !validArguments then
    println(s"Expected: set_assets <skin> <asset,asset,asset> <target>")
    return

  val skinName = if usingPrimary then Client.getPrimary else args.head

  if !GameSkin.exists(skinName) then
    println(s"Skin $skinName does not exist")

  val targetValue = if usingPrimary then Client.getTarget else args(2)

  if !GameSkin.exists(targetValue) then
    println(s"Skin $targetValue does not exist")

  val assetNames = (if usingPrimary then args.head else args(1)).split(',').toList

  for assetName <- assetNames
      asset <- GameSkinAsset.getAsset(assetName) do

    val skin = GameSkin.getGameSkin(skinName)
    val targetSkin = GameSkin.getGameSkin(targetValue)

    skin.get.copyAssetPartFrom(asset, targetSkin.get)

    println(s"Copied $assetName to $skinName from $targetValue")


private def gameSkinCommand(args: Seq[String], command: Command): Unit =
  println("GameSkin Help")
  command.printHelp()

private def setPrimary(args: Seq[String], command: Command): Unit =
  if args.isEmpty then
    println("Expected: set_primary <skin>")
    return

  val skinName = args.head
  val skin = GameSkin.getGameSkin(skinName)

  if skin.isEmpty then
    println(s"Skin $skinName does not exist")
    return

  Client.setPrimary(skinName)
  println(s"Set primary skin to $skinName")

private def setTarget(args: Seq[String], command: Command): Unit =
  if args.isEmpty then
    println("Expected: set_target <skin>")
    return

  val skinName = args.head
  val skin = GameSkin.getGameSkin(skinName)

  if skin.isEmpty then
    println(s"Skin $skinName does not exist")
    return

  Client.setTarget(skinName)
  println(s"Set target skin to $skinName")

private def setAsset(args: Seq[String]): Unit =
  val usingPrimary = Client.getPrimary != ""
  val usingTarget = Client.getTarget != ""

  val sufficientArguments =
    (args.nonEmpty && usingPrimary && usingTarget)
      || args.size >= 3

  if !sufficientArguments then
    println(s"Expected: set_asset <skin> <asset> <target>")
    return

  if (usingPrimary && !usingTarget) || (usingTarget && !usingPrimary) then
    println("Need to set both primary and target skin")
    return

  val skinName = if usingPrimary then Client.getPrimary else args.head

  if !GameSkin.exists(skinName) then
    println(s"Skin $skinName does not exist")

  val assetName = if usingPrimary then args.head else args(1)
  val asset = GameSkinAsset.getAsset(assetName)

  if asset.isEmpty then
    println(s"Asset $assetName does not exist")
    return

  val targetValue = if usingPrimary then Client.getTarget else args(2)

  if !GameSkin.exists(targetValue) then
    println(s"Skin $targetValue does not exist")
    return

  val skin = GameSkin.getGameSkin(skinName)
  val targetSkin = GameSkin.getGameSkin(targetValue)

  skin.get.copyAssetPartFrom(asset.get, targetSkin.get)

  println(s"Copied $assetName to $skinName from $targetValue")


private def saveGameskin(args: Seq[String]): Unit =
  val skinName = args.head
  val gameSkin = GameSkin.getGameSkin(skinName)
  if gameSkin.isEmpty then
    println(s"Skin $skinName does not exist")
  else
    gameSkin.get.saveGameSkin(s"${Client.getFolder}\\$skinName.png")
    println(s"Saved $skinName to ${Client.getFolder}\\$skinName.png")


private def saveAsset(args: Seq[String]): Unit =
  if args.size < 2 then return

  val skinName = args.head
  val assetName = args(1)
  val filePath = if (args.size > 2) args(2) else s"${Client.getFolder}\\${skinName}_$assetName.png"

  val gameSkin = GameSkin.getGameSkin(skinName)

  if gameSkin.isEmpty then
    println(s"Skin $skinName does not exist")
  else for (asset <- GameSkinAsset.getAsset(assetName)) do
    println(s"Saving $assetName to $filePath")
    gameSkin.get.saveAssetPart(asset, filePath)


private def listSkins(args: Seq[String]): Unit =
  if args.isEmpty then
    println("Expected: list <skins | assets>")
    return

  if args.head == "skins" then
    println("Loaded Skins: ")
    GameSkin.getGameSkinNames.foreach(println)
  else if args.head == "assets" then
    println("Valid Assets: ")
    GameSkinAsset.listAssetsStrings().foreach(println)


private def loadSkinFolder(path: String): Unit =
  val folder = File(path)
  println(s"Loading Skins from $path")
  if folder.isDirectory then
    GameSkin.loadAllGameSkins(folder.listFiles(f => f.isFile && f.getName.endsWith(".png")).map(_.getAbsolutePath).toList)
  else if path.endsWith(".png") then
    GameSkin.loadGameSkin(path)