package gameskin

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO


object GameSkinAsset:
  val Guns = List(Gun, Grenade, Laser, Shotgun)
  val Cursors = List(HammerCursor, GunCursor, ShotgunCursor, GrenadeCursor, LaserCursor, NinjaCursor)
  val Ammo = List(ShotgunAmmo, GunAmmo, GrenadeAmmo, LaserAmmo)
  val GunParticles = List(GunParticle1, GunParticle2, GunParticle3, ShotgunParticle1, ShotgunParticle2, ShotgunParticle3)
  val OtherParticles = List(Particle1, Particle2, Particle3, Particle4, Particle5, Particle6, Particle7, Particle8, Particle9)
  val NinjaParticles = List(NinjaParticle1, NinjaParticle2, NinjaParticle3)
  val Flags = List(FlagRed, FlagBlue)
  val Stars = List(Star1, Star2, Star3)
  val Health = List(HealthFull, HealthEmpty, Heart)
  val Armour = List(ArmorFull, ArmorEmpty, Shield)

  def getAsset(name: String): Option[GameSkinAsset] =
    GameSkinAsset.values.find(_.toString.toLowerCase() == name.toLowerCase())

  def listAssetsStrings(): Seq[String] =
    GameSkinAsset.values.map(_.toString).toList

enum GameSkinAsset(val x: Int, val y: Int, val width: Int, val height: Int):
  case Hook extends GameSkinAsset(64, 0, 128, 32)
  case HammerCursor extends GameSkinAsset(0, 0, 64, 64)
  case GunCursor extends GameSkinAsset(0, 128, 64, 64)
  case ShotgunCursor extends GameSkinAsset(0, 192, 64, 64)
  case GrenadeCursor extends GameSkinAsset(0, 256, 64, 64)
  case NinjaCursor extends GameSkinAsset(0, 320, 64, 64)
  case LaserCursor extends GameSkinAsset(0, 384, 64, 64)
  case Hammer extends GameSkinAsset(64, 32, 128, 96)
  case Gun extends GameSkinAsset(64, 128, 128, 64)
  case Shotgun extends GameSkinAsset(64, 192, 256, 64)
  case Grenade extends GameSkinAsset(64, 256, 256, 64)
  case Ninja extends GameSkinAsset(64, 320, 256, 64)
  case Laser extends GameSkinAsset(64, 384, 224, 96)
  case GunAmmo extends GameSkinAsset(192, 128, 64, 64)
  case ShotgunAmmo extends GameSkinAsset(320, 192, 64, 64)
  case GrenadeAmmo extends GameSkinAsset(320, 256, 64, 64)
  case LaserAmmo extends GameSkinAsset(320, 384, 64, 64)
  case GunParticle1 extends GameSkinAsset(256, 128, 128, 64)
  case GunParticle2 extends GameSkinAsset(384, 128, 128, 64)
  case GunParticle3 extends GameSkinAsset(512, 128, 128, 64)
  case ShotgunParticle1 extends GameSkinAsset(384, 192, 128, 64)
  case ShotgunParticle2 extends GameSkinAsset(512, 192, 128, 64)
  case ShotgunParticle3 extends GameSkinAsset(640, 192, 128, 64)
  case Particle1 extends GameSkinAsset(192, 0, 32, 32)
  case Particle2 extends GameSkinAsset(224, 0, 32, 32)
  case Particle3 extends GameSkinAsset(256, 0, 32, 32)
  case Particle4 extends GameSkinAsset(192, 32, 32, 32)
  case Particle5 extends GameSkinAsset(224, 32, 32, 32)
  case Particle6 extends GameSkinAsset(256, 32, 32, 32)
  case Particle7 extends GameSkinAsset(288, 0, 64, 64)
  case Particle8 extends GameSkinAsset(352, 0, 64, 64)
  case Particle9 extends GameSkinAsset(416, 0, 64, 64)
  case Star1 extends GameSkinAsset(480, 0, 64, 64)
  case Star2 extends GameSkinAsset(544, 0, 64, 64)
  case Star3 extends GameSkinAsset(608, 0, 64, 64)
  case HealthFull extends GameSkinAsset(672, 0, 64, 64)
  case HealthEmpty extends GameSkinAsset(736, 0, 64, 64)
  case ArmorFull extends GameSkinAsset(672, 64, 64, 64)
  case ArmorEmpty extends GameSkinAsset(736, 64, 64, 64)
  case Heart extends GameSkinAsset(320, 64, 64, 64)
  case Shield extends GameSkinAsset(384, 64, 64, 64)
  case Minus extends GameSkinAsset(256, 64, 64, 64)
  case NinjaTimer extends GameSkinAsset(672, 128, 128, 64)
  case NinjaParticle1 extends GameSkinAsset(800, 0, 224, 128)
  case NinjaParticle2 extends GameSkinAsset(800, 128, 224, 128)
  case NinjaParticle3 extends GameSkinAsset(800, 256, 224, 128)
  case FlagBlue extends GameSkinAsset(384, 256, 128, 256)
  case FlagRed extends GameSkinAsset(512, 256, 128, 256)


object GameSkin:
  private val gameSkins: collection.mutable.Map[String, GameSkin] = collection.mutable.Map()

  def createGameSkin(name: String, base: GameSkin): GameSkin =
    GameSkin(base.file, name)

  def getGameSkin(name: String): Option[GameSkin] =
    if gameSkins.contains(name) then
      Some(gameSkins(name))
    else None

  def getGameSkinNames: List[String] =
    gameSkins.keys.toList

  def exists(name: String): Boolean =
    gameSkins.contains(name)

  def loadAllGameSkins(paths: List[String]): Unit =
    paths.foreach(x => loadGameSkin(x))

  def loadGameSkin(path: String): Unit =
    val name = path.split('\\').last.split("\\.").head
    val gs = GameSkin(File(path), name)

    if gameSkins.contains(name) then
      println(s"GameSkin $name already exists")
      return

    println(s"Loaded GameSkin $name")

    gameSkins += (name -> gs)

  case class GameSkinPart(part: GameSkinAsset, original: String, bufferedImage: BufferedImage):
    val pixels: Array[Int] = bufferedImage.getRGB(0, 0, bufferedImage.getWidth(), bufferedImage.getHeight(), null, 0, bufferedImage.getWidth())


class GameSkin(val file: File, val name: String):
  private val bufferedImg = ImageIO.read(file)
  private val parts: collection.mutable.Map[GameSkinAsset, GameSkin.GameSkinPart] = collection.mutable.Map()

  if bufferedImg == null then
    throw new IllegalArgumentException(s"Image not found: ${file.getAbsolutePath}")

  if bufferedImg.getWidth() != 1024 || bufferedImg.getHeight() != 512 then
    throw new IllegalArgumentException(s"${file.getAbsolutePath}: Image width != 1024 or Image height != 512")

  for (asset <- GameSkinAsset.values)
    parts += (asset -> GameSkin.GameSkinPart(asset, name, getAssetPartBufferedImage(asset)))

  def saveAssetPart(part: GameSkinAsset, filePath: String): Unit =
    val out = BufferedImage(part.width, part.height, BufferedImage.TYPE_INT_ARGB)
    out.setRGB(0, 0, part.width, part.height, parts(part).pixels, 0, part.width)

    ImageIO.write(out, "png", File(filePath))

  def +=(part: GameSkinAsset, gameSkin: GameSkin): Unit =
    copyAssetPartFrom(part, gameSkin)

  def copyGunAssets(gameSkin: GameSkin): Unit =
    massCopyAssetPartFrom(GameSkinAsset.Guns, gameSkin)

  def massCopyAssetPartFrom(part: List[GameSkinAsset], gameSkin: GameSkin): Unit =
    part.foreach(x => copyAssetPartFrom(x, gameSkin))

  def copyAssetPartFrom(part: GameSkinAsset, gameSkin: GameSkin): Unit =
    parts(part) = gameSkin.getAssetPart(part)

  private def getAssetPart(part: GameSkinAsset): GameSkin.GameSkinPart =
    parts(part)

  def copyCursorAssets(gameSkin: GameSkin): Unit =
    massCopyAssetPartFrom(GameSkinAsset.Cursors, gameSkin)

  def saveGameSkin(filePath: String): Unit =
    parts.foreach { (asset, part) =>
      if part.original != name then writeAssetPart(asset)
    }

    ImageIO.write(bufferedImg, "png", File(filePath))

  private def writeAssetPart(part: GameSkinAsset): Unit =
    bufferedImg.setRGB(part.x, part.y, part.width, part.height, getAssetPartPixels(part), 0, part.width)

  private def getAssetPartPixels(part: GameSkinAsset): Array[Int] =
    parts(part).pixels

  private def getAssetPartBufferedImage(part: GameSkinAsset): BufferedImage =
    bufferedImg.getSubimage(part.x, part.y, part.width, part.height)


