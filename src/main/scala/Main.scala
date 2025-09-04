import client.{Client, Command, CommandHandler}
import commands.{createGameSkinCommand, createModeCommand}
import gameskin.{GameSkin, GameSkinAsset}

@main def main(): Unit = {
  CommandHandler += createModeCommand
  CommandHandler += createGameSkinCommand()

  Client.run()
}