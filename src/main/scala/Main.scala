import client.{Client, Command, CommandHandler}
import commands.{createModeCommand, createGameSkinCommand}
import gameskin.{GameSkin, GameSkinAsset}

@main def main(): Unit = {
  CommandHandler += createModeCommand
  CommandHandler += createGameSkinCommand()

  Client.run()
}