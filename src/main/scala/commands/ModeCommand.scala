package commands

import client.{Client, Command}

def createModeCommand: Command =
  Command("mode", "Change command mode", 1, Seq("m")) += modeCommand

private def modeCommand(args: Seq[String], cmd: Command): Unit =
  Client.setMode(args.head)