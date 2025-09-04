package client

// I think this command handler a bit overkill but yaaaa
// Maybe overcomplicated things but we move

object CommandHandler:
  private val commands: collection.mutable.Set[Command] = collection.mutable.Set()

  def +=(command: Command): Unit =
    get(command.name) match
      case Some(_) => println(s"Command '$command.name' already exists\n")
      case None =>
        commands += command
        printf("Registered command '%s'\n", command.name)

  def tryRun(args: Seq[String]): Unit =
    args match
      case cmd +: args =>
        for command <- get(cmd) do
          if command > args.length then
            println(">\tExpected " + command.expectedArgs + " arguments, got " + args.length + "\n")
          else
            command.trigger(args)
      case _ =>

  def get(name: String): Option[Command] =
    commands.find(x => x.name == name || (x.aliases contains name)) match
      case Some(command) => Some(command)
      case None => None