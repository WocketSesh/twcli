package client

// Doesnt have an option for optional args and cba tbh !!!
class Command(val name: String, val description: String, val expectedArgs: Int, val aliases: Seq[String] = Seq(), val subCommands: collection.mutable.Set[Command] = collection.mutable.Set()):
  private var onRun: (Seq[String], Command) => Unit = _

  def >(that: Int): Boolean =
    this.expectedArgs > that

  def +=(f: (Seq[String], Command) => Unit): Command =
    onRun = f
    this

  def ++=(command: Command): Command =
    subCommands += command
    this

  def ++=(commands: List[Command]): Command =
    commands.foreach(++=)
    this

  def printHelp(indentation: Int = 0): Unit =
    println(s"${" ".repeat(indentation)}$name${if aliases.nonEmpty then s" (${aliases.mkString(", ")}) " else ""} - $description")
    if subCommands.nonEmpty then
      println(s"${" ".repeat(indentation)}Sub Commands:")
      subCommands.foreach(_.printHelp(indentation + 2))

  def getSubCommand(name: String): Option[Command] =
    subCommands.find(x => x.name == name || (x.aliases contains name))

  def subCommandExists(name: String): Boolean =
    subCommands.exists(x => x.name == name || (x.aliases contains name))

  def trigger(args: Seq[String]): Unit =
    if args.nonEmpty && subCommandExists(args.head) then
      getSubCommand(args.head).get.trigger(args.tail)
    else
      onRun(args, this)








