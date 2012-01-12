import io.Source
import collection.Iterator

object AcsParser {
  def main(args: Array[String]) {
    if (args.length == 0) {
      parse(Source.fromInputStream(System.in))
    } else {
      parse(args.map(a => Source.fromFile(a, "utf-8")))
    }
  }

  implicit def stringToRicherString(s: String) = new RicherString(s)
  class RicherString(s: String) {
    def columnize(length: Int): String = columnize(length, ' ')
    def columnize(length: Int, pad: Char): String = s.substring(0,s.length.min(length)).padTo(length, pad)
    def trimToScreen: String = System.getenv("COLUMNS") match { 
      case null => s
      case columns: String => s.substring(0, s.length.min(columns.toInt))
    }
  }

  def parse(sources: Seq[Source]): Unit = parse(sources.foldLeft[Iterator[String]](Iterator.empty)(_ ++ _.getLines))

  def parse(source: Source): Unit = parse(source.getLines)

  def parse(lines: Iterator[String]): Unit = {
    val actions = lines.flatMap(lineToColumns)
    actions.map{ action =>
      Seq[(String,Int)](
        (action("datetime"), 13),
        (new java.util.Date(action("datetime").toLong).toString.substring(11), 8),
        (if (action("recordedby") == "server") "s" else if (action("recordedby") == "capture") "c" else "!", 1),
        (action.getOrElse("subject", "MISSING!"), 12),
        (action.getOrElse("verb", "MISSING!"), 15),
        (action.getOrElse("object", ""), 20),
        (action.getOrElse("url", "MISSING!"), 30), 
        (action.getOrElse("engagementid", if (action("recordedby") == "capture") "MISSING!" else ""), 8),
        (action.getOrElse("sessionid", "MISSING!"), 8),
        (action.getOrElse("billinggroupid", "MISSING!"), 8),
        (action.getOrElse("instanceid", "MISSING!"), 22),
        (action.getOrElse("productfamily", "!").substring(0, 1), 1),
        (action.getOrElse("product", "MISSING!"), 25),
        (action.getOrElse("useragent", "MISSING!"), 100),
        ("",0)
      ).map{case (s,l) => s.columnize(l)}.mkString(" ").trimToScreen
    }.foreach(println)
  }

  def lineToColumns(line: String): Option[Map[String,String]] = {
    try {
      return Some(com.codahale.jerkson.Json.parse[Map[String,AnyRef]](line).map {case (k,v) => (k,v.toString) })
    }
    catch {
      case ex: Exception => {
        println("ERROR PARSING: " + line)
        None
      }
    }
  }

}

