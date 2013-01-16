package plob

import java.nio.file.FileSystems
import java.nio.file.Path
import java.nio.file.Path

/**
 * @author David Bernard
 */
//TODO add testcase
//TODO provide build definition loader from FS (like rc.d or init.d on linux, ...)
object builders {
  type Filter = AnnotedPath => Boolean
  type AnnotedPathS = Traversable[AnnotedPath]
  type Builder = AnnotedPathS => AnnotedPathS

  implicit def toFilter(s : String) : Filter = {
    val matcher = FileSystems.getDefault.getPathMatcher(s)
    return { x => matcher.matches(x.path) }
  }

  implicit def toPath(s : String) : Path = {
    FileSystems.getDefault().getPath(s)//.normalize()
  }

  def toRelativeFilePaths(v : AnnotedPathS, cwd : Path) : Traversable[String] = {
    val cwdn = cwd.normalize() // normalize else "toto/.".relativize("toto/x") => "../x" instead of "x"
    v.map{x => cwdn.relativize(x.path).toString }
  }

  def identity : Builder = { a => a }

  def route(vs : (Filter, Builder)*) : Builder = { apaths =>
    val (mappings, unmapped) : (List[(AnnotedPathS, Builder)], AnnotedPathS) = vs.foldLeft((Nil : List[(AnnotedPathS, Builder)], apaths)){ (acc, fb) =>
      val (selected, unselected) = acc._2.partition(fb._1)
      selected.isEmpty match {
        case true => acc
        case false => ((selected, fb._2) :: acc._1, unselected)
      }
    }
    mappings.foldLeft(identity(unmapped)) { (acc, mapping) =>
      acc ++ mapping._2(mapping._1)
    }
  }

  def pipe(vs : Builder*) : Builder = { apaths =>
    vs.foldLeft(apaths){ (acc, builder) =>
      builder(acc)
    }
  }

  // TODO improve (see Eric Torrebore article about essence of iterator)
  def basicResultsConsolePrinter(apaths : AnnotedPathS) {
    var fatalCnt = 0
    var errorCnt = 0
    var warningCnt = 0
    val logs = for {
      apath <- apaths
      if (! apath.markers.isEmpty )
      m <- apath.markers
    } yield {
      m.level match {
        case Level.Fatal => fatalCnt += 1
        case Level.Error   => errorCnt += 1
        case Level.Warning => warningCnt += 1
        case _ => ()
      }
      "%s\t|%s\t|%s|%s|%s".format(m.level, m.who, apath.path, m.where.getOrElse(""), m.what)
    }
    logs.foreach{ x => println(x) }
    val level = if (fatalCnt > 0) { Level.Fatal }
      else if (errorCnt > 0) { Level.Error }
      else if (warningCnt > 0) { Level.Warning }
      else { Level.Info }
    println("%s\t|plob\t|||Fatal : %d, Errors : %d, Warnings : %d".format(level, fatalCnt, errorCnt, warningCnt))
  }
}
