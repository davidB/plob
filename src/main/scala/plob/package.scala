import java.nio.file.FileSystems
import java.nio.file.Path

package object plob {
  type Filter = Entity => Boolean
  type Stage = Run => Run
  type StageName = String
  type TimeStamp = Long
  type BasicLogs = List[String]

  def identity : Stage = { a => a }

  def route(vs : (Filter, Stage)*) : Stage = { run =>
    val (mappings, unmapped) : (List[(Run, Stage)], Run) = vs.foldLeft((Nil : List[(Run, Stage)], run)){ (acc, fb) =>
      val (selected, unselected) = acc._2.partition(fb._1)
      ((selected, fb._2) :: acc._1, unselected)
    }
    mappings.foldLeft(identity(unmapped)) { (acc, mapping) =>
      //TODO provide more user feedback if merge failed (None)
      (acc + mapping._2(mapping._1)).getOrElse(acc)
    }
  }

  def pipe(vs : Stage*) : Stage = { run =>
    vs.foldLeft(run){ (acc, stage) =>
      stage(acc)
    }
  }

//  implicit def toPathFilter(s : String) : Filter = {
//    val matcher = FileSystems.getDefault.getPathMatcher(s)
//    return { x => x.as[Path].map{ x2 => matcher.matches(x2) }.getOrElse(false) }
//  }
//
//  implicit def toPath(s : String) : Path = {
//    FileSystems.getDefault().getPath(s)//.normalize()
//  }
//
//  def toRelativeFilePaths(v : Traversable[Path], cwd : Path) : Traversable[String] = {
//    val cwdn = cwd.normalize() // normalize else "toto/.".relativize("toto/x") => "../x" instead of "x"
//    v.map{x => cwdn.relativize(x).toString }
//  }
  
  implicit def toRPath(v : String ) : RPath = {
    RPath(FileSystems.getDefault().getPath(v.path).normalize().toString(), None)
  }
  
  implicit def toPath(v : RPath ) : Path = {
    v.parent match {
      case None => FileSystems.getDefault().getPath(v.path).normalize()
      case Some(p) => toPath(p).resolve(v.path)
    }
  }

  implicit def toBasicLogs(v : (String) => Any) : (String) => BasicLogs = (s: String) => {
    try {
      v(s)
      Nil
    } catch {
      case e : Exception => List(e.getMessage) 
    }
  }

  private def pathToRPath(v : Path, parents : List[(Path, RPath)]) : RPath = {
    val v0 = v.normalize()
    parents.find(x => v0.startsWith(x._1)) match {
      case None => RPath(v0.toString(), None)
      case Some((p,r)) => RPath(p.relativize(v0).toString(), Option(r))
    }
  }

  // TODO improve (see Eric Torrebore article about essence of iterator)
  def basicResultsConsolePrinter(run : Run) = {
    var fatalCnt = 0
    var errorCnt = 0
    var warningCnt = 0
    val logs = for {
      entity <- run.entities
      markers <- entity.as[Markers]
      if(!markers.l.isEmpty)
    } yield {
      markers.l.map { m=> 
        m.level match {
          case Level.Fatal => fatalCnt += 1
          case Level.Error   => errorCnt += 1
          case Level.Warning => warningCnt += 1
          case _ => ()
        }
        "%s\t|%s\t|%s|%s|%s".format(m.level, m.who, entity.as[Path].getOrElse(""), m.where, m.what)
      }
    }
    logs.flatten.foreach{ x => println(x) }
    val level = if (fatalCnt > 0) { Level.Fatal }
      else if (errorCnt > 0) { Level.Error }
      else if (warningCnt > 0) { Level.Warning }
      else { Level.Info }
    println("%s\t|plob\t|||Fatal : %d, Errors : %d, Warnings : %d".format(level, fatalCnt, errorCnt, warningCnt))
  }


  trait StageCtrl {
    val ctrl = new CtrlInterface()
    def name : StageName = this.getClass.getName
    def stage : Run => Run
  }
}