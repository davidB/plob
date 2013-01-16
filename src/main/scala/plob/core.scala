package plob

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.SimpleFileVisitor
import java.nio.file.WatchEvent
import java.nio.file.FileSystems
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.LinkOption
import java.nio.file.Path
import java.nio.file.StandardCopyOption
import java.nio.file.StandardWatchEventKinds
import java.util.Calendar
import scala.annotation.tailrec
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable.ListBuffer
import scala.sys.process.Process
import builders.pipe
import plob.builders.toFilter
import plob.builders.toPath
import java.nio.file.WatchService
import java.nio.file.WatchKey


import scala.sys.process.ProcessLogger
class ProcessLoggerAnnotedPath(who : String, change : Change, path : Path, outLevel : Level = Level.Info, errLevel : Level = Level.Warning) extends ProcessLogger {
  private var _annotedPaths : List[AnnotedPath] = Nil

  def annotedPaths = _annotedPaths.toList
  def out(s: => String): Unit = { _annotedPaths = AnnotedPath(change, path, Set(Marker(who, s, outLevel))) :: _annotedPaths }
  def err(s: => String): Unit = { _annotedPaths = AnnotedPath(change, path, Set(Marker(who, s, errLevel))) :: _annotedPaths }
  def buffer[T](f: => T): T = f
//    def close(): Unit = writer.close()
//    def flush(): Unit = writer.flush()
}
  
sealed trait Position
object Position {
  case class OffSet(v : Int) extends Position
  case class LC(line : Int, column : Int) extends Position
  case class Range(begin : Position, end : Position) extends Position
}

sealed trait Level
object Level {
  case object Trace extends Level
  case object Debug extends Level
  case object Info extends Level
  case object Warning extends Level
  case object Error extends Level
  case object Fatal extends Level
}

case class Marker(who : String, what : String, level : Level, where : Option[Position] = None, when : Option[Calendar] = None)

sealed trait Change
object Change {
  case object Created extends Change
  case object Modified extends Change
  case object FakeModified extends Change
  case object Deleted extends Change
  case object Test extends Change
}

case class AnnotedPath(change : Change, path : Path, markers : Set[Marker] = Set.empty)

class AnnotedPathGenerator(rootDir : Path) {

  private val _rootDir = rootDir.normalize()

  def all : builders.AnnotedPathS = {
    val back = new ListBuffer[AnnotedPath]()
    Files.walkFileTree(_rootDir, new SimpleFileVisitor[Path]() {
      override def visitFile(f : Path, attrs : BasicFileAttributes) : FileVisitResult = {
        //println("...", f, f.getParent, f.getRoot())
        back += AnnotedPath(Change.FakeModified, f)
        FileVisitResult.CONTINUE
      }
    })
    back // .toSeq
  }
  //def watcher :(builders : builders.Builder,)

  //TODO manage StandardWatchEventKinds.OVERFLOW
  private def toAnnotedPath(dir : Path, event : WatchEvent[_]) : AnnotedPath = {
    val status = event.kind match {
      case StandardWatchEventKinds.ENTRY_CREATE => Change.Created
      case StandardWatchEventKinds.ENTRY_DELETE => Change.Deleted
      case _ => Change.Modified
    }
    AnnotedPath(status, dir.resolve(event.context().asInstanceOf[Path]).normalize())
  }

  def runAllOnce(build : builders.Builder, resultsCallback : (builders.AnnotedPathS) => Unit) {
    val apathsAfter = build(all)
    resultsCallback(apathsAfter)
  }

  def watch(build : builders.Builder, resultsCallback : (builders.AnnotedPathS) => Unit) {
     val watchService = _rootDir.getFileSystem().newWatchService()
     var watchKeys = Map.empty[WatchKey,Path]

     def register(dir : Path) = {
       val wkey = dir.register(watchService, StandardWatchEventKinds.ENTRY_CREATE, StandardWatchEventKinds.ENTRY_MODIFY, StandardWatchEventKinds.ENTRY_DELETE)
       println("++ ", (wkey -> dir))
       watchKeys += (wkey -> dir)
     }

    // loop forever to watch directory
    @tailrec
    def waitEvent(watchService : WatchService) {
      import scala.collection.JavaConversions._
      println("waiting FS event ...")

      val wkey0 = watchService.take() // this call is blocking until events are present
      val wkeys = ListBuffer[WatchKey](wkey0)
      println(">>> ", wkey0)

      // TODO refactor
      // grab all enqueued changes
      var wkeyi : WatchKey = null
      do {
        wkeyi = watchService.poll()
        if (wkeyi != null) wkeys += wkeyi
      } while(wkeyi != null)

      // poll for file system events on the WatchKeys
      val apathsBefore = for {
        wkey <- wkeys.distinct
        dir <- List(watchKeys(wkey))
        event <- wkey.pollEvents().toSeq
      } yield {
        println(">> ", dir, wkey)
        toAnnotedPath(dir, event)
      }

      println("build trigger by", apathsBefore)
      // watch newly created directory
      for (apath <- apathsBefore) {
        if (apath.change == Change.Created && Files.isDirectory(apath.path, LinkOption.NOFOLLOW_LINKS)) {
          register(apath.path)
        }
      }
      //TODO stop watching deleted directory (and subdirectory)

      val apathsAfter = build(apathsBefore)
      resultsCallback(apathsAfter)

      // if the watched directed gets deleted, get out of run method
      for (wkey <- wkeys) {
        if (!wkey.reset()) {
          //System.out.println("No longer valid");
          wkey.cancel()
          watchKeys -= (wkey)
        }
      }

      if (watchKeys.isEmpty) {
        watchService.close()
      } else {
        waitEvent(watchService)
      }
    }

    // register dir and subdirectory
    Files.walkFileTree(_rootDir, new SimpleFileVisitor[Path]() {
      override def preVisitDirectory(dir : Path, attrs : BasicFileAttributes) : FileVisitResult = {
        register(dir)
        FileVisitResult.CONTINUE
      }
    })
    waitEvent(watchService)
  }
}


