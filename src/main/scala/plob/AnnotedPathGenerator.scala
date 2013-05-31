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
//import plob.stages._
//import plob.builders.toFilter
//import plob.builders.toPath
import java.nio.file.WatchService
import java.nio.file.WatchKey
import plob._


//import scala.sys.process.ProcessLogger
//
//class ProcessLoggerAnnotedPath(who : String, change : Change, path : Path, outLevel : Level = Level.Info, errLevel : Level = Level.Warning) extends ProcessLogger {
//  private var _annotedPaths : List[AnnotedPath] = Nil
//
//  def annotedPaths = _annotedPaths.toList
//  def out(s: => String): Unit = { _annotedPaths = AnnotedPath(change, path, Set(Marker(who, s, outLevel))) :: _annotedPaths }
//  def err(s: => String): Unit = { _annotedPaths = AnnotedPath(change, path, Set(Marker(who, s, errLevel))) :: _annotedPaths }
//  def buffer[T](f: => T): T = f
////    def close(): Unit = writer.close()
////    def flush(): Unit = writer.flush()
//}

class AnnotedPathGenerator(rootDir : RPath, subParents : List[RPath] = Nil) {

  private val _rootDir = rootDir.normalize()
  
  private val _parents = {
    ((rootDir :: subParents)
      .map(x => (toPath(x), x))
      .sortWith(_._1.toString > _._1.toString)
    ).toList
  }
  private def pathToRPath(v : Path, parents : List[(Path, RPath)]) : RPath = {
    val v0 = v.normalize()
    parents.find(x => v0.startsWith(x._1)) match {
      case None => RPath(v0.toString(), None)
      case Some((p,r)) => RPath(p.relativize(v0).toString(), Option(r))
    }
  }

  private def toRPath(v : Path) = pathToRPath(v, _parents)
  
  def all : Run = {
    val ts = TimeStamp.now()
    val entities = new ListBuffer[Entity]()
    Files.walkFileTree(_rootDir, new SimpleFileVisitor[Path]() {
      override def visitFile(f : Path, attrs : BasicFileAttributes) : FileVisitResult = {
        //println("...", f, f.getParent, f.getRoot())
        entities += new Entity().link(Change.FakeModified(ts)).link(toRPath(f))
        FileVisitResult.CONTINUE
      }
    })
    new Run(entities, ts, false) // .toSeq
  }
  //def watcher :(builders : builders.Builder,)

  //TODO manage StandardWatchEventKinds.OVERFLOW
  private def toAnnotedPath(dir : Path, event : WatchEvent[_], ts : TimeStamp) : (Change, RPath) = {
    val status = event.kind match {
      case StandardWatchEventKinds.ENTRY_CREATE => Change.Created(ts)
      case StandardWatchEventKinds.ENTRY_DELETE => Change.Deleted(ts)
      case _ => Change.Modified(ts)
    }
    (status, toRPath(dir.resolve(event.context().asInstanceOf[Path])))
  }

  def runAllOnce(build : Stage, resultsCallback : (Run) => Unit) {
    val apathsAfter = build(all)
    resultsCallback(apathsAfter)
  }

  def watch(build : Stage, resultsCallback : (Run) => Unit) {
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

      // TODO refactor
      // grab all enqueued changes
      var wkeyi : WatchKey = null
      do {
        wkeyi = watchService.poll()
        if (wkeyi != null) wkeys += wkeyi
      } while(wkeyi != null)

      val ts = TimeStamp.now()
      // poll for file system events on the WatchKeys
      val apathsBefore = for {
        wkey <- wkeys.distinct
        dir <- List(watchKeys(wkey))
        event <- wkey.pollEvents().toSeq
      } yield {
        println(">> ", dir, wkey)
        toAnnotedPath(dir, event, ts)
      }

      println("build trigger by", apathsBefore)
      // watch newly created directory
      for (apath <- apathsBefore) {
        if (apath._1.isInstanceOf[Change.Created] && Files.isDirectory(apath._2, LinkOption.NOFOLLOW_LINKS)) {
          register(apath._2)
        }
      }
      //TODO stop watching deleted directory (and subdirectory)
      //TODO merge runBefore into previous run to always submit every things ?
      val runBefore = new Run(apathsBefore.map{ x => new Entity().link(x)}, ts, false)
      val runAfter = build(runBefore)
      resultsCallback(runAfter)

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


