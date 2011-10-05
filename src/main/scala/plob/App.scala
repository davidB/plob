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

/**
 * @author David Bernard
 */
object Main {
  import builders._
  
  def foo(x : Array[String]) = x.foldLeft("")((a,b) => a + b)
  
  def main(args : Array[String]) {
    main2(args)
  }

  def main2(args : Array[String]) {
    var build = pipe(
        CoffeeScriptCompiler("src/main/coffee", "glob:**/*.coffee", "target/webapp/_scripts"),
        JadeCompiler("src/main/jade", "glob:**/*.jade", "target/webapp"),
        CoffeeScriptCompiler("src/test/coffee", "glob:**/*.coffee", "target/webapp"),
        VowsRunner("src/test/coffee", "glob:**/*.js")
    )
    val input = new AnnotedPathGenerator("src")
    
    val apaths = build(input.all)
    //println("result", apaths)
  }

  def main3(args : Array[String]) {
    var build = pipe(
        CoffeeScriptCompiler("src/main/coffee", "**/*.coffee", "target/webapp/_scripts"),
        JadeCompiler("src/main/jade", "**/*.jade", "target/webapp"),
        CoffeeScriptCompiler("src/test/coffee", "**/*.coffee", "target/webapp"),
        VowsRunner("src/test/coffee", "**/*.js")
    )
    val input = new AnnotedPathGenerator("src")
    
    val apaths = build(input.all)
    println("result", apaths)
    println( "Hello World!" )
    println("concat arguments = " + foo(args))
  }
}

object Sync {
  
  // TODO implement
  def apply(inputDir : Path, outputDir : Path) : builders.Builder = { apaths : builders.AnnotedPathS =>
    val n = for (apath <- apaths) yield {
      val src = apath.path
      val dest = outputDir.resolve(inputDir.relativize(src))
      apath.change match {
        case Change.Deleted => Files.deleteIfExists(dest) //TODO delete recursively for Directory
        case _ => Files.copy(src, dest, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES, LinkOption.NOFOLLOW_LINKS)
      }
      AnnotedPath(apath.change, dest)
    }
    n ++ apaths
  }

  def apply(inputDir : Path, inputFilter : builders.Filter, outputDir : Path) : builders.Builder = builders.route((inputFilter, apply(inputDir, outputDir)))

}

// sample to use rhino instead of node : java -classpath path/to/rhino/js.jar:path/to/closure/compiler.jar org.mozilla.javascript.tools.shell.Main r.js -o path/to/buildconfig.js

object CoffeeScriptCompiler {

  
  // TODO implement
  def apply(inputDir : Path, outputDir : Path) : builders.Builder = { apaths : builders.AnnotedPathS =>
    import scala.sys.process.Process
    println("---", apaths)
    val files = builders.toRelativeFilePaths(apaths, inputDir).toList
    println("coffee", "-o" :: outputDir.toFile.getAbsolutePath :: files)
    val listed: String = Process("coffee", "-o" :: outputDir.toFile.getAbsolutePath :: files ) !!;
    println(listed)
    //builders.identity
    apaths
  }

  def apply(inputDir : Path, inputFilter : builders.Filter, outputDir : Path) : builders.Builder = builders.route((inputFilter, apply(inputDir, outputDir)))

}

object JadeCompiler {
  // TODO implement
  def apply(inputDir : Path, inputFilter : builders.Filter, outputDir : Path) : builders.Builder = builders.identity
}

object VowsRunner {
  // TODO implement
  def apply(inputDir : Path, inputFilter : builders.Filter) : builders.Builder = builders.identity
}

object builders {
  type Filter = AnnotedPath => Boolean
  type AnnotedPathS = Traversable[AnnotedPath]
  type Builder = AnnotedPathS => AnnotedPathS

  implicit def toFilter(s : String) : Filter = {
    val matcher = FileSystems.getDefault.getPathMatcher(s)
    return { x => matcher.matches(x.path) }
  }
  
  // TODO implement
  implicit def toPath(s : String) : Path = {
    FileSystems.getDefault().getPath(s);
  }
  
  def toRelativeFilePaths(v : AnnotedPathS, cwd : Path) : Traversable[String] = {
    v.map{x => cwd.relativize(x.path).toString }
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
      apaths ++ builder(apaths)
    }
  }

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

class AnnotedPathGenerator(val rootDir : Path) {
  def all : builders.AnnotedPathS = {
    val back = new ListBuffer[AnnotedPath]() 
    Files.walkFileTree(rootDir, new SimpleFileVisitor[Path]() {
      override def visitFile(f : Path, attrs : BasicFileAttributes) : FileVisitResult = {
        println("...", f, f.getParent, f.getRoot())
        back += AnnotedPath(Change.FakeModified, f)
        FileVisitResult.CONTINUE
      }
    })
    back // .toSeq 
  }
  //def watcher :(builders : builders.Builder,)
  
  private def toAnnotedPath(event : WatchEvent[_]) : AnnotedPath = {
    val status = event.kind match {
      case StandardWatchEventKinds.ENTRY_CREATE => Change.Created
      case StandardWatchEventKinds.ENTRY_DELETE => Change.Deleted
      case _ => Change.Modified
    }
    AnnotedPath(status, event.context().asInstanceOf[Path])
  }

  def watch(build : builders.Builder) {
    
    val watchService = rootDir.getFileSystem().newWatchService()
    rootDir.register(watchService, StandardWatchEventKinds.ENTRY_CREATE, StandardWatchEventKinds.ENTRY_MODIFY, StandardWatchEventKinds.ENTRY_DELETE)
    
    // loop forever to watch directory
    @tailrec
    def waitEvent() {
      import scala.collection.JavaConversions._
      
      val watchKey = watchService.take(); // this call is blocking until events are present
      // poll for file system events on the WatchKey
      val apathsBefore = watchKey.pollEvents().toSeq.map{ x => toAnnotedPath(x) }
      println("build trigger by", apathsBefore)
      val apathsAfter = build(apathsBefore)
      println(apathsAfter)
 
      // if the watched directed gets deleted, get out of run method
      if (!watchKey.reset()) {
        //System.out.println("No longer valid");
        watchKey.cancel()
        watchService.close()
      } else {
        waitEvent()
      }
    }
    
    waitEvent()
  }
}


/*
TODO

# 0.1.0
- basic sample that allow to build a basic static site 
- builder : directory sync
- builder : Compiler_CoffeeScript,
- builder : Compiler_Jade
- builder : Checker_JsHint
- builder : Checker_Vows
- builder : Compressor_UglifyJS
- builder : Compressor_YuiCompressorJs
- builder : Compressor_YuiCompressorCss
- builder : Compressor_Rjs
- can build jdrol_vtt (internal project)  

# 0.2.0
- capture output of command, parse and generate uniform log and Marker for AnnotedPath
- builder : Compiler_Scalate
- builder : Compiler_Sass
- builder : Compiler_Compass
- can build my blog (originally nanoc3 based)
- can generate the doc of plob

# 0.3.0
- packaging of plugin
- packaging of the main app
- ease of deploy, share, retrieve builders
- stabilize api
- document (sample build, how to create a Builder,...)

# 0.4.0
- Integration : vim, eclipse, maven

*/