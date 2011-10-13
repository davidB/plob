package plob

//TODO provide sample to use rhino instead of node : java -classpath path/to/rhino/js.jar:path/to/closure/compiler.jar org.mozilla.javascript.tools.shell.Main r.js -o path/to/buildconfig.js

object Compiler_CoffeeScript {
  import java.nio.file.Path
  
  // TODO implement
  // TODO monitor outputDir, if deleted => rebuild all (how ?)
  def apply(inputDir : Path, outputDir : Path, options : Seq[String] = Nil) : builders.Builder = {
    //TODO init (eg create outputDir),
    //TODO check context (eg coffee executable existe, else warning and fallback to rhino + ...)
    //TODO return a Validation or a Logger ??
    //TODO manage Deleted 
    return { apaths : builders.AnnotedPathS =>
      import scala.sys.process.Process
      var back = apaths.toList
      val files = builders.toRelativeFilePaths(apaths, inputDir).toList
      val dest = outputDir.toFile
      dest.mkdirs()
      val cmdline = "coffee" :: "-o" :: dest.getAbsolutePath :: options.toList ::: files 
      back = AnnotedPath(Change.Modified, inputDir, Set(Marker("coffee", "cmdline :" + cmdline.mkString("'", "' '", "'"), Level.Debug))) :: back
      try {
        val stdout: String = Process(cmdline, inputDir.toFile) !!;
        println(stdout)
      } catch {
        case t => {
          t.printStackTrace()
          back = AnnotedPath(Change.Modified, inputDir, Set(Marker("coffee", "%s:%s".format(t.getClass, t.getMessage), Level.Error))) :: back
        }
      }
      back
    }
  }
 //def apply(inputDir : Path, inputFilter : builders.Filter, outputDir : Path, options : Seq[String] = Nil) : builders.Builder = builders.route((inputFilter, exec(inputDir, outputDir, options)))

}

object Compiler_Jade {
  import java.nio.file.Path
  
  // TODO implement
  def apply(inputDir : Path, outputDir : Path, options : Seq[String] = Nil) : builders.Builder = { apaths : builders.AnnotedPathS =>
    import scala.sys.process.Process
    var back = apaths.toList
    val files = builders.toRelativeFilePaths(apaths, inputDir).toList
    val dest = outputDir.toFile
    dest.mkdirs()
    val cmdline = "jade" ::  "--out" :: dest.getAbsolutePath :: options.toList ::: files
    back = AnnotedPath(Change.Modified, inputDir, Set(Marker("jade", "cmdline :" + cmdline.mkString("'", "' '", "'"), Level.Debug))) :: back
    val stdout: String = Process(cmdline, inputDir.toFile) !!;
    println(stdout)
    back
  }
}
