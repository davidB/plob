package plob.b

import plob._
import java.nio.file.Path

object Compiler_CoffeeScript {
  
  // TODO implement
  // TODO monitor outputDir, if deleted => rebuild all (how ?)
  def apply(inputDir : Path, outputDir : Path, options : Seq[String] = Nil) : builders.Builder = {
    //TODO init (eg create outputDir),
    //TODO check context (eg coffee executable existe, else warning and fallback to rhino + ...)
    //TODO return a Validation or a Logger ??
    //TODO manage Deleted 
    //HACK group files by target directory, because coffee does not create the outputDir/sub for intput of type list of files (sub/file.coffee)
    
    

    return { apaths : builders.AnnotedPathS =>
      import scala.sys.process.Process
      var back = apaths.toList
      val iofiles = (builders.toRelativeFilePaths(apaths, inputDir)
        .toList
        .map{ x => (x, outputDir.resolve(x.replace(".coffee", ".js").replace(".cs", ".js")))}
        .groupBy{ x => x._2.getParent }
      )
      val result = iofiles.map { case (outdir, files) =>
        outdir.toFile.mkdirs()
        files.foreach{ _._2.toFile.delete()}
        val cmdline = "coffee" :: "-o" :: outdir.toAbsolutePath.toString :: options.toList ::: files.map(_._1)
        //val stdout: String = Process(cmdline, inputDir.toFile) !!;
        //println(stdout)
        //TODO parse stdout to generate result
        val logger = new ProcessLoggerAnnotedPath("coffee", Change.Modified, inputDir)
        val p = Process(cmdline, inputDir.toFile)
     
        val exitValue = p !< logger
        back = logger.annotedPaths ::: back
        if (exitValue != 0) {
          back = AnnotedPath(Change.Modified, inputDir, Set(Marker("vows", "exit value : " + exitValue, Level.Error))) :: back
        }
        val changes = files.map{ x => x._2.toFile.exists() match {
         case true => AnnotedPath(Change.Modified, x._2, Set())
         case false => AnnotedPath(Change.Deleted, x._2, Set())
        }}
        AnnotedPath(Change.Modified, inputDir, Set(Marker("coffee", "cmdline :" + cmdline.mkString("'", "' '", "'"), Level.Debug))) :: changes
      }
      result.flatten ++ back
    }
  }
 //def apply(inputDir : Path, inputFilter : builders.Filter, outputDir : Path, options : Seq[String] = Nil) : builders.Builder = builders.route((inputFilter, exec(inputDir, outputDir, options)))

}