package plob.b

import plob._
import java.nio.file.Path

object Checker_Vows {

  def setup(version : Option[String] = None) {
    //TODO check node + npm are installed
    //TODO check vows is installed (npm test vows@version) else npm install vows@version
  }

  // TODO implement
  // TODO monitor outputDir, if deleted => rebuild all (how ?)
  def apply(inputDir : Path, options : Seq[String] = Nil) : builders.Builder = {
  //TODO init : check node is installed + npm install vows
    return { apaths : builders.AnnotedPathS =>
      import scala.sys.process.{Process, BasicIO}
      var back = apaths.toList
      println(inputDir, inputDir.toFile.getAbsolutePath)
      val files = builders.toRelativeFilePaths(apaths, inputDir).toList
      println(files)
      //val files = apaths.map(g)
      //val cmdline = "node" :: options.toList ::: files
      val cmdline = "vows" :: /*"--json"*/ "--dot-matrix" :: options.toList ::: files
      back = AnnotedPath(Change.Modified, inputDir, Set(Marker("vows", "cmdline :" + cmdline.mkString("'", "' '", "'"), Level.Debug))) :: back
      val logger = new ProcessLoggerAnnotedPath("vows", Change.Modified, inputDir)
      val p = Process(cmdline, inputDir.toFile)
     
      val exitValue = p !< logger
      back = logger.annotedPaths ::: back
      if (exitValue != 0) {
        back = AnnotedPath(Change.Modified, inputDir, Set(Marker("vows", "exit value : " + exitValue, Level.Error))) :: back
      }
      //TODO parse output to log into back 
      back
    }
  }

}