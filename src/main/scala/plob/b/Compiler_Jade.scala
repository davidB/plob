package plob.b

import plob._
import java.nio.file.Path

object Compiler_Jade {
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
 //def apply(inputDir : Path, inputFilter : builders.Filter, outputDir : Path, options : Seq[String] = Nil) : builders.Builder = builders.route((inputFilter, exec(inputDir, outputDir, options)))
}