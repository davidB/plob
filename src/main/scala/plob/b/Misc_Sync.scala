package plob.b

import java.nio.file.Files
import java.nio.file.LinkOption
import java.nio.file.Path
import java.nio.file.StandardCopyOption
import plob.Change
import plob.builders
import plob.AnnotedPath

object Misc_Sync {

  def apply(inputDir : Path, inputFilter : builders.Filter, outputDir : Path) : builders.Builder = builders.route((inputFilter, apply(inputDir, outputDir)))
  
  def apply(inputDir : Path, outputDir : Path) : builders.Builder = { apaths : builders.AnnotedPathS =>
    val n = for (apath <- apaths) yield {
      val src = apath.path
      val dest = outputDir.resolve(inputDir.relativize(src))
      (src.toFile.isDirectory, apath.change) match {
        case (_, Change.Deleted) => Files.deleteIfExists(dest) //TODO delete recursively for Directory
        case (true, _) => dest.toFile.mkdirs
        case (_, _) => {
          dest.toFile.getParentFile.mkdirs()
          Files.copy(src, dest, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES, LinkOption.NOFOLLOW_LINKS)
        }
      }

      AnnotedPath(apath.change, dest)
    }
    n ++ apaths
  }

}

