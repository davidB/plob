package plob.b

import plob._
import freemarker.template.Template

//class Compiler_Freemarker(val name : String = "freemarker") {
//
//  private def filterNotUpdated(e : Entity, stage : StageName) : Boolean = {
//    e.as[Change] match {
//      case None => false 
//      case Some(c0) => {
//        val outs = InputsOutputs.findOuts(e, stage)
//        outs.foldLeft(false){(acc, x) => acc || (x.as[Change].map{ c1 => c1.ts < c0.ts}).getOrElse(false)}
//      }
//    }
//  }
//
//  //TODO manage error
//  val stage : Stage = { run : Run =>
//    for {
//      entity <- run.entities
//      tmpl <- entity.as[Template]
//      if filterNotUpdated(entity, name)
//    } {
//      val dest = outputDir.resolve(inputDir.relativize(src))
//      val destChange = entity.asOrElse[Change](Change.FakeModified(run.ts))
//      (src.toFile.isDirectory, destChange) match {
//        case (_, _ : Change.Deleted) => Files.deleteIfExists(dest) //TODO delete recursively for Directory
//        case (true, _) => dest.toFile.mkdirs
//        case (_, _) => {
//          dest.toFile.getParentFile.mkdirs()
//          Files.copy(src, dest, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES, LinkOption.NOFOLLOW_LINKS)
//        }
//      }
//      InputsOutputs.addInOut(entity, name, new Entity().link(dest).link(destChange))
//    }
//    run
//  }
//
//}