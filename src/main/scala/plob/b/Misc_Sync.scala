package plob.b

import java.nio.file.Files
import java.nio.file.LinkOption
import java.nio.file.StandardCopyOption
import plob._

//class Misc_Sync(var outputDir : RPath, val name : String = "sync") extends StageCtrl{
//  ctrl.add("outputDir", "targetDirectory", () => outputDir, toBasicLogs((s :String) => outputDir = s))
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
//  val stage = (run : Run) => {
//    for {
//      entity <- run.entities
//      src <- entity.as[RPath]
//      if filterNotUpdated(entity, name)
//    } {
//      val dest = src.copy(parent = Option(outputDir))
//      val destChange = entity.asOrElse[Change](Change.FakeModified(run.ts))
//      (src.toFile.isDirectory, destChange) match {
//        case (_, _ : Change.Deleted) => Files.deleteIfExists(dest) //TODO delete recursively for Directory
//        case (true, _) => dest.toFile.mkdirs
//        case (_, _) => {
//          dest.toFile.getParentFile.mkdirs()
//          Files.copy(src, dest, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES, LinkOption.NOFOLLOW_LINKS)
//        }
//      }
//      InputsOutputs.findOuts(entity, name, true).foreach { _.link(dest).link(destChange) }
//    }
//    run
//  }
//
//}
