package plob.b

import plob._

//class Reader_Article(var outputDir : RPath, val name : StageName = getClass.getSimpleName) extends StageCtrl {
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
//      src <- entity.as[FileContent]
//      if filterNotUpdated(entity, name)
//    } {
//      val dest = InputsOutputs.findOuts(entity, name, true).head
//      val destArticle = src.copy(parent = outputDir)
//      val destChange = dest.asOrElse[Change](Change.FakeModified(run.ts))
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
//}