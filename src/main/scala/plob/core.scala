package plob

/**
 * @author David Bernard
 */
//TODO add testcase
//TODO provide build definition loader from FS (like rc.d or init.d on linux, ...)
//TODO take inspiration from https://github.com/aztek/scala-idioms
//TODO take inspiration from http://gamadu.com/artemis/
import java.util.Calendar
import java.io.File
import java.nio.file.Files
  
object TimeStamp {
  def now() = System.currentTimeMillis()
}

//class TimeStamp(val v : Long) extends AnyVal

/**
 * a very simple (non optimized) implementation of Entity
 * TODO: for optimisation can take a look at artemis (Entity System)
 */
class Entity {
  var _components : List[Any] = Nil

  // only one instance of C is linkable to the entity
  def link[C](c : C) = {
    _components = c :: _components.filter{ x => c.getClass.isInstance(x) }
    this
  }
  def unlink[C](c : C) = {
    _components = _components.filter(_ != c)
    this
  }
  def as[C: Manifest] : Option[C] = _components.find{ x => manifest[C].erasure.isInstance(x)}.map{ _.asInstanceOf[C]}
  def asOrElse[C: Manifest](newC : => C) : C = as[C].getOrElse{
    val c = newC
    this.link(c)
    c
  }
}


case class Run(val entities : Traversable[Entity], val ts : TimeStamp, val simulate : Boolean = false) {

  def +(v : Run) = {
    if (v.ts == ts) {
      Some(new Run(entities ++ v.entities, ts, simulate))
    } else {
      None
    }
  }
  
  def partition(f : Filter) = {
    val (selected, unselected) = entities.partition(f)
    (new Run(selected, ts, simulate), new Run(unselected, ts, simulate))
  }
}


case class RPath(path : String, parent : Option[RPath] = None)

//------------------------------------------------------------------------------
  
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

class Markers {
  val l : Set[Marker] = Set.empty;
}

sealed trait Change {
  def ts : TimeStamp
}
object Change {
  case class Created(ts : TimeStamp) extends Change
  case class Modified(ts : TimeStamp) extends Change
  case class FakeModified(ts : TimeStamp) extends Change
  case class Deleted(ts : TimeStamp) extends Change
  //case class Test(ts : Long) extends Change
}

trait FileContent {
  def asFile : File
  def asCharSequence : CharSequence
}

//object FileContent{
//  // no cache of CharSequence
//  def fromFile(f : File) = new FileContent {
//    val asFile = f
//    def asCharSequence = Files.newBufferedReader
//  }
//}
  
//}
object InputsOutputs {
  import scala.collection.mutable
  
 
  
  protected[InputsOutputs] class InputsOf {
    val m = new mutable.HashMap[StageName, mutable.Set[Entity]]()
  }
  
  protected[InputsOutputs] class OutputsOf {
    val m = new mutable.HashMap[StageName, mutable.Set[Entity]]()
  }

  def addInOut(in : Entity, stage : StageName, out: Entity) = {
    findOuts(in, stage).add(out)
    findIns(stage, out).add(in)
  }

  def removeInOut(in : Entity, stage : StageName, out: Entity) = {
    val iof = in.asOrElse[InputsOf](new InputsOf())
    iof.m.get(stage).foreach{ x => x.remove(out) }
    val oof = out.asOrElse[OutputsOf](new OutputsOf())
    oof.m.get(stage).foreach{ x => x.remove(in) }
  }

  def findIns(stage : StageName, out: Entity) = {
    val oof = out.asOrElse[OutputsOf](new OutputsOf())
    oof.m.getOrElseUpdate(stage, new mutable.HashSet[Entity]())
  }

  def findOuts(in : Entity, stage : StageName, atLessOne : Boolean = false) = {
    val iof = in.asOrElse[InputsOf](new InputsOf())
    val b = iof.m.getOrElseUpdate(stage, new mutable.HashSet[Entity]())
    if (b.isEmpty && atLessOne) {
      b.add(new Entity())
    }
    b
  }

}
