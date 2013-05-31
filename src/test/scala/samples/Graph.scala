package samples

import org.junit._
import Assert._
import plob.Entity
import java.nio.file.FileSystems
import java.nio.file.Path
import java.nio.file.PathMatcher
import scala.collection.mutable
import sun.nio.fs.Globs
import scala.util.matching.Regex
import java.nio.file.Paths

import scalaz._
//import scalaz.std.AllInstances._
import scalaz.Scalaz._
//import scalaz.syntax.std._
//import scalaz.std.option._
//import scalaz.std.list._
//import scalaz.syntax.traverse._
//import scalaz.Validation
//import scalaz.Validation._


//  http://gittup.org/tup/ex_dependencies.html
//  Example of Tupefile
//  
//  : |> echo "generated text" > %o |> generated.txt
//  : |> ./test.sh > %o |> output.txt
//  
//  : |> echo "generated text" > %o |> generated.txt
//  : |> echo "unused text" > %o |> unused.txt
//
//  : generated.txt unused.txt |> ./test.sh > %o |> output.txt
//  : hello.c |> gcc -Wall -c hello.c -o hello.o |> hello.o
//  : square.c |> gcc -Wall -c square.c -o square.o |> square.o
//  
//  : foreach *.c |> gcc -Wall -c %f -o %o |> %B.o
//  : *.o |> gcc %f -o %o |> hello

//TODO tool to detect useless rule (no matching ins)
//TODO make insPattern where args can be pattern of other args, eg to process pair of files "*.* ${name[0]}.md5"
//TODO create builders :
// * copy in to out (and delete)
// * filter for default if no other builder connected  
class BuildGraphTest {
//    @Test
//    def newGraph() = {
//      val builder0 = 0
//      val builder1 = 0
//      val data0 = new NodeData[String]("samples/f0.dat", "data0")
//      val data00 = new NodeData[String]("samples/f0.dat", "data00")
//      val data1 = new NodeData[String]("samples/f1.dat", "data1")
//      val data2 = new NodeData[String]("samples/f2.dat", "data2")
//      
//      val dataGenerator = NodeBuild(builder0, 0, Nil, List(data0, data1, data2), Status.OK)
//      val p0 = NodeBuild(builder1, 0, List(data0), List(data00))
//      println("id :"  + graph.addVertex(null).getId())
//      //graph.getVertex(0).setProperty(key, value)
//    }
    
//  @Test
//  def stringToFilters() {
//    val v0 = PathPattern.split("").map(PathPattern.toFilter)
//    assertEquals(0, v0.length)
//
//    val v1 = PathPattern.split("x.y aaaa.zz   b.aaa").map(PathPattern.toFilter)
//    assertEquals(3, v1.length)
//    //assertEquals(v1, List(List(List("x.y"), List("aaa.zz"), List("b.aaa"))))
//  }

  val b_foo = new BuilderFactory("foo"){
    def make(args: Iterable[String], entityP : EntityProvider) = new Builder(name, args) { 
      def run() {
        print("foo xxx")
      }
    }.success[String].toValidationNel
  }

  //TODO detect when 2 nodebuild generate (contains) same out
  @Test
  def makeRule0() {
    val entityP = new EntityProviderMemoryMap()
    val builders = new BuilderFactoryRegistry()
    val r0 = Rule(Nil, "foo", Nil, Nil)
    val r0_nb0 = nodeBuildMaker.make(r0, List(), builders, entityP)
    assertEquals("builderFactory not found : foo".failureNel, r0_nb0)

    builders.register(b_foo)
    val r0_nb1 = nodeBuildMaker.make(r0, List(), builders, entityP)
    assertEquals(List(NodeBuild(Nil, b_foo.make(Nil, entityP).getOrElse(null), Nil)).successNel, r0_nb1)
  }
  
  @Test
  def makeRule1() {
    val entityP = new EntityProviderMemoryMap()
    val builders = new BuilderFactoryRegistry()

    builders.register(b_foo)

    val rules = List(
      "*.foo |> foo --in $name --out $nameo |> $fname.bar",
      "*.aaa |> foo --in $name --out $nameo |> $fname.bbb",        
      "*.xyz |> foo --in $name --out $nameo |> $fname.ccc"        
    ).map(Rule.parse).collect { case Success(r) => r }
    assertEquals(3, rules.length)

    val entities = List(
      entityP.findOrCreate("f0.dat"),
      entityP.findOrCreate("f0.foo"),
      entityP.findOrCreate("f1.xxx")
    )

    val nodes0 = rules.map{rule => 
      nodeBuildMaker.make(rule, entities, builders, entityP)
    }
    assertEquals(3, nodes0.length)
    nodes0.collect { case Failure(x) => x }.foreach{ x => print("!!!" + x)}
    val nodes = nodes0.collect { case Success(x) => x }.flatten
    assertEquals(1, nodes.length)
    assertEquals(
      NodeBuild(
        List(entityP.findOrCreate("f0.foo")),
        b_foo.make(List("--in", "f0.foo", "--out", "f0.bar"), entityP).getOrElse(null),
        List(entityP.findOrCreate("f0.bar"))
      ),
      nodes(0)
    )
  }
  
  @Test
  def parseRule() {
    // doesn't detect wrong semantic (eg : undefined builder)
    // doesn't detect invalid arguments for builder
    // doesn't detect invalid identifier ($xxx)
    assertEquals(
      Rule(List("*.foo"), "foo", List("--in", "$name", "--out", "$nameo"), List("$fname.bar")).successNel,
      Rule.parse("*.foo |> foo --in $name --out $nameo |> $fname.bar")
    )

    // accept no inputs
    // doesn't detect wrong semantic (eg : $name is invalid when no inputs)
    assertEquals(
      Rule(List(), "foo", List("--in", "$name", "--out", "$nameo"), List("$fname.bar")).successNel,
      Rule.parse("|> foo --in $name --out $nameo |> $fname.bar")
    )

    // accept no outputs
    // doesn't detect wrong semantic (eg : $nameo is invalid when no outputs)
    assertEquals(
      Rule(List(), "foo", List("--in", "$name", "--out", "$nameo"), List()).successNel,
      Rule.parse("|> foo --in $name --out $nameo |>")
    )

    // failed if no builder name
    // doesn't detect wrong semantic (eg : $nameo is invalid when no outputs)
    assertEquals(
      "invalid syntax to make rule".failureNel,
      Rule.parse("|> --in $name --out $nameo |>")
    )
//    // TODO support quoted string (with space, ...)
//    //  : |> echo "generated text" > %o |> generated.txt
//    assertEquals(
//      Rule(List(), "echo", List("generated text", ">", "$nameo"), List("generated.txt")).successNel,
//      Rule.parse("""|> echo "generated text" > $nameo |> generated.txt""")
//    )

  }


  def toListOfEntity(l : List[List[List[Entity]]]) = l.map{_.map{_.map{_.as[Path].toString}}}

    // builder case :
    // * splitter (eg : post : file => markdown + yaml)
    // * combiner (eg : render : markdown + data(yaml) + template => html)
    // * simple (eg : checker, linter : file => log)
    // * side-effect (eg : indexer : file = (update database) => log)
}

//trait Status
//object Status {
//  case object OK extends Status
//  case object Pending extends Status
//  case object Invalid extends Status
//}

abstract class Builder(val name : String, val args: Iterable[String]) {
  def run()

  /**
   * override of equals and hashCode is crappy (for test only)
   */
  override def equals(o : Any) = {
    if (this.getClass() == o.getClass) {
      val o2 = o.asInstanceOf[Builder]
      this.name == o2.name && this.args == o2.args
    } else {
      false
    }
  } 
  override def hashCode() = (name.hashCode() << 13) + args.hashCode()
  override def toString() = "[" + name + " " + args.mkString("'", "' '", "'") +"]-" + super.hashCode()
}

abstract class BuilderFactory(val name: String) {
  def make(args: Iterable[String], entityP : EntityProvider) : ValidationNel[String, Builder]
}

object PathPattern {
  type Filter = (Iterable[Path]) => List[List[Path]]

  def constant(e :Path) : Filter = (_) => List(List(e))

  def each(matcher : PathMatcher) : Filter = {
    (es : Iterable[Path]) => {
      es.filter{ x2 => matcher.matches(x2) }
        .map{x => List(x)}
        .toList
    }
  }

  def all(matcher : PathMatcher) : Filter = {
    (es : Iterable[Path]) => {
      List(
        es.filter{ x => matcher.matches(x) }
          .toList
      )
    }
  }

  def toPathMatcher(s : String) = {
    val s0 = if (s.startsWith("glob:") || s.startsWith("regexp:"))  s else "glob:" + s
    FileSystems.getDefault.getPathMatcher(s0)
  }

  //TODO optim support constant Entity
  def toFilter(s : String) = {
    if (s.startsWith("all:")) {
      all(toPathMatcher(s.substring("all:".length)))
    } else if (s.startsWith("glob:") || s.contains("*")){
      each(toPathMatcher(s))
    } else {
      constant(FileSystems.getDefault.getPath(s))
    }
  }
  

}

object nodeBuildMaker {

  def make(rule : Rule, datas : Iterable[Entity], builders : BuilderFactoryRegistry, entityP : EntityProvider) : ValidationNel[String, Iterable[NodeBuild]] = {
    builders.find(rule.builderAlias).toValidationNel[String, BuilderFactory].flatMap{ builderF => 
      var paths = datas.map{x => x.as[Path]}.collect{case Some(x) => x}
      var ns : Iterable[ValidationNel[String, NodeBuild]]= makeIns(rule.insPattern, paths).map{ insv =>
        for {
          ins <- insv
          outs <- makeOuts(rule.outsPattern, ins)
          args <- makeArgs(rule.argsPattern, ins, outs)
          builder <- builderF.make(args, entityP)
        } yield {
          NodeBuild(
            ins.flatten.toList.map{x => entityP.findOrCreate(x.toString)},
            builder,
            outs.map{ x =>  entityP.findOrCreate(x.toString)}
          )
        }
      }
      ns.toList.sequence[({type X[a] = ValidationNel[String,a]})#X, NodeBuild]
    }
  }
  
  def makeIns(insPattern : Iterable[String], datas : Iterable[Path]) : Iterable[ValidationNel[String, List[List[Path]]]]= {
    val ins = if (insPattern.isEmpty) {
      List(List(List[Path]()))
    } else {
      insPattern.map{PathPattern.toFilter}.map{x => x(datas) }
    }
    crossProduct(ins.toList).map{_.success[String].toValidationNel}
  }
  
  def makeOuts(outsPattern: Iterable[String], ins : List[List[Path]]) : ValidationNel[String, Iterable[Path]] = (
    outsPattern
    .map{x => interpolate(x, ins, None)}
    .toList.sequence[({type X[a] = ValidationNel[String,a]})#X, String]
    .map( _.map{x => Paths.get(x)})
  )


  def makeArgs(argsPattern: Iterable[String], ins : List[List[Path]], outs: Iterable[Path]) : ValidationNel[String, Iterable[String]] = (
    argsPattern
    .map{x => interpolate(x, ins, Option(outs.toSeq))}
    .toList.sequence[({type X[a] = ValidationNel[String,a]})#X, String]
  )

  //TODO return error on unknown entry
  def interpolate(s: String, ins: List[List[Path]], outs : Option[Seq[Path]] = None) : ValidationNel[String, String] = {
    import java.io.File
    def base(s : String) = s.lastIndexOf(".") match {
      case -1 => s
      case p => s.substring(0, p)
    }
    def ext(s : String) = s.lastIndexOf(".") match {
      case -1 => ""
      case p => s.substring(p+1)
    }
    //TODO use a more functional way to define success or failure
    val r = new Regex("""\$(\w+)(\d*)""")
    var invalidRefs = Set[String]()
    val n = r.replaceAllIn(s, (m) => {
      val idx = if (!m.group(2).isEmpty) java.lang.Integer.parseInt(m.group(2), 10) else 0
      m.group(1) match {
        case "names_w" => ins(idx).map(_.getFileName.toString()).mkString(" ")
        case "names_p" => ins(idx).map(_.getFileName.toString()).mkString(File.pathSeparator)
        case "name" => ins(idx).head.getFileName.toString()
        case "fname" => base(ins(idx).head.getFileName.toString())
        case "ext" => ext(ins(idx).head.getFileName.toString())
        case "dname" => ins(idx).head.getParent.getFileName.toString()
        case "paths_w" => ins(idx).mkString(" ")
        case "paths_p" => ins(idx).mkString(File.pathSeparator)
        case "path" => ins(idx).head.toString()
        case "fpath" => base(ins(idx).head.toString())
        case "dpath" => base(ins(idx).head.getParent.toString())
        case "nameos_w" if outs.isDefined => outs.get.map(_.getFileName.toString()).mkString(" ")
        case "nameos_p" if outs.isDefined => outs.get.map(_.getFileName.toString()).mkString(File.pathSeparator)
        case "nameo" if outs.isDefined => outs.get.apply(idx).getFileName.toString()
        case "fnameo" if outs.isDefined => base(outs.get.apply(idx).getFileName.toString())
        case "exto" if outs.isDefined => ext(outs.get.apply(idx).getFileName.toString())
        case "dnameo" if outs.isDefined => outs.get.apply(idx).getParent.getFileName.toString()
        case "pathos_w" if outs.isDefined => outs.get.mkString(" ")
        case "pathos_p" if outs.isDefined => outs.get.mkString(File.pathSeparator)
        case "patho" if outs.isDefined => outs.get.apply(idx).toString()
        case "fpatho" if outs.isDefined => base(outs.get.apply(idx).toString())
        case "dpatho" if outs.isDefined => base(outs.get.apply(idx).getParent.toString())
        case _ => invalidRefs += m.group(0); "$" + m.group(0)
      }
    })
    (invalidRefs
      .map{ x => ("invalid param " + x).failure[String].toValidationNel}
      .foldLeft(n.success[String].toValidationNel){_ |+| _}
    )
  }
  
  /**
   * inspiration from http://stackoverflow.com/questions/13567543/cross-product-of-arbitrary-number-of-lists-in-scala
   */ 
  def crossProduct[V](vs : List[List[V]]) = {
//    def fun(xs: List[V], zss: List[List[V]]): List[List[V]] = for {
//      x <- xs
//      zs <- zss
//    } yield x :: zs
//    vs.foldRight(List(List[V]()))(fun _)
    vs.sequence
  }
}



trait EntityProvider {
  def findOrCreate(loc : String) : Entity = new Entity()
}

trait EntityProviderRecorder extends EntityProvider {
  val requested = new mutable.ListBuffer[Entity]()
  override def findOrCreate(loc : String) : Entity = {
    val b = super.findOrCreate(loc)
    requested += b
    b
  }
}

class EntityProviderMemoryMap extends EntityProvider {
  val _entities = new mutable.HashMap[String, Entity]()
  override def findOrCreate(loc : String) : Entity = _entities.getOrElseUpdate(loc, new Entity().link(Paths.get(loc)))
}

class BuilderFactoryRegistry {
  var _builders = Map[String, BuilderFactory]()

  def builders = _builders.toSeq

  def register(builder : BuilderFactory) = {
    if (_builders.contains(builder.name)) {
      Failure(s"builer.name already registered : ${builder.name}")
    } else {
      _builders = _builders + (builder.name -> builder)
      Success(builder)
    }
  }

  def find(name : String) = _builders.get(name).toSuccess(s"builderFactory not found : $name")

  def unregister(name : String) {
    _builders = _builders - name
  }

}
object Rule{
  //TODO use a parser (scala parser combinator or parboiled) to provide better parsing
  //* accept string between quote
  //* check that $id are valid
  //* return better error message than "invalid"
  private val ruleP1 = """^([^\|]*)\|>\s*(\w+)([^\|]*)\|>(.*)$""".r
  def parse(s : String) = {
    def split(s : String) = (
      (if (s == null) "" else s)
      .split(' ')
      .map{_.trim()}
      .filter{!_.isEmpty()}
      .toList
    )
    ruleP1.findFirstMatchIn(s) match {
      case Some(m) => {
        Rule(split(m.group(1)), m.group(2).trim(), split(m.group(3)), split(m.group(4))).successNel[String]
      }
      case None => "invalid syntax to make rule".failureNel[Rule]
    }
  }
  
//  def parse(s : String) = BuilderRuleParser.apply(s)
}
case class Rule(insPattern : Iterable[String], builderAlias: String, argsPattern: Iterable[String], outsPattern : Iterable[String])

//TODO Cfg should be a Path dependent type : cfg.type = builder.type.Cfg
case class NodeBuild(ins : Iterable[Entity], builder: Builder, outs : Iterable[Entity])

/*
import org.parboiled.scala._
import org.parboiled.errors.{ErrorUtils, ParsingException}
import org.parboiled.Context
import java.lang.StringBuilder

/**
 * This Rule parser is experimental
 */
object BuilderRuleParser extends Parser {
  lazy val BRule = rule { WhiteSpace ~ BRuleObject ~ EOI }

  def BRuleObject: Rule1[Rule] = rule {
    InsPattern ~ "|>" ~ BuilderName ~ ArgsPattern ~ "|>" ~ OutsPattern ~ EOI ~~>
    ((a : Iterable[String], b: String, c: Iterable[String], d: Iterable[String]) => Rule(a, b, c, d))}

  def InsPattern = rule { zeroOrMore(String) }

  def BuilderName = rule { StringUnquoted }
  
  def ArgsPattern = rule { zeroOrMore(String) }

  def OutsPattern = rule { zeroOrMore(String) }

  def String = rule { StringQuoted | StringUnquoted }

  def StringQuoted = rule { "\"" ~ Characters ~ "\" " ~~> (_.toString) }

  def StringUnquoted = rule { push(new StringBuilder) ~ zeroOrMore(StringFragChar) ~~> (_.toString) }

  def Characters = rule { push(new StringBuilder) ~ zeroOrMore("\\" ~ EscapedChar | NormalChar) }
  
  def EscapedChar = rule (
       anyOf("\"\\/") ~:% withContext(appendToSb(_)(_)) 
    | "b" ~ appendToSb('\b')
    | "f" ~ appendToSb('\f')
    | "n" ~ appendToSb('\n')
    | "r" ~ appendToSb('\r')
    | "t" ~ appendToSb('\t')
    | Unicode ~~% withContext((code, ctx) => appendToSb(code.asInstanceOf[Char])(ctx)) 
  )

  def NormalChar = rule { !anyOf("\"\\") ~ ANY ~:% (withContext(appendToSb(_)(_))) }
  
  def StringFragChar = rule { !anyOf(" \n\r\t\f\"\\") ~:% (withContext(appendToSb(_)(_))) }

  def Unicode = rule { "u" ~ group(HexDigit ~ HexDigit ~ HexDigit ~ HexDigit) ~> (java.lang.Integer.parseInt(_, 16)) }

  def HexDigit = rule { "0" - "9" | "a" - "f" | "A" - "Z" }
  
  def WhiteSpace: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }
    
  // helper method for fast string building
  // for maximum performance we use a somewhat unorthodox parsing technique that is a bit more verbose (and somewhat
  // less readable) but reduces object allocations during the parsing run to a minimum:
  // the Characters rules pushes a StringBuilder object onto the stack which is then directly fed with matched
  // and unescaped characters in the sub rules (i.e. no string allocations and value stack operation required)
  def appendToSb(c: Char): Context[Any] => Unit = { ctx =>
    ctx.getValueStack.peek.asInstanceOf[StringBuilder].append(c)
    ()
  }

  /**
   * We redefine the default string-to-rule conversion to also match trailing whitespace if the string ends with
   * a blank, this keeps the rules free from most whitespace matching clutter
   */
  override implicit def toRule(string: String) = {
    if (string.endsWith(" ")) str(string.trim) ~ WhiteSpace
    else str(string)
  }

  /**
   * The main parsing method. Uses a ReportingParseRunner (which only reports the first error) for simplicity.
   */
  def apply(s: String): ValidationNel[String, Rule] = apply(s.toCharArray)
  
  /**
   * The main parsing method. Uses a ReportingParseRunner (which only reports the first error) for simplicity.
   */
  def apply(s: Array[Char]): ValidationNel[String, Rule] = {
    val parsingResult = ReportingParseRunner(BRule).run(s)
    parsingResult.result match {
      case Some(r) => r.successNel[String]
      case None =>  ("Invalid Rule syntax:" + ErrorUtils.printParseErrors(parsingResult)).failureNel[Rule] 
    }
  }

}
*/