package plob.b

import plob._
import java.nio.file.Path

object Compiler_GoogleClosureCompiler {
  // see http://blog.bolinfest.com/2009/11/calling-closure-compiler-from-java.html
  // TODO implement
  // TODO monitor outputDir, if deleted => rebuild all (how ?)
  def apply(outputDir : Path, options : Seq[String] = Nil) : builders.Builder = {
    //TODO init (eg create outputDir),
    //TODO generate annothed path as reporter of the compiler, provide a custom com.google.javascript.jscomp.ErrorManager
    //TODO support requirejs (config)

    import com.google.javascript.jscomp.CompilationLevel
    import com.google.javascript.jscomp.Compiler
    import com.google.javascript.jscomp.CompilerOptions
    import com.google.javascript.jscomp.JSSourceFile
    import com.google.javascript.jscomp.{CommandLineRunner, BasicErrorManager, CheckLevel, JSError}
    import java.nio.charset.Charset
    //import scala.collection.JavaConversions._
    import scala.collection.JavaConverters._
    return { apaths : builders.AnnotedPathS =>
      var back = apaths.toList
      //TODO parse options 
      //HACK create a CommandLineRunner to parse options
      class MyCommandLineRunner(args : Seq[String]) extends CommandLineRunner(args.toArray){
        override def createOptions() = {
          val options = super.createOptions() // protected => public
          //val customPasses = getCustomPasses(options)
          //customPasses.put(CustomPassExecutionTime.BEFORE_CHECKS, new CheckDoubleEquals(getCompiler()));
          options
        }
        override def createExterns() = super.createExterns()
        override def createCompiler() = super.createCompiler()
      }
      def toAnnotedPath(v: JSError) : AnnotedPath = {
        val l = v.getDefaultLevel match {
          case CheckLevel.ERROR => Level.Error
          case CheckLevel.WARNING => Level.Warning
          case CheckLevel.OFF => Level.Trace  
        }
        println(v.sourceName)
        AnnotedPath(Change.Modified, builders.toPath(Option(v.sourceName).getOrElse("")), Set(Marker("closure-compiler",  v.getType.key + ":" + v.description, l, Option(/*Position.Range(v.getNodeSourceOffset, v.getNodeLength)*/ Position.LC(v.lineNumber, -1)))))
      }
      val cli = new MyCommandLineRunner(options)
      val compiler = cli.createCompiler()
      compiler.setErrorManager(new BasicErrorManager(){
        def println(level : CheckLevel, error: JSError) {
    //      System.err.println("----- " +  level + " -- "+ error)
          back = toAnnotedPath(error) :: back
        }
        def printSummary() {}
      })
      val coptions = cli.createOptions()
      // Advanced mode is used here, but additional options could be set, too.
      //CompilationLevel.ADVANCED_OPTIMIZATIONS.setOptionsForCompilationLevel(coptions)
      // To get the complete set of externs, the logic in CompilerRunner.getDefaultExterns() should be used here.
      val externs = cli.createExterns()//CommandLineRunner.getDefaultExterns()
      //val externs = JSSourceFile.fromCode("externs.js", "function alert(x) {}")
      println("call", back)
      //val files = builders.toRelativeFilePaths(apaths, inputDir).map(f => JSSourceFile.fromFile(f, Charset.forName("utf-8"))).toList.asJava
      val files = apaths.map(ap => JSSourceFile.fromFile(ap.path.toFile, Charset.forName("utf-8"))).toList.asJava
      val dest = outputDir.toFile
      dest.mkdirs()
      //back = AnnotedPath(Change.Modified, inputDir, Set(Marker("closure-compiler", "running", Level.Debug))) :: back
      //val stdout: String = Process(cmdline, inputDir.toFile) !!;      
      //println(stdout)

      val result = compiler.compile(externs, files, coptions)
      /*
      back = result.warnings.toList.map{ i =>
      } ::: back
      back = result.errors.toList.map{ i =>
      AnnotedPath(Change.Modified, toPath(Option(i.sourceName).getOrElse("")), Set(Marker("closure-compiler",  i.getType.key + ":" + i.description, Level.Error, Option(/*Position.Range(i.getNodeSourceOffset, i.getNodeLength)*/ Position.LC(i.lineNumber, -1)))))
      } ::: back
      */
      //compiler.toSource()
      back
    }
  }
 //def apply(inputDir : Path, inputFilter : builders.Filter, outputDir : Path, options : Seq[String] = Nil) : builders.Builder = builders.route((inputFilter, exec(inputDir, outputDir, options)))

}

/**
* Checks for the presence of the == and != operators, as they are frowned upon
* according to Appendix B of JavaScript: The Good Parts.
*
* @author bolinfest@gmail.com (Michael Bolin)
*/
//object CheckDoubleEquals {
//  // Both of these DiagnosticTypes are disabled by default to demonstrate how
//  // they can be enabled via the command line.
//  /** Error to display when == is used. */
//  val NO_EQ_OPERATOR = DiagnosticType.disabled("JSC_NO_EQ_OPERATOR", "Use the === operator instead of the == operator.")
//
//  /** Error to display when != is used. */
//  val NO_NE_OPERATOR = DiagnosticType.disabled("JSC_NO_NE_OPERATOR", "Use the !== operator instead of the != operator.")
//}
//
//class CheckDoubleEquals(compiler : AbstractCompiler) extends CompilerPass {
//  override def process(externs : Node, root : Node) {
//    NodeTraversal.traverse(compiler, root, new FindDoubleEquals());
//  }
//
//  /**
//   * Traverses the AST looking for uses of == or !=. Upon finding one, it will
//   * report an error unless {@code @suppress {double-equals}} is present.
//   */
//  class FindDoubleEquals extends AbstractPostOrderCallback {
//    override def visit(t : NodeTraversal, n : Node, parent : Node) {
//      val typ = n.getType()
//      if (typ == Token.EQ || typ == Token.NE) {
//        val info = n.getJSDocInfo();
//        if (info != null && info.getSuppressions().contains("double-equals")) {
//          return;
//        }
//        val diagnosticType = (typ == Token.EQ) ? NO_EQ_OPERATOR : NO_NE_OPERATOR
//        val error = JSError.make(t.getSourceName(), n, diagnosticType)
//        compiler.report(error);
//      }
//    }
//  }
//}

