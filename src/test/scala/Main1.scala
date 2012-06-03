//#!/bin/sh
//SCRIPT="$(cd "${0%/*}" 2>/dev/null; echo "$PWD"/"${0##*/}")"
//DIR=`dirname "${SCRIPT}"}`
//exec scala -save -cp "$DIR/tools/lib/plob-0.0.1-SNAPSHOT.jar" $0 $DIR $SCRIPT
//::!#

import plob._
import plob.builders._

object Main {
  def main(args : Array[String]) {
    main2(args.contains("--watch"))
    println("END")
  }

  def main2(isWatching : Boolean) {
    var build = builders.pipe(
      builders.route(
        ("glob:src/main/coffee/**.coffee", Compiler_CoffeeScript("src/main/coffee", "target/webapp/_scripts", List("--bare")))
        , ("glob:src/main/jade/**.jade", Compiler_Jade("src/main/jade", "target/webapp"))
        , ("glob:src/main/webapp/**", Misc_Sync("src/main/webapp", "target/webapp"))
      )
        //, JadeCompiler("src/main/jade", "glob:**/*.jade", "target/webapp"),
        //, CoffeeScriptCompiler2("src/test/coffee", "glob:**/*.coffee", "target/webapp"),
        //, VowsRunner("src/test/coffee", "glob:**/*.js")
    )
    val input = new AnnotedPathGenerator("src")
    isWatching match {
      case true  => input.watch(build, basicResultsConsolePrinter)
      case false => input.runAllOnce(build, basicResultsConsolePrinter)
    }
  }
}