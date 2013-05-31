//#!/bin/sh
//SCRIPT="$(cd "${0%/*}" 2>/dev/null; echo "$PWD"/"${0##*/}")"
//DIR=`dirname "${SCRIPT}"}`
//exec scala -save -cp "$DIR/tools/lib/plob-0.2.0-SNAPSHOT.jar" $0 $DIR $SCRIPT
//::!#

import plob._

object Main {
//  def main(args : Array[String]) {
//    main0(args.contains("--watch"))
//    println("END")
//  }
//  
//  def main0(isWatching : Boolean) {
//    var build = new b.Misc_Sync("target/webapp").stage
//    val input = new AnnotedPathGenerator("src", List("src/webapp"))
//    isWatching match {
//      case true  => input.watch(build, basicResultsConsolePrinter)
//      case false => input.runAllOnce(build, basicResultsConsolePrinter)
//    }
//  }

//  def main2(isWatching : Boolean) {
//    var build = pipe(
//      route(
//        (toPathFilter("glob:src/main/coffee/**.coffee") -> b.Compiler_CoffeeScript("src/main/coffee", "target/webapp/_scripts", List("--bare")))
//        , (toPathFilter("glob:src/main/jade/**.jade") -> b.Compiler_Jade("src/main/jade", "target/webapp"))
//        , (toPathFilter("glob:src/main/webapp/**") -> b.Misc_Sync("src/main/webapp", "target/webapp"))
//      )
//        //, JadeCompiler("src/main/jade", "glob:**/*.jade", "target/webapp"),
//        //, CoffeeScriptCompiler2("src/test/coffee", "glob:**/*.coffee", "target/webapp"),
//        //, VowsRunner("src/test/coffee", "glob:**/*.js")
//    )
//    val input = new AnnotedPathGenerator("src")
//    isWatching match {
//      case true  => input.watch(build, basicResultsConsolePrinter)
//      case false => input.runAllOnce(build, basicResultsConsolePrinter)
//    }
//  }
}