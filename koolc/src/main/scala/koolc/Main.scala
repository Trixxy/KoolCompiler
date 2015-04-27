package koolc

import utils._
import java.io.File

import lexer._
import ast._
import analyzer._
import code._

object Main {

  def processOptions(args: Array[String]): Context = {

    val reporter = new Reporter()
    var outDir: Option[File] = None
    var files: List[File] = Nil

    def processOption(args: List[String]): Unit = args match {
      case "-d" :: out :: args =>
        outDir = Some(new File(out))
        processOption(args)

      case f :: args =>
        files = new File(f) :: files
        processOption(args)

      case Nil =>
    }

    processOption(args.toList)

    if (files.size != 1) {
      reporter.fatal("Exactly one file expected, " + files.size + " file(s) given.")
    }

    Context(reporter = reporter, file = files.head, outDir = outDir)
  }

  def main(args: Array[String]) {
    val ctx = processOptions(args)

    val pipelineFrontend = Lexer andThen
      Parser andThen
      NameAnalysis andThen
      TypeChecking

    if ( /* TODO: test if tokens or AST should be printed */ false) {
      val program = pipelineFrontend.run(ctx)(ctx.file)
      println(Printer(program))
    } else { // generate code
      val pipeline = pipelineFrontend andThen CodeGeneration
      pipeline.run(ctx)(ctx.file)
    }
  }

  //IF WE ANT TO TEST THE PARSER
  /*val ctx = processOptions(args)

    val pipeline = Lexer andThen Parser

    val program = pipeline.run(ctx)(ctx.file)
    
    println(Printer(program))*/

  //IF WE WANT TO TEST THE LEXER
  /*val ctx = processOptions(args)

    val pipeline = Lexer andThen PrintTokens

    val program = pipeline.run(ctx)(ctx.file)

    for(t <- program) {
      t
      println()
    }*/
}
