package koolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{ New => _, _ }
import ByteCodes._
import utils._

object CodeGeneration extends Pipeline[Program, Unit] {

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      // TODO: Create code handler, save to files ...

      val classFile = new cafebabe.ClassFile(ct.id.value, if (ct.parent.isEmpty) None else Some(ct.parent.get.value))
      classFile.setSourceFile(sourceName)
      classFile.addDefaultConstructor

      //      for (v <- ct.vars) {
      //        PutField(ct.id.value, v.id.value, getTypeNotation(v.tpe.getType))
      //      }

      for (m <- ct.methods) {
        var loa = List[String]()
        m.args.foreach(a => loa = loa :+ getTypeNotation(a.tpe.getType))
        val mh = classFile.addMethod(getTypeNotation(m.retType.getType), m.id.value, loa)
        generateMethodCode(mh.codeHandler, m)
      }

      //      //DO SOME STUFF
      //      codeHandler <<
      //        GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;") <<
      //        Ldc("Hello world!") <<
      //        InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V") <<
      //        RETURN
      //      //STOP DOING STUFF

      classFile.writeToFile("./classfiles/" + ct.id.value + ".class")
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol
      //      var vars = List[Int]()
      //      mt.vars.foreach(v => vars = vars :+ ch.getFreshVar(2))
      //ch.
      mt.stats.foreach(s => compileStat(ch, s))
      compileExpr(ch, mt.retExpr)

      methSym.getType match {
        case TInt | TBoolean => ch << IRETURN
        case TString => ch << ARETURN
        case _ => ch << RETURN
      }

      //      mt.

      // TODO: Emit code

      ch.freeze
    }

    def generateMainMethodCode(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {
      // TODO: Emit code
      stmts.foreach(s => compileStat(ch, s))
      ch << RETURN
      ch.freeze
    }

    def compileStat(ch: cafebabe.CodeHandler, stat: StatTree): Unit = {
      stat match {
        /*case IntArrayType() =>
        case IntType() =>
        case BooleanType() =>
        case StringType() =>*/

        case Block(stats: List[StatTree]) => {
          stats.foreach(s => compileStat(ch, s))
        }
        case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) => {
          val nElse = ch.getFreshLabel("else")

          compileExpr(ch, expr);
          ch << IfEq(nElse) //if pop != 0 

          compileStat(ch, thn)

          if (!els.isEmpty) {
            val nAfter = ch.getFreshLabel("after")
            ch << Goto(nAfter)
            ch << Label(nElse)
            compileStat(ch, els.get)
            ch << Label(nAfter)
          } else ch << Label(nElse)

        }
        case While(expr: ExprTree, stat: StatTree) => {
          val loop = ch.getFreshLabel("loop")
          val break = ch.getFreshLabel("break")

          ch << Label(loop)
          compileExpr(ch, expr)

          ch << IfEq(break) //if pop != 0 
          compileStat(ch, stat)
          ch << Goto(loop)

          ch << Label(break)
        }
        case Println(expr: ExprTree) => {
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          compileExpr(ch, expr)
          ch << InvokeVirtual("java/io/PrintStream", "println", if (expr.getType == TString) "(Ljava/lang/String;)V" else "(I)V")
        }
        case Assign(id: Identifier, expr: ExprTree) => {
          //          compileExpr(ch, expr)

        }
        case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
      }
    }
    def compileExpr(ch: cafebabe.CodeHandler, expr: ExprTree): Unit = {
      println(expr)
      expr match {
        case And(lhs: ExprTree, rhs: ExprTree) => {
          ch << Ldc(0)
          val earlyFalse = ch.getFreshLabel("ef")
          compileExpr(ch, lhs)

          ch << IfEq(earlyFalse) << POP
          compileExpr(ch, rhs)

          ch << Label(earlyFalse)
        }
        case Or(lhs: ExprTree, rhs: ExprTree) => {
          ch << Ldc(1)
          val earlyTrue = ch.getFreshLabel("et")
          compileExpr(ch, lhs)

          ch << IfNe(earlyTrue) << POP
          compileExpr(ch, rhs)

          ch << Label(earlyTrue)

        }
        case Plus(lhs: ExprTree, rhs: ExprTree) => {

          expr.getType match {
            case TString => {
              ch << DefaultNew("java/lang/StringBuilder")

              compileExpr(ch, lhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", (if (lhs.getType == TString) "(Ljava/lang/String;)" else "(I)") + "Ljava/lang/StringBuilder;")

              compileExpr(ch, rhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", (if (rhs.getType == TString) "(Ljava/lang/String;)" else "(I)") + "Ljava/lang/StringBuilder;")

              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")

            }
            case TInt => {
              compileExpr(ch, lhs)
              compileExpr(ch, rhs)

              ch << IADD
            }
            case _ => sys.error("Internal error cg.1")
          }

        }
        case Minus(lhs: ExprTree, rhs: ExprTree) => {
          compileExpr(ch, lhs)
          compileExpr(ch, rhs)
          ch << ISUB
        }
        case Times(lhs: ExprTree, rhs: ExprTree) => {
          compileExpr(ch, lhs)
          compileExpr(ch, rhs)
          ch << IMUL
        }
        case Div(lhs: ExprTree, rhs: ExprTree) => {
          compileExpr(ch, lhs)
          compileExpr(ch, rhs)
          ch << IDIV
        }
        case LessThan(lhs: ExprTree, rhs: ExprTree) => {
          ch << Ldc(1)
          compileExpr(ch, lhs)
          compileExpr(ch, rhs)
          val fLabel = ch.getFreshLabel("lt")
          ch << If_ICmpLt(fLabel) << POP << Ldc(0) << Label(fLabel)
        }
        case Equals(lhs: ExprTree, rhs: ExprTree) => {
          val fLabel = ch.getFreshLabel("eq")
          ch << Ldc(0)
          compileExpr(ch, rhs)
          compileExpr(ch, lhs)
          ch << If_ICmpNe(fLabel) << POP << Ldc(1) << Label(fLabel)
        }
        case ArrayRead(arr: ExprTree, index: ExprTree) => //TODO ArrayRead
        case ArrayLength(arr: ExprTree) => //TODO ArrayLength
        case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) => {
          
          compileExpr(ch, obj)
          var sb = new StringBuilder
          sb.append("(")
          args.foreach(a => sb.append(getTypeNotation(a.getType)))
          sb.append(")")
          sb.append(getTypeNotation(meth.getSymbol.getType))

          //TODO check when args are implemented
          ch << InvokeVirtual(obj.getType.toString(), meth.value, sb.toString)
        }
        case IntLit(value: Int) => {
          ch << Ldc(value)
        }
        case StringLit(value: String) => {
          ch << Ldc(value)
        }

        case True() => {
          ch << Ldc(1)
        }
        case False() => {
          ch << Ldc(0)
        }
        case Identifier(value: String) => {
          val sym = expr.asInstanceOf[Identifier].getSymbol
        }
        case This() =>
        case NewIntArray(size: ExprTree) => {
          //compileExpr(ch, size)
          //ch << NewIntArray(ch.)
        }
        case New(tpe: Identifier) => {
          ch << DefaultNew(tpe.value)
        }
        case Not(expr: ExprTree) => {
          val ok = ch.getFreshLabel("ok")
          ch << Ldc(1)
          compileExpr(ch, expr)
          ch << IfEq(ok) << POP << Ldc(0)
          ch << Label(ok)
        }
      }
    }

    def getTypeNotation(tpe: Type): String = {
      tpe match {
        case TInt => "I"
        case TString => "Ljava/lang/String;"
        case TBoolean => "Z"
        case TIntArray => "[I"
        case TObject(classSymbol: ClassSymbol) => "Lpackage/" + tpe.toString()
        case TUntyped => "ERROR untyped"
        case TError => "ERROR TYPE"
      }
    }

    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.getName

    // output code
    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

    // Now do the main method
    // ...

    val mainObjectFile = new cafebabe.ClassFile(prog.main.id.value, None)
    mainObjectFile.setSourceFile("mycode.kool")
    mainObjectFile.addDefaultConstructor
    val codeHandler = mainObjectFile.addMainMethod.codeHandler
    generateMainMethodCode(codeHandler, prog.main.stats, "main")
    mainObjectFile.writeToFile("./classfiles/" + prog.main.id.value + ".class")
  }

}
