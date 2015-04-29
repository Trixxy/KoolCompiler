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

      for (v <- ct.vars) {
        PutField(ct.id.value, v.id.value, getTypeNotation(v.tpe.getType))
      }

      for (m <- ct.methods) {
        var loa = List[String]()
        m.args.foreach(a => loa = loa :+ getTypeNotation(a.tpe.getType))
        val mh = classFile.addMethod(getTypeNotation(m.retType.getType), m.id.value, loa)
        generateMethodCode(mh.codeHandler, m)
        mh.codeHandler.print
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
      var slotOf = Map[Int, Int]()
      var i = 1
      //      var vars = List[Int]()
      //      mt.vars.foreach(v => vars = vars :+ ch.getFreshVar(2))
      //ch.
      
      //Generate code for the arguments and variables
      mt.args.foreach(a => { slotOf += (a.getSymbol.id -> i); i += 1; })
      mt.vars.foreach(v => { slotOf += (v.getSymbol.id -> i); i += 1; })

      //Generate code for the method body
      mt.stats.foreach(s => compileStat(ch, s, slotOf))
      
      println("+++"+mt.getSymbol.classSymbol.name+"+++")
      
      //Generate code for the return statement
      compileExpr(ch, mt.retExpr, slotOf)
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
      stmts.foreach(s => compileStat(ch, s, null))
      ch << RETURN
      ch.freeze
    }

    def compileStat(ch: cafebabe.CodeHandler, stat: StatTree, slotOf: Map[Int, Int]): Unit = {
      println(stat)
      stat match {
        /*case IntArrayType() =>
        case IntType() =>
        case BooleanType() =>
        case StringType() =>*/

        case Block(stats: List[StatTree]) => {
          stats.foreach(s => compileStat(ch, s, slotOf))
        }
        case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) => {
          val nElse = ch.getFreshLabel("else")

          compileExpr(ch, expr, slotOf);
          ch << IfEq(nElse) //if pop != 0 

          compileStat(ch, thn, slotOf)

          if (!els.isEmpty) {
            val nAfter = ch.getFreshLabel("after")
            ch << Goto(nAfter)
            ch << Label(nElse)
            compileStat(ch, els.get, slotOf)
            ch << Label(nAfter)
          } else ch << Label(nElse)

        }
        case While(expr: ExprTree, stat: StatTree) => {
          val loop = ch.getFreshLabel("loop")
          val break = ch.getFreshLabel("break")

          ch << Label(loop)
          compileExpr(ch, expr, slotOf)

          ch << IfEq(break) //if pop != 0 
          compileStat(ch, stat, slotOf)
          ch << Goto(loop)

          ch << Label(break)
        }
        case Println(expr: ExprTree) => {
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          compileExpr(ch, expr, slotOf)
          ch << InvokeVirtual("java/io/PrintStream", "println", if (expr.getType == TString) "(Ljava/lang/String;)V" else "(I)V")
        }
        case Assign(id: Identifier, expr: ExprTree) => {
          compileExpr(ch, expr, slotOf)
          if (slotOf.get(id.getSymbol.id).isEmpty) {
            println("ASSIGN ----> " + expr)
            //TODO Problement är att vi inte kan få tag på class namnet utifrån det vi har nu, eller?
            println("GETFIELD("+expr.getType.toString()+", "+id.value+", "+getTypeNotation(expr.getType)+")")
            ch << GetField("B", id.value, getTypeNotation(expr.getType))
          } else {
            ch.getFreshVar(getTypeNotation(expr.getType))
            val slot = slotOf.get(id.getSymbol.id).get
            if (expr.getType == TInt || expr.getType == TBoolean) {
              ch << IStore(slot)
            } else ch << AStore(slot)
          }
        }
        case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
      }
    }
    def compileExpr(ch: cafebabe.CodeHandler, expr: ExprTree, slotOf: Map[Int, Int]): Unit = {
      println(expr)
      expr match {
        case And(lhs: ExprTree, rhs: ExprTree) => {
          ch << Ldc(0)
          val earlyFalse = ch.getFreshLabel("ef")
          compileExpr(ch, lhs, slotOf)

          ch << IfEq(earlyFalse) << POP
          compileExpr(ch, rhs, slotOf)

          ch << Label(earlyFalse)
        }
        case Or(lhs: ExprTree, rhs: ExprTree) => {
          ch << Ldc(1)
          val earlyTrue = ch.getFreshLabel("et")
          compileExpr(ch, lhs, slotOf)

          ch << IfNe(earlyTrue) << POP
          compileExpr(ch, rhs, slotOf)

          ch << Label(earlyTrue)

        }
        case Plus(lhs: ExprTree, rhs: ExprTree) => {

          expr.getType match {
            case TString => {
              ch << DefaultNew("java/lang/StringBuilder")

              compileExpr(ch, lhs, slotOf)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", (if (lhs.getType == TString) "(Ljava/lang/String;)" else "(I)") + "Ljava/lang/StringBuilder;")

              compileExpr(ch, rhs, slotOf)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", (if (rhs.getType == TString) "(Ljava/lang/String;)" else "(I)") + "Ljava/lang/StringBuilder;")

              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")

            }
            case TInt => {
              compileExpr(ch, lhs, slotOf)
              compileExpr(ch, rhs, slotOf)

              ch << IADD
            }
            case _ => sys.error("Internal error cg.1")
          }

        }
        case Minus(lhs: ExprTree, rhs: ExprTree) => {
          compileExpr(ch, lhs, slotOf)
          compileExpr(ch, rhs, slotOf)
          ch << ISUB
        }
        case Times(lhs: ExprTree, rhs: ExprTree) => {
          compileExpr(ch, lhs, slotOf)
          compileExpr(ch, rhs, slotOf)
          ch << IMUL
        }
        case Div(lhs: ExprTree, rhs: ExprTree) => {
          compileExpr(ch, lhs, slotOf)
          compileExpr(ch, rhs, slotOf)
          ch << IDIV
        }
        case LessThan(lhs: ExprTree, rhs: ExprTree) => {
          ch << Ldc(1)
          compileExpr(ch, lhs, slotOf)
          compileExpr(ch, rhs, slotOf)
          val fLabel = ch.getFreshLabel("lt")
          ch << If_ICmpLt(fLabel) << POP << Ldc(0) << Label(fLabel)
        }
        case Equals(lhs: ExprTree, rhs: ExprTree) => {
          val fLabel = ch.getFreshLabel("eq")
          ch << Ldc(0)
          compileExpr(ch, rhs, slotOf)
          compileExpr(ch, lhs, slotOf)
          ch << If_ICmpNe(fLabel) << POP << Ldc(1) << Label(fLabel)
        }
        case ArrayRead(arr: ExprTree, index: ExprTree) => //TODO ArrayRead
        case ArrayLength(arr: ExprTree) => //TODO ArrayLength
        case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) => {

          compileExpr(ch, obj, slotOf)
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
          if (slotOf.get(sym.id).isEmpty) {
            println("Identifier----> "+slotOf+" <- "+expr)
            println("GETFIELD("+expr.getType.toString()+", "+sym.name+", "+getTypeNotation(expr.getType)+")")
            ch << GetField("B", sym.name, getTypeNotation(expr.getType))
          } else {
            val slot = slotOf.get(sym.id).get
            if (sym.getType == TInt || sym.getType == TBoolean) ch << ILoad(slot)
            else ch << ALoad(slot)

          }
        }
        case This() => {
          ch << ALoad(0)
        }
        case NewIntArray(size: ExprTree) => {
          //compileExpr(ch, size, slotOf)
          //ch << NewIntArray(ch.)
        }
        case New(tpe: Identifier) => {
          ch << DefaultNew(tpe.value)
        }
        case Not(expr: ExprTree) => {
          val ok = ch.getFreshLabel("ok")
          ch << Ldc(1)
          compileExpr(ch, expr, slotOf)
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
