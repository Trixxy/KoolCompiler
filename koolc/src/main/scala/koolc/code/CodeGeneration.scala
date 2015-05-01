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
      val classFile = new cafebabe.ClassFile(ct.id.value, if (ct.parent.isEmpty) None else Some(ct.parent.get.value))
      classFile.setSourceFile(sourceName)
      val ch = classFile.addDefaultConstructor.codeHandler

      for (v <- ct.vars) {
        classFile.addField(getTypeNotation(v.getSymbol.getType), v.id.value)
      }

      for (m <- ct.methods) {
        var loa = List[String]()
        m.args.foreach(a => loa = loa :+ getTypeNotation(a.id.getType))
        //        println("\nMETHOD ENTER: " + m.id.value + " -> " + loa)
        val mh = classFile.addMethod(getTypeNotation(m.retType.getType), m.id.value, loa)
        generateMethodCode(mh.codeHandler, m)
      }

      classFile.writeToFile("./classfiles/" + ct.id.value + ".class")
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol
      val className = methSym.classSymbol.name
      var slotOf = Map[Int, Int]()
      var i = 1

      //Generate code for the arguments and variables
      mt.args.foreach(a => { slotOf += (a.getSymbol.id -> i); i += 1; })
      mt.vars.foreach(v => { slotOf += (v.getSymbol.id -> i); i += 1; ch.getFreshVar(getTypeNotation(v.id.getType)) })

      //Generate code for the method body
      mt.stats.foreach(s => compileStat(ch, s, slotOf, mt))

      //Generate code for the return statement
      compileExpr(ch, mt.retExpr, slotOf, mt, null)

      methSym.getType match {
        case TInt | TBoolean => ch << IRETURN
        case TString | TIntArray => ch << ARETURN
        case _ => (if (methSym.getType.isInstanceOf[TObject]) { ch << ARETURN } else ch << RETURN)
      }

      ch.freeze
    }

    def generateMainMethodCode(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {
      stmts.foreach(s => compileStat(ch, s, null, null))
      ch << RETURN
      ch.freeze
    }

    def compileStat(ch: cafebabe.CodeHandler, stat: StatTree, slotOf: Map[Int, Int], mt: MethodDecl): Unit = {
      val className = if (mt == null) null else mt.getSymbol.classSymbol.name
      
      stat match {
        /*case IntArrayType() =>
        case IntType() =>
        case BooleanType() =>
        case StringType() =>*/

        case Block(stats: List[StatTree]) => {
          stats.foreach(s => compileStat(ch, s, slotOf, mt))
        }
        case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) => {
          val nElse = ch.getFreshLabel("else")

          compileExpr(ch, expr, slotOf, mt, stat);
          ch << IfEq(nElse) //if pop != 0 

          compileStat(ch, thn, slotOf, mt)

          if (!els.isEmpty) {
            val nAfter = ch.getFreshLabel("after")
            ch << Goto(nAfter)
            ch << Label(nElse)
            compileStat(ch, els.get, slotOf, mt)
            ch << Label(nAfter)
          } else ch << Label(nElse)

        }
        case While(expr: ExprTree, stat: StatTree) => {
          val loop = ch.getFreshLabel("loop")
          val break = ch.getFreshLabel("break")

          ch << Label(loop)
          compileExpr(ch, expr, slotOf, mt, stat)

          ch << IfEq(break) //if pop != 0 
          compileStat(ch, stat, slotOf, mt)
          ch << Goto(loop)

          ch << Label(break)
        }
        case Println(expr: ExprTree) => {
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          compileExpr(ch, expr, slotOf, mt, stat)
          val oneLiner = if (expr.getType == TString) "(Ljava/lang/String;)V" else if (expr.getType == TBoolean) "(Z)V" else "(I)V"
          ch << InvokeVirtual("java/io/PrintStream", "println", oneLiner)
        }
        case Assign(id: Identifier, expr: ExprTree) => {
          if (slotOf.get(id.getSymbol.id).isEmpty) {
            ch << ALoad(0)
            compileExpr(ch, expr, slotOf, mt, stat)

            //            println("PutField(" + className + ", " + id.value + ", " + getTypeNotation(expr.getType) + ")")
            ch << PutField(className, id.value, getTypeNotation(expr.getType))
          } else {
            compileExpr(ch, expr, slotOf, mt, stat)

            val slot = slotOf.get(id.getSymbol.id).get
            if (expr.getType == TInt || expr.getType == TBoolean) {
              ch << IStore(slot)
            } else ch << AStore(slot)
          }
        }
        case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) => {
          //          println("IASTORE: " + id + "; " + index + "; " + expr)
          compileExpr(ch, id, slotOf, mt, stat)
          compileExpr(ch, index, slotOf, mt, stat)
          compileExpr(ch, expr, slotOf, mt, stat)
          ch << IASTORE
        }
      }
    }
    def compileExpr(ch: cafebabe.CodeHandler, expr: ExprTree, slotOf: Map[Int, Int], mt: MethodDecl, stat : StatTree): Unit = {
      val className = if (mt == null) null else mt.getSymbol.classSymbol.name
      //      println(expr)
      expr match {
        case And(lhs: ExprTree, rhs: ExprTree) => {
          ch << Ldc(0)
          val earlyFalse = ch.getFreshLabel("ef")
          compileExpr(ch, lhs, slotOf, mt, stat)
          ch << LineNumber(expr.line)
          ch << IfEq(earlyFalse) << POP
          compileExpr(ch, rhs, slotOf, mt, stat)

          ch << Label(earlyFalse)
        }
        case Or(lhs: ExprTree, rhs: ExprTree) => {
          ch << Ldc(1)
          val earlyTrue = ch.getFreshLabel("et")
          compileExpr(ch, lhs, slotOf, mt, stat)

          ch << LineNumber(expr.line)

          ch << IfNe(earlyTrue) << POP
          compileExpr(ch, rhs, slotOf, mt, stat)

          ch << Label(earlyTrue)

        }
        case Plus(lhs: ExprTree, rhs: ExprTree) => {
          expr.getType match {
            case TString => {
              ch << DefaultNew("java/lang/StringBuilder")

              compileExpr(ch, lhs, slotOf, mt, stat)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", (if (lhs.getType == TString) "(Ljava/lang/String;)" else "(I)") + "Ljava/lang/StringBuilder;")

              compileExpr(ch, rhs, slotOf, mt, stat)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", (if (rhs.getType == TString) "(Ljava/lang/String;)" else "(I)") + "Ljava/lang/StringBuilder;")

              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")

              ch << LineNumber(expr.line)
            }
            case TInt => {
              ch << LineNumber(expr.line)
              compileExpr(ch, lhs, slotOf, mt, stat)
              compileExpr(ch, rhs, slotOf, mt, stat)

              ch << IADD
            }
            case _ => sys.error("Internal error cg.1")
          }
        }

        case Minus(lhs: ExprTree, rhs: ExprTree) => {
          ch << LineNumber(expr.line)
          compileExpr(ch, lhs, slotOf, mt, stat)
          compileExpr(ch, rhs, slotOf, mt, stat)
          ch << ISUB
        }
        case Times(lhs: ExprTree, rhs: ExprTree) => {
          ch << LineNumber(expr.line)
          compileExpr(ch, lhs, slotOf, mt, stat)
          compileExpr(ch, rhs, slotOf, mt, stat)
          ch << IMUL
        }
        case Div(lhs: ExprTree, rhs: ExprTree) => {
          ch << LineNumber(expr.line)
          compileExpr(ch, lhs, slotOf, mt, stat)
          compileExpr(ch, rhs, slotOf, mt, stat)
          ch << IDIV
        }
        case LessThan(lhs: ExprTree, rhs: ExprTree) => {
          ch << Ldc(1)
          ch << LineNumber(expr.line)
          compileExpr(ch, lhs, slotOf, mt, stat)
          compileExpr(ch, rhs, slotOf, mt, stat)
          val fLabel = ch.getFreshLabel("lt")
          ch << If_ICmpLt(fLabel) << POP << Ldc(0) << Label(fLabel)
        }
        case Equals(lhs: ExprTree, rhs: ExprTree) => {
          val fLabel = ch.getFreshLabel("eq")
          ch << Ldc(1)

          compileExpr(ch, lhs, slotOf, mt, stat)

          ch << LineNumber(expr.line)

          compileExpr(ch, rhs, slotOf, mt, stat)

          if (lhs.getType.isInstanceOf[TObject] || lhs.getType == TString || lhs.getType == TIntArray)
            ch << If_ACmpEq(fLabel) << POP << Ldc(0) << Label(fLabel)
          else
            ch << If_ICmpEq(fLabel) << POP << Ldc(0) << Label(fLabel)

        }
        case ArrayRead(arr: ExprTree, index: ExprTree) => {
          ch << LineNumber(expr.line)
          compileExpr(ch, arr, slotOf, mt, stat)
          compileExpr(ch, index, slotOf, mt, stat)

          ch << IALOAD
        }
        case ArrayLength(arr: ExprTree) => {
          ch << LineNumber(expr.line)
          compileExpr(ch, arr, slotOf, mt, stat)
          ch << ARRAYLENGTH
        }
        case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) => {
          ch << LineNumber(expr.line)
          compileExpr(ch, obj, slotOf, mt, stat)

          var sb = new StringBuilder
          sb.append("(")

          meth.getSymbol.asInstanceOf[MethodSymbol].argList.foreach(a => sb.append(getTypeNotation(a.getType)))
          args.foreach(a => compileExpr(ch, a, slotOf, mt, stat))

          sb.append(")")
          sb.append(getTypeNotation(meth.getSymbol.getType))

          val polly = {
            if (meth.getSymbol.asInstanceOf[MethodSymbol].classSymbol.name != obj.getType.toString())
              meth.getSymbol.asInstanceOf[MethodSymbol].classSymbol.name
            else
              obj.getType.toString()
          }
          //        	println("InvokeVirtual(" + obj.getType.toString() + ", " + meth.value + ", " + sb.toString + ")")

          //          if (meth.getSymbol.isInstanceOf[MethodSymbol]){
          //          
          //          }
          ch << LineNumber(expr.line)
          ch << InvokeVirtual(polly, meth.value, sb.toString)
        }
        case IntLit(value: Int) => {
          ch << LineNumber(expr.line)
          ch << Ldc(value)
        }
        case StringLit(value: String) => {
          ch << LineNumber(expr.line)
          ch << Ldc(value)
        }

        case True() => {
          ch << LineNumber(expr.line)
          ch << Ldc(1)
        }
        case False() => {
          ch << LineNumber(expr.line)
          ch << Ldc(0)
        }
        case Identifier(value: String) => {
          val sym = expr.asInstanceOf[Identifier].getSymbol
          
          if (!(stat.isInstanceOf[ArrayAssign]))  
            ch << LineNumber(expr.line)

          if (slotOf.get(sym.id).isEmpty) {
            //            println("GETFIELD(" + className + ", " + sym.name + ", " + getTypeNotation(expr.getType) + ")")
            ch << ALoad(0) << GetField(className, sym.name, getTypeNotation(expr.getType))
          } else {
            val slot = slotOf.get(sym.id).get
            if (sym.getType == TInt || sym.getType == TBoolean) ch << ILoad(slot)
            else ch << ALoad(slot)
          }
          
          if((stat.isInstanceOf[ArrayAssign]))  
            ch << LineNumber(expr.line)

        }
        case This() => {
          ch << LineNumber(expr.line)
          ch << ALoad(0)
        }
        case NewIntArray(size: ExprTree) => {
          ch << LineNumber(expr.line)
          compileExpr(ch, size, slotOf, mt, stat)
          ch << NewArray.primitive("T_INT")
        }
        case New(tpe: Identifier) => {
          ch << LineNumber(expr.line)
          ch << DefaultNew(tpe.value)
        }
        case Not(expr: ExprTree) => {
          val ok = ch.getFreshLabel("ok")
          compileExpr(ch, expr, slotOf, mt, stat)
          ch << Ldc(1) << SWAP
          ch << LineNumber(expr.line)
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
        case TObject(classSymbol: ClassSymbol) => {
          "L" + tpe.toString() + ";";
        }
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
