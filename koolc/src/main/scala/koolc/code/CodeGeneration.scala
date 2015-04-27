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

      
      
      for(m <- ct.methods){
        var loa = List[String]()
        m.args.foreach(a => loa = loa :+ a.id.value)
        val mh = classFile.addMethod(m.retType.getType.toString(), m.id.value, loa)
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

      // TODO: Emit code

      ch.freeze
    }

    def generateMainMethodCode(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {

      // TODO: Emit code
      ch.freeze
    }

    def codeGenerationFuncs(ch: cafebabe.CodeHandler, node: Tree) = {
      node match {
        case IntArrayType() =>
        case IntType() =>
        case BooleanType() =>
        case StringType() =>

        case Block(stats: List[StatTree]) =>
        case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) =>
        case While(expr: ExprTree, stat: StatTree) =>
        case Println(expr: ExprTree) => {
          ch <<
            GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;") <<
            Ldc("HW") <<
            InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V") <<
            RETURN
        }
        case Assign(id: Identifier, expr: ExprTree) =>
        case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>

        case And(lhs: ExprTree, rhs: ExprTree) => {
          
        }
        case Or(lhs: ExprTree, rhs: ExprTree) =>
        case Plus(lhs: ExprTree, rhs: ExprTree) => {
          
        }
        case Minus(lhs: ExprTree, rhs: ExprTree) => {
          
          ch << ILoad(1) << ALoad(0) <<
               ILoad(1) << Ldc(1) << ISUB
        }
        case Times(lhs: ExprTree, rhs: ExprTree) =>
        case Div(lhs: ExprTree, rhs: ExprTree) =>
        case LessThan(lhs: ExprTree, rhs: ExprTree) =>
        case Equals(lhs: ExprTree, rhs: ExprTree) =>
        case ArrayRead(arr: ExprTree, index: ExprTree) =>
        case ArrayLength(arr: ExprTree) =>
        case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) =>
        case IntLit(value: Int) =>
        case StringLit(value: String) =>

        case True() =>
        case False() =>
        case Identifier(value: String) =>
        case This() =>
        case NewIntArray(size: ExprTree) =>
        case New(tpe: Identifier) =>
        case Not(expr: ExprTree) =>

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
//  TODO    val codeHandler = classFile.addMainMethod.codeHandler
//      codeHandler.freeze

      

  }

}
