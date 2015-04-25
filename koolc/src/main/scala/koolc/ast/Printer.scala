package koolc
package ast

import Trees._

/* This is our Pretty Printer.
 * It's pretty self explanatory.
 */

object Printer {
  def apply(t: Tree, depth: Int = 0): String = {
    var sb = new StringBuilder
    t match {
      case Program(main: MainObject, classes: List[ClassDecl]) => {
        sb.append(Printer(main))
        sb.append(expandList(classes, depth))
      }
      case MainObject(id: Identifier, stats: List[StatTree]) => {
        sb.append("object ")
        sb.append(Printer(id))
        sb.append(" {\n")
        sb.append(indent(depth))
        sb.append("def main() : Unit = {\n")
        sb.append(expandList(stats, depth+1))
        sb.append(indent(depth))
        sb.append("}\n")
        sb.append("}\n")
      }
      case ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl]) => {
        sb.append("class ")
        sb.append(Printer(id))
        
        //OPTIONAL PARENT
        parent match {
          case None => ""
          case Some(value) => {
            sb.append(" extends ")
            sb.append(Printer(value))
          }
        }
        sb.append(" {\n");
        sb.append(expandList(vars, depth))
        sb.append(expandList(methods, depth))
        sb.append("}\n")
      }
      case VarDecl(tpe: TypeTree, id: Identifier) => {
        sb.append(indent(depth))
        sb.append("var ")
        sb.append(Printer(id))
        sb.append(" : ")
        sb.append(Printer(tpe))
        sb.append(";\n")
      }
      case MethodDecl(retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], stats: List[StatTree], retExpr: ExprTree) => {
        sb.append(indent(depth))
        sb.append("def ")
        sb.append(Printer(id))
        sb.append("(")
        //ARGUMENTS WITH ', '
        var first = true
        for(l <- args) {
          if(!first) sb.append(", ")
          sb.append(Printer(l))
          first = false
        }
        sb.append(") : ")
        sb.append(Printer(retType))
        sb.append(" = {\n")
        sb.append(expandList(vars, depth+1))
        sb.append(expandList(stats, depth+1))
        sb.append(indent(depth+1))
        sb.append("return ")
        sb.append(Printer(retExpr))
        sb.append(";\n")
        sb.append(indent(depth))
        sb.append("}\n")
      }
      case Formal(tpe: TypeTree, id: Identifier) => {
        sb.append(Printer(id))
        sb.append(": ")
        sb.append(Printer(tpe))
      }
      case IntArrayType() => {
        sb.append("Int[]")
      }
      case IntType() => {
        sb.append("Int")
      }
      case BooleanType() => {
        sb.append("Bool")
      }
      case StringType() => {
        sb.append("String")
      }
      case Block(stats: List[StatTree]) => {
        sb.append(indent(depth))
        sb.append("{\n")
        sb.append(expandList(stats, depth+1))
        sb.append(indent(depth))
        sb.append("}\n")
      }
      case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) => {
        sb.append(indent(depth))
        sb.append("if(")
        sb.append(Printer(expr))
        sb.append(")")
        sb.append(Printer(thn, depth+1))
        els match {
          case None => ""
          case Some(value) => {
            sb.append(indent(depth))
            sb.append("else")
            sb.append(Printer(value, depth+1))
          }
        }
      }
      case While(expr: ExprTree, stat: StatTree) => {
        sb.append(indent(depth))
        sb.append("while(")
        sb.append(Printer(expr))
        sb.append(")")
        sb.append(Printer(stat, depth+1))
      }
      case Println(expr: ExprTree) => {
        sb.append(indent(depth))
        sb.append("println(")
        sb.append(Printer(expr))
        sb.append(");\n")
      }
      case Assign(id: Identifier, expr: ExprTree) => {
        sb.append(indent(depth))
        sb.append(Printer(id))
        sb.append(" = ")
        sb.append(Printer(expr))
        sb.append(";\n")
      }
      case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) => {
        sb.append(indent(depth))
        sb.append(Printer(id))
        sb.append("[")
        sb.append(Printer(index))
        sb.append("] = ")
        sb.append(Printer(expr))
        sb.append(";\n")
      }
      case And(lhs: ExprTree, rhs: ExprTree) => {
        sb.append('(')
        sb.append(Printer(lhs))
        sb.append(" && ")
        sb.append(Printer(rhs))
        sb.append(')')
      }
      case Or(lhs: ExprTree, rhs: ExprTree) => {
        sb.append('(')
        sb.append(Printer(lhs))
        sb.append(" || ")
        sb.append(Printer(rhs))
        sb.append(')')
      }
      case Plus(lhs: ExprTree, rhs: ExprTree) => {
        sb.append('(')
        sb.append(Printer(lhs))
        sb.append(" + ")
        sb.append(Printer(rhs))
        sb.append(')')
      }
      case Minus(lhs: ExprTree, rhs: ExprTree) => {
        sb.append('(')
        sb.append(Printer(lhs))
        sb.append(" - ")
        sb.append(Printer(rhs))
        sb.append(')')
      }
      case Times(lhs: ExprTree, rhs: ExprTree) => {
        sb.append('(')
        sb.append(Printer(lhs))
        sb.append(" * ")
        sb.append(Printer(rhs))
        sb.append(')')
      }
      case Div(lhs: ExprTree, rhs: ExprTree) => {
        sb.append('(')
        sb.append(Printer(lhs))
        sb.append(" / ")
        sb.append(Printer(rhs))
        sb.append(')')
      }
      case LessThan(lhs: ExprTree, rhs: ExprTree) => {
        sb.append('(')
        sb.append(Printer(lhs))
        sb.append(" < ")
        sb.append(Printer(rhs))
        sb.append(')')
      }
      case Equals(lhs: ExprTree, rhs: ExprTree) => {
        sb.append('(')
        sb.append(Printer(lhs))
        sb.append(" == ")
        sb.append(Printer(rhs))
        sb.append(')')
      }
      case ArrayRead(arr: ExprTree, index: ExprTree) => {
        sb.append(Printer(arr))
        sb.append("[")
        sb.append(Printer(index))
        sb.append("]")
      }
      case ArrayLength(arr: ExprTree) => {
        sb.append(Printer(arr))
        sb.append(".length")
      }
      case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) => {
        sb.append(Printer(obj))
        sb.append(".")
        sb.append(Printer(meth))
        sb.append("(")
        //ARGUMENTS WITH ', '
        var first = true
        for(l <- args) {
          if(!first) sb.append(", ")
          sb.append(Printer(l))
          first = false
        }
        sb.append(")")
      }
      case IntLit(value: Int) => {
        sb.append(value)
      }
      case StringLit(value: String) => {
        sb.append('"');
        sb.append(value)
        sb.append('"');
      }
      case True() => {
        sb.append("true")
      }
      case False() => {
        sb.append("false")
      }
      case Identifier(value: String) => {
        sb.append(value)
        sb.append("#"+t.asInstanceOf[Identifier].getSymbol.id)
      }
      case This() => {
        sb.append("this")
      }
      case NewIntArray(size: ExprTree) => {
        sb.append(" new Int[")
        sb.append(Printer(size))
        sb.append("]")
      }
      case New(tpe: Identifier) => {
        sb.append(" new ")
        sb.append(Printer(tpe))
        sb.append("()")
      }
      case Not(expr: ExprTree) => {
        sb.append("!(")
        sb.append(Printer(expr))
        sb.append(')')
      }
    }
    
  

    def expandList(list: List[Tree], depth: Int): String = {
      var sb = new StringBuilder
      
      list.foreach { l => sb.append(Printer(l, depth)) }
      
      sb.toString()
    }
    
    def indent(depth: Int): String = {
      var sb = new StringBuilder
      
      for(i <- 0 to depth) {
        sb.append("\t")
      }
      
      sb.toString
    }

    sb.toString
  }
  
  /*def apply(t: Tree, depth: Int = 0): String = {
    t.toString
  }*/
  
}
