package koolc
package analyzer

import ast.Trees._

import Symbols._
import Types._
import utils._

object TypeChecking extends Pipeline[Program, Program] {

  /**
   * Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages.
   */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr.getType // TODO: Compute type for each kind of expression

      // Check result and return a valid type in case of error
      if (expected.isEmpty) {
        tpe
      } 
      else if (tpe == TError) {
        error("Type error: expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
        expected.head
      } //TODO MORE SPECIFIC ERROR MSG FOR ERROR TYPE
      else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
        error("Type error: expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
        expected.head
      } else {
        tpe
      }
    }

    def tcStat(stat: StatTree): Unit = {
      stat match {
        case Block(stats: List[StatTree]) => {
          stats.foreach(stat => tcStat(stat))
        }
        case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) => {
          tcExpr(expr, TBoolean)
          tcStat(thn)
          els match {
            case None => {}
            case Some(res) => tcStat(res)
          }
        }
        case While(expr: ExprTree, stat: StatTree) => {
          tcExpr(expr, TBoolean)
          tcStat(stat)
        }
        case Println(expr: ExprTree) => {
          tcExpr(expr, TInt , TBoolean , TString)
        }
        case Assign(id: Identifier, expr: ExprTree) => {
          tcExpr(expr, id.getType)
        }
        case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) => {
          tcExpr(index, TInt)
          tcExpr(expr, TInt)
        }
      }
    }
    
    prog.main.stats.foreach(s => tcStat(s))
    for(c <- prog.classes) {
      for(m <- c.methods) {
        m.stats.foreach(s => tcStat(s))
        tcExpr(m.retExpr, m.getSymbol.getType)
      }
    }
    
    prog
  }
}
