package koolc
package analyzer

import utils._
import ast.Trees._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    var gs = new GlobalScope

    //Program(main: MainObject, classes: List[ClassDecl])
    //Block(stats: List[StatTree])
    //If(expr: ExprTree, thn: StatTree, els: Option[StatTree])
    //While(expr: ExprTree, stat: StatTree)
    //Println(expr: ExprTree)
    //Assign(id: Identifier, expr: ExprTree)
    //ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree)
    //And(lhs: ExprTree, rhs: ExprTree)
    //Or(lhs: ExprTree, rhs: ExprTree)
    //Plus(lhs: ExprTree, rhs: ExprTree)
    //Minus(lhs: ExprTree, rhs: ExprTree)
    //Times(lhs: ExprTree, rhs: ExprTree)
    //Div(lhs: ExprTree, rhs: ExprTree)
    //LessThan(lhs: ExprTree, rhs: ExprTree)
    //Equals(lhs: ExprTree, rhs: ExprTree)
    //ArrayRead(arr: ExprTree, index: ExprTree)
    //ArrayLength(arr: ExprTree)
    //MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree])
    //IntLit(value: Int)
    //StringLit(value: String)
    //Identifier(value: String) extends TypeTree
    //NewIntArray(size: ExprTree)
    //New(tpe: Identifier)
    //Not(expr: ExprTree)

    def firstRound() {

      _MainObject(prog.main)
      prog.classes.foreach(c => gs.classes += c.id.value -> _ClassDeclaration(c))

      //MainObject(id: Identifier, stats: List[StatTree]) extends Tree with Symbolic[ClassSymbol]
      def _MainObject(main: MainObject) = {
        gs.mainClass = new ClassSymbol(main.id.value)
      }

      //ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl])
      def _ClassDeclaration(c: ClassDecl): ClassSymbol = {
        //    	gs.classes = new Map[String,ClassSymbol]

        gs.lookupClass(c.id.value) match {
          case None => {}
          case Some(res) => error("class '" + c.id.value + "' is defined twice. First definition here: " + res.position)
        }

        var cls = new ClassSymbol(c.id.value)

        // second_Round 
        //c.parent Option[Identifier]

        c.vars.foreach(v => cls.members += v.id.value -> new VariableSymbol(v.id.value)) //List[VarDecl]
        c.methods.foreach(m => cls.methods += m.id.value -> _MethodDeclaration(m, cls)) //List[MethodDecl])

        cls
      }

      //MethodDecl(retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], stats: List[StatTree], retExpr: ExprTree)
      def _MethodDeclaration(m: MethodDecl, cls: ClassSymbol): MethodSymbol = {
        var ms = new MethodSymbol(m.id.value, null)

        m.args.foreach(a => ms.params += a.id.value -> new VariableSymbol(a.id.value)) // Map[String,VariableSymbol]()
        m.vars.foreach(v => ms.members += v.id.value -> new VariableSymbol(v.id.value)) // Map[String,VariableSymbol]()
        //		 ms.argList // List[VariableSymbol] = Nil

        //second_Round
        //ms.overridden // Option[MethodSymbol] = None
        
        var ms_newyearedition = new MethodSymbol(m.id.value, cls)
        ms_newyearedition.params = ms.params
        ms_newyearedition.members = ms.members
        
      }
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////////////////////////////////////
    def secondRound() {

      _MainObject(prog.main)
      prog.classes.foreach(c => _ClassDeclaration(c))

      //MainObject(id: Identifier, stats: List[StatTree]) extends Tree with Symbolic[ClassSymbol]
      def _MainObject(main: MainObject) = {
        main.setSymbol(gs.mainClass)
        main.stats.foreach(s => _Statement(s))
      }

      //ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl])
      def _ClassDeclaration(c: ClassDecl) = {
        //    	gs.classes = new Map[String,ClassSymbol]

        var currentCls: ClassSymbol = null
        gs.lookupClass(c.id.value) match {
          case None => error("‘" + c.id.value + "’ was not declared in this scope at " + c.position)
          case Some(res) => {
            c.setSymbol(res)
            currentCls = res
          }
        }

        // second_Round 
        //c.parent Option[Identifier]

        for (v <- c.vars) {
          currentCls.lookupVar(v.id.value) match {
            case None => error("‘" + v.id.value + "’ was not declared in this scope at " + v.position)
            case Some(res) => v.setSymbol(res)
          }
        }
        //c.vars.foreach(v => VariableSymbol(v.id.value)) //List[VarDecl]

        c.methods.foreach(m => _MethodDeclaration(m, currentCls)) //List[MethodDecl])
      }

      //MethodDecl(retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], stats: List[StatTree], retExpr: ExprTree)
      def _MethodDeclaration(m: MethodDecl, cls: ClassSymbol) = {
        var currentMs: MethodSymbol = null
        cls.lookupMethod(m.id.value) match {
          case None => error("‘" + m.id.value + "’ was not declared in this scope at " + m.position)
          case Some(res) => {
            m.setSymbol(res)
            currentMs = res
          }
        }

        for(a <- m.args) {
          currentMs.lookupVar(a.id.value) match {
            case None => error("‘" + a.id.value + "’ was not declared in this scope at " + a.position)
            case Some(res) => a.setSymbol(res)
          }
        }
        m.args.foreach(a => a.id.value -> new VariableSymbol(a.id.value)) // Map[String,VariableSymbol]()
        m.vars.foreach(v => v.id.value -> new VariableSymbol(v.id.value)) // Map[String,VariableSymbol]()
        //		 ms.argList // List[VariableSymbol] = Nil

        //second_Round
        //ms.ov n // Option[MethodSymbol] = None

        ms
      }

      //TBD later
      def _Statement(s: StatTree): Unit = {
        s match {
          case Block(stats: List[StatTree]) => {
            stats.foreach(stat => _Statement(stat))
          }
          case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) => {
            _Expression(expr)
            _Statement(thn)
            els match {
              case None => {}
              case Some(res) => _Statement(res)
            }
          }
          case While(expr: ExprTree, stat: StatTree) => {
            _Expression(expr)
            _Statement(stat)
          }
          case Println(expr: ExprTree) => {
            _Expression(expr)
          }
          case Assign(id: Identifier, expr: ExprTree) => {

          }
          case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) => {}
        }
      }

      def _Expression(expr: ExprTree) = {

      }

    }

    firstRound()
    secondRound()

    //    println(prog)

    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check for all constraints
    prog
  }
}
