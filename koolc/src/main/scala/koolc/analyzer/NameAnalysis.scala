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
        gs.mainClass = new ClassSymbol(main.id.value).setPos(main.id)
//        gs.mainClass.methods += "main" -> new MethodSymbol("main", gs.mainClass).setPos(main) //TODO Don't give IDID 
      }

      //ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl])
      def _ClassDeclaration(c: ClassDecl): ClassSymbol = {
        //    	gs.classes = new Map[String,ClassSymbol]

        if(c.id.value == gs.mainClass.name){
          error("Class " + c.id.value + " has the same name as the main class.", c.id)
        }
        
        gs.lookupClass(c.id.value) match {
          case None => {}
          case Some(res) => error("Class " + c.id.value + " is defined more than once. First definition here: " + res.position, c.id)
        }
        
        var cls = new ClassSymbol(c.id.value).setPos(c)

        c.vars.foreach(v => cls.members += v.id.value -> new VariableSymbol(v.id.value).setPos(v.id)) //List[VarDecl]
        for(m <- c.methods) {
          cls.lookupMethod(m.id.value, false) match {
            case None => cls.methods += m.id.value -> _MethodDeclaration(m, cls)
            case _ => error("method '" + m.id.value + "' is defined twice. First definition here: ", m)
          }
        } //List[MethodDecl])

        cls
      }

      //MethodDecl(retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], stats: List[StatTree], retExpr: ExprTree)
      def _MethodDeclaration(m: MethodDecl, cls: ClassSymbol): MethodSymbol = {
        var ms = new MethodSymbol(m.id.value, cls).setPos(m)

        for(a <- m.args) {
          ms.lookupVar(a.id.value) match {
            case (_, 2) => error("Parameter name " + a.id.value + " is used twice in " + m.id.value + ".", a)
            case (_, 0) => {
              ms.params += a.id.value -> new VariableSymbol(a.id.value).setPos(a.id)
              ms.argList = ms.argList :+ new VariableSymbol(a.id.value).setPos(a.id)
            }
          }
        } // Map[String,VariableSymbol]()
        
        for(v <- m.vars) {
          ms.lookupVar(v.id.value) match {
            case (_, 2) => error("Declaration of " + v.id.value + " as local shadows method parameter of the same name.", v.id) //TODO POSITION
            case (Some(firstDecl), 1) => error(v.id.value + " is declared more than once. First declaration here: "+ firstDecl.position) //TODO POSITION
            case (_, 0) => ms.members += v.id.value -> new VariableSymbol(v.id.value).setPos(v.id)
          }
        }
        
        ms
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
        main.id.setSymbol(gs.mainClass)
        main.stats.foreach(s => _Statement(s, new MethodSymbol(null, null)))
      }

      //ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl])
      def _ClassDeclaration(c: ClassDecl) = {
        //    	gs.classes = new Map[String,ClassSymbol]

        var currentCls: ClassSymbol = null
        gs.lookupClass(c.id.value) match {
          case None => sys.error("Internal error, please report with error code 1.")
          case Some(res) => {
            c.id.setSymbol(res)
            currentCls = res
          }
        }

        c.parent match {
          case None => {}
          case Some(class_ref) => {
          	gs.lookupClass(class_ref.value) match {
          	  case None => error("Class " + c.id.value + " extends class "+class_ref.value+ " which is not defined.", class_ref)
          	  case Some(res2) => {
                class_ref.setSymbol(res2)
                currentCls.parent = gs.lookupClass(class_ref.value)
              }
          	}
          }
        }
        
        var cyclic, cyc_check_done = false
        var current_parent = currentCls.parent.getOrElse(null)
        var sb = new StringBuilder
//        sb.append(current_parent.name)
//        sb.append(" <: ")
        sb.append(currentCls.name)
        while(!cyc_check_done){
          if(current_parent != null && current_parent == currentCls){
            cyclic = true
            cyc_check_done = true
          }else if(current_parent != null){
            sb.append(" <: ")
            sb.append(current_parent.name)
            current_parent = current_parent.parent.getOrElse(null)
          }else{
            cyc_check_done = true
          }
        }
        
        if(cyclic) {
          sb.append(" <: ")
          sb.append(currentCls.name)
          fatal(sb) //TODO complete error msg
        } 
        
        
        for (v <- c.vars) {
          currentCls.lookupVar(v.id.value) match {
            case None => sys.error("Internal error, please report with error code 2.")
            case Some(res) => v.id.setSymbol(res)
          }
        }

        
        //** TODO:
        //1) set overridden
        //2) check duplicates (and 1)
        c.methods.foreach(m => _MethodDeclaration(m, currentCls)) //List[MethodDecl])
      }

      //MethodDecl(retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], stats: List[StatTree], retExpr: ExprTree)
      def _MethodDeclaration(m: MethodDecl, cls: ClassSymbol) = {
        var currentMs: MethodSymbol = null
        
        cls.lookupMethod(m.id.value, false) match {
          case None => sys.error("Internal error, please report with error code 3.")
          case Some(res) => {
            m.id.setSymbol(res)
            currentMs = res
          }
        }

        for(a <- m.args) {
          currentMs.lookupVar(a.id.value) match {
            case (None, _) => sys.error("Internal error, please report with error code 4.")
            case (Some(res), _) => a.id.setSymbol(res)
          }
        }
        for(v <- m.vars) {
          currentMs.lookupVar(v.id.value) match {
            case (None, _) => sys.error("Internal error, please report with error code 5.")
            case (Some(res), _) => v.id.setSymbol(res)
          }
        }

        //TODO just double check overridden 
        cls.parent match {
          case None => {}
          case Some(parent_ref) => {
            parent_ref.lookupMethod(m.id.value) match {
              case None => {}
              case Some(method_ref) => {
                currentMs.overridden = parent_ref.lookupMethod(m.id.value)
                if(method_ref.argList.size != currentMs.argList.size) //TODO position
                  error(m.id.value + " overrides previous definition from " + method_ref.position + " with a different number of parameters.", m)
              }
            }
          }
        }
        
        m.stats.foreach(s => _Statement(s, currentMs))
        
        _Expression(m.retExpr, currentMs)
      }

      def _Statement(s: StatTree, ms: MethodSymbol): Unit = {
        s match {
          case Block(stats: List[StatTree]) => {
            stats.foreach(stat => _Statement(stat, ms))
          }
          case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) => {
            _Expression(expr, ms)
            _Statement(thn, ms)
            els match {
              case None => {}
              case Some(res) => _Statement(res, ms)
            }
          }
          case While(expr: ExprTree, stat: StatTree) => {
            _Expression(expr, ms)
            _Statement(stat, ms)
          }
          case Println(expr: ExprTree) => {
            _Expression(expr, ms)
          }
          case Assign(id: Identifier, expr: ExprTree) => {
            if(ms.name == null) {
              error("Undeclared identifier: "+ id.value)
            }
          	ms.lookupVar(id.value) match {
          	  case (None, _) => error("Undeclared identifier: " + id.value + ".", id)
          	  case (Some(var_ref), _) => {
          	    id.setSymbol(var_ref)
          	    _Expression(expr, ms)
          	  }
          	}
          }
          case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) => {
            if(ms.name == null) {
              error("Undeclared identifier: "+ id.value)
            }
            ms.lookupVar(id.value) match {
              case (None, _) => error("'" + id.value + "' was not declared in this scope at " + id.position)
              case (Some(var_ref), _) => {
                id.setSymbol(var_ref)
                _Expression(index, ms)
                _Expression(expr, ms)
              }
            }
          }
        }
      }

      def _Expression(expr: ExprTree, ms: MethodSymbol) : ClassSymbol = {
        var newClass : ClassSymbol = null
        expr match {
		  case And(lhs: ExprTree, rhs: ExprTree) => {
		    _Expression(lhs, ms)
		    _Expression(rhs, ms)
		  }
		  case Or(lhs: ExprTree, rhs: ExprTree) => {
		    _Expression(lhs, ms)
		    _Expression(rhs, ms)
		  }
		  case Plus(lhs: ExprTree, rhs: ExprTree) => {
		    _Expression(lhs, ms)
		    _Expression(rhs, ms)
		  }
		  case Minus(lhs: ExprTree, rhs: ExprTree) => {
		    _Expression(lhs, ms)
		    _Expression(rhs, ms)
		  }
		  case Times(lhs: ExprTree, rhs: ExprTree) => {
		    _Expression(lhs, ms)
		    _Expression(rhs, ms)
		  }
		  case Div(lhs: ExprTree, rhs: ExprTree) => {
		    _Expression(lhs, ms)
		    _Expression(rhs, ms)
		  }
		  case LessThan(lhs: ExprTree, rhs: ExprTree) => {
		    _Expression(lhs, ms)
		    _Expression(rhs, ms)
		  }
		  case Equals(lhs: ExprTree, rhs: ExprTree) => {
		    _Expression(lhs, ms)
		    _Expression(rhs, ms)
		  }
		  case ArrayRead(arr: ExprTree, index: ExprTree) => {
		    _Expression(arr, ms)
		    _Expression(index, ms)
		  }
		  case ArrayLength(arr: ExprTree) => {
		    _Expression(arr, ms)
		  }
		  case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) => {
		    val tmp = _Expression(obj, ms)
		    if(tmp != null) 
		      tmp.lookupMethod(meth.value) match {
		      case None => error("'" + meth.value + "' was not declared in this scope at " + meth.position)
		      case Some(method_ref) => {
		        meth.setSymbol(method_ref)
		        args.foreach(a => _Expression(a, method_ref))
		      }
		    }
		    
		  }
		  case IntLit(value: Int) => {
		    
		  }
		  case StringLit(value: String) => {
		    
		  }
		
		  case True() => {
		    
		  }
		  case False() => {
		    
		  }
		  case Identifier(value: String) => {
		    ms.lookupVar(value) match {
		      case (None, _) => error("Undeclared identifier: " + value, expr)
		      case (Some(class_ref), _) => newClass = ms.classSymbol
		    }
		  }
		
		  case This() => {
		    
		  }
		  case NewIntArray(size: ExprTree) => {
		    _Expression(size, ms)
		  }
		  case New(tpe: Identifier) => {
		    gs.lookupClass(tpe.value) match {
		      case None => error("Undeclared type: " + tpe.value + ".", tpe)
		      case Some(class_ref) => { tpe.setSymbol(class_ref); newClass = class_ref; }
		    }
		  }
		  case Not(expr: ExprTree) => {
		    _Expression(expr, ms)
		  }
        }
        
        if(newClass != null) newClass else ms.classSymbol
      }
    }

    firstRound()
//    terminateIfErrors
    secondRound()
    terminateIfErrors
    //    println(prog)

    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check for all constraints
    prog
  }
}
