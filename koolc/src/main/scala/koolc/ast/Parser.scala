package koolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)
    var nextToken: Token = new Token(BAD)
    var peekaboo = false

    if(tokens.hasNext){
      nextToken = tokens.next
      // skips bad tokens
      while (nextToken.kind == BAD && tokens.hasNext) {
        nextToken = tokens.next
      }
    }

    def readToken: Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        
        currentToken = nextToken
        nextToken = tokens.next
        
        // skips bad tokens
        while (nextToken.kind == BAD && tokens.hasNext) {
          nextToken = tokens.next
        }
      }else if(!peekaboo){
        currentToken = nextToken
        nextToken = new Token(BAD)
        peekaboo = true
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    //DONE
    def parseGoal: Program = {
      val main = _MainObject()
      val classes = _ClassDeclaration()
      eat(EOF)
      new Program(main, classes)
    }


    //DONE
    def _MainObject() : MainObject = {
      eat(OBJECT) 
      val id = _Identifier()
      eat(LBRACE)
      eat(DEF)
      eat(MAIN) 
      eat(LPAREN) 
      eat(RPAREN) 
      eat(COLON) 
      eat(UNIT) 
      eat(EQSIGN) 
      eat(LBRACE) 
      val stats = _Statement() 
      eat(RBRACE) 
      eat(RBRACE)
      new MainObject(id, stats)
    }

    //DONE
    def _ClassDeclaration() : List[ClassDecl] = {
      //OPTIONAL

      var classes : List[ClassDecl] = Nil

      while(currentToken.kind == CLASS){
        eat(CLASS) 
        val id = _Identifier()

        var parent : Option[Identifier] = None
        if(currentToken.kind == EXTENDS){
          eat(EXTENDS) 
          parent = Option(_Identifier()) 
        }

        eat(LBRACE)
        val vars = _VarDeclaration() 
        val methods = _MethodDeclaration() 
        eat(RBRACE)  

        classes = classes :+ new ClassDecl(id, parent, vars, methods)
      }

      classes
    }

    //DONE
    def _VarDeclaration() : List[VarDecl] = {
      //OPTIONAL
      var vars : List[VarDecl] = Nil

      while(currentToken.kind == VAR){
        eat(VAR) 
        val id = _Identifier() 
        eat(COLON)
        val tpe = _Type() 
        eat(SEMICOLON)

        vars = vars :+ new VarDecl(tpe, id)
      }

      vars
    }


    /**
    MethodDeclaration ::= 
    def Identifier ( ( Identifier : Type ( , Identifier : Type )* )? ) : Type = { 
      ( VarDeclaration )* 
      ( Statement )* 
      return Expression ; 
    }
    **/
    //DONE
    def _MethodDeclaration() : List[MethodDecl] = {
      //OPTIONAL
      var methods : List[MethodDecl] = Nil

      while(currentToken.kind == DEF){
        eat(DEF) 
        val id = _Identifier() 
        eat(LPAREN)

        var args : List[Formal] = Nil

        if(currentToken.kind != RPAREN){
          val arg_id = _Identifier() 
          eat(COLON)
          val arg_type = _Type() 
          
          args = args :+ new Formal(arg_type, arg_id)

          while(currentToken.kind == COMMA){
            eat(COMMA) 
            val arg_id = _Identifier() 
            eat(COLON) 
            val arg_type = _Type()
            args = args :+ new Formal(arg_type, arg_id)
          }
        }

        eat(RPAREN) 
        eat(COLON) 
        val retType = _Type()
        eat(EQSIGN) 
        eat(LBRACE)
        val vars = _VarDeclaration() 
        val stats = _Statement()
        eat(RETURN) 
        val retExpr = _Expression()
        eat(SEMICOLON) 
        eat(RBRACE)

        methods = methods :+ new MethodDecl(retType, id, args, vars, stats, retExpr)
      }

      methods
    }

    //DONE
    def _Type() = {
      currentToken.kind match {
        case INT => {
          eat(INT)
          if(currentToken.kind == LBRACKET){
            eat(LBRACKET)
            eat(RBRACKET)
            new IntArrayType()
          }else{
            new IntType()
          }
        }  
        case BOOLEAN => {
          eat(BOOLEAN)
          new BooleanType()
        }
        case STRING => {
          eat(STRING)
          new StringType()
        }
        case IDKIND => {
          _Identifier() 
        }
        case _ => expected(INT, BOOLEAN, STRING, IDKIND)
      }
    }

    /*
    Statement ::= { ( Statement )* }
                | if ( Expression ) Statement ( else Statement )?
                | while ( Expression ) Statement
                | println ( Expression ) ;
                | Identifier = Expression ;
                | Identifier [ Expression ] = Expression ;
    */
    def _Statement () : List[StatTree] = {
      //OPTIONAL

      var stats : List[StatTree] = Nil

      while(currentToken.kind == LBRACE || currentToken.kind == IF || currentToken.kind == WHILE || currentToken.kind == PRINTLN ||
        (currentToken.kind == IDKIND && (nextToken.kind == EQSIGN || nextToken.kind == LBRACKET))){
        stats = stats :+ one_Statement()
      } //While

      def one_Statement () : StatTree = {
        currentToken.kind match {
          case LBRACE => {
            eat(LBRACE)
            val stats = _Statement()
            eat(RBRACE)

            new Block(stats)
          }
          case IF => {
            eat(IF) 
            eat(LPAREN) 
            val expr = _Expression()
            eat(RPAREN) 
            val thn = one_Statement()

            var els : Option[StatTree] = None
            if(currentToken.kind == ELSE){
              eat(ELSE) 
              els = Option(one_Statement())
            }

            new If(expr, thn, els)
          }
          case WHILE => {
            eat(WHILE)
            eat(LPAREN) 
            val expr = _Expression() 
            eat(RPAREN) 
            val stat = one_Statement()

            new While(expr, stat)
          }
          case PRINTLN => {
            eat(PRINTLN) 
            eat(LPAREN) 
            val expr = _Expression() 
            eat(RPAREN) 
            eat(SEMICOLON)

            new Println(expr)
          }
          case IDKIND => {
            val id = _Identifier()
            
            currentToken.kind match {
              case EQSIGN => {
                eat(EQSIGN) 
                val expr = _Expression() 
                eat(SEMICOLON)

                new Assign(id, expr)
              }
              case LBRACKET => {
                eat(LBRACKET) 
                val index = _Expression() 
                eat(RBRACKET) 
                eat(EQSIGN) 
                val expr = _Expression() 
                eat(SEMICOLON)

                new ArrayAssign(id, index, expr)
              }
              case _ => expected(EQSIGN, LBRACKET)

            } //IDKIND Match

          } //IDKIND case
          case _ => expected(LBRACE, IF, WHILE, PRINTLN, IDKIND)

        } //Match

      } //innerFunc

      stats

    } //Func

    def _Expression  () : ExprTree = {

      //FACTOR
      var lhs : ExprTree = null
      currentToken.kind match{
        case INTLITKIND => {
          val num = currentToken.toString
          val Pattern = """INT\((.+)\)""".r
          val Pattern(value) = num
          eat(INTLITKIND)
          lhs = new IntLit(value.toInt)
        }
        case STRLITKIND => {
          val str = currentToken.toString
          val Pattern = """STR\((.+)\)""".r
          val Pattern(value) = str
          eat(STRLITKIND)

          lhs = new StringLit(value)
        }
        case TRUE => {
          eat(TRUE)

          lhs = new True()
        }
        case FALSE => {
          eat(FALSE)

          lhs = new False()
        } 
        case IDKIND => {
          lhs = _Identifier()
        }
        case THIS => {
          eat(THIS)

          lhs = new This()
        }
        case NEW => {
          eat(NEW)
          currentToken.kind match{
            case INT => {
              eat(INT) 
              eat(LBRACKET) 
              val size = _Expression() 
              eat(RBRACKET)

              lhs = new NewIntArray(size)
            }
            case IDKIND => {
              val tpe = _Identifier() 
              eat(LPAREN) 
              eat(RPAREN)

              lhs = new New(tpe)
            }
            case _ => expected(INT, IDKIND)
          }          
        }
        case BANG => {
          eat(BANG)
          val expr = _Expression()

          lhs = new Not(expr)
        }
        case LPAREN => {
          eat(LPAREN)
          lhs = _Expression()
          eat(RPAREN)
        }
        case _ => expected(INTLITKIND, STRLITKIND, TRUE, FALSE, IDKIND, THIS, NEW, BANG, LPAREN)      

      }



      //EXTENDABLES



      currentToken.kind match{
        case AND => {
          eat(AND)
          val rhs = _Expression()

          new And(lhs, rhs)
        }  
        case OR => {
          eat(OR)
          val rhs = _Expression()

          new Or(lhs: ExprTree, rhs: ExprTree)
        }  
        case EQUALS => {
          eat(EQUALS)
          val rhs = _Expression()

          new Equals(lhs, rhs)
        }  
        case LESSTHAN => {
          eat(LESSTHAN)
          val rhs = _Expression()

          new LessThan(lhs, rhs)
        }  
        case PLUS => {
          eat(PLUS)
          val rhs = _Expression()

          new Plus(lhs, rhs)
        }  
        case MINUS => {
          eat(MINUS)
          val rhs = _Expression()

          new Minus(lhs, rhs)
        }  
        case TIMES => {
          eat(TIMES)
          val rhs = _Expression()

          new Times(lhs, rhs)
        }  
        case DIV => {
          eat(DIV)
          val rhs = _Expression()

          new Div(lhs, rhs)
        }

        case LBRACKET => {
          eat(LBRACKET) 
          val index = _Expression() 
          eat(RBRACKET)

          new ArrayRead(lhs, index)
        }

        case DOT => {
          eat(DOT)
          currentToken.kind match{
            case LENGTH => {
              eat(LENGTH)

              new ArrayLength(lhs)
            }
            
            case IDKIND => {
              val meth = _Identifier()
              eat(LPAREN) 

              var args : List[ExprTree] = Nil

              if(currentToken.kind == INTLITKIND ||  currentToken.kind == STRLITKIND ||  currentToken.kind == TRUE ||  currentToken.kind == FALSE ||  currentToken.kind == IDKIND ||  currentToken.kind == THIS ||  currentToken.kind == NEW ||  currentToken.kind == BANG ||  currentToken.kind == LPAREN){
                args = args :+ _Expression()
                while(currentToken.kind == COMMA){
                  eat(COMMA)
                  args = args :+ _Expression()
                }
              }
              eat(RPAREN)

              new MethodCall(lhs, meth, args)
            }

            case _ => expected(LENGTH, IDKIND)

          } 
                             
        }
        case _ => lhs
      }
    }













    def _Expression2  () : ExprTree = {
      
//    --> Expr || AndTerm
//      | AndTerm 
      def _Expr() : ExprTree = {
        val expr = _AndTerm()
        if (currentToken == OR){
          eat(OR)
          new Or(expr, _AndTerm())
        }else expr
      }

//    --> AndTerm && RelExpr
//      | RelExpr
      def _AndTerm() : ExprTree = {
//        case AND => {

        // &&
      }

//    --> RelExpr < NumExpr
//        RelExpr = NumExpr
//      | NumExpr
      def _RelExpr() : ExprTree = {
//        case EQUALS => {
//        case LESSTHAN => {
      }

//    --> NumExpr + Term 
//      | NumExpr - Term
//      | Term
      def _NumExpr() : ExprTree = {
//        case PLUS => {
//        case MINUS => {
      }

//    --> Term * Value 
//      | Term / Value
//      | Value
      def _Term() : ExprTree = {
//        case TIMES => {
//        case DIV => {                
      }

//    --> ! Factor
//      | Factor      
      def _Value() : ExprTree = {

        // !
      }

//    --> 
      def _Factor() : ExprTree = {
        currentToken.kind match{
          case INTLITKIND => {
            
          }
          case STRLITKIND => {
            
          }
          case TRUE => {
            
          }
          case FALSE => {
            
          }
          case IDKIND => {
            
          }
          case THIS => {
            
          }
          case NEW => {
            
          }
          case INT => {
            
          }
          case IDKIND => {
            
          }
          case LPAREN => { 
            
          }
        }
        
        /*
        

        case LBRACKET => {
        case DOT => {
            case LENGTH => {
            case IDKIND => {
        */
        
        
        /*
        Expression [ Expression ]
        Expression . length
        Expression . Identifier ( ( Expression ( , Expression )* )? )
        <INTEGER_LITERAL>
        " <STRING_LITERAL> "
        true
        false
        Identifier
        this
        new Int [ Expression ]
        new Identifier ( )
        ! Expression
        ( Expression )
        */
        


      }
      
    }










    //DONE
    def _Identifier () : Identifier = {
      val id : String = currentToken.toString
      eat(IDKIND)
      val Pattern = """ID\((.+)\)""".r
      val Pattern(value) = id
      new Identifier(value)
    }

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }

}
/*

      currentToken match {
      case A => // compare x to A, because of the uppercase
      case b => // assign x to b
      case `b` => // compare x to b, because of the backtick
      case _ => error()
    }

*/

/****************************************************************/

