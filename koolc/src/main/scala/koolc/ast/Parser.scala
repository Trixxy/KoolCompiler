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

    def readToken: Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }
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
          parent = Some(_Identifier()) 
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

    //
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

    def _Statement () : List[StatTree] = {
      //OPTIONAL
      currentToken.kind match {
        case LBRACE => {
          eat(LBRACE)
          _Statement() 
          eat(RBRACE)
        }
        case IF => {
          eat(IF) 
          eat(LPAREN) 
          _Expression() 
          eat(RPAREN) 
          _Statement() 
          if(currentToken.kind == ELSE){
            eat(ELSE) 
            _Statement()
          }
        }
        case WHILE => {
          eat(WHILE)
          eat(LPAREN) 
          _Expression() 
          eat(RPAREN) 
          _Statement()
        }
        case PRINTLN => {
          PRINTLN 
          eat(LPAREN) 
          _Expression() 
          eat(RPAREN) 
          eat(SEMICOLON)
        }
        case IDKIND => {
          _Identifier()
          
          currentToken.kind match {
            case EQSIGN => {
              eat(EQSIGN) 
              _Expression() 
              eat(SEMICOLON)
            }
            case LBRACKET => {
              eat(LBRACKET) 
              _Expression() 
              eat(RBRACKET) 
              eat(EQSIGN) 
              _Expression() 
              eat(SEMICOLON)
            }
            case _ => expected(EQSIGN, LBRACKET)
          }

        }

      }

    }
    def _Expression  () : Unit = {

      //FACTOR
      currentToken.kind match{
        case INTLITKIND => {
          eat(INTLITKIND)
        }
        case STRLITKIND => {
          eat(STRLITKIND)
        }
        case TRUE => {
          eat(TRUE)
        }
        case FALSE => {
          eat(FALSE)
        } 
        case IDKIND => {
          _Identifier()
        }
        case THIS => {
          eat(THIS)
        }
        case NEW => {
          eat(NEW)
          currentToken.kind match{
            case INT => {
              eat(INT) 
              eat(LBRACKET) 
              _Expression() 
              eat(RBRACKET)
            }
            case IDKIND => {
              _Identifier() 
              eat(LPAREN) 
              eat(RPAREN)
            }
            case _ => expected(INT, IDKIND)
          }
          
        }
        case BANG => {
          eat(BANG)
          _Expression()
        }
        case LPAREN => {
          eat(LPAREN)
          _Expression()
          eat(RPAREN)
        }
        case _ => expected(INTLITKIND, STRLITKIND, TRUE, FALSE, IDKIND, THIS, NEW, BANG, LPAREN)      

      }



      //EXTENDABLES


      currentToken.kind match{
        case AND => {
          eat(AND)
          _Expression()
        }  
        case OR => {
          eat(OR)
          _Expression()
        }  
        case EQUALS => {
          eat(EQUALS)
          _Expression()
        }  
        case LESSTHAN => {
          eat(LESSTHAN)
          _Expression()
        }  
        case PLUS => {
          eat(PLUS)
          _Expression()
        }  
        case MINUS => {
          eat(MINUS)
          _Expression()
        }  
        case TIMES => {
          eat(TIMES)
          _Expression()
        }  
        case DIV => {
          eat(DIV)
          _Expression()
        }

        case LBRACKET => {
          eat(LBRACKET) 
          _Expression() 
          eat(RBRACKET)
        }

        case DOT => {
          eat(DOT)
          currentToken.kind match{
            case LENGTH => {
              eat(LENGTH)
            }
            
            case IDKIND => {
              _Identifier()
              eat(LPAREN) 

              if(currentToken.kind == INTLITKIND ||  currentToken.kind == STRLITKIND ||  currentToken.kind == TRUE ||  currentToken.kind == FALSE ||  currentToken.kind == IDKIND ||  currentToken.kind == THIS ||  currentToken.kind == NEW ||  currentToken.kind == BANG ||  currentToken.kind == LPAREN){
                _Expression()
                while(currentToken.kind == COMMA){
                  eat(COMMA)
                  _Expression()
                }
              }
              eat(RPAREN)
            }

            case _ => expected(LENGTH, IDKIND)

          } 
                             
        }

      }



    }
    def _Identifier () : Identifier = {
      val id = currentToken
      eat(IDKIND)
      val test : ID = id 
      new Identifier(test.value)
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

