package koolc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  def run(ctx: Context)(f: File): Iterator[Token] = {
    val source = Source.fromFile(f)
    import ctx.reporter._

    // Complete this file

/*  
    var isEOF = false
    var pos = 0
    
    /** Store the current token, as read from the lexer. */
    var currentChar: Char = ' '
    var nextChar: Char = ' '
    var peekaboo = false

    if(!source.isEmpty){
      nextChar = source.next
      pos = source.pos
    } else {
      isEOF = true
    }
*/

    /**
    Read Mechanics
    **/
    var sp : Array[Char] = Array(' ',' ') //stream pointer
    var ps : Array[Boolean] = Array(false, false) //pointer set
    var sp_pos : Array[Int] = Array(0, 0)
    var eof_delivered = false //a flag to indicate whether eof have been signaled 
    
    def isCurrentEmpty = !ps(0)
    def isNextEmpty = !ps(1)
    def isPeekNext(sym: Char) = ps(1) && (sym == sp(1))
    def isPeekCurrent(sym: Char) = ps(0) && (sym == sp(0))
    def peekCurrent = if(!isCurrentEmpty) sp(0); else throw new NoSuchElementException
    def peekNext = if(ps(1)) sp(1); else throw new NoSuchElementException
    def getCurrentPos = sp_pos(0)
    def popCurrent: Char = {
      if(isCurrentEmpty) throw new NoSuchElementException

      val ret = sp(0)
      if(ps(1)){
        sp(0) = sp(1)
        sp_pos(0) = sp_pos(1)
        if(source.isEmpty) ps(1) = false
        else {sp(1) = source.next; sp_pos(1) = source.pos;}
      } else ps(0) = ps(1)
      ret
    }
    
    for(i <- 0 to 1){
      if(!source.isEmpty){
        sp(i) = source.next
        ps(i) = true
        sp_pos(i) = source.pos
      }else if(i == 1){
        sp_pos(i) = sp_pos(i-1)
      }
    }
    
    //END OF READ MECHANICS
    
    val keywords = Map(
      "object" -> new Token(OBJECT),
      "class" -> new Token(CLASS),
      "def" -> new Token(DEF),
      "var" -> new Token(VAR),
      "Unit" -> new Token(UNIT),
      "main" -> new Token(MAIN),
      "String" -> new Token(STRING),
      "extends" -> new Token(EXTENDS),
      "Int" -> new Token(INT),
      "Bool" -> new Token(BOOLEAN),
      "while" -> new Token(WHILE),
      "if" -> new Token(IF),
      "else" -> new Token(ELSE),
      "return" -> new Token(RETURN),
      "length" -> new Token(LENGTH),
      "true" -> new Token(TRUE),
      "false" -> new Token(FALSE),
      "this" -> new Token(THIS),
      "new" -> new Token(NEW),
      "println" -> new Token(PRINTLN))
    val symbols = Map(
      ":" -> new Token(COLON),
      ";" -> new Token(SEMICOLON),
      "." -> new Token(DOT),
      "," -> new Token(COMMA),
      "=" -> new Token(EQSIGN),
      "==" -> new Token(EQUALS),
      "!" -> new Token(BANG),
      "(" -> new Token(LPAREN),
      ")" -> new Token(RPAREN),
      "[" -> new Token(LBRACKET),
      "]" -> new Token(RBRACKET),
      "{" -> new Token(LBRACE),
      "}" -> new Token(RBRACE),
      "&&" -> new Token(AND),
      "||" -> new Token(OR),
      "<" -> new Token(LESSTHAN),
      "+" -> new Token(PLUS),
      "-" -> new Token(MINUS),
      "*" -> new Token(TIMES),
      "/" -> new Token(DIV))

    new Iterator[Token] {
      def hasNext = {
        !isCurrentEmpty || !eof_delivered
      }
      
      def next = {
        def isIgnorable(c: Char) : Boolean = {
          c match{
            case ' ' => true
            case '\t' => true
            case '\n' => true
            case _ => false
          }
        }

        def lexID() : Token = {
          var sb = new StringBuilder
          val t_pos = getCurrentPos
          
          while((peekCurrent.isLetterOrDigit || isPeekCurrent('_')) && !isCurrentEmpty) {
            sb.append(popCurrent)
          }

          val str = sb.toString

          keywords.get(str) match {
            case None => new ID(str).setPos(f, t_pos)
            case Some(res) => res.setPos(f, t_pos)
          }
        }

        def lexInt(): Token = {
          var sb = new StringBuilder
          val t_pos = getCurrentPos
          
          if(!isPeekCurrent('0')) {
            while(peekCurrent.isDigit && !isCurrentEmpty) {
              sb.append(popCurrent)
            }
          }else{
            sb.append(popCurrent)
          }

          new INTLIT(sb.toInt).setPos(f, t_pos)
        }

        def lexStr(): Token = {
          var sb = new StringBuilder
          val t_pos = getCurrentPos
          
          if(isPeekCurrent('"')) popCurrent //No failsafe; depends on user
          while(!isPeekCurrent('"') && !isPeekCurrent('\n') && !isCurrentEmpty) {
            sb.append(popCurrent)
          }

          if(isCurrentEmpty){
            new Token(BAD).setPos(f, t_pos)
          }else{ /* VERY SENSITIVE CODE; DON'T EDIT; */
            if(popCurrent == '\n'){
              new Token(BAD).setPos(f, t_pos)
            }else{
              new STRLIT(sb.toString).setPos(f, t_pos)
            }
          }

        }

        def lexSLC() {
          while(!isPeekCurrent('\n') && !isCurrentEmpty){
            popCurrent
          }
        }

        def lexMLC() = {
          //OBS: no failsafe
          if(isPeekCurrent('/')) popCurrent
          if(isPeekCurrent('*')) popCurrent
          while(!(isPeekCurrent('*') && isPeekNext('/')) && !isCurrentEmpty){
            popCurrent
          }
        }

        def lexSym(): Token = {
          val t_pos = getCurrentPos
          var ret = new Token(BAD).setPos(f, t_pos)
          
          val one : String = peekCurrent.toString
          if(symbols.contains(one)){
            popCurrent
            ret = symbols.getOrElse(one, new Token(BAD)).setPos(f, t_pos)
          }
          if(!isCurrentEmpty){
            val two = one + peekCurrent.toString
            if(symbols.contains(two)){
              ret = symbols.getOrElse(two, new Token(BAD)).setPos(f, t_pos)
            }
          }
          
          ret
        }

        def lexIgnored() {
          while(isIgnorable(peekCurrent) && !isCurrentEmpty){
            popCurrent
          }
        }
        
        
        //START RUNNING NEXT

        //Get rid of junk
        if(!isCurrentEmpty){
          var nonTokens = true
          while(nonTokens){
            if(isIgnorable(peekCurrent)){
              lexIgnored()
            } else if (isPeekCurrent('/') && !isNextEmpty && isPeekNext('/')) {
            lexSLC()
          } else if (isPeekCurrent('/') && !isNextEmpty && isPeekNext('*')) {
              lexMLC()
            } else {
              nonTokens = false
            }
          }
        }
        
        //Start lexing next token
        if(!isCurrentEmpty){
          if(peekCurrent.isLetter) {
            lexID()
          } else if(peekCurrent.isDigit) {
            lexInt()
          } else if(isPeekCurrent('"')) {
            lexStr()
          } else {
            lexSym()
          }

        }else{ //Returns EOF as many times as the user requires (by design)
          eof_delivered = true
          new Token(EOF).setPos(f, sp_pos(1)+1)
        }
        
      } // Lexer.run.new Iterator.next
    } //Lexer.run.new Iterator
  } // Lexer.run
} // Lexer
