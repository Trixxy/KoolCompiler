package koolc
package lexer

import utils._
import scala.io.Source
import java.io.File

/* Lexer consists of an Iterator of Tokens. */

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  def run(ctx: Context)(f: File): Iterator[Token] = {
    val source = Source.fromFile(f)
    import ctx.reporter._

    // Complete this file
  
    var isEOF = false
    var pos = 0
    
    /** Store the current token, as read from the lexer. */
    var currentChar: Char = ' '
    var nextChar: Char = ' '
    var peekaboo = false
    var currentIsEmpty = false

    //Initialize the nextChar
    if(!source.isEmpty){
      nextChar = source.next
      pos = source.pos
    }
    
    // All keywords mapped to their specified Token
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
    // All symbols mapped to their specified Token
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
      /* False iff last token returned was EOF
       * True otherwise
       */
      def hasNext = {
        !isEOF
      }
 
      /* The next call will return a new Token based on what the next read
       * char from the filestream is.
       */
      def next = {
        // Reads next char 
        def readChar: Unit = {
          //If nextChar can be set as source.next
          if (!source.isEmpty) {
            currentChar = nextChar
            nextChar = source.next
          //else will set currentChar = nextChar
          //and say that there are no more chars to be read
          //for the next readChar call.
          } else if(!peekaboo){
            currentChar = nextChar
            nextChar = ' '
            peekaboo = true
            currentIsEmpty = true
          }
        }
        //Returns a Bool of if c is an ignorable char
        def isIgnorable(c: Char) = {
          c match{
            case ' ' => true
            case '\t' => true
            case '\n' => true
            case _ => false
          }
        }

        /* Definition:
         * State      Descr.
         * 1          ID or keyword
         * 2          INT
         * 3          STRING
         * 4          COMMENT //
         * 5          COMMENT /* */
         * 6          IgnorableChars
         * 7          SYMBOL or BAD
         */
        
        var state = 0
        var sb = new StringBuilder
        
        pos = source.pos
        
        //Do as long as last char hasn't been read.
        //Otherwise return new Token(EOF)
        if(!currentIsEmpty) {
          //DETECT STATE:
          readChar

          if(currentChar.isLetter) {
            state = 1
          } else if(currentChar.isDigit) {
            state = 2
          } else if(currentChar == '"') {
            state = 3
          } else if (currentChar == '/' && (nextChar == '/' || nextChar == '*')) {
            if(nextChar == '/') {
              state = 4
            } else if(nextChar == '*') {
              state = 5
            }
          } else if (isIgnorable(currentChar)) {
            state = 6
          } else {
            state = 7
          }

          if(state == 1 || state == 2 || state == 7) {
            sb.append(currentChar)
          }
          
          state match{
            //ID or Keyword
            case 1 => {
              while(nextChar.isLetterOrDigit || nextChar == '_') {
                readChar
                sb.append(currentChar)
              }

              keywords.get(sb.toString) match {
                case None => new ID(sb.toString).setPos(f, pos)
                case Some(res) => res.setPos(f, pos)
              }
            }
            //INT
            case 2 => {
              if(currentChar != '0') {
                while(nextChar.isDigit) {
                  readChar
                  sb.append(currentChar)
                }
              } 

              new INTLIT(sb.toInt).setPos(f, pos)
            }
            //STRING
            case 3 => {
              while(nextChar != '"') {
                readChar
                sb.append(currentChar)
              }
              readChar

              new STRLIT(sb.toString).setPos(f, pos)
            }
            // COMMENT //
            case 4 => {
              var stop = false
              while(nextChar != '\n' && !source.isEmpty) {
                readChar
              }
              readChar

              next
            }
            // COMMENT /* */
            case 5 => {
              var stop = false
              while(!stop && !source.isEmpty) {
                readChar
                if(nextChar == '*') {
                  readChar
                  if(nextChar == '/') {
                    stop = true
                  }
                }
              }
              readChar

              next
            }
            //IgnorableChar ' ' or '\t' or '\n'
            case 6 => {
              while(isIgnorable(nextChar) && !source.isEmpty) {
                readChar
              }

              next
            }
            //SYMBOL or BAD
            case _ => {
                if(currentChar == '=' && nextChar == '=') {
                  readChar
                  sb.append(currentChar)
                } else if(currentChar == '&' && nextChar == '&') {
                  readChar
                  sb.append(currentChar)
                } else if(currentChar == '|' && nextChar == '|') {
                  readChar
                  sb.append(currentChar)
                }

              symbols.get(sb.toString) match {
                case None => new Token(BAD).setPos(f, pos)
                case Some(res) => res.setPos(f, pos)
              }
            }
          }
        //The hasNext will now return false and a new Token(EOF) is returned
        } else {
          isEOF = true
          new Token(EOF).setPos(f, pos)
        }
      }
    }
  }
}