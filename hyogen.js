/**
 * https://github.com/silent-tempest/Hyogen
 *
 * MIT License
 *
 * Copyright (c) 2017-2018 SILENT
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

// jshint esversion: 5
// jshint unused: true
// jshint undef: true
;( function ( window, undefined ) {

'use strict';

var io = {
  print: function ( value ) {
    alert( value );
  },

  scan: function () {
    return prompt( '' );
  }
};

/**
 * #types
 */
var TYPES = {

  /** t_type */
  LITERAL   : 0,
  PUNCTUATOR: 1,
  KEYWORD   : 2,
  IDENTIFIER: 3,
  WHITESPACE: 4,
  EXPRESSION: 5,
  STATEMENT : 6,
  EOF       : 7,

  /** d_type */
  NULL   : 100,
  BOOLEAN: 101,
  STRING : 102,
  NUMBER : 103,
  COLOR  : 104,
  ILLEGAL: 105,

  LPAREN    : 200,
  RPAREN    : 201,
  LBRACK    : 202,
  RBRACK    : 203,
  LBRACE    : 204,
  RBRACE    : 205,
  TYPEOF    : 206,
  NOT       : 207,
  INC       : 208,
  DEC       : 209,
  COMMA     : 210,
  ASSIGN    : 211,
  ASSIGN_ADD: 212,
  ASSIGN_SUB: 213,
  ASSIGN_MUL: 214,
  ASSIGN_DIV: 215,
  OR        : 216,
  AND       : 217,
  ADD       : 218,
  SUB       : 219,
  MUL       : 220,
  DIV       : 221,
  EQ        : 222,
  NE        : 223,
  LT        : 224,
  GT        : 225,
  LTE       : 226,
  GTE       : 227,
  IN        : 228,
  PERIOD    : 229,
  ARROW     : 230,
  CONDITIONAL: 231,
  GROUPING  : 232,
  SEMICOLON : 233,

  /** whitespaces */
  NLINE : 300,
  INDENT: 301,

  /** keyword-tokens */
  DEF      : 400,
  DO       : 401,
  ELSE     : 402,
  FROM     : 403,
  IF       : 404,
  IMPORT   : 405,
  OF       : 407,
  PRINT    : 408,
  RETURN   : 409,
  TO       : 410,
  WHILE    : 411,
  CLASS    : 412,
  NEW      : 416,
  THIS     : 417,
  BREAK    : 418,
  CONTINUE : 419,
  FOR      : 420,
  FOR_TO   : 421,
  SCAN     : 422,
  CALL     : 423,

  /** p_type */
  UNARY   : 500,
  BINARY  : 501,
  TERNARY : 502,
  SEQUENCE: 503

};

var conversions = {
  convert: function ( token, type ) {
    var converter = conversions[ token.d_type ] &&
      conversions[ token.d_type ][ type ];

    if ( converter ) {
      return converter( token );
    }

    throw TypeError( "Can't convert " + get_tag( token ) +
      ' to ' + get_tag( type, true ) );
  }
};

conversions[ TYPES.NULL ] = {};

conversions[ TYPES.NULL ][ TYPES.BOOLEAN ] = function () {
  return TOKENS.FALSE;
};

conversions[ TYPES.NULL ][ TYPES.NUMBER ] = function () {
  return TOKENS.ZERO;
};

conversions[ TYPES.NULL ][ TYPES.STRING ] = function () {
  return STRINGS.NULL;
};

conversions[ TYPES.BOOLEAN ] = {};

conversions[ TYPES.BOOLEAN ][ TYPES.NUMBER ] = function ( value ) {
  return value.value ? TOKENS.ONE : TOKENS.ZERO;
};

conversions[ TYPES.BOOLEAN ][ TYPES.STRING ] = function ( value ) {
  return value.value ? STRINGS.TRUE : STRINGS.FALSE;
};

conversions[ TYPES.NUMBER ] = {};

conversions[ TYPES.NUMBER ][ TYPES.BOOLEAN ] = function ( token ) {
  return value.value ? TOKENS.TRUE : TOKENS.FALSE;
};

conversions[ TYPES.NUMBER ][ TYPES.STRING ] = function ( token ) {
  var value = token.value;

  if ( value !== value ) {
    return STRINGS.NAN;
  }

  /**
   * if value is Infinity
   *   return STRINGS.POS_INFINITY
   * else if value is -Infinity
   *   return STRINGS.NEG_INFINITY
   */

  return '' + value;
};

var cond_exprs = {};

cond_exprs[ TYPES.OR ] =
  cond_exprs[ TYPES.AND ] =
  cond_exprs[ TYPES.CONDITIONAL ] = true;

var create_literal = function ( d_type ) {
  var Literal = function ( value ) {
    this.value = value;
  };

  Literal.prototype = _.create( null );
  Literal.prototype.t_type = TYPES.LITERAL;
  Literal.prototype.d_type = d_type;

  return Literal;
};

var create_stmt = function ( constructor, d_type ) {
  constructor.prototype = _.create( null );
  constructor.prototype.t_type = TYPES.STATEMENT;
  constructor.prototype.d_type = d_type;
  return constructor;
};

var create_expr = function ( constructor, type ) {
  constructor.prototype = _.create( null );
  constructor.prototype.t_type = TYPES.EXPRESSION;
  constructor.prototype.type = type;
  return constructor;
};

var stmt_with_one_arg = function ( type ) {
  return create_stmt( function ( value ) {
    this.value = value;
  }, type );
};

var create_punctuator = function ( p_type ) {
  var Punctuator = function ( value, d_type, priority, rtl ) {
    if ( rtl ) {
      this.rtl = true;
    }

    this.value = value;
    this.d_type = d_type;
    this.priority = priority;
  };

  Punctuator.prototype = _.create( null );
  Punctuator.prototype.t_type = TYPES.PUNCTUATOR;

  if ( p_type !== undefined ) {
    Punctuator.prototype.p_type = p_type;
  }

  return Punctuator;
};

var Function = create_stmt( function ( id, params, body ) {
  this.identifier = id;
  this.parameters = params;
  this.length = new Number( params.length );
  this.body = body;
}, TYPES.DEF );

var Null = function () {};

Null.prototype = _.create( null );
Null.prototype.t_type = TYPES.LITERAL;
Null.prototype.d_type = TYPES.NULL;
Null.prototype.value = null;

var Boolean = create_literal( TYPES.BOOLEAN ),
    Number = create_literal( TYPES.NUMBER );

var String = function ( value ) {
  this.value = value;
  this.length = new Number( value.length );
};

String.prototype.t_type = TYPES.LITERAL;
String.prototype.d_type = TYPES.STRING;

String.prototype.toLowerCase = function () {
  return new String( this.value.toLowerCase() );
};

String.prototype.toUpperCase = function () {
  return new String( this.value.toUpperCase() );
};

var IfStatement = create_stmt( function ( cond, body, alt ) {
  this.condition = cond;
  this.body = body;
  set_if_exists( alt, 'alt', this );
}, TYPES.IF );

var ForToStatement = create_stmt( function ( init, final_expr, body ) {
  this.init = init;
  this.final_expr = final_expr;
  this.body = body;
}, TYPES.FOR_TO );

var set_if_exists = function ( value, name, object ) {
  if ( value ) {
    object[ name ] = value;
  }
};

var ForStatement = create_stmt( function ( init, cond, final_expr, body ) {
  set_if_exists( init, 'init', this );
  set_if_exists( cond, 'condition', this );
  set_if_exists( final_expr, 'final_expr', this );
  this.body = body;
}, TYPES.FOR );

var WhileStatement = create_stmt( function ( cond, body ) {
  this.condition = cond;
  this.body = body;
}, TYPES.WHILE );

var DoWhileStatement = create_stmt( function ( cond, body ) {
  this.condition = cond;
  this.body = body;
}, TYPES.DO );

var PrintStatement = stmt_with_one_arg( TYPES.PRINT ),
    ImportStatement = stmt_with_one_arg( TYPES.IMPORT ),
    ReturnStatement = stmt_with_one_arg( TYPES.RETURN );

var ScanStatement = create_stmt( function ( id ) {
  this.identifier = id;
}, TYPES.SCAN );

var UnaryExpression = create_expr( function ( p, a ) {
  this.d_type = p;
  this.a = a;
}, TYPES.UNARY );

var BinaryExpression = create_expr( function ( p, a, b ) {
  if ( cond_exprs[ this.d_type = p ] ) {
    this.conditional = true;
  }

  this.a = a;
  this.b = b;
}, TYPES.BINARY );

var TernaryExpression = create_expr( function ( p, a, b, c ) {
  if ( cond_exprs[ this.d_type = p ] ) {
    this.conditional = true;
  }

  this.a = a;
  this.b = b;
  this.c = c;
}, TYPES.TERNARY );

var SequenceExpression = create_expr( function ( seq ) {
  this.sequence = seq;
}, TYPES.SEQUENCE );

var CallExpression = create_expr( function ( id, params ) {
  this.identifier = id;
  this.parameters = params;
}, TYPES.CALL );

var BreakStatement = {
  t_type: TYPES.STATEMENT,
  d_type: TYPES.BREAK
};

var ContinueStatement = {
  t_type: TYPES.STATEMENT,
  d_type: TYPES.CONTINUE
};

var Punctuator = create_punctuator(),
    UnaryPunctuator = create_punctuator( TYPES.UNARY ),
    BinaryPunctuator = create_punctuator( TYPES.BINARY );

var Keyword = function ( value, d_type ) {
  this.value = value;
  this.d_type = d_type;
};

Keyword.prototype = _.create( null );
Keyword.prototype.t_type = TYPES.KEYWORD;

var Identifier = function ( value ) {
  this.value = value;
};

Identifier.prototype = _.create( null );
Identifier.prototype.t_type = TYPES.IDENTIFIER;

var Whitespace = function ( d_type ) {
  this.d_type = d_type;
};

Whitespace.prototype = _.create( null );
Whitespace.prototype.t_type = TYPES.WHITESPACE;

/**
 * Returns `true` if `ch`
 * is a digit character.
 * is_digit( '5' ) // -> true
 * is_digit( 'b' ) // -> false
 */
var is_digit = function ( ch ) {
  return ch >= '0' && ch <= '9';
};

/**
 * Returns `true` if `ch`
 * is an IdentifierStart.
 */
var is_identifier_start = function ( ch ) {
  return ch >= 'a' && ch <= 'z' ||
    ch >= 'A' && ch <= 'Z' ||
    ch == '$' || ch == '_';
};

/**
 * class Scanner
 */
var Scanner = function () {};

/**
 * Returns an instance of the `Number`
 * class with the value of the
 * successfully scanned number.
 */
Scanner.prototype.number = function ( number, has_point ) {
  var line = this.line,
    len = this.line_len,
    ch = this.ch;

  for ( ; this.j < len; ++this.j ) {
    if ( !is_digit( ch = line.charAt( this.j ) ) ) {
      if ( ch == '.' ) {
        if ( has_point ) {
          throw SyntaxError( 'Expected digit, got "."' );
        }

        has_point = true;
        number += ch;
      } else if ( number[ number.length - 1 ] == '.' ||
        is_identifier_start( ch ) )
      {
        throw SyntaxError( 'Expected digit, got "' + ch + '"' );
      } else {
        break;
      }
    } else {
      number += ch;
    }
  }

  if ( ch == '.' ) {
    throw SyntaxError( 'Expected number, got "' + this.number + '"' );
  }

  /**
   * If not to do decrement we'll skip
   * symbol after the number, fix that.
   */
  --this.j;

  return new Number( window.Number( number ) );
};

var table = function ( hash ) {
  return _.assign( _.create( null ), hash );
};

var escaping_map = table( {
  'n' : '\n',
  'r' : '\r',
  't' : '\t',
  '\\': '\\',
  '"' : '"',
  "'" : "'"
} );

/**
 * Returns an instance of the `String`
 * class with the value of the
 * successfully scanned string.
 */
Scanner.prototype.string = function ( quote ) {
  var line = this.line,
    len = this.line_len,
    str = '',
    ch;

  for ( ; this.j < len; ++this.j ) {
    ch = line.charAt( this.j );

    if ( ch == quote ) {
      return new String( str );
    }

    if ( ch == '\\' ) {
      if ( ( ch = escaping_map[ line.charAt( this.j + 1 ) ] ) == null ) {
        ch = '';
      }

      ++this.j;
    }

    str += ch;
  }

  throw SyntaxError( 'Unexpected end of line' );
};

/**
 * Returns an instance of the `String`
 * class with the value of the
 * successfully scanned string.
 */
Scanner.prototype.identifier = function ( id ) {
  var line = this.line,
    len = this.line_len,
    ch, token;

  for ( ; this.j <= len; ++this.j ) {
    if ( this.j === len ||
      !is_identifier_start( ch = line.charAt( this.j ) ) &&
      !is_digit( ch ) )
    {
      /**
       * Create and cache new token
       * if not found in `IDENTIFIERS`.
       */
      if ( !( token = IDENTIFIERS[ id ] ) ) {
        token = IDENTIFIERS[ id ] = new Identifier( id );
      }

      break;
    } else {
      id += ch;
    }
  }

  /**
   * If not to do decrement we'll skip
   * symbol after the identifier, fix that.
   */
  --this.j;

  return token;
};

/**
 * Skips the line.
 */
Scanner.prototype.skip = function () {
  this.j = this.line_len;
};

/**
 * Adds spaces or tabs from
 * the beginning of the line.
 * They are needed to determine
 * blocks of code.
 */
Scanner.prototype.indent = function () {
  var k = 1,
    j = this.j,
    tokens = this.tokens,
    line = this.line,
    len = this.line_len,
    ch;

  for ( ; j + k <= len; ++k ) {
    if ( j + k === len ) {
      k = 0;
      break;
    }

    ch = line.charAt( j + k );

    if ( ch !== ' ' && ch !== '\t' ) {
      if ( ch !== '#' ) {
        j += k - 1;
      } else {
        k = 0;
      }

      break;
    }
  }

  /*
   * `k` is `0` when the next
   * iterarion will be on new line.
   */
  this.line_started = !k;

  for ( ; k > 0; --k ) {
    tokens.push( TOKENS.INDENT );
  }
};

/**
 * Returns all tokens found in the `code`.
 * new Scanner().scan( 'a + b' );
 * It returns something like this:
 * [
 *   <Identifier>,
 *   TOKENS.ADD,
 *   <Identifier>,
 *   TOKENS.EOF
 * ]
 */
Scanner.prototype.scan = function ( code ) {
  if ( code == null ) {
    throw TypeError( "Can't scan " + code );
  }

  if ( !( code += '' ).length ) {
    return [];
  }

  var tokens = this.tokens = [],
    lines = this.lines = code.split( /\r?\n/ ),
    lines_len = this.lines_len = lines.length,
    token, next, line, line_len, ch;

  for ( this.i = 0; this.i < lines_len; ++this.i ) {
    this.line_len = line_len =
      ( this.line = line = lines[ this.i ] ).length;
    this.line_started = true;

    for ( this.j = 0; this.j < line_len; ++this.j ) {
      this.ch = ch = line.charAt( this.j );

      if ( ch === ' ' || ch === '\t' ) {
        /**
         * Take the spaces only at
         * the beginning of the line.
         */
        if ( this.line_started ) {
          this.indent();
        }

        continue;
      }

      if ( this.line_started ) {
        this.line_started = false;
      }

      switch ( ch ) {
        case '"':
        case "'":
          ++this.j;
          token = this.string( ch );
          break;

        // "<=", "<"
        case '<':
          if ( ( ch = line.charAt( this.j + 1 ) ) == '=' ) {
            token = TOKENS.LTE;
            ++this.j;
          } else {
            token = TOKENS.LT;
          }

          break;

        // ">=", ">"
        case '>':
          if ( ( ch = line.charAt( this.j + 1 ) ) == '=' ) {
            token = TOKENS.GTE;
            ++this.j;
          } else {
            token = TOKENS.GT;
          }

          break;

        // "==", "=", "=>"
        case '=':
          if ( ( ch = line.charAt( this.j + 1 ) ) === '=' ) {
            ++this.j;
            token = TOKENS.EQ;
          } else if ( ch === '>' ) {
            ++this.j;
            token = TOKENS.ARROW;
          } else {
            token = TOKENS.ASSIGN;
          }

          break;

        // "!=", "!"
        case '!':
          if ( ( ch = line.charAt( this.j + 1 ) ) == '=' ) {
            token = TOKENS.NE;
            ++this.j;
          } else {
            token = TOKENS.NOT;
          }

          break;

        // "+=", "++", "+"
        case '+':
          if ( ( ch = line.charAt( this.j + 1 ) ) == '=' ) {
            token = TOKENS.ASSIGN_ADD;
            ++this.j;
          } else if ( ch === '+' ) {
            token = TOKENS.INC;
            ++this.j;
          } else {
            token = TOKENS.ADD;
          }

          break;

        // "-=", "--", "-"
        case '-':
          if ( ( ch = line.charAt( this.j + 1 ) ) == '=' ) {
            token = TOKENS.ASSIGN_SUB;
            ++this.j;
          } else if ( ch === '-' ) {
            token = TOKENS.DEC;
            ++this.j;
          } else {
            token = TOKENS.SUB;
          }

          break;

        // "*=", "*"
        case '*':
          if ( ( ch = line.charAt( this.j + 1 ) ) == '=' ) {
            token = TOKENS.ASSIGN_MUL;
            ++this.j;
          } else {
            token = TOKENS.MUL;
          }

          break;

        // "/=", "/"
        case '/':
          if ( ( ch = line.charAt( this.j + 1 ) ) == '=' ) {
            token = TOKENS.ASSIGN_DIV;
            ++this.j;
          } else {
            token = TOKENS.DIV;
          }

          break;

        // "#" ( as comment )
        case '#':
          this.skip(); break;

        // "." ( as number floating point ) or accessor
        case '.':
          next = line.charAt( this.j + 1 );

          if ( is_digit( next ) ) {
            this.j += 2;
            this.number( ch + next, true );
          } else {
            token = TOKENS.PERIOD;
          }

          break;

        case ',':
          token = TOKENS.COMMA; break;
        case '(':
          token = TOKENS.LPAREN; break;
        case ')':
          token = TOKENS.RPAREN; break;
        case '[':
          token = TOKENS.LBRACK; break;
        case ']':
          token = TOKENS.RBRACK; break;
        case '{':
          token = TOKENS.LBRACE; break;
        case '}':
          token = TOKENS.RBRACE; break;
        case ';':
          token = TOKENS.SEMICOLON; break;

        default:
          if ( is_identifier_start( ch ) ) {
            ++this.j;
            token = this.identifier( ch );
          } else if ( is_digit( ch ) ) {
            ++this.j;
            token = this.number( ch );
          } else {
            throw SyntaxError( 'Unexpected "' + ch + '"' );
          }
      }

      if ( token ) {
        // token.line = this.i + 1;
        // token.column = this.j + 1;
        tokens.push( token );
        token = null;
      }
    }

    if ( tokens.length &&
      tokens[ tokens.length - 1 ] !== TOKENS.NLINE )
    {
      tokens.push( TOKENS.NLINE );
    }
  }

  /**
   * tokens[ tokens.length - 1 ] is
   * the `TOKENS.NLINE`, this token
   * isn't needed, but we need
   * the `TOKENS.EOF` to specify
   * the end of the file.
   */
  tokens[ tokens.length - 1 ] = TOKENS.EOF;

  return tokens;
};

/**
 * class Parser
 */
var Parser = function () {};

/**
 * Sorts punctuators by priority.
 */
Parser.sort = function ( a, b ) {
  return a[ 0 ].token.priority - b[ 0 ].token.priority;
};

/**
 * Returns the expression created
 * by the `punctuator`.
 */
Parser.expression = function ( punctuator, tokens ) {
  var token = punctuator.token;
  var len = tokens.length;

  switch ( token.p_type ) {
    case TYPES.UNARY:
      throw SyntaxError();

    case TYPES.BINARY:
      var a, b;

      for ( var j = punctuator.index - 1; j >= 0; --j ) {
        if ( ( a = tokens[ j ] ) ) {
          tokens[ j ] = null;

          if ( a.t_type !== TYPES.WHITESPACE && a.t_type !== TYPES.EOF ) {
            break;
          }

          if ( !j ) {
            a = null;
          }
        }
      }

      if ( !a ) {
        throw SyntaxError( 'Unexpected left operand for "' +
          token.value + '"' );
      }
        
      for ( var k = punctuator.index + 1; k < len; ++k ) {
        if ( ( b = tokens[ k ] ) ) {
          tokens[ k ] = null;

          if ( b.t_type !== TYPES.WHITESPACE && b.t_type !== TYPES.EOF ) {
            break;
          }

          if ( k >= len - 1 ) {
            b = null;
          }
        }
      }

      if ( !b ) {
        throw SyntaxError( 'Unexpected right operand for "' +
          token.value + '"' );
      }

      tokens[ punctuator.index ] =
        new BinaryExpression( token.d_type, a, b );

      break;

      case TYPES.TERNARY:
        throw SyntaxError();
    }
};

/**
 * class TempPunctuator
 */
var TempPunctuator = function ( token, index ) {
  this.token = token;
  this.index = index;
};

var p_hooks = {};

p_hooks[ TYPES.ASSIGN_ADD ] =
  p_hooks[ TYPES.ASSIGN_SUB ] =
  p_hooks[ TYPES.ASSIGN_MUL ] =
  p_hooks[ TYPES.ASSIGN_DIV ] = TYPES.ASSIGN;

/**
 * Constructs expressions
 * from an array `tokens`.
 * Note: this function mutates `tokens`.
 */
Parser.prototype.expressions = function ( tokens ) {
  var types = [];
  var added = {};
  var j, len;

  for ( var k = tokens.length - 1; k >= 0; --k ) {
    var token = tokens[ k ];

    if ( token.t_type === TYPES.PUNCTUATOR ) {
      var type = p_hooks[ token.d_type ] || token.d_type;

      if ( added[ type ] == null ) {
        added[ type ] = types.length;

        types.push( [
          new TempPunctuator( token, k )
        ] );
      } else {
        types[ added[ type ] ].push( new TempPunctuator( token, k ) );
      }
    }
  }

  types.sort( Parser.sort );

  for ( var i = types.length - 1; i >= 0; --i ) {
    var punctuators = types[ i ];

    if ( punctuators[ 0 ].token.rtl ) {
      for ( j = 0, len = punctuators.length; j < len; ++j ) {
        Parser.expression( punctuators[ j ], tokens );
      }
    } else {
      for ( j = punctuators.length - 1; j >= 0; --j ) {
        Parser.expression( punctuators[ j ], tokens );
      }
    }
  }

  return _.compact( tokens );
};

/**
 * Returns an array of a sequence
 * separated by the `TYPES.COMMA`.
 */
Parser.prototype.sequence = function ( pred ) {
  var seq = [],
    len = this.length,
    tokens = this.tokens,
    token;

  while ( this.i < len ) {
    if ( !pred( token = tokens[ this.i ] ) ) {
      break;
    }

    seq.push( token );

    if ( tokens[ ++this.i ].d_type !== TYPES.COMMA ) {
      break;
    }

    ++this.i;
  }

  return seq;
};

/**
 * Returns code block.
 */
Parser.prototype.block = function ( lvl, stmt ) {
  expected( this.next(), 'd_type', TYPES.NLINE );
  this.indent();

  if ( this.level < lvl ) {
    throw SyntaxError( 'Empty ' + stmt + ' statement body' );
  }

  return this.body();
};

/**
 * Returns `true` if the `token`
 * is a FunctionDefinitionParameter.
 */
var is_param = function ( token ) {
  return token.t_type === TYPES.IDENTIFIER ||
    token.d_type === TYPES.ASSIGN;
};

/**
 * Changes position of the "cursor"
 * in the `tokens`, and
 * returns next token.
 */
Parser.prototype.next = function () {
  return this.tokens[ ++this.i ];
};

/**
 * Returns successfully parsed Function.
 *
 * 'def' Identifier '(' [ Parameters ] ')' NLine
 *   Body
 * 'def' Identifier '(' [ Parameters ] ')' '=>'
 *   Expression ( NLine | EOF )
 * 'def' Identifier Parameter NLine
 *   Body
 * 'def' Identifier Parameter '=>'
 *   Expression ( NLine | EOF )
 *
 * Parameters:
 *   One or more Parameter separated by comma.
 *
 * Parameter:
 *   Identifier | AssignExpression
 */
Parser.prototype[ TYPES.DEF ] = function () {
  var id = expected( this.next(), 't_type', TYPES.IDENTIFIER ),
    lvl = this.level,
    tok = this.next(),
    params, body;

  if ( tok.d_type === TYPES.LPAREN ) {
    ++this.i;
    params = this.sequence( is_param );
    expected( this.tokens[ this.i ], 'd_type', TYPES.RPAREN );
  } else if ( is_param( tok ) ) {
    params = [ tok ];
  } else {
    unexpected_token( tok );
  }

  switch ( this.next().d_type ) {
    case TYPES.NLINE:
      this.indent();

      if ( this.level <= lvl ) {
        throw SyntaxError( 'Empty "def" body' );
      }

      body = this.body();
      break;

    case TYPES.ARROW:
      body = [
        new ReturnStatement( expected_expr( this.next() ) )
      ];

      tok = this.next();

      if ( tok.d_type !== TYPES.NLINE &&
        tok.t_type !== TYPES.EOF )
      {
        unexpected_token( tok );
      }

      break;

    default:
      unexpected_token( this.tokens[ this.i ] );
  }

  return new Function( id, params, body );
};

/**
 * Returns successfully parsed PrintStatement.
 */
Parser.prototype[ TYPES.PRINT ] = function () {
  return new PrintStatement( expected_expr( this.next() ) );
};

/**
 * Returns successfully parsed ImportStatement.
 */
Parser.prototype[ TYPES.IMPORT ] = function () {
  return new ImportStatement( expected_expr( this.next() ) );
};

/**
 * Returns successfully parsed ReturnStatement.
 */
Parser.prototype[ TYPES.RETURN ] = function () {
  return new ReturnStatement( expected_expr( this.next() ) );
};

/**
 * 'scan' Identifier
 */
Parser.prototype[ TYPES.SCAN ] = function () {
  return new ScanStatement( expected(
    this.next(), 't_type', TYPES.IDENTIFIER ) );
};

/**
 * Returns successfully parsed IfStatement.
 * 'if' Expression NLine Body [ 'else' Body ]
 */
Parser.prototype[ TYPES.IF ] = function () {
  var tokens = this.tokens,
    lvl = this.level,
    cond = expected_expr( this.next() ),
    body;

  expected( this.next(), 'd_type', TYPES.NLINE );
  this.indent();

  if ( this.level <= lvl ) {
    throw SyntaxError( 'Empty "if" body' );
  }

  body = this.body();

  if ( this.level === lvl &&
    ( this.i + 1 >= this.length ||
    tokens[ this.i + 1 ].d_type !== TYPES.ELSE ) )
  {
    return new IfStatement( cond, body );
  }

  // skip "else"
  this.i += 2;
  this.indent();
  ++this.level;
  return new IfStatement( cond, body, this.body() );
};

/**
 * Returns successfully parsed WhileStatement.
 * 'while' Expression NLine Body
 */
Parser.prototype[ TYPES.WHILE ] = function () {
  var tokens = this.tokens,
    lvl = this.level,
    cond = tokens[ ++this.i ];

  if ( tokens[ ++this.i ].d_type !== TYPES.NLINE ||
    cond.t_type !== TYPES.LITERAL &&
    cond.t_type !== TYPES.IDENTIFIER &&
    cond.t_type !== TYPES.EXPRESSION )
  {
    throw SyntaxError();
  }

  this.indent();

  if ( this.level <= lvl ) {
    throw SyntaxError( 'Empty "while" body' );
  }

  return new WhileStatement( cond, this.body() );
};

/**
 * Returns successfully parsed DoWhileStatement.
 * 'do' NLine Body 'while' Expression ( NLine | EOF )
 */
Parser.prototype[ TYPES.DO ] = function () {
  var tokens = this.tokens,
    lvl = this.level,
    cond, body, term;

  expected( tokens[ ++this.i ], 'd_type', TYPES.NLINE );
  this.indent();

  if ( this.level <= lvl ) {
    throw SyntaxError( 'Empty "do-while" body' );
  }

  body = this.body();
  expected( tokens[ ++this.i ], 'd_type', TYPES.WHILE );
  cond = tokens[ ++this.i ];
  term = tokens[ this.i + 1 ];

  if ( term.d_type !== TYPES.NLINE &&
    term.t_type !== TYPES.EOF ||
    cond.t_type !== TYPES.LITERAL &&
    cond.t_type !== TYPES.IDENTIFIER &&
    cond.t_type !== TYPES.EXPRESSION )
  {
    throw SyntaxError();
  }

  return new DoWhileStatement( cond, body );
};

/**
 * Returns successfully parsed
 * one this statements:
 *
 * ForToStatement:
 *   'for' ( Identifier | AssignExpression ) 'to' Expression NLine Body
 * ForOfStatement:
 *   'for' Identifier 'of' Expression NLine Body
 * ForInStatement:
 *   'for' Identifier 'in' Expression NLine Body
 * ForStatement:
 *   'for' '(' Expression ';' Expression ';' Expression ')' NLine Body
 */
Parser.prototype[ TYPES.FOR ] = function () {
  var lvl = this.level,
    a = this.next(),
    b = this.next(),
    c;

  switch ( b.d_type ) {
    case TYPES.TO:
      if ( a.t_type !== TYPES.EXPRESSION ?
        a.t_type !== TYPES.IDENTIFIER :
        a.d_type !== TYPES.ASSIGN &&
        a.d_type !== TYPES.ASSIGN_ADD &&
        a.d_type !== TYPES.ASSIGN_SUB &&
        a.d_type !== TYPES.ASSIGN_MUL &&
        a.d_type !== TYPES.ASSIGN_DIV )
      {
        unexpected_token( a );
      }

      c = this.next();

      if ( c.t_type !== TYPES.LITERAL &&
        c.t_type !== TYPES.IDENTIFIER &&
        c.t_type !== TYPES.EXPRESSION )
      {
        unexpected_token( c );
      }

      return new ForToStatement( a, c, this.block( lvl, 'for-to' ) );

    case TYPES.IN:
      throw SyntaxError();
    case TYPES.OF:
      throw SyntaxError();
  }

  // Parse ForStatement.
  expected( a, 'd_type', TYPES.LPAREN );
  // The Initialization.
  a = this.not_required_expr( b, TYPES.SEMICOLON );
  // The Condition.
  b = this.not_required_expr( this.next(), TYPES.SEMICOLON );
  // The FinalExpression.
  c = this.not_required_expr( this.next(), TYPES.RPAREN );
  return new ForStatement( a, b, c, this.block( lvl, 'for' ) );
};

Parser.prototype.not_required_expr = function ( token, req ) {
  var expr;

  if ( token.d_type !== req ) {
    expr = expected_expr( token );
    token = this.next();
  }

  expected( token, 'd_type', req );
 
  return expr;
};

var get_tag = function ( token, is_type ) {
  return TAGS[ is_type ? token : token.d_type ] || TAGS[ TYPES.ILLEGAL ];
};

var TAGS = _.create( null );

TAGS[ TYPES.NULL ]    = '<Null>';
TAGS[ TYPES.BOOLEAN ] = '<Boolean>';
TAGS[ TYPES.NUMBER ]  = '<Number>';
TAGS[ TYPES.STRING ]  = '<String>';
TAGS[ TYPES.COLOR ]   = '<Color>';
TAGS[ TYPES.DEF ]     = '<Function>';
TAGS[ TYPES.ILLEGAL ] = '<Illegal>';

var valid_cond = _.create( null );

valid_cond[ TYPES.NULL ] =
  valid_cond[ TYPES.BOOLEAN ] =
  valid_cond[ TYPES.NUMBER ] =
  valid_cond[ TYPES.STRING ] = true;

var check_cond = function ( token ) {
  if ( !valid_cond[ token.d_type ] ) {
    throw TypeError( 'Expected boolean, got ' + get_tag( token ) );
  }

  return token;
};

var unexpected_token = function ( token ) {
  throw SyntaxError( 'Unexpected token "' + token.value + '"' );
};

var expected = function ( token, type, expected ) {
  if ( token[ type ] !== expected ) {
    unexpected_token( token );
  }

  return token;
};

var expected_expr = function ( token ) {
  if ( !is_expr( token ) ) {
    unexpected_token( token );
  }

  return token;
};

var is_expr = function ( token ) {
  return token.t_type === TYPES.LITERAL ||
    token.t_type === TYPES.IDENTIFIER ||
    token.t_type === TYPES.EXPRESSION;
};

Parser.prototype[ TYPES.BREAK ] = function () {
  return BreakStatement;
};

Parser.prototype[ TYPES.CONTINUE ] = function () {
  return ContinueStatement;
};

/**
 * Counts the current level of
 * indentation in current line.
 */
Parser.prototype.indent = function ( continues ) {
  var tokens = this.tokens,
    len = this.length;

  if ( tokens[ this.i ].d_type === TYPES.NLINE ) {
    ++this.i;
  }

  this.level = 0;

  for ( ; this.i < len &&
    tokens[ this.i ].d_type === TYPES.INDENT;
    ++this.i, ++this.level );

  // fix skipping token after indent
  if ( continues ) {
    --this.i;
  }
};

Parser.prototype.body = function () {
  var tokens = this.tokens,
    len = this.length,
    lvl = this.level,
    body = [],
    token, value;

  if ( this.level < lvl ) {
    return body;
  }

  for ( ; this.i < len; ++this.i ) {
    value = null;
    token = tokens[ this.i ];

    switch ( token.t_type ) {
      case TYPES.EXPRESSION:
        value = token; break;
      case TYPES.KEYWORD:
        value = this[ token.d_type ](); break;
      case TYPES.WHITESPACE:
        this.indent( body.length > 0 ); break;

      case TYPES.LITERAL:
      case TYPES.IDENTIFIER:
        if ( tokens[ this.i - 1 ] &&
          tokens[ this.i - 1 ].t_type !== TYPES.WHITESPACE )
        {
          throw SyntaxError( 'Unexpected token "' + token.value + '"' );
        }

        break;

      case TYPES.EOF:
        return body;
      default:
        throw TypeError();
    }

    if ( value ) {
      body.push( value );
    }

    if ( this.level < lvl ) {
      return body;
    }
  }

  return body;
};

/**
 * Returns Abstract Syntax Tree.
 * https://en.m.wikipedia.org/wiki/Abstract_syntax_tree
 */
Parser.prototype.ast = function ( tokens ) {
  this.tokens = this.expressions( tokens );
  this.length = this.tokens.length;
  this.level = this.i = 0;
  return this.body();
};

/**
 * class Scope
 */
var EmptyScope = function () {};
EmptyScope.prototype = _.create( null );

/**
 * class GlobalScope
 */
var GlobalScope = function () {};
GlobalScope.prototype = _.create( null );

/**
 * class ScopeManager
 */
var ScopeManager = function ( scope ) {
  this.scope = scope;
};

ScopeManager.prototype.get = function ( identifier ) {
  if ( identifier.t_type !== TYPES.IDENTIFIER ) {
    throw TypeError( identifier.value + ' is not an identifier' );
  }

  var value = this.scope[ identifier.value ];

  if ( !value ) {
    throw ReferenceError( identifier.value + ' is not defined' );
  }

  return value;
};

ScopeManager.prototype.set = function ( identifier, value ) {
  if ( identifier.t_type !== TYPES.IDENTIFIER ) {
    throw TypeError( 'Invalid assignment left-hand side' );
  }

  switch ( value.t_type ) {
    case TYPES.LITERAL:
    case TYPES.STATEMENT:
      return ( this.scope[ identifier.value ] = value );
    case TYPES.IDENTIFIER:
      return ( this.scope[ identifier.value ] = this.get( value ) );
  }

  throw TypeError( 'Expected expression, got "' + value.value + '"' );
};

ScopeManager.prototype.data = function ( token ) {
  switch ( token.t_type ) {
    case TYPES.LITERAL:
      return token;
    case TYPES.IDENTIFIER:
      return this.get( token );
  }

  throw TypeError( token.value + ' is not a literal' );
};

/**
 * class Runtime
 */
var Runtime = function () {
  this.scanner = new Scanner();
  this.parser = new Parser();
  this.scope = new GlobalScope();
  this.scope_manager = new ScopeManager( this.scope );
};

Runtime.prototype.loop_scope = 0;

Runtime.prototype.break_loop =
  Runtime.prototype.continue_loop = false;

Runtime.prototype[ 'eval' ] = function ( code ) {
  return this.run( this.parser.ast( this.scanner.scan( code ) ) ), this;
};

Runtime.prototype.expression = function ( expression ) {
  if ( expression.t_type !== TYPES.EXPRESSION ) {
    return expression;
  }

  switch ( expression.type ) {
    case TYPES.UNARY:
      return this[ expression.d_type ](
        this.expression( expression.a ) );
    case TYPES.BINARY:
      return expression.conditional ?
        this[ expression.d_type ]( expression.a, expression.b ) :
        this[ expression.d_type ](
          this.expression( expression.a ),
          this.expression( expression.b ) );
    case TYPES.TERNARY:
      return expression.conditional ?
        this[ expression.d_type ](
          expression.a, expression.b, expression.c ) :
        this[ expression.d_type ](
          this.expression( expression.a ),
          this.expression( expression.b ),
          this.expression( expression.c ) );
  }

  throw TypeError();
};

Runtime.prototype.run = function ( ast ) {
  var i = 0,
      len = ast.length,
      token;

  for ( ; i < len; ++i ) {
    if ( this.break_loop || this.continue_loop ) {
      return;
    }

    switch ( ( token = ast[ i ] ).t_type ) {
      case TYPES.EXPRESSION:
        this.expression( token ); break;
      case TYPES.STATEMENT:
        this[ token.d_type ]( token ); break;
      case TYPES.IDENTIFIER:
        this.scope_manager.get( token ); break;
      case TYPES.LITERAL:
        break;
    }
  }
};

Runtime.prototype.data = function ( token ) {
  if ( token.t_type === TYPES.IDENTIFIER ) {
    token = this.scope_manager.get( token );
  } else if ( token.t_type === TYPES.EXPRESSION ) {
    token = this.expression( token );
  } else if ( token.t_type !== TYPES.LITERAL ) {
    throw SyntaxError();
  }

  return token;
};

Runtime.prototype[ TYPES.ASSIGN ] = function ( a, b ) {
  return this.scope_manager.set( a, b );
};

Runtime.prototype[ TYPES.ASSIGN_ADD ] = function ( a, b ) {
  return this.scope_manager.set( a,
    this[ TYPES.ADD ]( this.scope_manager.get( a ), b ) );
};

Runtime.prototype[ TYPES.ASSIGN_SUB ] = function ( a, b ) {
  return this.scope_manager.set( a,
    this[ TYPES.SUB ]( this.scope_manager.get( a ), b ) );
};

Runtime.prototype[ TYPES.ASSIGN_MUL ] = function ( a, b ) {
  return this.scope_manager.set( a,
    this[ TYPES.MUL ]( this.scope_manager.get( a ), b ) );
};

Runtime.prototype[ TYPES.ASSIGN_DIV ] = function ( a, b ) {
  return this.scope_manager.set( a,
    this[ TYPES.DIV ]( this.scope_manager.get( a ), b ) );
};

Runtime.prototype[ TYPES.ADD ] = function ( a, b ) {
  a = this.scope_manager.data( a );
  b = this.scope_manager.data( b );

  return new ( a.d_type === TYPES.STRING || b.d_type === TYPES.STRING ?
    String : Number )( a.value + b.value );
};

Runtime.prototype[ TYPES.SUB ] = function ( a, b ) {
  a = this.scope_manager.data( a );
  b = this.scope_manager.data( b );

  return new ( a.d_type === TYPES.STRING || b.d_type === TYPES.STRING ?
    String : Number )( a.value - b.value );
};

Runtime.prototype[ TYPES.MUL ] = function ( a, b ) {
  a = this.scope_manager.data( a );
  b = this.scope_manager.data( b );

  return new ( a.d_type === TYPES.STRING || b.d_type === TYPES.STRING ?
    String : Number )( a.value * b.value );
};

Runtime.prototype[ TYPES.DIV ] = function ( a, b ) {
  a = this.scope_manager.data( a );
  b = this.scope_manager.data( b );

  return new ( a.d_type === TYPES.STRING || b.d_type === TYPES.STRING ?
    String : Number )( a.value / b.value );
};

Runtime.prototype[ TYPES.LT ] = function ( a, b ) {
  a = this.scope_manager.data( a );
  b = this.scope_manager.data( b );

  if ( a.d_type !== b.d_type ) {
    return TOKENS.FALSE;
  }

  switch ( a.d_type ) {
    case TYPES.NULL:
    case TYPES.BOOLEAN:
    case TYPES.NUMBER:
    case TYPES.STRING:
      return a.value < b.value ? TOKENS.TRUE : TOKENS.FALSE;
  }

  throw TypeError();
};

Runtime.prototype[ TYPES.GT ] = function ( a, b ) {
  a = this.scope_manager.data( a );
  b = this.scope_manager.data( b );

  if ( a.d_type !== b.d_type ) {
    return TOKENS.FALSE;
  }

  switch ( a.d_type ) {
    case TYPES.NULL:
    case TYPES.BOOLEAN:
    case TYPES.NUMBER:
    case TYPES.STRING:
      return a.value > b.value ? TOKENS.TRUE : TOKENS.FALSE;
  }

  throw TypeError();
};

Runtime.prototype[ TYPES.LTE ] = function ( a, b ) {
  a = this.scope_manager.data( a );
  b = this.scope_manager.data( b );

  if ( a.d_type !== b.d_type ) {
    return TOKENS.FALSE;
  }

  switch ( a.d_type ) {
    case TYPES.NULL:
    case TYPES.BOOLEAN:
    case TYPES.NUMBER:
    case TYPES.STRING:
      return a.value <= b.value ? TOKENS.TRUE : TOKENS.FALSE;
  }

  throw TypeError();
};

Runtime.prototype[ TYPES.GTE ] = function ( a, b ) {
  a = this.scope_manager.data( a );
  b = this.scope_manager.data( b );

  if ( a.d_type !== b.d_type ) {
    return TOKENS.FALSE;
  }

  switch ( a.d_type ) {
    case TYPES.NULL:
    case TYPES.BOOLEAN:
    case TYPES.NUMBER:
    case TYPES.STRING:
      return a.value >= b.value ? TOKENS.TRUE : TOKENS.FALSE;
  }

  throw TypeError();
};

Runtime.prototype[ TYPES.EQ ] = function ( a, b ) {
  a = this.scope_manager.data( a );
  b = this.scope_manager.data( b );

  if ( a.d_type !== b.d_type ) {
    return TOKENS.FALSE;
  }

  switch ( a.d_type ) {
    case TYPES.NULL:
      return TOKENS.TRUE;
    case TYPES.BOOLEAN:
    case TYPES.NUMBER:
    case TYPES.STRING:
      return a.value === b.value ? TOKENS.TRUE : TOKENS.FALSE;
  }

  throw TypeError();
};

Runtime.prototype[ TYPES.NE ] = function ( a, b ) {
  a = this.scope_manager.data( a );
  b = this.scope_manager.data( b );

  if ( a.d_type !== b.d_type ) {
    return TOKENS.TRUE;
  }

  switch ( a.d_type ) {
    case TYPES.NULL:
      return TOKENS.FALSE;
    case TYPES.BOOLEAN:
    case TYPES.NUMBER:
    case TYPES.STRING:
      return a.value !== b.value ? TOKENS.TRUE : TOKENS.FALSE;
  }

  throw TypeError();
};

Runtime.prototype[ TYPES.COND ] = function ( cond, a, b ) {
  switch ( ( cond = check_cond( this.scope_manager.data(
    this.expression( cond ) ) ) ).d_type )
  {
    case TYPES.NULL:
      return check_cond( this.expression( b ) );
    case TYPES.BOOLEAN:
    case TYPES.NUMBER:
    case TYPES.STRING:
      return cond.value ?
        check_cond( this.expression( a ) ) :
        check_cond( this.expression( b ) );
  }
};

Runtime.prototype[ TYPES.OR ] = function ( a, b ) {
  switch ( ( a = check_cond( this.scope_manager.data(
    this.expression( a ) ) ) ).d_type )
  {
    case TYPES.NULL:
      return b;
    case TYPES.BOOLEAN:
    case TYPES.NUMBER:
      return a.value ?
        a : check_cond( this.expression( b ) );
  }
};

Runtime.prototype[ TYPES.AND ] = function ( a, b ) {
  switch ( ( a = check_cond( this.scope_manager.data(
    this.expression( a ) ) ) ).d_type )
  {
    case TYPES.NULL:
      return a;
    case TYPES.BOOLEAN:
    case TYPES.NUMBER:
    case TYPES.STRING:
      return a.value ?
        check_cond( this.expression( b ) ) : a;
  }
};

Runtime.prototype[ TYPES.PERIOD ] = function ( a, b ) {
  expected( b, 't_type', TYPES.IDENTIFIER );
  a = this.scope_manager.data( a );

  if ( a.d_type === TYPES.NULL ) {
    throw TypeError( "Can't read property " +
      b.value + " of " + a.value );
  }

  return a[ b.value ] || TOKENS.NULL;
};

Runtime.prototype[ TYPES.FOR ] = function ( stmt ) {
  if ( stmt.init ) {
    this.data( stmt.init );
  }

  ++this.loop_scope;

  while ( !stmt.condition ||
    check_cond( this.data( stmt.condition ) ).value )
  {
    this.run( stmt.body );
    this.check_continue();

    if ( this.check_break() ) {
      break;
    }

    if ( stmt.final_expr ) {
      this.data( stmt.final_expr );
    }
  }

  --this.loop_scope;
};

/**
 * Bug: if we change the `begin` value
 * in the scope, then the reference
 * to `begin` will no longer point
 * to a value from the scope.
 */
Runtime.prototype[ TYPES.FOR_TO ] = function ( stmt ) {
  var body = stmt.body,
    end = this.data( stmt.final_expr ),
    begin;

  if ( stmt.init.t_type === TYPES.EXPRESSION ) {
    begin = this.expression( stmt.init );
  } else {
    begin = this.scope_manager.get( stmt.init );
  }

  ++this.loop_scope;

  if ( end.value < begin.value ) {
    for ( ; begin.value > end.value; --begin.value ) {
      this.run( body );
      this.check_continue();

      if ( this.check_break() ) {
        break;
      }
    }
  } else {
    for ( ; begin.value < end.value; ++begin.value ) {
      this.run( body );
      this.check_continue();

      if ( this.check_break() ) {
        break;
      }
    }
  }

  --this.loop_scope;
};

Runtime.prototype[ TYPES.IF ] = function ( statement ) {
  if ( check_cond( this.data( statement.condition ) ).value ) {
    this.run( statement.body );
  } else if ( statement.alt ) {
    this.run( statement.alt );
  }
};

Runtime.prototype[ TYPES.DEF ] = function ( statement ) {
  this.scope_manager.set( statement.identifier, statement );
};

Runtime.prototype[ TYPES.CALL ] = function ( call ) {
  var def = this.scope_manager.get( call.identifier );

  if ( def.d_type !== TYPES.DEF ) {
    throw TypeError( call.identifier + ' is not a function' );
  }

  def.runtime = new Runtime( this );
};

Runtime.prototype[ TYPES.WHILE ] = function ( statement ) {
  ++this.loop_scope;

  while ( check_cond( this.data( statement.condition ) ).value ) {
    this.run( statement.body );
    this.check_continue();

    if ( this.check_break() ) {
      break;
    }
  }

  --this.loop_scope;
};

Runtime.prototype[ TYPES.DO ] = function ( statement ) {
  ++this.loop_scope;

  do {
    this.run( statement.body );
    this.check_continue();

    if ( this.check_break() ) {
      break;
    }
  } while ( check_cond( this.data( statement.condition ) ).value );

  --this.loop_scope;
};

Runtime.prototype[ TYPES.BREAK ] = function () {
  if ( !this.loop_scope ) {
    throw SyntaxError( 'Illegal break statement' );
  }

  this.break_loop = true;
};

Runtime.prototype[ TYPES.CONTINUE ] = function () {
  if ( !this.loop_scope ) {
    throw SyntaxError( 'Illegal continue statement' );
  }

  this.continue_loop = true;
};

Runtime.prototype[ TYPES.PRINT ] = function ( statement ) {
  var value = this.data( statement.value );

  switch ( value.d_type ) {
    case TYPES.DEF:
      value = TAGS[ value.d_type ]; break;
    default:
      value = value.value;
  }

  io.print( value );
};

Runtime.prototype[ TYPES.SCAN ] = function ( stmt ) {
  /* var that = this;

  io.scan().then( function ( value ) {
    that.scope_manager.set( stmt.identifier, new String( value ) );
  } ); */

  this.scope_manager.set( stmt.identifier, new String( io.scan() ) );
};

Runtime.prototype[ TYPES.IMPORT ] = function ( statement ) {
  this[ 'eval' ]( _.file( this.data( statement.value ).value ) );
};

Runtime.prototype.check_continue = function () {
  return this.continue_loop &&
    ( this.continue_loop = false, true );
};

Runtime.prototype.check_break = function () {
  return this.break_loop &&
    ( this.break_loop = false, true );
};

/**
 * #tokens
 */
var TOKENS = {

  /**
   * #literal-tokens
   */
  NULL    : new Null(),
  TRUE    : new Boolean( true ),
  FALSE   : new Boolean( false ),
  INFINITY: new Number( 1 / 0 ),
  NAN     : new Number( 0 / 0 ),
  ONE     : new Number( 1 ),
  ZERO    : new Number( 0 ),
  STRING  : new String( '' ),

  /**
   * #punctuator-tokens
   */
  LPAREN: new Punctuator( '(', TYPES.LPAREN, 0 ),
  RPAREN: new Punctuator( ')', TYPES.RPAREN, 0 ),
  LBRACK: new Punctuator( '[', TYPES.LBRACK, 0 ),
  RBRACK: new Punctuator( ']', TYPES.RBRACK, 0 ),
  LBRACE: new Punctuator( '{', TYPES.LBRACE, 0 ),
  RBRACE: new Punctuator( '}', TYPES.RBRACE, 0 ),
  COMMA : new Punctuator( ',', TYPES.COMMA,  1 ),
  ARROW : new Punctuator( '=>', TYPES.ARROW, 0 ),
  SEMICOLON: new Punctuator( ';', TYPES.SEMICOLON, 0 ),

  /**
   * #unary-operator-tokens
   */
  TYPEOF : new UnaryPunctuator( 'typeof', TYPES.TYPEOF,  0, true ),
  NOT    : new UnaryPunctuator( '!',      TYPES.NOT,     0, true ),
  INC    : new UnaryPunctuator( '++',     TYPES.INC,     0 ),
  DEC    : new UnaryPunctuator( '--',     TYPES.DEC,     0 ),

  /**
   * #assignment-operator-tokens
   */
  ASSIGN    : new BinaryPunctuator( '=',  TYPES.ASSIGN,     2, true ),
  ASSIGN_ADD: new BinaryPunctuator( '+=', TYPES.ASSIGN_ADD, 2, true ),
  ASSIGN_SUB: new BinaryPunctuator( '-=', TYPES.ASSIGN_SUB, 2, true ),
  ASSIGN_MUL: new BinaryPunctuator( '*=', TYPES.ASSIGN_MUL, 2, true ),
  ASSIGN_DIV: new BinaryPunctuator( '/=', TYPES.ASSIGN_DIV, 2, true ),

  /**
   * #binary-operator-tokens
   */
  OR    : new BinaryPunctuator( 'or',  TYPES.OR,     4 ),
  AND   : new BinaryPunctuator( 'and', TYPES.AND,    5 ),
  ADD   : new BinaryPunctuator( '+',   TYPES.ADD,    12 ),
  SUB   : new BinaryPunctuator( '-',   TYPES.SUB,    12 ),
  MUL   : new BinaryPunctuator( '*',   TYPES.MUL,    13 ),
  DIV   : new BinaryPunctuator( '/',   TYPES.DIV,    13 ),
  PERIOD: new BinaryPunctuator( '.',   TYPES.PERIOD, 19 ),

  /**
   * #compare-operator-tokens
   */
  EQ : new BinaryPunctuator( '==', TYPES.EQ,  9 ),
  NE : new BinaryPunctuator( '!=', TYPES.NE,  9 ),
  LT : new BinaryPunctuator( '<',  TYPES.LT,  10 ),
  GT : new BinaryPunctuator( '>',  TYPES.GT,  10 ),
  LTE: new BinaryPunctuator( '<=', TYPES.LTE, 10 ),
  GTE: new BinaryPunctuator( '>=', TYPES.GTE, 10 ),
  IN : new BinaryPunctuator( 'in', TYPES.IN,  10 ),

  /**
   * #whitespace-tokens
   */
  NLINE : new Whitespace( TYPES.NLINE ),
  INDENT: new Whitespace( TYPES.INDENT ),

  /**
   * #keyword-tokens
   */
  CLASS    : new Keyword( 'class',     TYPES.CLASS ),
  DEF      : new Keyword( 'def',       TYPES.DEF ),
  DO       : new Keyword( 'do',        TYPES.DO ),
  ELSE     : new Keyword( 'else',      TYPES.ELSE ),
  FOR      : new Keyword( 'for',       TYPES.FOR ),
  FROM     : new Keyword( 'from',      TYPES.FROM ),
  IF       : new Keyword( 'if',        TYPES.IF ),
  IMPORT   : new Keyword( 'import',    TYPES.IMPORT ),
  NEW      : new Keyword( 'new',       TYPES.NEW ),
  OF       : new Keyword( 'of',        TYPES.OF ),
  PRINT    : new Keyword( 'print',     TYPES.PRINT ),
  RETURN   : new Keyword( 'return',    TYPES.RETURN ),
  THIS     : new Keyword( 'this',      TYPES.THIS ),
  TO       : new Keyword( 'to',        TYPES.TO ),
  WHILE    : new Keyword( 'while',     TYPES.WHILE ),
  BREAK    : new Keyword( 'break',     TYPES.BREAK ),
  CONTINUE : new Keyword( 'continue',  TYPES.CONTINUE ),
  SCAN     : new Keyword( 'scan',      TYPES.SCAN ),

  EOF: {
    t_type: TYPES.EOF
  }

};

/**
 * #keywords
 */
var IDENTIFIERS = _.create( null );

_.forEach( [
  'AND',
  'CLASS',
  'DEF',
  'DO',
  'ELSE',
  'FALSE',
  'FOR',
  'FROM',
  'IF',
  'IMPORT',
  'IN',
  'NEW',
  'NULL',
  'OF',
  'OR',
  'PRINT',
  'RETURN',
  'THIS',
  'TRUE',
  'TO',
  'TYPEOF',
  'WHILE',
  'BREAK',
  'CONTINUE',
  'SCAN'
], function ( name ) {
  IDENTIFIERS[ this[ name ].value ] = this[ name ];
}, TOKENS );

window.hyogen = {
  Scanner: Scanner,
  Parser : Parser,
  Runtime: Runtime,
  GlobalScope: GlobalScope,
  EmptyScope: EmptyScope,
  io: io
};

} )( this );
