# QangLang

An embedded scripting language written in Rust 🦀

## Grammar Definition

<table>
  <thead>
  <thead/>
  <tbody>
    <tr>
      <td>program</td>
      <td>=</td>
      <td>declaration* EOF</td>
      <td>;</td>
    </tr>
    <tr>
      <td>declaration</td>
      <td>=</td>
      <td>classDecl | fnDecl | lambdaDecl | varDecl | statement</td>
      <td>;</td>
    </tr>
    <tr>
      <td>classDecl</td>
      <td>=</td>
      <td>"class" IDENTIFIER ( ":" IDENTIFIER )? "{" classMember* "}"</td>
      <td>;</td>
    </tr>
    <tr>
      <td>classMember</td>
      <td>=</td>
      <td>function | fieldDecl</td>
      <td>;</td>
    </tr>
    <tr>
      <td>fieldDecl</td>
      <td>=</td>
      <td>IDENTIFIER ( "=" expression )? ";"</td>
      <td>;</td>
    </tr>
    <tr>
      <td>fnDecl</td>
      <td>=</td>
      <td>"fn" function</td>
      <td>;</td>
    </tr>
    <tr>
      <td>function</td>
      <td>=</td>
      <td>IDENTIFIER "(" parameters? ")" block</td>
      <td>;</td>
    </tr>
    <tr>
      <td>lambdaDecl</td>
      <td>=</td>
      <td>"var" IDENTIFIER "=" lambda</td>
      <td>;</td>
    </tr>
    <tr>
      <td>lambda</td>
      <td>=</td>
      <td>"(" parameters? ")" "->" ( block | expression )</td>
      <td>;</td>
    </tr>
    <tr>
      <td>parameters</td>
      <td>=</td>
      <td>( IDENTIFIER | destructurePattern ) ( "," ( IDENTIFIER | destructurePattern ) )* ","?</td>
      <td>;</td>
    </tr>
    <tr>
      <td>varDecl</td>
      <td>=</td>
      <td>"var" ( IDENTIFIER | destructurePattern ) ( "=" expression )? ";"</td>
      <td>;</td>
    </tr>
    <tr>
      <td>statement</td>
      <td>=</td>
      <td>exprStmt | block | ifStmt | whileStmt | forStmt | breakStmt | continueStmt | returnStmt</td>
      <td>;</td>
    </tr>
      <tr>
      <td>exprStmt</td>
      <td>=</td>
      <td>expression ";"</td>
      <td>;</td>
    </tr>
    <tr>
      <td>block</td>
      <td>=</td>
      <td>"{" declaration* "}"</td>
      <td>;</td>
    </tr>
    <tr>
      <td>ifStmt</td>
      <td>=</td>
      <td>"if" "(" expression ")" statement ("else" statement)?</td>
      <td>;</td>
    </tr>
    <tr>
      <td>whileStmt</td>
      <td>=</td>
      <td>"while" "(" expression ")" statement</td>
      <td>;</td>
    </tr>
    <tr>
      <td>forStmt</td>
      <td>=</td>
      <td>"for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement</td>
      <td>;</td>
    </tr>
    <tr>
      <td>returnStmt</td>
      <td>=</td>
      <td>"return" expression? ";"</td>
      <td>;</td>
    </tr>
    <tr>
      <td>breakStmt</td>
      <td>=</td>
      <td>"break" ";"</td>
      <td>;</td>
    </tr>
    <tr>
      <td>continueStmt</td>
      <td>=</td>
      <td>"continue" ";"</td>
      <td>;</td>
    </tr>
    <tr>
      <td>expression</td>
      <td>=</td>
      <td>assignment</td>
      <td>;</td>
    </tr>
    <tr>
      <td>assignment</td>
      <td>=</td>
      <td>( call "." IDENTIFIER | IDENTIFIER) ( "=" | "+=" | "-=" | "*=" | "/=" | "%=" ) assignment | pipe</td>
      <td>;</td>
    </tr>
    <tr>
      <td>pipe</td>
      <td>=</td>
      <td>ternary ( "|>" pipe )?</td>
      <td>;</td>
    </tr>
    <tr>
      <td>ternary</td>
      <td>=</td>
      <td>logicOr ( "?" expression ":" ternary )?</td>
      <td>;</td>
    </tr>
    <tr>
      <td>logicOr</td>
      <td>=</td>
      <td>logicAnd ( "or" logicAnd)*</td>
      <td>;</td>
    </tr>
    <tr>
      <td>logicAnd</td>
      <td>=</td>
      <td>equality ( "and" equality)*</td>
      <td>;</td>
    </tr>
    <tr>
      <td>equality</td>
      <td>=</td>
      <td>comparison ( ( "!=" | "==" | "is" ) comparison)*</td>
      <td>;</td>
    </tr>
    <tr>
      <td>comparison</td>
      <td>=</td>
      <td>term ( ( ">" | ">=" | "<" | "<=" ) term)*</td>
      <td>;</td>
    </tr>
    <tr>
      <td>term</td>
      <td>=</td>
      <td>factor ( ( "+" | "-" ) factor )*</td>
      <td>;</td>
    </tr>
    <tr>
      <td>factor</td>
      <td>=</td>
      <td>unary ( ( "/" | "*" | "%" ) unary )*</td>
      <td>;</td>
    </tr>
    <tr>
      <td>unary</td>
      <td>=</td>
      <td>( "-" | "!" ) unary | call</td>
      <td>;</td>
    </tr>
    <tr>
      <td>call</td>
      <td>=</td>
      <td>primary ( "(" arguments? ")" | "." IDENTIFIER | "?." IDENTIFIER | "[" expression "]" | "||" primary "->" expression "|" | "?|" primary "->" expression "|" )*</td>
      <td>;</td>
    </tr>
    <tr>
      <td>arguments</td>
      <td>=</td>
      <td>expression ( "," expression )* ","?</td>
      <td>;</td>
    </tr>
    <tr>
      <td>primary</td>
      <td>=</td>
      <td>NUMBER | STRING | "true" | "false" | "nil" | "this" | "(" expression ")" | IDENTIFIER | "super" | "super" "." IDENTIFIER | lambda | arrayLiteral | objectLiteral</td>
      <td>;</td>
    </tr>
    <tr>
      <td>arrayLiteral</td>
      <td>=</td>
      <td>"[" ( expression ( "," expression )* ","? )? "]"</td>
      <td>;</td>
    </tr>
    <tr>
      <td>objectLiteral</td>
      <td>=</td>
      <td>"{{" ( objectField ( "," objectField )* ","? )? "}}"</td>
      <td>;</td>
    </tr>
    <tr>
      <td>objectField</td>
      <td>=</td>
      <td>IDENTIFIER ( "=" expression )?</td>
      <td>;</td>
    </tr>
    <tr>
      <td>destructurePattern</td>
      <td>=</td>
      <td>"(" IDENTIFIER ( "," IDENTIFIER )* ( "," ".." IDENTIFIER )? ")"</td>
      <td>;</td>
    </tr>
  </tbody>
</table>
```

## Roadmap

[ ] - array destructuring for variable declaration `var (item1, item2, ..others) = [1, 2, 3, 4, 5];`

[ ] - array destructuring for function parameters `fn the_function((item1, item2, ..others), arg2) {}` and lambda parameters `((item1, item2, ..others), arg2) -> nil`

[ ] - array destructuring for map operator and map optional operator `var first = maybe_array?|(first) -> first|;`

[ ] - tail call optimization
