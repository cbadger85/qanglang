use qanglang_core::{SourceMap, tokenizer::{Tokenizer, TokenType}};

fn main() {
    let source_code = "||item -> item * 2|";
    let source_map = SourceMap::new(source_code.to_string());
    let mut tokenizer = Tokenizer::new(&source_map);

    println!("Tokenizing: {}", source_code);
    for token in tokenizer {
        let lexeme: String = token.lexeme(&source_map).iter().collect();
        println!("Token: {:?}, Lexeme: '{}', Span: {}..{}", 
                 token.token_type, lexeme, token.start, token.end);
    }
}