#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer, token::{Token, TokenKind}};
    use insta::assert_snapshot;

    pub fn test_common(name: &str, input: &str) {
        let mut lexer = Lexer::new(input);
        let token = test_token_set(&mut lexer);

        assert_snapshot!(name, serde_json::to_string_pretty(&token).unwrap(), input);
    }

    fn test_token_set(lexer: &mut Lexer) -> Vec<Token> {
        let mut tokens: Vec<Token> = vec![];
        
        loop {
            let token = lexer.next_token();

            if token.kind == TokenKind::EOF {
                tokens.push(token);
                break;
            } else {
                tokens.push(token);
            }
        }

        return tokens;
    }

    #[test]
    fn test_lex_simple() {
        test_common("let", "let x = 5;");
    }

    #[test]
    fn test_complex() {
        test_common("complex", "
            component Person {
                let age = 5;
                let name = \"John\";
                let is_male = true;

                if age > 18 {
                    println(\"age is: {age}\")
                } else {
                    throw \"Age is less than 18\"
                }

                render {
                    text {
                        content: \"Hello World\";
                    }
                }
            }
        ");
    }
}