// MAL (Make-a-Lisp) Step 1: Read and Print
// Basic tokenization and parsing of S-expressions

// Token types
let TOKEN_LPAREN = "LPAREN";
let TOKEN_RPAREN = "RPAREN";
let TOKEN_SYMBOL = "SYMBOL";
let TOKEN_NUMBER = "NUMBER";
let TOKEN_STRING = "STRING";

// Simple tokenizer
let tokenize = fn(input) {
    let tokens = [];
    let i = 0;
    let len_input = len(input);
    
    while (i < len_input) {
        let ch = input[i];
        
        // Skip whitespace
        if (ch == " " || ch == "\t" || ch == "\n") {
            i = i + 1;
        }
        // Left paren
        else if (ch == "(") {
            tokens = push(tokens, {"type": TOKEN_LPAREN, "value": "("});
            i = i + 1;
        }
        // Right paren
        else if (ch == ")") {
            tokens = push(tokens, {"type": TOKEN_RPAREN, "value": ")"});
            i = i + 1;
        }
        // String (simplified - doesn't handle escapes)
        else if (ch == "\"") {
            let j = i + 1;
            while (j < len_input && input[j] != "\"") {
                j = j + 1;
            }
            let s = "";
            let k = i + 1;
            while (k < j) {
                s = s + input[k];
                k = k + 1;
            }
            tokens = push(tokens, {"type": TOKEN_STRING, "value": s});
            i = j + 1;
        }
        // Number or symbol
        else {
            let j = i;
            while (j < len_input && input[j] != " " && input[j] != "(" && input[j] != ")") {
                j = j + 1;
            }
            let s = "";
            let k = i;
            while (k < j) {
                s = s + input[k];
                k = k + 1;
            }
            
            // Try to parse as number
            let num = int(s);
            if (str(num) == s) {
                tokens = push(tokens, {"type": TOKEN_NUMBER, "value": num});
            } else {
                tokens = push(tokens, {"type": TOKEN_SYMBOL, "value": s});
            }
            i = j;
        }
    }
    
    tokens
};

// AST node types  
let ast_list = fn(elements) {
    {"type": "list", "elements": elements}
};

let ast_symbol = fn(name) {
    {"type": "symbol", "name": name}
};

let ast_number = fn(value) {
    {"type": "number", "value": value}
};

let ast_string = fn(value) {
    {"type": "string", "value": value}
};

// Simple recursive descent parser
let parse_form = fn(tokens, pos) {
    if (pos >= len(tokens)) {
        return [nil, pos];
    }
    
    let token = tokens[pos];
    let token_type = token["type"];
    
    if (token_type == TOKEN_LPAREN) {
        // Parse list
        let elements = [];
        pos = pos + 1;
        
        while (pos < len(tokens) && tokens[pos]["type"] != TOKEN_RPAREN) {
            let result = parse_form(tokens, pos);
            let elem = result[0];
            pos = result[1];
            elements = push(elements, elem);
        }
        
        if (pos < len(tokens)) {
            pos = pos + 1; // Skip closing paren
        }
        
        return [ast_list(elements), pos];
    }
    else if (token_type == TOKEN_NUMBER) {
        return [ast_number(token["value"]), pos + 1];
    }
    else if (token_type == TOKEN_STRING) {
        return [ast_string(token["value"]), pos + 1];
    }
    else if (token_type == TOKEN_SYMBOL) {
        return [ast_symbol(token["value"]), pos + 1];
    }
    else {
        return [nil, pos + 1];
    }
};

let READ = fn(input) {
    let tokens = tokenize(input);
    if (len(tokens) == 0) {
        return nil;
    }
    let result = parse_form(tokens, 0);
    result[0]
};

let EVAL = fn(ast) {
    // For now, just return the AST unchanged
    ast
};

// Pretty printer for AST
let PRINT = fn(ast) {
    if (type(ast) == "NULL") {
        return "nil";
    }
    
    let ast_type = ast["type"];
    
    if (ast_type == "list") {
        let s = "(";
        let elements = ast["elements"];
        let i = 0;
        while (i < len(elements)) {
            if (i > 0) {
                s = s + " ";
            }
            s = s + PRINT(elements[i]);
            i = i + 1;
        }
        s = s + ")";
        return s;
    }
    else if (ast_type == "symbol") {
        return ast["name"];
    }
    else if (ast_type == "number") {
        return str(ast["value"]);
    }
    else if (ast_type == "string") {
        return "\"" + ast["value"] + "\"";
    }
    else {
        return "???";
    }
};

let rep = fn(input) {
    PRINT(EVAL(READ(input)))
};

// Test cases
puts("MAL Step 1 - Read and Print");
puts("============================");

let test_cases = [
    "123",
    "abc",
    "\"hello world\"",
    "(+ 1 2)",
    "(list 1 2 3)",
    "(def! x 10)",
    "(+ (* 2 3) 4)",
    "()"
];

let i = 0;
while (i < len(test_cases)) {
    let input = test_cases[i];
    puts("user> " + input);
    puts(rep(input));
    i = i + 1;
}