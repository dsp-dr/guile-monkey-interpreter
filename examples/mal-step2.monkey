// MAL (Make-a-Lisp) Step 2: Eval
// Add evaluation of arithmetic operations

// Reuse tokenizer and parser from step1
let TOKEN_LPAREN = "LPAREN";
let TOKEN_RPAREN = "RPAREN";
let TOKEN_SYMBOL = "SYMBOL";
let TOKEN_NUMBER = "NUMBER";
let TOKEN_STRING = "STRING";

let tokenize = fn(input) {
    let tokens = [];
    let i = 0;
    let len_input = len(input);
    
    while (i < len_input) {
        let ch = input[i];
        
        if (ch == " " || ch == "\t" || ch == "\n") {
            i = i + 1;
        }
        else if (ch == "(") {
            tokens = push(tokens, {"type": TOKEN_LPAREN, "value": "("});
            i = i + 1;
        }
        else if (ch == ")") {
            tokens = push(tokens, {"type": TOKEN_RPAREN, "value": ")"});
            i = i + 1;
        }
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
            
            let num = int(s);
            if (str(num) == s || (s[0] == "-" && str(num) == s)) {
                tokens = push(tokens, {"type": TOKEN_NUMBER, "value": num});
            } else {
                tokens = push(tokens, {"type": TOKEN_SYMBOL, "value": s});
            }
            i = j;
        }
    }
    
    tokens
};

// AST constructors
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

// Parser
let parse_form = fn(tokens, pos) {
    if (pos >= len(tokens)) {
        return [nil, pos];
    }
    
    let token = tokens[pos];
    let token_type = token["type"];
    
    if (token_type == TOKEN_LPAREN) {
        let elements = [];
        pos = pos + 1;
        
        while (pos < len(tokens) && tokens[pos]["type"] != TOKEN_RPAREN) {
            let result = parse_form(tokens, pos);
            let elem = result[0];
            pos = result[1];
            elements = push(elements, elem);
        }
        
        if (pos < len(tokens)) {
            pos = pos + 1;
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

// Create the REPL environment with built-in functions
let repl_env = {
    "+": fn(args) {
        let sum = 0;
        let i = 0;
        while (i < len(args)) {
            sum = sum + args[i];
            i = i + 1;
        }
        ast_number(sum)
    },
    "-": fn(args) {
        if (len(args) == 0) {
            return ast_number(0);
        }
        if (len(args) == 1) {
            return ast_number(0 - args[0]);
        }
        let result = args[0];
        let i = 1;
        while (i < len(args)) {
            result = result - args[i];
            i = i + 1;
        }
        ast_number(result)
    },
    "*": fn(args) {
        let product = 1;
        let i = 0;
        while (i < len(args)) {
            product = product * args[i];
            i = i + 1;
        }
        ast_number(product)
    },
    "/": fn(args) {
        if (len(args) < 2) {
            return ast_number(0);
        }
        let result = args[0];
        let i = 1;
        while (i < len(args)) {
            if (args[i] == 0) {
                puts("Error: Division by zero");
                return ast_number(0);
            }
            result = result / args[i];
            i = i + 1;
        }
        ast_number(result)
    }
};

// Evaluate AST
let eval_ast = fn(ast, env) {
    if (type(ast) == "NULL") {
        return nil;
    }
    
    let ast_type = ast["type"];
    
    if (ast_type == "symbol") {
        let name = ast["name"];
        let func = env[name];
        if (type(func) == "NULL") {
            puts("Error: Unknown symbol: " + name);
            return nil;
        }
        return func;
    }
    else if (ast_type == "list") {
        let elements = ast["elements"];
        let new_elements = [];
        let i = 0;
        while (i < len(elements)) {
            let evaled = EVAL(elements[i], env);
            new_elements = push(new_elements, evaled);
            i = i + 1;
        }
        return ast_list(new_elements);
    }
    else {
        return ast;
    }
};

// Main EVAL function
let EVAL = fn(ast, env) {
    if (type(ast) == "NULL") {
        return nil;
    }
    
    // If not a list, return eval_ast result
    if (ast["type"] != "list") {
        return eval_ast(ast, env);
    }
    
    // Empty list evaluates to itself
    let elements = ast["elements"];
    if (len(elements) == 0) {
        return ast;
    }
    
    // Evaluate the list
    let evaled = eval_ast(ast, env);
    let evaled_elements = evaled["elements"];
    
    // First element should be a function
    let func = evaled_elements[0];
    if (type(func) != "FUNCTION") {
        puts("Error: First element is not a function");
        return nil;
    }
    
    // Extract numeric arguments
    let args = [];
    let i = 1;
    while (i < len(evaled_elements)) {
        let elem = evaled_elements[i];
        if (elem["type"] == "number") {
            args = push(args, elem["value"]);
        } else {
            puts("Error: Non-numeric argument");
            return nil;
        }
        i = i + 1;
    }
    
    // Apply function to arguments
    return func(args);
};

// Pretty printer
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
    else if (type(ast) == "FUNCTION") {
        return "#<function>";
    }
    else {
        return "???";
    }
};

let rep = fn(input, env) {
    let ast = READ(input);
    let result = EVAL(ast, env);
    PRINT(result)
};

// Test cases
puts("MAL Step 2 - Eval");
puts("==================");
puts("");

let test_cases = [
    "123",
    "456",
    "(+ 1 2)",
    "(+ 5 (* 2 3))",
    "(- (+ 5 (* 2 3)) 3)",
    "(/ (- (+ 5 (* 2 3)) 3) 4)",
    "(+ 2 3 4)",
    "(* 2 3 4)",
    "(- 10 3 2)",
    "(/ 100 5 2)"
];

let i = 0;
while (i < len(test_cases)) {
    let input = test_cases[i];
    puts("user> " + input);
    puts(rep(input, repl_env));
    puts("");
    i = i + 1;
}