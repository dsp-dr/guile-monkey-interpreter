#!/usr/bin/env guile
!#
;;; Generate Mermaid sequence diagram for interpreter execution

(use-modules (ice-9 format))

(define (generate-sequence-diagram)
  "Generate a sequence diagram showing interpreter execution flow"
  (display "sequenceDiagram
    participant User
    participant REPL
    participant Lexer
    participant Parser
    participant Evaluator
    participant Environment
    participant Object
    
    User->>REPL: Input Monkey code
    activate REPL
    
    REPL->>Lexer: Create lexer with input
    activate Lexer
    Lexer-->>REPL: Lexer instance
    deactivate Lexer
    
    REPL->>Parser: Create parser with lexer
    activate Parser
    Parser->>Lexer: next-token()
    activate Lexer
    Lexer-->>Parser: Token
    deactivate Lexer
    
    Parser->>Parser: parse-program()
    loop For each statement
        Parser->>Lexer: next-token()
        activate Lexer
        Lexer-->>Parser: Token
        deactivate Lexer
        Parser->>Parser: parse-statement()
        alt Let statement
            Parser->>Parser: parse-let-statement()
        else Return statement
            Parser->>Parser: parse-return-statement()
        else Expression statement
            Parser->>Parser: parse-expression-statement()
        end
    end
    
    Parser-->>REPL: AST (Program node)
    deactivate Parser
    
    REPL->>Evaluator: eval-program(ast, env)
    activate Evaluator
    
    Evaluator->>Environment: Create global environment
    activate Environment
    Environment-->>Evaluator: Environment instance
    deactivate Environment
    
    loop For each statement in AST
        Evaluator->>Evaluator: eval(statement, env)
        
        alt Let statement
            Evaluator->>Evaluator: eval(value expression, env)
            Evaluator->>Environment: set(name, value)
            activate Environment
            Environment-->>Evaluator: OK
            deactivate Environment
            
        else Return statement
            Evaluator->>Evaluator: eval(return value, env)
            Evaluator->>Object: Create return object
            activate Object
            Object-->>Evaluator: Return object
            deactivate Object
            
        else Function call
            Evaluator->>Environment: get(function name)
            activate Environment
            Environment-->>Evaluator: Function object
            deactivate Environment
            
            Evaluator->>Evaluator: eval-arguments(args, env)
            
            Evaluator->>Environment: Create extended environment
            activate Environment
            Environment-->>Evaluator: New environment
            deactivate Environment
            
            Evaluator->>Evaluator: eval(function body, new-env)
            
        else Infix expression
            Evaluator->>Evaluator: eval(left, env)
            Evaluator->>Evaluator: eval(right, env)
            Evaluator->>Evaluator: apply-operator(op, left, right)
            Evaluator->>Object: Create result object
            activate Object
            Object-->>Evaluator: Result object
            deactivate Object
            
        else While/For loop
            Evaluator->>Evaluator: eval(condition, env)
            loop While condition is true
                Evaluator->>Evaluator: eval(body, env)
                alt Break statement
                    Evaluator->>Object: Create break object
                    activate Object
                    Object-->>Evaluator: Break object
                    deactivate Object
                    Evaluator->>Evaluator: Exit loop
                else Continue statement
                    Evaluator->>Object: Create continue object
                    activate Object
                    Object-->>Evaluator: Continue object
                    deactivate Object
                    Evaluator->>Evaluator: Next iteration
                end
                Evaluator->>Evaluator: eval(update, env)
                Evaluator->>Evaluator: eval(condition, env)
            end
        end
    end
    
    Evaluator->>Object: Get final result
    activate Object
    Object-->>Evaluator: Result object
    deactivate Object
    
    Evaluator-->>REPL: Result object
    deactivate Evaluator
    
    REPL->>User: Display result
    deactivate REPL
    
    Note over User,Object: Built-in functions follow similar pattern
    Note over Parser,Evaluator: Error handling omitted for clarity
")
  (newline))

(generate-sequence-diagram)