#!/usr/bin/env guile
!#
;;; Generate Mermaid control flow diagram for the interpreter

(use-modules (ice-9 format))

(define (generate-control-flow)
  "Generate a control flow diagram showing interpreter stages"
  (display "flowchart TD
    %% Control flow of Monkey interpreter
    
    Start([\"Monkey Code\"]) --> Lexer
    
    Lexer --> |\"Tokenize\"| Tokens[\"Token Stream\"]
    Tokens --> Parser
    
    Parser --> |\"Parse\"| AST[\"Abstract Syntax Tree\"]
    
    AST --> Evaluator
    
    Evaluator --> Environment[\"Environment Setup\"]
    Environment --> EvalLoop{\"Eval Node\"}
    
    EvalLoop --> |\"Statement\"| EvalStmt[\"Eval Statement\"]
    EvalLoop --> |\"Expression\"| EvalExpr[\"Eval Expression\"]
    
    EvalStmt --> |\"Let\"| BindVar[\"Bind Variable\"]
    EvalStmt --> |\"Return\"| ReturnVal[\"Return Value\"]
    EvalStmt --> |\"Expression\"| EvalExpr
    
    EvalExpr --> |\"Literal\"| LiteralVal[\"Return Literal\"]
    EvalExpr --> |\"Identifier\"| LookupVar[\"Lookup Variable\"]
    EvalExpr --> |\"Prefix\"| EvalPrefix[\"Eval Prefix Op\"]
    EvalExpr --> |\"Infix\"| EvalInfix[\"Eval Infix Op\"]
    EvalExpr --> |\"If\"| EvalIf[\"Eval Conditional\"]
    EvalExpr --> |\"Function\"| CreateFunc[\"Create Function\"]
    EvalExpr --> |\"Call\"| CallFunc[\"Call Function\"]
    EvalExpr --> |\"Array\"| EvalArray[\"Eval Array\"]
    EvalExpr --> |\"Hash\"| EvalHash[\"Eval Hash\"]
    EvalExpr --> |\"Index\"| EvalIndex[\"Eval Index\"]
    EvalExpr --> |\"While\"| EvalWhile[\"Eval While Loop\"]
    EvalExpr --> |\"For\"| EvalFor[\"Eval For Loop\"]
    
    BindVar --> UpdateEnv[\"Update Environment\"]
    LookupVar --> CheckEnv[\"Check Environment\"]
    CallFunc --> NewEnv[\"New Environment\"]
    
    UpdateEnv --> Continue{\"More Statements?\"}
    ReturnVal --> Result([\"Result\"])
    LiteralVal --> Continue
    CheckEnv --> Continue
    EvalPrefix --> Continue
    EvalInfix --> Continue
    EvalIf --> Continue
    CreateFunc --> Continue
    NewEnv --> EvalLoop
    EvalArray --> Continue
    EvalHash --> Continue
    EvalIndex --> Continue
    EvalWhile --> LoopControl{\"Break/Continue?\"}
    EvalFor --> LoopControl
    
    LoopControl --> |\"Break\"| ExitLoop[\"Exit Loop\"]
    LoopControl --> |\"Continue\"| NextIter[\"Next Iteration\"]
    LoopControl --> |\"Normal\"| Continue
    
    ExitLoop --> Continue
    NextIter --> EvalLoop
    
    Continue --> |\"Yes\"| EvalLoop
    Continue --> |\"No\"| Result
    
    %% Styling
    classDef process fill:#f9f,stroke:#333,stroke-width:2px
    classDef data fill:#bbf,stroke:#333,stroke-width:2px
    classDef decision fill:#fbf,stroke:#333,stroke-width:2px
    classDef terminal fill:#bfb,stroke:#333,stroke-width:2px
    
    class Lexer,Parser,Evaluator,EvalStmt,EvalExpr,EvalPrefix,EvalInfix,EvalIf,CreateFunc,CallFunc,EvalArray,EvalHash,EvalIndex,EvalWhile,EvalFor,BindVar,LookupVar,UpdateEnv,CheckEnv,NewEnv process
    class Tokens,AST,Environment,LiteralVal,ReturnVal data
    class EvalLoop,Continue,LoopControl decision
    class Start,Result terminal
")
  (newline))

(generate-control-flow)