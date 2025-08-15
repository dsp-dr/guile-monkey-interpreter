# Experiment 102: LSP and Tree-IL Tooling for Guile

## Objective
Explore Language Server Protocol (LSP) support and Tree-IL analysis tools for Guile to improve development experience and enable better structural editing, especially for complex parenthesis matching.

## Background
While debugging the parser.scm parenthesis issues, it became clear that better tooling could help:
- Visual parenthesis matching
- Structural editing (like paredit)
- Tree-IL visualization
- Language server capabilities

## Tools and Resources

### 1. Language Server Protocol
- Official Specification: https://langserver.org/
- Microsoft's protocol for language features
- Editor-agnostic approach to language support

### 2. Guile Language Server  
- Repository: https://github.com/Johni0702/guile-language-server
- Provides LSP implementation for Guile
- Features: go to definition, hover, completion, diagnostics

### 3. Emacs LSP Clients
- **lsp-mode**: https://emacs-lsp.github.io/lsp-mode/
  - Most popular LSP client for Emacs
  - Supports 50+ language servers
  - Rich UI with lsp-ui package
- **eglot**: Built-in to Emacs 29+
  - Minimal, focused LSP client
  - Less configuration required
  - Good for testing

### 4. Combined Server + Emacs Setup for Testing
```elisp
;; Emacs configuration for Guile LSP
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((scheme-mode . lsp))
  :commands lsp
  :config
  (add-to-list 'lsp-language-id-configuration '(scheme-mode . "scheme"))
  (lsp-register-client
   (make-lsp-client 
    :new-connection (lsp-stdio-connection "guile-lsp-server")
    :major-modes '(scheme-mode)
    :server-id 'guile-lsp)))

;; Alternative with eglot (simpler)
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs 
               '(scheme-mode . ("guile-lsp-server"))))
```

### 5. Tree-IL (Tree Intermediate Language)
- Documentation: https://www.gnu.org/software/guile/manual/html_node/Tree_002dIL.html
- Guile's intermediate representation
- Can be used for analysis and transformation

### 6. nREPL for Guile
- Network REPL protocol
- Enables IDE integration
- Live code evaluation and inspection

### 7. Structural Editing Tools
- Paredit mode for Emacs
- vim-sexp for Vim
- Parinfer for automatic parenthesis inference

## Implementation Plan

### Phase 1: LSP Setup
1. Install guile-language-server
2. Configure with editors (VS Code, Emacs, Vim)
3. Test basic functionality

### Phase 2: Tree-IL Analysis
1. Create Tree-IL dumper for our parser
2. Visualize AST transformations
3. Identify structural issues programmatically

### Phase 3: Custom Tools
1. Parenthesis balance checker with visual output
2. S-expression tree visualizer
3. Automatic parenthesis fixer

### Phase 4: IDE Integration
1. Custom LSP extensions for Monkey language
2. Syntax highlighting for Monkey in Guile files
3. Jump to definition across Monkey/Guile boundary

## Example: Tree-IL Viewer

```scheme
(use-modules (system base compile)
             (system base language)
             (language tree-il)
             (ice-9 pretty-print))

(define (show-tree-il code)
  "Display Tree-IL representation of Scheme code"
  (let ((tree-il (compile code #:from 'scheme #:to 'tree-il)))
    (pretty-print tree-il)))

;; Example usage
(show-tree-il '(define (foo x) (+ x 1)))
```

## Example: Parenthesis Visualizer

```scheme
(define (visualize-parens filename)
  "Create visual representation of parenthesis nesting"
  (with-input-from-file filename
    (lambda ()
      (let loop ((depth 0) (line 1) (col 0))
        (let ((c (read-char)))
          (unless (eof-object? c)
            (cond
             ((char=? c #\newline)
              (newline)
              (loop depth (+ line 1) 0))
             ((char=? c #\()
              (display (make-string depth #\space))
              (display "[")
              (loop (+ depth 1) line (+ col 1)))
             ((char=? c #\))
              (set! depth (- depth 1))
              (display (make-string depth #\space))
              (display "]")
              (loop depth line (+ col 1)))
             (else
              (loop depth line (+ col 1))))))))))
```

## Benefits for Our Project

1. **Immediate**: Fix parser.scm parenthesis issues
2. **Short-term**: Better debugging of AST generation
3. **Long-term**: Professional IDE support for Monkey development

## Resources
- [Guile Language Server](https://github.com/Johni0702/guile-language-server)
- [Tree-IL Documentation](https://www.gnu.org/software/guile/manual/html_node/Tree_002dIL.html)
- [nREPL Protocol](https://nrepl.org/)
- [Paredit Cheatsheet](http://danmidwood.com/content/2014/11/21/animated-paredit.html)

## Next Steps
1. Set up guile-language-server locally
2. Create parenthesis fixing tool using Tree-IL
3. Integrate with current development workflow
4. Document setup for other developers