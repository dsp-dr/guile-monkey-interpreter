;;; test-lsp-setup.el --- Test LSP setup for Guile development -*- lexical-binding: t -*-

;; This file sets up a minimal Emacs environment to test Guile LSP
;; Run with: emacs -nw -Q -l test-lsp-setup.el parser.scm

;;; Bootstrap straight.el for package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Install and configure packages
(straight-use-package 'use-package)

;; Scheme mode
(straight-use-package 'geiser)
(straight-use-package 'geiser-guile)

;; Paredit for structural editing
(straight-use-package 'paredit)

;; LSP Mode
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)

;; Company for completion
(straight-use-package 'company)

;;; Configuration

;; Enable paredit for Scheme
(add-hook 'scheme-mode-hook #'paredit-mode)

;; Configure Geiser for Guile
(setq geiser-guile-binary "guile")
(setq geiser-active-implementations '(guile))

;; Configure LSP
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((scheme-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-enable-snippet nil)
  (setq lsp-prefer-flymake nil)
  
  ;; Register Guile LSP server
  (add-to-list 'lsp-language-id-configuration '(scheme-mode . "scheme"))
  
  ;; Try to find guile-lsp-server
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection 
                    (lambda () 
                      (or (executable-find "guile-lsp-server")
                          (expand-file-name "~/bin/guile-lsp-server")
                          "guile-lsp-server")))
    :major-modes '(scheme-mode)
    :priority -1
    :server-id 'guile-lsp)))

;; Configure LSP UI
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t))

;; Company mode for completion
(use-package company
  :hook (scheme-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0))

;;; Parenthesis visualization helpers

(defun show-paren-data ()
  "Show parenthesis balance information for current buffer."
  (interactive)
  (let ((open-count 0)
        (close-count 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ((looking-at "(") (setq open-count (1+ open-count)))
         ((looking-at ")") (setq close-count (1+ close-count))))
        (forward-char 1)))
    (message "Parentheses - Open: %d, Close: %d, Balance: %d" 
             open-count close-count (- open-count close-count))))

(defun find-unmatched-parens ()
  "Find and highlight unmatched parentheses."
  (interactive)
  (let ((unmatched-opens '())
        (unmatched-closes '()))
    (save-excursion
      (goto-char (point-min))
      (condition-case err
          (while (not (eobp))
            (cond
             ((looking-at "(")
              (let ((start (point)))
                (forward-sexp)
                ;; If we get here, it's matched
                ))
             ((looking-at ")")
              (push (point) unmatched-closes)))
            (forward-char 1))
        (scan-error
         (message "Scan error at position %d: %s" (point) err))))
    (when unmatched-closes
      (message "Unmatched closing parens at: %s" unmatched-closes))))

;;; Key bindings

(global-set-key (kbd "C-c p b") 'show-paren-data)
(global-set-key (kbd "C-c p f") 'find-unmatched-parens)
(global-set-key (kbd "C-c p r") 'paredit-reindent-defun)

;;; Display help

(defun display-help ()
  "Display help for LSP testing setup."
  (with-current-buffer (get-buffer-create "*LSP Test Help*")
    (erase-buffer)
    (insert "=== Guile LSP Test Setup ===\n\n")
    (insert "Key Bindings:\n")
    (insert "  C-c l   - LSP prefix key\n")
    (insert "  C-c p b - Show parenthesis balance\n")
    (insert "  C-c p f - Find unmatched parentheses\n")
    (insert "  C-c p r - Reindent current defun\n")
    (insert "\nParedit Commands:\n")
    (insert "  C-M-f   - Forward s-expression\n")
    (insert "  C-M-b   - Backward s-expression\n")
    (insert "  C-)     - Slurp forward\n")
    (insert "  C-}     - Barf forward\n")
    (insert "  M-s     - Splice s-expression\n")
    (insert "\nLSP Commands:\n")
    (insert "  C-c l s s - Start LSP workspace\n")
    (insert "  C-c l g g - Go to definition\n")
    (insert "  C-c l g r - Find references\n")
    (insert "  C-c l r r - Rename symbol\n")
    (insert "  C-c l = = - Format buffer\n")
    (insert "\nIf LSP server is not found:\n")
    (insert "  1. Install: git clone https://github.com/Johni0702/guile-language-server\n")
    (insert "  2. Build according to README\n")
    (insert "  3. Ensure guile-lsp-server is in PATH\n")
    (display-buffer (current-buffer))))

;; Show help on startup
(add-hook 'emacs-startup-hook 'display-help)

;; Enable show-paren-mode for better visualization
(show-paren-mode 1)
(setq show-paren-style 'expression)

;; Message to user
(message "Guile LSP test environment loaded. Press C-h m for mode help.")

;;; test-lsp-setup.el ends here