;;; mcf-mode.el --- Major mode for editing Minecraft mcfunction -*- lexical-binding: t -*-

;; Copyright (C) 2019 rasensuihei

;; Author: rasensuihei <rasensuihei@gmail.com>
;; URL: https://github.com/rasensuihei/mcf-mode
;; Version: 0.2.2
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The main features of this mode are Minecraft mcfunction syntax
;; highlighting.

;; Settings example
;; (require 'mcf-mode)
;; (setq mcf-rcon-password "PASSWORD")

;; Default keybindings:
;;   C-c C-c  mcf-execute
;;   C-c C-e  mcf-execute-at-point

;; Usage
;; `M-x mcf-rcon` to connect to Minecraft RCON server.
;; `M-x mcf-rcon-disconnect` to disconnect from Minecraft RCON server.

;; See also:
;; https://github.com/rasensuihei/mcf-mode

;;; Code:
(require 'font-lock)
(require 'mcf-rcon)

(defgroup mcf nil
  "Major mode for editing minecraft mcfunction."
  :group 'languages)

(defface mcf-syntax-warning
  '((((class color) (background light)) (:background "IndianRed1"))
    (((class color) (background dark)) (:background "firebrick4")))
  "Illegal space face"
  :group 'mcf)
(defvar mcf-syntax-warning 'mcf-syntax-warning)

;; See https://www.emacswiki.org/emacs/FontLockKeywords for how to define
(defvar mcf--font-lock-keywords)
(setq mcf--font-lock-keywords
      (list
       ;; Execute
       '("\\<\\(execute\\)\\>"
         (1 font-lock-keyword-face))
       ;; Command
       '("\\(^\\|run \\)\\([a-z]+\\)\\>"
         (1 font-lock-keyword-face)
         (2 font-lock-builtin-face))
       '("\\(function\\) \\([a-z0-9_.+-:]+\\)\\>"
         (1 font-lock-keyword-face)
         (2 font-lock-variable-name-face))
       ;; Syntax warning
       '("\\( \s+\\|^\s+\\|\s+$\\|@[aeprs]\s+\\[\\)"
         (1 mcf-syntax-warning))
       ;; Selector variable
       '("\\(@[aeprs]\\)"
         (1 font-lock-variable-name-face))
       '("\\(@[aeprs]\\)\\(\\[.*?\\]\\)\\s-"
         (1 font-lock-variable-name-face t)
         (2 font-lock-type-face t))
       ;; Selector arguments
       '("\\([a-zA-Z][a-zA-Z0-9_.+-]+\\)\s*="
         (1 font-lock-builtin-face t))
       '("\\([,=:]\\)"
         (1 font-lock-builtin-face t))
       ;; Numbers
       '("\\<\\([0-9]+\\(\\.[0-9]+\\)?\\)[bsilfd]?\\>"
         (1 font-lock-constant-face t))
       '("\\([\\^\\~]\\)\\([0-9]\\|\\s-\\)" (1 font-lock-builtin-face t))
       '("\\.\\." (0 font-lock-builtin-face t))
       ;; Negation char
       '("=\\(!\\)"
         (1 font-lock-negation-char-face t))
       ;; String
       '("\"\\(\\\\.\\|[^\"]\\)*\""
         (0 font-lock-string-face t))
       ;; Line comment
       '("^\\(#.*\\)$"
         (1 font-lock-comment-face t))
       ))

(defvar mcf-mode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c" 'mcf-execute)
    (define-key map "\C-e" 'mcf-execute-at-point)
    (define-key map "\C-r" 'mcf-reload)
    map))

(defvar mcf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c" mcf-mode-prefix-map)
    map))

(defvar mcf-mode-hook nil
  "This hook is run when mcf-mode starts.")

(defvar mcf-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; (modify-syntax-entry ?# "<" table)
    ;; (modify-syntax-entry ?\r ">" table)
    ;; (modify-syntax-entry ?\n ">" table)
    table))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mcfunction\\'" . mcf-mode))

;;;###autoload
(define-derived-mode mcf-mode prog-mode "Minecraft-Function"
  "Set major mode for editing Minecraft mcfunction file."
  :group 'mcf
  (setq-local font-lock-defaults
              (list mcf--font-lock-keywords nil nil nil nil))
  (setq-local comment-start "#")
  (setq-local comment-end ""))

(defun mcf-execute (str)
  "Execute Minecraft command STR and display the result in the minibuffer."
  (interactive "MCommand: ")
  ;; (mcf-rcon-execute str handler)
  (mcf-rcon-execute str (lambda (payload)
                          (if (eq payload "")
                              (message "Minecraft: <empty>")
                            (message "Minecraft: %s" payload)))))

(defun mcf-execute-at-point ()
  "Execute a command at point."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (when (string-match "^[# ]*\\(.+\\)$" line)
      (mcf-execute (match-string 1 line)))))

(defun mcf-reload ()
  "Execute a reload command."
  (interactive)
  (mcf-execute "reload"))

(defmacro mcf-eval (command &rest args)
  "(mcf-eval COMMAND ARGS...)

Evaluate minecraft command with RCON server.  A First argument must be payload variable name.  (mcf-eval \"COMMAND\" (PAYLOAD) BODY...)"
  (declare (indent defun) (debug args))
  (if args
      `(mcf-rcon-execute ,command (lambda (x) (let ((,(caar args) x)) ,@(cdr args))))
    `(mcf-rcon-execute ,command)))

(provide 'mcf-mode)
;;; mcf-mode.el ends here
