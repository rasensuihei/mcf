;;; mcfunction-mode.el --- Major mode for editing Minecraft mcfunction -*- lexical-binding: t -*-

;; Copyright (C) 2019 rasensuihei

;; Author: rasensuihei <rasensuihei@gmail.com>
;; URL: https://github.com/rasensuihei/mcfunction-mode
;; Version: 0.2.1
;; Keywords: languages

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
;; (require 'mcfunction-mode)

;; Default keybindings:
;;   C-c C-c  mcfunction-execute-command
;;   C-c C-e  mcfunction-execute-command-at-point

;; See also:
;; https://github.com/rasensuihei/mcfunction-mode

;;; Code:
(require 'font-lock)
(require 'mcrcon)

(defgroup mcfunction nil
  "Major mode for editing minecraft mcfunction."
  :group 'languages)

(defface mcfunction-syntax-warning
  '((((class color) (background light)) (:background "IndianRed1"))
    (((class color) (background dark)) (:background "firebrick4")))
  "Illegal space face"
  :group 'mcfunction)
(defvar mcfunction-syntax-warning 'mcfunction-syntax-warning)

(defvar mcfunction--font-lock-keywords
  (list
   ;; Execute
   '("\\<\\(execute\\)\\>"
     (1 font-lock-keyword-face))
   ;; Command
   '("\\(^\\|run \\)\\([a-z]+\\)\\>"
     (1 font-lock-keyword-face)
     (2 font-lock-builtin-face))
   ;; Syntax warning
   '("\\( \s+\\|^\s+\\|\s+$\\|@[aeprs]\s+\\[\\)"
     (1 mcfunction-syntax-warning))
   ;; Selector variable
   '("\\(@[aeprs]\\)"
     (1 font-lock-variable-name-face))
   '("\\(@[aeprs]\\)\\[\\([^]]*\\)\\]"
     (1 font-lock-variable-name-face t)
     (2 font-lock-constant-face t))
   ;; Selector arguments
   '("\\([a-zA-Z0-9_]+\\)\s*="
     (1 font-lock-builtin-face t))
   '("\\([,=:]\\)"
     (1 font-lock-builtin-face t))
     ;; (2 default t))
   ;; Negation char
   '("=\\(!\\)"
     (1 font-lock-negation-char-face t))
   ;; String
   '("\"\\(\\\\.\\|[^\"]\\)*\""
     (1 font-lock-string-face t))
   ;; Line comment
   '("^\\(#.*\\)$"
     (1 font-lock-comment-face t))
   ))

(defvar mcfunction-mode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c" 'mcfunction-execute-command)
    (define-key map "\C-e" 'mcfunction-execute-command-at-point)
    map))

(defvar mcfunction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c" mcfunction-mode-prefix-map)
    map))

(defvar mcfunction-mode-hook nil
  "This hook is run when mcfunction mode starts.")

(defvar mcfunction-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; (modify-syntax-entry ?# "<" table)
    ;; (modify-syntax-entry ?\r ">" table)
    ;; (modify-syntax-entry ?\n ">" table)
    table))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mcfunction\\'" . mcfunction-mode))

;;;###autoload
(define-derived-mode mcfunction-mode prog-mode "mcfunction"
  "Set major mode for editing Minecraft mcfunction file."
  :group 'mcfunction
  (setq-local font-lock-defaults
              (list mcfunction--font-lock-keywords nil nil nil nil))
  (setq-local comment-start "#")
  (setq-local comment-end ""))

(defalias 'mcfunction-execute-command 'mcrcon-execute-command
  "Execute Minecraft command STR.  HANDLER is a function for server response handle.")

(defalias 'mcfunction-execute-command-at-point 'mcrcon-execute-command-at-point
  "Execute Minecraft command at point.")

;;; --- Deprecated ---

(defvar mcfunction-display-server-messages t "Display received server messages on minibuffer.")

(defvar mcfunction-server-command "java -Xms1024M -Xmx1024M -jar server.jar nogui")

(defvar mcfunction-server-directory ".")

(defvar mcfunction-server-working-directory nil "When this is nil, Working directory is mcfunction-server-directory.")

(defconst mcfunction-server-buffer-name "*Minecraft-Server*")

(defvar mcfunction--server-state nil "This variable represents the state of the server.  Value is nil, 'starting', 'ready' or 'finishing'.")

(defvar mcfunction--server-message-buffer nil "Buffered server messages.")

(defun mcfunction--server-ready ()
  "Return server is completely ready."
  (and (eq mcfunction--server-state 'ready)
       (processp (get-process "mcserver"))))

(defun mcfunction-format-server-message (time thread level body)
  "Return a formatted server message string.  Parameter is TIME, THREAD, LEVEL and BODY."
  (format "%s %s" level body)
)

(defun mcfunction--server-filter (proc output)
  "Filter function for Minecraft server process.  PROC is a server process, OUTPUT is a server output."
  (setq mcfunction--server-message-buffer
        (append mcfunction--server-message-buffer (list output)))
  (if (string-match ".+\n$" output)
      (let ((msg (mapconcat 'identity mcfunction--server-message-buffer ""))
            (fmt-msg nil))
        (setq mcfunction--server-message-buffer nil)
        (setq fmt-msg
              (mapconcat
               (lambda (line)
                 (if (string-match "\\[\\([0-9:]+\\)\\] \\[\\([^]]+\\)/\\([^]]+\\)\\]: \\(.+\\)$" line)
                     ;; Matched.
                     (let ((time (match-string-no-properties 1 line))
                           (thread (match-string-no-properties 2 line))
                           (level (match-string-no-properties 3 line))
                           (body (match-string-no-properties 4 line)))
                       (when (string-match
                              ;; Done (4.422s)! For help, type "help"
                              ;; Server initialized message like this.
                              "^Done ([0-9.]+s)! For help, type \"help\"$"
                              body)
                         (setq mcfunction--server-state 'ready))
                       (mcfunction-format-server-message time thread level body))
                   ;; Not matched.
                   (if (eq (string-width line) 0)
                       ""
                     (concat "UnknownMessage:" line))))
               (split-string msg "\n")
               "\n"))
        (when mcfunction-display-server-messages
          (princ fmt-msg))
        (with-current-buffer mcfunction-server-buffer-name
          (goto-char (point-max))
          ;; (if my-out
          ;;     (insert msg)
          (if (not (string-match "Summoned" msg))
              (insert msg))
          (goto-char (point-max))))))


(defun mcfunction-send-string (str)
  "Send STR to minecraft server."
  (interactive "MCommand: ")
  (when (mcfunction--server-ready) ;; finishing!!
    (progn
      (when (string-equal str "stop")
        (setq mcfunction--server-state 'finishing))
      (setq mcfunction--server-message-buffer nil)
      ;; (with-current-buffer mcfunction-server-buffer-name
      ;;   (goto-char (point-max))
      ;;   (insert (concat ">> " str "\n"))
      ;;   (goto-char (point-max)))
      (process-send-string "mcserver" (concat str "\n")))))

(defun mcfunction-start-server ()
  "Start minecraft server."
  (interactive)
  (setq mcfunction--server-message-buffer nil)
  (if (processp (get-process "mcserver"))
      (message "Minecraft server is already running.")
    ;; else
    (let ((default-directory (or mcfunction-server-working-directory
                                 mcfunction-server-directory))
          (server nil))
      (setq mcfunction--server-state 'starting)
      (apply 'start-process
             (append (list "mcserver" mcfunction-server-buffer-name)
                     (split-string mcfunction-server-command " +")))
      (setq server (get-process "mcserver"))
      (set-process-filter server 'mcfunction--server-filter)
      (set-process-sentinel server 'mcfunction-server-sentinel)
      (message "Starting Minecraft server..."))))


(defun mcfunction-server-sentinel (process signal)
  "Minecraft server process sentinel function.  PROCESS is server process.  SIGNAL is server signal(hope \"finished\\n\")."
  (princ (format "Process: %s received the msg: %s" process signal))
  (when (string-equal "finished\n" signal)
    (progn
      (setq mcfunction--server-state 'nil)
      (princ "Minecraft server has been finished.")
      (kill-buffer mcfunction-server-buffer-name))))

(defun mcfunction-stop-server ()
  "Stop minecraft server."
  (interactive)
  (when (mcfunction--server-ready)
    (mcfunction-send-string "stop")))

(defun mcfunction-server-execute-command-at-point ()
  "Execute a command at point."
  (interactive)
  (if (mcfunction--server-ready)
      (let ((raw (thing-at-point 'line t))
            (line))
        (when (string-match "^[# ]*\\(.+\\)$" raw)
          (progn (setq line (match-string 1 raw))
                 (mcfunction-send-string line))))
    (message "Minecraft server is not ready.")))

(provide 'mcfunction-mode)
;;; mcfunction-mode.el ends here
