;;; mcrcon.el --- Minecraft RCON client -*- lexical-binding: t -*-

;; Copyright (C) 2019 rasensuihei

;; Author: rasensuihei <rasensuihei@gmail.com>
;; URL: https://github.com/rasensuihei/mcfunction-mode
;; Version: 0.2
;; Keywords: network

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

;; This is software to communicate with Minecraft RCON server.

;; Settings example

;; (require 'mcrcon)
;; (setq mcrcon-password "PASSWORD")

;; Usage

;; `M-x mcrcon` to connect to Minecraft RCON server.
;; `M-x mcrcon-disconnect` to disconnect from Minecraft RCON server.

;; See also:
;; https://github.com/rasensuihei/mcfunction-mode

;;; Code:
(require 'bindat)
(require 'let-alist)

(defvar mcrcon-port 25575 "RCON server port.")
(defvar mcrcon-address "localhost" "RCON server address.")
(defvar mcrcon-password "" "RCON server password.")
(defvar mcrcon-family 'ipv4 "Network family.")
(defvar mcrcon-print-packet-information nil "Print packet information to RCON buffer.")
(defvar mcrcon--packet-spec
  ;; Size field and body vector length are pretty tricky.
  ;; Size : ID(4) + type(4) + body-length(n) + 0x00(1) + 0x00(1)
  ;;        = body-length(n) + 10
  '((:size u32r)
    (:id u32r)
    (:type u32r)
    ;; Body length is calculated dynamically.
    (:body str (eval (- (bindat-get-field struct :size) 10)))
    (:terminator byte)
    (:terminator byte)))
(defvar mcrcon--packet-header-spec
  '((:size u32r)
    (:id u32r)
    (:type u32r)))
;; Private variables.
(defvar mcrcon--proc nil "RCON process.")
(defvar mcrcon--buffer-name "*RCON*" "RCON buffer name.")
(defvar mcrcon--id-count 0 "Current packet id.")
(defvar mcrcon--authorized nil "RCON is authorized when not nil.")
(defvar mcrcon--auth-timer nil "RCON auth timer.")
(defvar mcrcon--64bit-environment (> most-positive-fixnum (lsh 1 31)) "Value is 't' when 64-bit environment.")
(defvar mcrcon--response-handlers (make-hash-table :test 'eql) "Hashtable for server response.  Structure is ID : (COMMAND_STR HANDLER_FUNCTION).")
;; (defvar mcrcon--packet-log nil "")

;;;###autoload
(defun mcrcon ()
  "Start Minecraft RCON client."
  (interactive)
  (let ((buffer (get-buffer-create mcrcon--buffer-name)))
    (display-buffer buffer)
    (with-current-buffer buffer
      (unless (processp mcrcon--proc)
        (mcrcon--make-network-process)
        ))
    (mcrcon--auth)))

(defun mcrcon-disconnect ()
  "Disconnect to/from the server."
  (interactive)
  (when (timerp mcrcon--auth-timer)
    (cancel-timer mcrcon--auth-timer))
  (when (processp mcrcon--proc)
    (delete-process mcrcon--proc))
  (setq mcrcon--authorized nil
        mcrcon--proc nil))

(defun mcrcon-execute-command (str &optional handler)
  "Execute Minecraft command STR.  HANDLER is a function for server response handle."
  (interactive "MCommand: ")
  (mcrcon--check-status)
  (let ((id (mcrcon--provide-id)))
    (mcrcon--send-packet id 2 str)
    (mcrcon--send-packet -2 0 "") ; Send empty SERVERDATA_RESPONSE_VALUE packet.
    (puthash id
             ;; (list str (if handler handler (lambda (payload) payload)))
             (cons str handler)
             mcrcon--response-handlers)
    id))

(defun mcrcon-execute-command-at-point ()
  "Execute a command at point."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (when (string-match "^[# ]*\\(.+\\)$" line)
      (mcrcon-execute-command (match-string 1 line)))))

(defun mcrcon--pre-pack-s32 (n)
  "Convert a signed integer to bindat's unsigned 32-bit integer.  N is a signed 29/61-bit integer."
  (if mcrcon--64bit-environment
      (logior (if (< n 0) (lsh 1 31) 0) (logand n 2147483647)) ; sign | n & 0x7fffffff
    n))

(defun mcrcon--post-unpack-s32 (n)
  "Convert bindat's unsigned 32-bit integer to a signed integer.  N is an unsigned integer(64-bit environment) or singned integer(32-bit environment)."
  (if mcrcon--64bit-environment
      (if (= 0 (logand n (lsh 1 31)))
          n
        (logior (ash most-negative-fixnum -30) n))
    n))

(defun mcrcon--log (str)
  "Insert log STR to RCON buffer."
  (if (eq (current-buffer) (get-buffer mcrcon--buffer-name))
    (save-excursion
      (goto-char (point-max))
      (insert str))
    (with-current-buffer mcrcon--buffer-name
      (goto-char (point-max))
      (insert str)
      (goto-char (point-max)))))

(defun mcrcon--make-network-process ()
  "Connect to RCON server."
  (unless (processp mcrcon--proc)
    (setq mcrcon--proc
          (make-network-process
           :name "mcrcon"
           :family mcrcon-family
           :buffer mcrcon--buffer-name
           :address mcrcon-address
           :service mcrcon-port
           :nowait t
           :filter 'mcrcon--filter
           :sentinel 'mcrcon--sentinel))))

(defun mcrcon--auth ()
  "Auth server."
  (when (not mcrcon--authorized)
    (setq mcrcon--auth-timer
          (run-at-time
           "1 sec" 10
           (lambda ()
             (unwind-protect
                 (when (processp mcrcon--proc)
                   (mcrcon--send-packet (mcrcon--provide-id) 3 mcrcon-password))
               (cancel-timer mcrcon--auth-timer)))))))

(defun mcrcon--send-packet (id type body)
  "Create and send a packet by calculating size, ID, TYPE and BODY."
  (let* ((body (encode-coding-string body 'utf-8))
         (size (+ (length body) 10))
         (struct (bindat-pack mcrcon--packet-spec
                              `((:size . ,(mcrcon--pre-pack-s32 size))
                                (:id . ,(mcrcon--pre-pack-s32 id))
                                (:type . ,(mcrcon--pre-pack-s32 type))
                                (:body . ,body)
                                (:terminator . 0)))))
    (with-current-buffer mcrcon--buffer-name
      (process-send-string mcrcon--proc struct))
    (if mcrcon-print-packet-information
        (mcrcon--log (format "[%s %s %s %s] =>\n" size id type body)))
    id))

(defun mcrcon--check-status ()
  "Check network status."
  (when (not (processp mcrcon--proc))
    (error "RCON client is not running.  Try M-x mcrcon"))
  (when (not mcrcon--authorized)
    (error "RCON is not authorized")))

(defvar mcrcon--stream-buffer nil "Network stream buffer.  Value is a unibyte.")
(defvar mcrcon--current-header nil "Processing packet's header alist.")
(defvar mcrcon--body-list nil "Multiple-packet body list.  It's a second stream buffer.")
(defvar mcrcon--last-response-id nil "Last read multiple-packet id.")

(defun mcrcon--clear ()
  "Clear variables."
  (setq mcrcon--stream-buffer nil
        mcrcon--current-header nil
        mcrcon--body-list nil))

(defun mcrcon--filter(proc string)
  "RCON filter function.  PROC is a server process, STRING is multibyte string."
  ;; (push string mcrcon--packet-log)
  (mcrcon--append-buffer string)
  (let ((loop t))
    (while loop
      (unless mcrcon--current-header
        (setq mcrcon--current-header (mcrcon--read-header)))
      (if (not mcrcon--current-header)
          (setq loop nil)
        (let-alist mcrcon--current-header
          (if (<= .rest-size (length mcrcon--stream-buffer))
              (let ((header mcrcon--current-header)
                    (body (decode-coding-string
                           (mcrcon--read (- .rest-size 2))
                           'utf-8)))
                (setq mcrcon--current-header nil)
                (mcrcon--read 2) ;; Read last 0x0 0x0.
                (mcrcon--filter-response header body))
            (setq loop nil)))))))

(defun mcrcon--filter-response(header body)
  "Process response packet HEADER and BODY."
  (let-alist header
    (cond
     ((= .id -1) ; Auth failed.
      (setq mcrcon--authorized nil)
      (mcrcon--log "RCON is not authorized."))
     ((= .id -2) ; End of RESPONSE_VALUE packet.
      (let ((payload (mcrcon--concat-multiple-packets))
            (command (gethash mcrcon--last-response-id mcrcon--response-handlers)))
        (mcrcon--command-log (car command) payload)
        (when (cdr command)
          (funcall (cdr command) payload))
        (remhash mcrcon--last-response-id mcrcon--response-handlers)))
     ((= .type 3) ; SERVERDATA_AUTH
      (setq mcrcon--authorized t))
     ((= .type 2) ; SERVERDATA_AUTH_RESPONSE
      (setq mcrcon--authorized t))
     ((= .type 0) ; SERVERDATA_RESPONSE_VALUE
      (setq mcrcon--last-response-id .id)
      (push body mcrcon--body-list)))
    (when mcrcon-print-packet-information
      (mcrcon--log
       (format "[%s %s %s %s]\n" .size .id .type body)))))

(defun mcrcon--concat-multiple-packets ()
  "Return a concatenated packet bodies."
  (let ((payload
         (decode-coding-string
          (mapconcat 'identity (reverse mcrcon--body-list) "") 'utf-8)))
    (setq mcrcon--body-list nil)
    payload))

(defun mcrcon--command-log (command payload)
  "Insert executed COMMAND and result PAYLOAD to RCON buffer."
  (mcrcon--log (concat command " => " payload "\n")))

(defun mcrcon--append-buffer (string)
  "Append STRING to stream buffer."
  (setq mcrcon--stream-buffer
        (concat mcrcon--stream-buffer (encode-coding-string string 'binary))))

(defun mcrcon--read (length)
  "Read a unibyte according to the specified LENGTH or nil."
  (if (> length (length mcrcon--stream-buffer))
      nil
    (let ((bytes mcrcon--stream-buffer)
          result)
      (setq result (substring bytes 0 length))
      (setq mcrcon--stream-buffer
            (substring bytes length (length bytes)))
      result)))

(defun mcrcon--read-header ()
  "Return a header alist or nil."
  (let ((bytes (mcrcon--read 12)))
    (if bytes
        (let* ((struct (bindat-unpack mcrcon--packet-header-spec bytes))
               (size (mcrcon--post-unpack-s32 (bindat-get-field struct :size))))
          (list
           (cons 'size size)
           (cons 'id (mcrcon--post-unpack-s32 (bindat-get-field struct :id)))
           (cons 'type (mcrcon--post-unpack-s32 (bindat-get-field struct :type)))
           (cons 'rest-size (- size 8))))
      nil)))

(defun mcrcon--sentinel (proc msg)
  "RCON sentinel function.  PROC is a server process, MSG is server message."
  (with-current-buffer mcrcon--buffer-name
    (goto-char (point-max))
    (insert (format "%s: %s" proc msg))
    (when (or (string-match "^connection broken by remote peer" msg)
              (string-match "^deleted" msg)
              (string-match "^failed with code" msg))
      (mcrcon-disconnect))))

(defun mcrcon--provide-id ()
  "Provide RCON packet id."
  (setq mcrcon--id-count (+ mcrcon--id-count 1))
  (unless (integerp mcrcon--id-count)
    (setq mcrcon--id-count 0))
  mcrcon--id-count)

(defmacro mceval (command &rest args)
  "(mceval COMMAND ARGS...)

Evaluate minecraft command with RCON server."
  (declare (indent defun) (debug t))
  (if args
      `(mcrcon-execute-command ,command (lambda (x) (let ((,(caar args) x)) ,@(cdr args))))
    `(mcrcon-execute-command ,command)))

(provide 'mcrcon)
;;; mcrcon.el ends here
