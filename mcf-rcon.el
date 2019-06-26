;;; mcf-rcon.el --- Minecraft RCON client -*- lexical-binding: t -*-

;; Copyright (C) 2019 rasensuihei

;; Author: rasensuihei <rasensuihei@gmail.com>
;; URL: https://github.com/rasensuihei/mcf-mode
;; Version: 0.2.2
;; Keywords: comm
;; Package-Requires: ((emacs "24.1"))

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

;; See also:
;; https://github.com/rasensuihei/mcf-mode

;;; Code:
(require 'bindat)
(require 'let-alist)

(defvar mcf-rcon-port 25575 "RCON server port.")
(defvar mcf-rcon-address "localhost" "RCON server address.")
(defvar mcf-rcon-password "" "RCON server password.")
(defvar mcf-rcon-family 'ipv4 "Network family.")
(defvar mcf-rcon-print-packet-information nil "Print packet information to RCON buffer.")
;; Private constants.
(defconst mcf-rcon--packet-header-spec
  '((:size u32r)
    (:id u32r)
    (:type u32r)))
;; Size field and body string length are pretty tricky.
;; Size : ID(4) + type(4) + body-length(n) + 0x00(1) + 0x00(1)
;;        = body-length(n) + 10
(defconst mcf-rcon--packet-spec
  '((:size u32r)
    (:id u32r)
    (:type u32r)
    ;; Body length is calculated dynamically.
    (:body str (eval (- (bindat-get-field struct :size) 10)))
    (:terminator byte)
    (:terminator byte)))
(defconst mcf-rcon--64bit-environment (> most-positive-fixnum (lsh 1 31)) "Value is 't' when 64-bit environment.")
;; Private variables.
(defvar mcf-rcon--proc nil "RCON process.")
(defvar mcf-rcon--proc-name "mcf-rcon")
(defvar mcf-rcon--buffer-name "*RCON*" "RCON buffer name.")
(defvar mcf-rcon--id-count 0 "Current packet id.")
(defvar mcf-rcon--authorized nil "RCON is authorized when not nil.")
(defvar mcf-rcon--auth-timer nil "RCON auth timer.")
(defvar mcf-rcon--response-handlers (make-hash-table :test 'eql) "Hashtable for server response.  Structure is ID : (COMMAND_STR HANDLER_FUNCTION).")
;; (defvar mcf-rcon--packet-log nil "")

;;;###autoload
(defun mcf-rcon ()
  "Start Minecraft RCON client."
  (interactive)
  (let ((buffer (get-buffer-create mcf-rcon--buffer-name)))
    (display-buffer buffer)
    (with-current-buffer buffer
      (unless (processp mcf-rcon--proc)
        (mcf-rcon--make-network-process)
        ))
    (mcf-rcon--auth)))

(defun mcf-rcon-disconnect ()
  "Disconnect to/from the server."
  (interactive)
  (when (timerp mcf-rcon--auth-timer)
    (cancel-timer mcf-rcon--auth-timer))
  (when (processp mcf-rcon--proc)
    (delete-process mcf-rcon--proc))
  (setq mcf-rcon--authorized nil
        mcf-rcon--proc nil))

(defun mcf-rcon-execute-command (str &optional handler)
  "Execute Minecraft command STR.  HANDLER is a function for server response handle."
  (mcf-rcon--check-status)
  (let ((id (mcf-rcon--provide-id)))
    (mcf-rcon--send-packet id 2 str)
    (mcf-rcon--send-packet -2 0 "") ; Send a empty SERVERDATA_RESPONSE_VALUE packet.
    (puthash id (cons str handler) mcf-rcon--response-handlers)
    id))

(defun mcf-rcon--pre-pack-s32 (n)
  "Convert a signed integer to bindat's unsigned 32-bit integer.  N is a signed 29/61-bit integer."
  (if mcf-rcon--64bit-environment
      (logior (if (< n 0) (lsh 1 31) 0) (logand n 2147483647)) ; sign | n & 0x7fffffff
    n))

(defun mcf-rcon--post-unpack-s32 (n)
  "Convert bindat's unsigned 32-bit integer to a signed integer.  N is an unsigned integer(64-bit environment) or singned integer(32-bit environment)."
  (if mcf-rcon--64bit-environment
      (if (= 0 (logand n (lsh 1 31)))
          n
        (logior (ash most-negative-fixnum -30) n))
    n))

(defun mcf-rcon--log (str)
  "Insert log STR to RCON buffer."
  (if (eq (current-buffer) (get-buffer mcf-rcon--buffer-name))
    (save-excursion
      (goto-char (point-max))
      (insert str))
    (with-current-buffer mcf-rcon--buffer-name
      (goto-char (point-max))
      (insert str)
      (goto-char (point-max)))))

(defun mcf-rcon--make-network-process ()
  "Connect to RCON server."
  (unless (processp mcf-rcon--proc)
    (setq mcf-rcon--proc
          (make-network-process
           :name mcf-rcon--proc-name
           :family mcf-rcon-family
           :buffer mcf-rcon--buffer-name
           :address mcf-rcon-address
           :service mcf-rcon-port
           :nowait t
           :filter 'mcf-rcon--filter
           :sentinel 'mcf-rcon--sentinel))))

(defun mcf-rcon--auth ()
  "Auth to the server."
  (unless mcf-rcon--authorized
    (let ((password (if mcf-rcon-password mcf-rcon-password (read-passwd "RCON password: "))))
      (setq mcf-rcon--auth-timer
            (run-at-time
             "1 sec" 10
             (lambda ()
               (unwind-protect
                   (when (processp mcf-rcon--proc)
                     (mcf-rcon--send-packet (mcf-rcon--provide-id) 3 password))
                 (cancel-timer mcf-rcon--auth-timer))))))))
      
(defun mcf-rcon--send-packet (id type body)
  "Create and send a packet by calculating size, ID, TYPE and BODY."
  (let* ((body (encode-coding-string body 'utf-8))
         (size (+ (length body) 10))
         (struct (bindat-pack mcf-rcon--packet-spec
                              `((:size . ,(mcf-rcon--pre-pack-s32 size))
                                (:id . ,(mcf-rcon--pre-pack-s32 id))
                                (:type . ,(mcf-rcon--pre-pack-s32 type))
                                (:body . ,body)
                                (:terminator . 0)))))
    (with-current-buffer mcf-rcon--buffer-name
      (process-send-string mcf-rcon--proc struct))
    (if mcf-rcon-print-packet-information
        (mcf-rcon--log (format "[%s %s %s %s] =>\n" size id type body)))
    id))

(defun mcf-rcon--check-status ()
  "Check network status."
  (unless (processp mcf-rcon--proc)
    (error "RCON client is not running.  Try M-x mcf-rcon"))
  (unless mcf-rcon--authorized
    (error "RCON is not authorized")))

(defvar mcf-rcon--stream-buffer nil "Network stream buffer.  Value is a unibyte.")
(defvar mcf-rcon--current-header nil "Processing packet's header alist.")
(defvar mcf-rcon--body-list nil "Multiple-packet body list.  It's a second stream buffer.")
(defvar mcf-rcon--last-response-id nil "Last read multiple-packet id.")

(defun mcf-rcon--clear ()
  "Clear variables."
  (setq mcf-rcon--stream-buffer nil
        mcf-rcon--current-header nil
        mcf-rcon--body-list nil
        mcf-rcon--last-response-id nil))

(defun mcf-rcon--filter(_proc string)
  "RCON filter function. STRING is multibyte string."
  ;; (push string mcf-rcon--packet-log)
  (mcf-rcon--append-buffer string)
  (let ((loop t))
    (while loop
      (unless mcf-rcon--current-header
        (setq mcf-rcon--current-header (mcf-rcon--read-header)))
      (if (not mcf-rcon--current-header)
          (setq loop nil)
        (let-alist mcf-rcon--current-header
          (if (<= .rest-size (length mcf-rcon--stream-buffer))
              (let ((header mcf-rcon--current-header)
                    (body (decode-coding-string
                           (mcf-rcon--read (- .rest-size 2))
                           'utf-8)))
                (setq mcf-rcon--current-header nil)
                (mcf-rcon--read 2) ;; Read last 0x0 0x0.
                (mcf-rcon--filter-response header body))
            (setq loop nil)))))))

(defun mcf-rcon--filter-response(header body)
  "Process response packet HEADER and BODY."
  (let-alist header
    (cond
     ((= .id -1) ; Auth failed.
      (setq mcf-rcon--authorized nil)
      (mcf-rcon--log "RCON is not authorized."))
     ((= .id -2) ; End of RESPONSE_VALUE packet.
      (let ((payload (mcf-rcon--concat-multiple-packets))
            (command (gethash mcf-rcon--last-response-id mcf-rcon--response-handlers)))
        (mcf-rcon--command-log (car command) payload)
        (when (cdr command)
          (funcall (cdr command) payload))
        (remhash mcf-rcon--last-response-id mcf-rcon--response-handlers)))
     ((= .type 3) ; SERVERDATA_AUTH
      (setq mcf-rcon--authorized t))
     ((= .type 2) ; SERVERDATA_AUTH_RESPONSE
      (setq mcf-rcon--authorized t))
     ((= .type 0) ; SERVERDATA_RESPONSE_VALUE
      (setq mcf-rcon--last-response-id .id)
      (push body mcf-rcon--body-list))
     (t (error (format "Unknown packet header: %s" header))))
    (when mcf-rcon-print-packet-information
      (mcf-rcon--log
       (format "[%s %s %s %s]\n" .size .id .type body)))))

(defun mcf-rcon--concat-multiple-packets ()
  "Return a concatenated packet bodies."
  (let ((payload
         (decode-coding-string
          (mapconcat 'identity (reverse mcf-rcon--body-list) "") 'utf-8)))
    (setq mcf-rcon--body-list nil)
    payload))

(defun mcf-rcon--command-log (command payload)
  "Insert executed COMMAND and result PAYLOAD to RCON buffer."
  (mcf-rcon--log (concat command " => " payload "\n")))

(defun mcf-rcon--append-buffer (string)
  "Append STRING to stream buffer."
  (setq mcf-rcon--stream-buffer
        (concat mcf-rcon--stream-buffer (encode-coding-string string 'binary))))

(defun mcf-rcon--read (length)
  "Read a unibyte according to the specified LENGTH or nil."
  (if (> length (length mcf-rcon--stream-buffer))
      nil
    (let ((bytes mcf-rcon--stream-buffer)
          result)
      (setq result (substring bytes 0 length))
      (setq mcf-rcon--stream-buffer
            (substring bytes length (length bytes)))
      result)))

(defun mcf-rcon--read-header ()
  "Return a header alist or nil."
  (let ((bytes (mcf-rcon--read 12)))
    (if bytes
        (let* ((struct (bindat-unpack mcf-rcon--packet-header-spec bytes))
               (size (mcf-rcon--post-unpack-s32 (bindat-get-field struct :size))))
          (list
           (cons 'size size)
           (cons 'id (mcf-rcon--post-unpack-s32 (bindat-get-field struct :id)))
           (cons 'type (mcf-rcon--post-unpack-s32 (bindat-get-field struct :type)))
           (cons 'rest-size (- size 8))))
      nil)))

(defun mcf-rcon--sentinel (proc msg)
  "RCON sentinel function.  PROC is a server process, MSG is server message."
  (with-current-buffer mcf-rcon--buffer-name
    (goto-char (point-max))
    (insert (format "%s: %s" proc msg))
    (when (or (string-match "^connection broken by remote peer" msg)
              (string-match "^deleted" msg)
              (string-match "^failed with code" msg))
      (mcf-rcon-disconnect))))

(defun mcf-rcon--provide-id ()
  "Provide RCON packet id."
  (setq mcf-rcon--id-count (+ mcf-rcon--id-count 1))
  (unless (integerp mcf-rcon--id-count)
    (setq mcf-rcon--id-count 0))
  mcf-rcon--id-count)

(provide 'mcf-rcon)
;;; mcf-rcon.el ends here
