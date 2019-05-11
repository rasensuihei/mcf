# mcfunction-mode
Emacs major mode for editing Minecraft mcfunction.

The main features of this mode are Minecraft mcfunction syntax highlighting and interprocess communication (IPC) with the Minecraft server.

## Setting example
~~~elisp
(require 'mcfunction-mode)
;; Your server.jar location.
(setq mcfunction-server-directory "~/.minecraft/server/")
;; This is a default value.
(setq mcfunction-server-command "java -Xms1024M -Xmx1024M -jar server.jar nogui")

(add-to-list 'auto-mode-alist '("\\.mcfunction\\'" . mcfunction-mode))
~~~


## Default key bindings:
* C-c C-c mcfunction-send-string
* C-c C-e mcfunction-execute-command-at-point
* C-c C-k mcfunction-stop-server
* C-c C-r mcfunction-start-server

## TODO
* Scanning syntax from the server's help results, It's to use for highlighting and completion.
