# mcfunction-mode

![Screenshot](ss00.png)

Emacs major mode for editing Minecraft mcfunction.

The main features of this mode are Minecraft mcfunction syntax highlighting.

**As it is currently under development, there can be significant changes.**

## Setting example

~~~elisp
(require 'mcfunction-mode)
~~~

## Default key bindings
* `C-c C-c` `mcfunction-execute-command`
* `C-c C-e` `mcfunction-execute-command-at-point`

---

# mcrcon.el

mcrcon.el is software to communicate with Minecraft RCON server.

*Warning: Minecraft RCON is not thread safe before 1.14.3-pre2.*

## Setup RCON server

Edit `server.properties` file located in the Minecraft server directory.
~~~
rcon.port=25575
enable-rcon=true
rcon.password=PASSWORD
~~~

## Settings example

~~~elisp
(require 'mcrcon)
(setq mcrcon-password "PASSWORD")

;; Another settings.
(setq 
 ;; Default host address.
 mcrcon-address "localhost"
 ;; Default port number.
 mcrcon-port 25575
 ;; Describe packet information.
 mcrcon-print-packet-infomation t)
~~~

## Usage

* `M-x mcrcon` to connect to Minecraft RCON server.
* `M-x mcrcon-disconnect` to disconnect from Minecraft RCON server.

### Minecraft command macro `mceval`

~~~ elisp
(mceval "help help")

(mceval "list" (payload)
  (string-match "There are \\([0-9]+\\) of a max 20 players online" payload)
  (when (equal (match-string 1 payload) "0")
    (message "Nobody is here :(")
    (mceval "summon creeper 0 10 0 {CustomName:\"{\\\"text\\\":\\\"Player\\\"}\"}")))
~~~

`mceval` does not block processing, and BODY is evaluated when the server responds.

`mceval` must be nested.  If you call consecutively in a loop, the Minecraft RCON server will disconnect the client.

## Changelog
### 0.2.1
* Fixed: `mcfunction-execute-command-at-point' is duplicated.
### 0.2
* Minecraft RCON client (mcrcon.el) supported.
* The IPC server communication feature has been deprecated.
* More colorful highlights.
### 0.1
* First commit.

## TODO
* Scanning syntax from the server's help results, It's to use for highlighting and completion.
* datatag <=> sexp conversion.
* Automatic reconnect when disconnected from server.
* More hooks.
