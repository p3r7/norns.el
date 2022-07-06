# norns.el

<div align=center><img alt="logo" width="572" height="222" src="icon.png"></div>

Interactive development environment for monome norns from Emacs.

Support for targeting remote norns instances (via TRAMP) or even Emacs running on norns itself (untested).

Several norns instances can be interacted with concurently (all buffers are namespaced by norns' hostname).

[![GNU Emacs](https://github.com/minad/corfu/blob/screenshots/emacs.svg?raw=true)](https://www.gnu.org/software/emacs/)
[![MELPA](https://melpa.org/packages/norns-badge.svg)](https://melpa.org/#/norns)


## Commands

All commands (unless specified otherwise) will analyze if currently visited file is on a norns.

If it's the case, this particular norns is targeted by the command execution. Otherwise the default norns instance (configurable w/ `norns-host` / `norns-mdns-domain` is targeted instead).

A maiden REPL (`*maiden/<NORNS_HOSTNAME>*`) or SuperCollider REPL (`*norns-sc/<NORNS_HOSTNAME>*`) is spawned and pops in a new window.

Those behaviors can be customized by tweaking the values of `norns-access-policy` and `norns-repl-switch-on-cmd`.


#### `(norns-maiden-repl)` / `(norns-sc-repl)`

Spawn and switch to maiden REPL / SuperCollider REPL for norns instance.


#### `(norns-docker-maiden-repl)` / `(norns-docker-sc-repl)`

Same as above but connects to a localy-running dockerized norns instance (see [winder/norns-dev](https://github.com/winder/norns-dev/)).

Requires package [docker-tramp](https://github.com/emacs-pe/docker-tramp.el) to access the container's filesystem.

If using a custom container name, tweak value of `norns-docker-container`. Likewise, you may need to adjust the value of `norns-local-mdns-domain` if your LAN domain name is other than `"lan"`.


#### `(norns-maiden-send TXT)` / `(norns-sc-send TXT)` / `(norns-send TXT)`

Prompt user to enter raw text `TXT` command and sends it to maiden / SuperCollider.

Generic version (`norns-send`) auto-selects the right command according to current buffer mode.


#### `(norns-maiden-send-selection)` / `(norns-sc-send-selection)` / `(norns-send-selection)`

Same as above, but acts on selection (*active region* in Emacs lingo).


#### `(norns-load-current-script)`

Ask the visited script to be loaded by the visited norns.

Will fail if currently visited file is not part of a norns script.


#### `(norns-load-script)`

Prompt user for list of available norns scripts and launch the one selected.


#### `(norns-restart)` / `(norns-reboot)`

`norns-restart` restarts all norns services. Respawns the maiden and SuperCollider REPLs.

`norns-reboot` performs a full OS reboot (for when things get stuck bad).


#### `(norns-screenshot <FILENAME>)` / `(norns-screen-dump <FILENAME>)`

Take a screenshot of norns screen. Save it on norns itself under `norns-screenshot-folder`.

`norns-screen-dump` does the same but is lower level. It directly dumps what is in the screen memory buffer and doesn't perform scaling / quantized greyscale conversion to match how it appears IRL.


## Installation

```el
(use-package norns
  :config
  (add-hook 'lua-mode-hook #'norns-mode-maybe-activate)
  (add-hook 'sclang-mode-mode-hook #'norns-mode-maybe-activate))
```


## Configuration

Several norns can be managed by this package.

Default value of `norns-access-policy` (`:current-fallback-default`) makes the execution of commands resolve to the currently visited norns instance or fallbacks to the "default" one.

To always use the default instance (absolute lookup), change this value to `:default`. Otherwise, to only try the currently visited instance (relative lookup) set it to `:current`.

The default norns instance declaration is accessible through vars `norns-host` & `norns-mdns-domain`.

On command execution, the maiden or SuperCollider buffer (of the corresponding instance) will pop to current frame (can be disabled by setting `norns-repl-switch-on-cmd` to `nil`) w/ method `norns-repl-switch-fn` (defaults to `switch-to-buffer-other-window`).

This new window will not steal focus, but one can change that by setting `norns-repl-switch-no-focus` to `nil`.


## Advanced usages

If you want commands to interact w/ a specific norns instance independently of your current location, just define your own commands like so:

```elisp
(use-package norns
  ;;  [...]

  :config
  (defun norns2-send (cmd)
    (interactive "s> ")
    (let ((norns-access-policy :default)
          (norns-host "norns2"))
      (norns-send cmd))))
```

## OSC commands

The package used to embed OSC-based commands to simulate button & encoder presses.

The `osc.el` package appears to be doing some unholy stuff (negative Unix timestamps) which somehow works on Linux x82_64 builds but does not on ARM nor WinNT. Furthermore, as it's in a `defconst` it cannot be monkeypatched.

As a result I decided to remove this dependency.

I may reintroduce it once it gets fixed.

In the meantime, you could add back the OSC-based feature by dropping that in your `init.el`:

```el
(require 'osc)


;; VARS

(defvar norns-osc-port 10111 "Default norns OSC protocol port.")


;; IO - OSC

(defun norns--osc-send (p &rest args)
  "Send OSC message to current norns (w/ path P and optional ARGS)."
  (let* ((default-directory (norns--location-from-access-policy))
         (host (norns--core-curr-host))
         (client (osc-make-client host norns-osc-port)))
    (apply #'osc-send-message client p args)
    (delete-process client)))

(defun norns-key (n z)
  "Change state of key N to value Z (either 0 or 1) on current norns."
  (norns--osc-send "/remote/key" n z))

(defun norns-key-toggle (n)
  "Simulate a user key press on key N on current norns."
  (norns-key n 1)
  (norns-key n 0))

(defun norns-enc (n delta)
  "Simulate a rotation of value DELTA on encoder N on current norns."
  (norns--osc-send "/remote/enc" n delta))
```


## Implementation details

Major modes for REPLs (`norns-maiden-repl-mode` / `norns-sc-repl-mode`) are based on `comint-mode`.

As those communications doesn't rely on a process (but websocket communication instead), we bind a "fake" process and handle output manually by calling `comint-output-filter` (inside of `norns--maiden-output` / `norns--sc-output`).

This trick comes from `ielm`.


## Legibility

This code uses form feeds (`^L` character) as separators.

Either package [form-feed](https://github.com/wasamasa/form-feed) or [page-break-lines](https://github.com/purcell/page-break-lines) makes them appear as intended.
