# norns.el

<div align=center><img alt="logo" width="572" height="222" src="icon.png"></div>

Control one (or several) monome norns from within Emacs.

Support for targeting remote norns instances (via TRAMP) or even Emacs running on norns itself (untested).


## Commands

All commands (unless specified otherwise) will analyze if currently visited file is on a norns.

If it's the case, this particular norns is targeted by the command execution. Otherwise the default norns instance (configurable w/ `norns-host` / `norns-mdns-domain` is targeted instead).

A maiden REPL (`*maiden/<NORNS_HOSTNAME>*`) or SuperCollider REPL (`*norns-sc/<NORNS_HOSTNAME>*`) is spawned and pops in a new window.

Those behaviors can be customized by tweaking the values of `norns-access-policy` and `norns-repl-switch-on-cmd`.


#### `(norns-maiden-repl)` / `(norns-sc-repl)`

Spawn and switch to maiden REPL / SuperCollider REPL for norns instance.


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


## Installation

The package is not yet available on [MELPA](https://melpa.org/).

For now, the recommended way to install is with [use-package](https://github.com/jwiegley/use-package), [quelpa](https://github.com/quelpa/quelpa) and [quelpa-use-package](https://github.com/quelpa/quelpa-use-package).

To get only the web browsing mode:

```el
(use-package norns
  :quelpa (norns :fetcher github :repo "p3r7/norns.el")
  :config
  (add-hook 'lua-mode-hook #'norns-mode-hook)
  (add-hook 'sclang-mode-mode-hook #'norns-mode-hook))
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
  (defun norns2-send-command (cmd)
    (let ((norns-access-policy :default)
          (norns-host "norns2"))
      (norns-send-command cmd))))
```

## Implementation details

Major mode for maiden REPL (`norns-maiden-repl-mode`) is based on `comint-mode`.

As maiden communication doesn't rely on a process (but websocket communication instead), we bind a "fake" process and handle output manually by calling `comint-output-filter` (inside of `norns--maiden-output`).

This trick comes from `ielm`.


## Legibility

This code uses form feeds (`^L` character) as separators.

Either package [form-feed](https://github.com/wasamasa/form-feed) or [page-break-lines](https://github.com/purcell/page-break-lines) makes them appear as intended.
