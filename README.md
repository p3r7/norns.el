# norns.el

<div align=center><img alt="logo" width="572" height="222" src="icon.png"></div>

Control one (or several) monome norns from within Emacs.

Support for targeting remote norns instances (via TRAMP) or even Emacs running on norns itself.


## Commands

All commands (unless specified otherwise) will analyze if currently visited file is on a norns.

If it's the case, this particular norns is targeted by the command execution. Otherwise the default norns instance (configurable w/ `norns-host` / `norns-mdns-domain` is targeted instead).

A maiden REPL `*maiden/<NORNS_HOSTNAME>*` is spawned and pops in a new window.

Those behaviors can be customized by tweaking the values of `norns-access-policy` and `norns-maiden-switch-on-cmd`.


#### `(norns-maiden-repl)`

Spawn of switch to maiden REPL for norns instance.


#### `(norns-send-command CMD)`

Send raw text `CMD` to maiden.


#### `(norns-load-current-script)`

Ask the visited script to be loaded by the visited norns.

Will fail if currently visited file is not part of a norns script.


#### `(norns-load-script)`

Prompt user for list of available norns scripts and launch the one selected.


#### `(norns-send-selection)`

Send current selection to maiden.


## Installation

The package is not yet available on [MELPA](https://melpa.org/).

For now, the recommended way to install is with [use-package](https://github.com/jwiegley/use-package), [quelpa](https://github.com/quelpa/quelpa) and [quelpa-use-package](https://github.com/quelpa/quelpa-use-package).

To get only the web browsing mode:

```el
(use-package norns
  :quelpa (norns :fetcher github :repo "p3r7/norns.el")
  :config (add-hook 'lua-mode-hook #'norns-lua-mode-hook))
```


## Configuration

Several norns can be managed by this package.

Default value of `norns-access-policy` (`:current-fallback-default`) makes the execution of commands resolve to the currently visited norns instance or fallbacks to the "default" one.

To always use the default instance (absolute lookup), change this value to `:default`. Otherwise, to only try the currently visited instance (relative lookup) set it to `:current`.

The default norns instance declaration is accessible through vars `norns-host` & `norns-mdns-domain`.

On command execution, the maiden buffer (of the corresponding instance) will pop to current frame (can be disabled by setting `norns-maiden-switch-on-cmd` to `nil`) w/ method `norns-maiden-switch-fn` (defaults to `switch-to-buffer-other-window`).

This new window will not steal focus, but one can change that by setting `norns-maiden-switch-no-focus` to `nil`.


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
