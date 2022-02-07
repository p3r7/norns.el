# norns.el

Helper Commands for interacting w/ one (or several) monome norns from within Emacs.

Support for targeting remote norns instances (via TRAMP) or even Emacs running on norns itself.


## Commands

All commands (unless specified otherwise) will analyze if currently visited file is on a norns.

If it's the case, this particular norns is targeted by the command execution. Otherwise the default norns instance (configurable w/ `norns-host` / `norns-mdns-domain` is targeted instead).

A buffer `*maiden/<NORNS_HOSTNAME>*` is spawned, displaying maiden output, and pops in a new window.

Those behaviors can be customized by tweaking the values of `norns-access-policy` and `norns-maiden-switch-on-cmd`.


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

The package is not yet available on [Melpa](https://melpa.org/).

For now, the recommended way to install is with [use-package](https://github.com/jwiegley/use-package), [quelpa](https://github.com/quelpa/quelpa) and [quelpa-use-package](https://github.com/quelpa/quelpa-use-package).

To get only the web browsing mode:

```el
(use-package norns
  :quelpa (norns :fetcher github :repo "p3r7/norns"))
```


## Configuration

Several norns can be managed by this package.

Default value of `norns-access-policy` (`:current-fallback-default`) makes the execution of commands resolve to the currently visited norns instance or fallbacks to the "default" one.

To always use the default instance, change this value to `:default`. Otherwise, to only try the currently visited instance set it to `:current`.

The default norns instance declaration is accessible through vars `norns-host` & `norns-mdns-domain`.

On command execution, the maiden buffer (of the corresponding instance) will pop to current frame (can be disabled by setting `norns-maiden-switch-on-cmd` to `nil`) w/ method `norns-maiden-switch-fn` (defaults to `switch-to-buffer-other-window`).

This new window will not steal focus, but one can change that by setting `norns-maiden-switch-no-focus` to `nil`.


## Legibility

This code uses form feeds (`^L` character) as separators.

Either package [form-feed](https://github.com/wasamasa/form-feed) or [page-break-lines](https://github.com/purcell/page-break-lines) makes them appear as intended.
