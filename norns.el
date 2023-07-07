;;; norns.el --- Interactive development environment for monome norns -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Jordan Besly
;;
;; Version: 0.0.1
;; Keywords: processes, terminals
;; URL: https://github.com/p3r7/norns.el
;; Package-Requires: ((emacs "27.1")(dash "2.17.0")(s "1.12.0")(f "0.20.0")(request "0.3.2")(websocket "1.13"))
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This package provides an interactive development for monome norns.
;;
;; This package allows to spawn REPLs that bind to remote maiden and
;; SuperCollider REPLs (via commands `norns-maiden-repl', `norns-sc-repl')
;; and associated commands to interact with them from Lua and SuperCollider
;; source files.
;;
;; All commands (unless specified otherwise) will analyze if currently
;; visited file is on a norns.  If it's the case, this particular norns is
;; targeted by the command execution.  Otherwise the default norns instance
;; (configurable w/ `norns-host' / `norns-mdns-domain' is targeted
;; instead).  This behaviors can be changed by setting value of
;; `norns-access-policy' to ":current" or ":default".

;;
;; To connect to a REPL, use commands `norns-maiden-repl' and
;; `norns-sc-repl'.  Those REPL provide prompts but one can send text
;; through the minibuffer with `norns-maiden-send' and `norns-sc-send'.
;;
;; Commands that send input to any of the REPL will automatically make the
;; REPL pop in a window if not already visible.  This can be turned off by
;; setting `norns-repl-switch-on-cmd' to nil.
;;
;; Additionally, to send a selected region to the appropriate REPL, use
;; `norns-maiden-send-selection'.
;;
;; The currently visited script can be loaded with
;; `norns-load-current-script'.  If current script has several
;; "sub-scripts", you'll get prompted to select one.
;;
;; `norns-load-script' will list all the scripts on current norns instance
;; and will load the one you would select.
;;
;; For detailed instructions, please look at the README.md at https://github.com/p3r7/norns.el/blob/master/README.md

;;; Code:



;; DEPS

(require 'dash)
(require 's)
(require 'rx)

(require 'f)
(require 'tramp)

(require 'request)
(require 'websocket)



;; VARS

(defvar norns-access-policy :current-fallback-default)

(defvar norns-user "we" "Default norns user.")
(defvar norns-host "norns" "Default norns hostname.")
(defvar norns-http-port 80 "Default norns HTTP port.")
(defvar norns-mdns-domain "local" "Default norns mDNS (aka zeroconf).")

(defvar norns-docker-container "norns-test-dummy" "Default norns docker container name.")
(defvar norns-docker-http-port 5000 "Default norns docker HTTP port.")
(defvar norns-local-mdns-domain "lan"
  "Default LAN mDNS (aka zeroconf), typically when accessing a
docker instance of norns.")

(defvar norns-screenshot-folder "/home/we/dust/" "Folder where to dump screenshots.")

(defvar norns-maiden-ws-port 5555 "Default norns maiden REPL websocket port.")
(defvar norns-maiden-ws-socket-alist nil "Alist containing HOST / MAIDEN-WS-SOCKET associations.")
(defvar norns-maiden-buffer-prefix "maiden" "Prefix for name of maiden REPL buffers.")
(defvar norns-maiden-buff-alist nil "Alist containing HOST / MAIDEN-COMINT-BUFFER associations.")
(defvar norns-maiden-repl-prompt "maiden>> " "Customizable maiden REPL buffer prompt.")
(defconst norns-maiden-repl-prompt-internal "maiden>> "
  "Version of `norns-maiden-repl-prompt' for handling when it gets
redefined at runtime.")
(defvar norns-lua-lib-inspect-url "https://raw.githubusercontent.com/kikito/inspect.lua/master/inspect.lua")

(defvar norns-sc-ws-port 5556 "Default norns SuperCollider REPL websocket port.")
(defvar norns-sc-ws-socket-alist nil "Alist containing HOST / SC-WS-SOCKET associations.")
(defvar norns-sc-buffer-prefix "norns-sc" "Prefix for name of SuperCollider REPL buffers.")
(defvar norns-sc-buff-alist nil "Alist containing HOST / SC-COMINT-BUFFER associations.")
(defvar norns-sc-repl-prompt "sc>> " "Customizable SuperCollider REPL buffer prompt.")
(defconst norns-sc-repl-prompt-internal "sc>> "
  "Version of `norns-sc-repl-prompt' for handling when it gets
redefined at runtime.")

(defvar norns-repl-switch-on-cmd t
  "If non-nil, switch to maiden/SuperCollider REPL buffer after
sending it a command.")
(defvar norns-repl-switch-fn #'switch-to-buffer-other-window "Function to use when `norns-repl-switch-on-cmd' is non-nil.")
(defvar norns-repl-switch-no-focus t
  "If non-nil, don't have popping REPL window steal focus after
calling `norns-repl-switch-fn'.")

(defvar norns-mode-lighter " norns" "Lighter for norns minor mode.")



;; CONSTS - PATHS

(defconst norns-script-path-prefix "/home/we/dust/code/" "Path of script dir on norns.")

(defconst norns-script-rx
  (rx bol
      (literal norns-script-path-prefix)
      (group (one-or-more (any "a-z" "A-Z" "0-9" "-" "_")))
      "/"
      (group (one-or-more (any "a-z" "A-Z" "0-9" "-" "_"))) ".lua"
      eol))

(defconst norns-in-script-dir-rx
  (rx bol
      (literal norns-script-path-prefix)
      (group (one-or-more (any "a-z" "A-Z" "0-9" "-" "_")))
      "/"))



;; CONSTS - LUA SYMBOLS

(defconst norns-lua-special-fns
  '("init" "cleanup"
    "redraw"
    "key" "enc"
    ("osc" . ("event"))
    ("midi" . ("add" "remove" "event"))
    ("arc" . ("add" "remove"))
    ("grid" . ("add" "remove"))
    ("keyboard" . ("code" "char"))
    ("gamepad" . ("dpad" "analog" "axis" "button")))
  "List of norns script-redefinable special lua functions.")

(defconst norns-lua-buitin-consts
  '("inf")
  "List of norns built-in lua constants.")

(defconst norns-lua-buitin-globals
  '("_norns"                            ; weaver
    "_startup" "engine"
    ("keyboard" . ("keymap" "selected_map"))
    ("norns" . ("battery_current" "battery_percent" "blank" "cpu" "cpu_avg" "crow" "disk" "enc" "encoders" "expand_filesystem" "init_done" "is_norns" "is_shield" "menu" "menu_midi_event" "none" "platform" "pmap" "rerun" "script" "scripterror" "shutdown" "state" "system_cmd" "system_glob" "temp" "try" "version"))
    ("_path" . ("enabled_mods" "dust" "data" "home" "tape" "code" "keyboard_layout"
                "favorites" "extn" "audio"))
    ("_menu" . ("page" "errormsg" "rebuild_params" "alt" "timer" "panels" "locked" "key" "previewfile" "keychar" "panel" "shownav" "gamepad_axis" "custom_gamepad_axis" "set_page" "mode" "enc" "scripterror" "showstats" "keycode" "draw_panel" "redraw" "penc" "m" "set_mode" "gamepad_button"))
    )
  "List of norns built-in global lua state vars.")


(defconst norns-lua-buitin-lib-module-methods
  '(
    ;; scheduling
    ("metro" . ("start" "stop"))
    ("poll" . ("callback" "time" "start" "stop" "update"))
    ;; params
    ("params" . (
                 ;; contructor
                 "add" "add_separator" "add_group"
                 "add_trigger" "add_option" "add_number" "add_control"
                 "add_file" "add_text" "add_taper" "add_binary"
                 "set_action"
                 ;; visibility
                 "hide" "show" "visible"
                 ;; lookup props
                 "print" "list" "get_id" "lookup_param" "t" "get_range" "get_allow_pmap"
                 ;; value
                 "get" "get_raw" "string"
                 "set" "set_raw"
                 "bang" "clear"
                 ;; pset
                 "write" "read" "delete" "default"
                 ))
    ;; i/o
    ("midi" . ("send" "note_on" "note_off" "cc" "pitchbend" "key_pressure" "channel_pressure" "program_change" "start" "stop" "continue" "clock" "song_position" "song_select"))
    ("arc" . ("led" "all" "segment" "refresh" "delta"))
    ("grid" . ("led" "all" "refresh" "intensity" ))
    ("gamepad" . ("up" "down" "left" "right"))
    )
  "List of norns built-in lua functions.")

(defconst norns-lua-buitin-lib-module-fns
  '(
    "include"
    ;; scheduling
    ("clock" . ("run" "cancel" "sleep" "sleep" "sync" "tempo_change_handler"))
    ("metro" . ("init" "new" "free" "free_all" "available" "assigned"))
    ("poll" . ("list_names" "set"  "clear_all"))
    ;; params
    ("params" . ("action_read" "action_write" "action_delete"))
    ;; UX
    ("screen" . ("clear" "update" "update_default" "update_low_battery"
                 "ping"
                 "aa"  "level" "line_width" "line_cap" "line_join" "miter_limit"
                 "move" "move_rel"
                 "pixel"
                 "line" "line_rel" "rect"
                 "arc" "circle" "curve" "curve_rel"
                 "close" "stroke" "fill"
                 "text" "text_rotate" "text_right" "text_center" "text_center_rotate" "text_extents" "font_face" "font_size"
                 "display_png" "load_png" "create_image" "display_image" "display_image_region" "draw_to"
                 "peek" "poke"
                 "rotate" "translate" "save" "blend_mode"))

    ;; i/o
    ("osc" . ("send"))
    ("midi" . ("connect" "to_msg" "to_data"))
    ("arc" . ("connect"))
    ("grid" . ("connect"))
    ;; utils
    ("tab" . ("print" "sort" "count" "contains" "invert" "key" "lines" "split" "save" "load" "readonly" "gather" "update" "select_values"))
    ("util" . ("time" "scandir" "file_exists" "file_size" "make_dir" "os_capture" "string_starts" "trim_string_to_width" "clamp" "linexp" "linlin" "explin" "expexp" "round" "round_up" "s_to_hms" "degs_to_rads" "rads_to_degs" "acronym" "wrap" "wrap_max"))
    )
  "List of norns built-in lua methods.")



;; CORE

(defun norns--deep-merge-alists (l1 l2)
  (let ((l1c (copy-alist l1)))
    (mapc (lambda (el)
            (if (and (consp el)
                     (assoc (car el) l1c))
                (let* ((k (car el))
                       (v (cdr el))
                       (other-v (cdr (assoc k l1c))))
                  ;; (push (cons k (append other-v v)) l1c) ; append (std way to replace value in alist)
                  (setf (cdr (assoc k l1c)) (append other-v v))) ; set in place
              ;; else, standard elem
              (push el l1c)))
          l2)
    l1c))



;; EXTRA FONT LOCK - LUA

(defgroup norns-lua-extra-font-lock nil
  "Faces for highlighting text."
  :prefix "norns-lua-extra-font-lock-"
  :group 'faces)

(defface norns-lua-extra-font-lock-norns-constant
  '((t :inherit font-lock-constant-face))
  "The face used to highlight norns' own builtin."
  :group 'norns-lua-extra-font-lock)

(defface norns-lua-extra-font-lock-norns-builtin
  '((t :inherit font-lock-builtin-face))
  "The face used to highlight norns' own builtin."
  :group 'norns-lua-extra-font-lock)

(defface norns-lua-extra-font-lock-norns-special-fn
  '((t :inherit font-lock-warning-face))
  "The face used to highlight norns' own builtin."
  :group 'norns-lua-extra-font-lock)

(defun norns-lua-get-buitin-lib-modules ()
  (-reduce #'norns--deep-merge-alists (list norns-lua-buitin-globals norns-lua-buitin-lib-module-fns norns-lua-buitin-lib-module-methods)))

(eval-and-compile
  (defconst
    norns--lua-special-fns-rx
    (cl-labels
        ((module-name-re (x)
                         (concat "\\(?1:\\_<"
                                 (if (listp x) (car x) x)
                                 "\\_>\\)"))
         (module-members-re (x) (if (listp x)
                                    (concat "\\(?:[ \t]*\\.[ \t]*"
                                            "\\_<\\(?2:"
                                            (regexp-opt (cdr x))
                                            "\\)\\_>\\)?")
                                  "")))

      (concat
       ;; common prefix:
       ;; - beginning-of-line
       ;; - or neither of [ '.', ':' ] to exclude "foo.string.rep"
       ;; - or concatenation operator ".."
       "\\(?:^\\|[^:. \t]\\|[.][.]\\)"
       ;; optional whitespace
       "[ \t]*"
       "\\(?:"
       ;; any of modules/functions
       (mapconcat (lambda (x) (concat (module-name-re x)
                                 (module-members-re x)))
                  norns-lua-special-fns
                  "\\|")
       "\\)"))
    "A regexp that matches norns' special script-level functions."))


;; NB; instpired by `lua-mode''s `lua--builtins'
(eval-and-compile
  (defconst
    norns--lua-builtins-rx
    (let*
        ((modules (norns-lua-get-buitin-lib-modules)))

      (cl-labels
          ((module-name-re (x)
                           (concat "\\(?1:\\_<"
                                   (if (listp x) (car x) x)
                                   "\\_>\\)"))
           (module-members-re (x) (if (listp x)
                                      (concat "\\(?:[ \t]*\\.[ \t]*"
                                              "\\_<\\(?2:"
                                              (regexp-opt (cdr x))
                                              "\\)\\_>\\)?")
                                    "")))

        (concat
         ;; common prefix:
         ;; - beginning-of-line
         ;; - or neither of [ '.', ':' ] to exclude "foo.string.rep"
         ;; - or concatenation operator ".."
         "\\(?:^\\|[^:. \t]\\|[.][.]\\)"
         ;; optional whitespace
         "[ \t]*"
         "\\(?:"
         ;; any of modules/functions
         (mapconcat (lambda (x) (concat (module-name-re x)
                                   (module-members-re x)))
                    modules
                    "\\|")
         "\\)")))
    "A regexp that matches norns' Lua builtin functions & variables."))

(eval-and-compile
  (defconst
    norns--lua-builtins-params-rx
    (let*
        ((modules
          `(("params" . ,(cdr (assoc "params" norns-lua-buitin-lib-module-methods))))))

      (cl-labels
          ((module-name-re (x)
                           (concat "\\(?1:\\_<"
                                   (if (listp x) (car x) x)
                                   "\\_>\\)"))
           (module-members-re (x) (if (listp x)
                                      (concat "\\(?:[ \t]*:[ \t]*"
                                              "\\_<\\(?2:"
                                              (regexp-opt (cdr x))
                                              "\\)\\_>\\)?")
                                    "")))

        (concat
         ;; common prefix:
         ;; - beginning-of-line
         ;; - or neither of [ '.', ':' ] to exclude "foo.string.rep"
         ;; - or concatenation operator ".."
         "\\(?:^\\|[^:. \t]\\|[.][.]\\)"
         ;; optional whitespace
         "[ \t]*"
         "\\(?:"
         ;; any of modules/functions
         (mapconcat (lambda (x) (concat (module-name-re x)
                                   (module-members-re x)))
                    modules
                    "\\|")
         "\\)"))))
  "A regexp that matches norns' Lua builtin global \"params\" object.")

(defun norns--lua-make-font-lock-keywords ()
  `((,norns--lua-builtins-rx
     (1 'norns-lua-extra-font-lock-norns-builtin) (2 'norns-lua-extra-font-lock-norns-builtin nil noerror))
    (,norns--lua-builtins-params-rx
     (1 'norns-lua-extra-font-lock-norns-builtin) (2 'norns-lua-extra-font-lock-norns-builtin nil noerror))
    (,norns--lua-special-fns-rx
     (1 'norns-lua-extra-font-lock-norns-special-fn) (2 'norns-lua-extra-font-lock-norns-special-fn nil noerror))))





;; CORE - PATH

(defun norns--core-curr-fq-path ()
  "Get path (i.e. file-name in Emacs lingo) of current buffer.
it is fully qualified, i.e. w/ a TRAMP prefix if the connection is remote."
  (if (member major-mode '(dired-mode shell-mode))
      default-directory
    (buffer-file-name)))

(defun norns--core-tramp-extract-path (tramp-path)
  "Remove tramp prefix out of TRAMP-PATH, keeping only the filesystem path."
  (let (vec _localname)
    (setq vec (ignore-errors (tramp-dissect-file-name tramp-path)))
    (if vec
        (tramp-file-name-localname vec)
      (user-error "Couldn't parse tramp path %s" tramp-path))))

(defun norns--core-untrampify-path-maybe (fp)
  "If FP is a TRAMP path, keep only the filesystem path (remove prefix)."
  (if (file-remote-p fp)
      (norns--core-tramp-extract-path fp)
    fp))

(defun norns--core-trampify-path-maybe (fp)
  "If `default-directory' is remote, add TRAMP prefix to FP."
  (if (file-remote-p default-directory)
      ;; REVIEW: there's certainly a TRAMP fn to do this...
      (let ((tramp-prefix (s-chop-suffix (norns--core-untrampify-path-maybe default-directory)
                                         default-directory)))
        (concat tramp-prefix fp))
    fp))

(defun norns--core-curr-path ()
  "Get current buffer filesystem path."
  (norns--core-untrampify-path-maybe (norns--core-curr-fq-path)))

(defun norns--core-curr-host ()
  "Get current hostname (for maiden / sc).
Defaults to \"localhost\" if not a TRAMP path."
  (cond
   ((and (file-remote-p default-directory 'host)
         (s-starts-with? "/docker:" default-directory))
    (concat "localhost." norns-local-mdns-domain))

   (t
    (let* ((remote-host (--> (file-remote-p default-directory 'host)
                             (s-chop-suffix (concat "." norns-mdns-domain) it))))
      (or remote-host "localhost")))))

(defun norns--core-curr-http-port ()
  "Get current HTTP port (for maiden web)."
  (cond
   ((and (file-remote-p default-directory 'host)
         (s-starts-with? "/docker:" default-directory))
    norns-docker-http-port)

   (t
    norns-http-port)))



;; CORE - WEBSOCKET-BACKED COMINT BUFFER

(defun norns--comint-true-line-beginning-position ()
  "Get true line beginning position.
Indeed, `comint-mode' tricks w/ `line-beginning-position' to make
it ignore the prompt."
  (save-excursion
    (let ((pos (line-beginning-position))
          (linum (line-number-at-pos)))
      (beginning-of-line)
      (left-char)
      (if (eq linum (line-number-at-pos)) ; on prompt line
          (line-beginning-position)       ; true position
        pos))))

(defun norns--comint-process ()
  "Get (dummy) process for (current) comint buffer."
  (get-buffer-process (current-buffer)))

(defun norns--comint-set-pm (pos)
  "Set marker POS for (dummy) process in (current) comint buffer."
  (set-marker (process-mark (get-buffer-process (current-buffer))) pos))

(defun norns--comint-pm nil
  "Get marker for last output of (dummy) process."
  (process-mark (get-buffer-process (current-buffer))))

(defun norns--comint-async-output-for-host (host-buff-alist host prompt txt)
  "Write TXT to comint buffer for HOST.

The comint buffer for HOST is stored in value of symbol HOST-BUFF-ALIST.

Ends output with a PROMPT to make comint believe it was a
standard command execution."
  (let* ((buff (cdr (assoc host (symbol-value host-buff-alist))))
         (visiting-windows (get-buffer-window-list buff 't))
         (eof-visiting-windows (--filter (with-selected-window it
                                           (eobp))
                                         visiting-windows))
         (output (concat txt prompt))
         prompt-entry)

    (with-current-buffer buff
      (save-excursion
        (goto-char (point-max))

        ;; remove active prompt
        (unless (eq (norns--comint-true-line-beginning-position) (line-end-position))
          (unless (eq (line-beginning-position) (line-end-position))
            (setq prompt-entry (buffer-substring (line-beginning-position) (line-end-position))))
          (delete-region (norns--comint-true-line-beginning-position) (line-end-position)))

        ;; insert incoming line + new prompt
        (comint-output-filter (norns--comint-process) output)

        (when prompt-entry
          (goto-char (point-max))
          (insert prompt-entry)))

      ;; make visiting windows "follow" (akin to `eshell-scroll-to-bottom-on-output')
      (when visiting-windows
        (message "moving: %s" eof-visiting-windows)
        (--map (set-window-point it (point-max)) eof-visiting-windows)))))

(defun norns--comint-register-buffer-for-host (host-buff-alist host prefix comint-mode)
  "Create new comint buffer for HOST and register it in HOST-BUFF-ALIST.

This buffer will have major COMINT-MODE activated on it and be
named \"*PREFIX/HOST*\"."
  (let ((buff (get-buffer-create (concat "*" prefix "/" host "*"))))
    (with-current-buffer buff
      (funcall comint-mode))
    (add-to-list host-buff-alist
                 (cons host buff))
    buff))

(defun norns--comint-ensure-buffer-for-host-exists (host-buff-alist host buff-register-fn)
  "Ensure comint buffer for HOST exists.

If not, create it and register it in HOST-BUFF-ALIST by calling
BUFF-REGISTER-FN."
  (let ((buff (cdr (assoc host (symbol-value host-buff-alist)))))
    (if (buffer-live-p buff)
        buff
      (funcall buff-register-fn host))))

(defun norns--comint-ensure-host-ws-open (host-ws-alist
                                          host ws-port
                                          ensure-host-buffer-exists-fn comint-output-fn)
  "Ensure websocket for norns HOST is open.

The websocket for HOST is stored in value of symbol HOST-WS-ALIST.

Also ensures the existence of associated comint output buffer by
calling ENSURE-HOST-BUFFER-EXISTS-FN.  WS-PORT is the remote
websocket listening port.

COMINT-OUTPUT-FN is the function that
gets called to pipe websocket output to the associated comint
buffer."
  (unless (websocket-openp (cdr (assoc host (symbol-value host-ws-alist))))
    (add-to-list
     host-ws-alist
     (cons
      host
      (websocket-open (format "ws://%s:%d" host ws-port)
                      :custom-header-alist '((Sec-WebSocket-Protocol . "bus.sp.nanomsg.org"))
                      :on-open (lambda (_ws)
                                 (funcall ensure-host-buffer-exists-fn host))
                      :on-message (lambda (_ws frame)
                                    (funcall ensure-host-buffer-exists-fn host)
                                    (funcall comint-output-fn host (websocket-frame-text frame)))
                      :on-close (lambda (_ws)
                                  (funcall ensure-host-buffer-exists-fn host)
                                  (funcall comint-output-fn host "\nwebsocket closed\n")))))))

(defun norns--ws-send (cmd
                       host-ws-alist host-comint-buff-alist
                       ensure-ws-open-fn ensure-comint-buff-exists-fn)
  "Send CMD to current norns via websocket.

Depending on value of `norns-repl-switch-on-cmd', eventually pop
a window to its associated REPL (comint buffer).

Current norns is determined with
`norns--location-from-access-policy', depending on the value of
`norns-access-policy'.

From it the associated websocket and comint buffer are retrieved
from HOST-WS-ALIST and HOST-COMINT-BUFF-ALIST, respectively.

We ensure those two exist by calling ENSURE-WS-OPEN-FN and
ENSURE-COMINT-BUFF-EXISTS-FN."
  (let* ((default-directory (norns--location-from-access-policy))
         (host (norns--core-curr-host)))
    (funcall ensure-ws-open-fn host)
    (funcall ensure-comint-buff-exists-fn host)
    (let* ((frame (selected-frame))
           (win (selected-window))
           (comint-buff (cdr (assoc host (symbol-value host-comint-buff-alist))))
           (visiting-windows (get-buffer-window-list comint-buff)))
      (websocket-send-text (cdr (assoc host (symbol-value host-ws-alist))) (concat cmd "\n"))
      (when (and norns-repl-switch-on-cmd
                 (null visiting-windows))
        (apply norns-repl-switch-fn (list comint-buff))
        (goto-char (point-max))
        (when norns-repl-switch-no-focus
          (set-frame-selected-window frame win))))))



;; NORNS - PATH

(defun norns--current-host-norns-p ()
  "Return t if host of `default-directory' is a norns."
  (f-directory? (norns--core-trampify-path-maybe "/home/we/dust")))

(defun norns--make-default-norns-tramp-prefix ()
  "Build the tramp prefix for default norns (`norns-user' @ `norns-host').

If `tramp-default-method' is \"docker\" we assume a local docker instance.
In that case `norns-user' @ `norns-docker-container' gets used."
  (let* ((hostname (cond
                    ((string= tramp-default-method "docker") norns-docker-container)
                    (t (concat norns-host (when norns-local-mdns-domain
                                            (concat "." norns-local-mdns-domain)))))))
    (concat "/" tramp-default-method ":"
            norns-user "@" hostname ":")))

(defun norns--make-default-norns-tramp-path ()
  "Build the tramp path for default norns (`norns-user' @ `norns-host')."
  (concat (norns--make-default-norns-tramp-prefix) "/home/" norns-user "/dust/"))

(defun norns--location-from-access-policy ()
  "Find current norns file path to work on based on `norns-access-policy'.

With default value \":current-fallback-default\", tries first on
`default-directory' (assuming we're visiting a norns), then
fallback to remote `norns-host'.

With value \"current\", will only try `default-directory' and
fail if it's not a norns.

With value \"default\", will always ignore `default-directory'
and use remote `norns-host'."
  (cond
   ((eq norns-access-policy :current-fallback-default)
    (or (and (norns--current-host-norns-p) default-directory)
        (norns--make-default-norns-tramp-path)))

   ((eq norns-access-policy :current)
    (unless (norns--current-host-norns-p)
      (user-error "Not visiting a norns!"))
    default-directory)

   (:default
    (norns--make-default-norns-tramp-path))))



;; NORNS - PATH - SCRIPTS

(defun norns--script-path-p (fp)
  "Return t if FP is a script path."
  (s-matches-p norns-script-rx fp))

(defun norns--script-from-path (fp)
  "Extract script name from FP."
  (cdr (s-match norns-script-rx fp)))

(defun norns--in-script-dir-path-p (fp)
  "Return t if FP is visiting somewhere under a script dir."
  (s-matches-p norns-in-script-dir-rx fp))

(defun norns--script-dir-from-path (fp)
  "Extract script dir name from FP."
  (nth 1 (s-match norns-in-script-dir-rx fp)))

(defun norns-all-scripts ()
  "Get list of scripts on current norns.

Current norns is determined with
`norns--location-from-access-policy', depending on the value of
`norns-access-policy'."
  (let ((default-directory (norns--location-from-access-policy)))
    (--> (f-glob (norns--core-trampify-path-maybe
                  (concat norns-script-path-prefix "*/*.lua")))
         (-map #'norns--core-untrampify-path-maybe it)
         (-map #'norns--script-from-path it))))

(defun norns-current-scripts ()
  "Get list of scripts corresponding to currently visited buffer."
  (unless (norns--current-host-norns-p)
    (user-error "Not visiting a norns!"))

  (let* ((fp (norns--core-curr-path)))
    (unless (s-starts-with? norns-script-path-prefix fp)
      (user-error "Not visiting a script source!"))

    (cond
     ((norns--script-path-p fp)
      (list (norns--script-from-path fp)))

     ((norns--in-script-dir-path-p fp)
      (let ((script-dir (norns--script-dir-from-path fp)))
        (--> (f-glob (norns--core-trampify-path-maybe
                      (concat norns-script-path-prefix script-dir "/*.lua")))
             (-map #'norns--core-untrampify-path-maybe it)
             (-map #'norns--script-from-path it))))

     (:default (error "Unexpected error")))))




;; NORNS - MAIDEN

(defun norns--maiden-output (host txt)
  "Function to forward output TXT from maiden websocket to the
corresponding comint buffer for HOST."
  (norns--comint-async-output-for-host 'norns-maiden-buff-alist host norns-maiden-repl-prompt-internal txt))

(defun norns--register-maiden-buffer (host)
  "Create a new maiden comint buffer for HOST and register it in
`norns-maiden-buff-alist'."
  (norns--comint-register-buffer-for-host 'norns-maiden-buff-alist host norns-maiden-buffer-prefix #'norns-maiden-repl-mode))

(defun norns--ensure-host-maiden-buffer-exists (host)
  "Ensure that a maiden comint buffer for HOST exists in `norns-maiden-buff-alist'."
  (norns--comint-ensure-buffer-for-host-exists 'norns-maiden-buff-alist host #'norns--register-maiden-buffer))

(defun norns--ensure-host-maiden-ws-open (host)
  "Ensure socket for norns HOST is open.

The REPL (comint buffer) for HOST is stored in `norns-maiden-ws-socket-alist'.

Also ensures the existence of maiden output buffer (stored in
`norns-maiden-buff-alist')."
  (norns--comint-ensure-host-ws-open 'norns-maiden-ws-socket-alist host
                                     norns-maiden-ws-port
                                     #'norns--ensure-host-maiden-buffer-exists
                                     #'norns--maiden-output))

(defun norns-maiden-send (cmd)
  "Send CMD to norns via maiden and eventually pop a window to the REPL buffer."
  (interactive "s> ")
  (cond
   ((string= ";restart" cmd)
    (norns-maiden-restart))

   ;; REVIEW: isn't working, might be a CORS thing
   ;; ((s-starts-with? ";install " cmd)
   ;; (norns-maiden-install-script (s-chop-prefix ";install " cmd)))

   (:default
    (norns--ws-send (s-replace "\n" "; " cmd)
                    'norns-maiden-ws-socket-alist
                    'norns-maiden-buff-alist
                    #'norns--ensure-host-maiden-ws-open
                    #'norns--ensure-host-maiden-buffer-exists))))

(defun norns-maiden-send-selection ()
  "Send selected buffer region to maiden."
  (interactive)
  (cond
   ((use-region-p)
    (norns-maiden-send (buffer-substring (region-beginning) (region-end)))
    (deactivate-mark))

   (:default (message "no selection"))))

(defun norns--inject-inspect-lib ()
  "Inject inspect.lua library onto norns."
  (let* ((default-directory (norns--location-from-access-policy))
         (tmp-dir (--> (temporary-file-directory)
                       (if (s-ends-with? "/" it) it (concat it "/"))))
         (dest-file (concat tmp-dir "inspect.lua")))
    (unless (file-exists-p dest-file)
      (url-copy-file norns-lua-lib-inspect-url dest-file))))

(defun norns-maiden-inspect-symbol (symbol)
  "Inspect value of SYMBOL at point in maiden.
If no symbol at point, prompt.

Please note that it will only work properly for non-local lua vars."
  (interactive (list
                (let ((tap (thing-at-point 'symbol)))
                  (if tap
                      (read-string (format "var (%s): " tap)
                                   nil nil tap)
                    (read-string "var: ")))))
  (norns--inject-inspect-lib)
  (norns-maiden-send (s-join "; " (list "local inspect = require '/tmp/inspect'"
                                        (concat "print(inspect(" symbol "))")))))



;; COMMANDS - SCRIPT LOAD

(defun norns-load-script-raw (script-name)
  "Ask norns to load SCRIPT-NAME."
  (interactive "sName: ")
  (unless (s-contains? "/" script-name)
    (setq script-name (concat script-name "/" script-name)))
  (norns-maiden-send (concat "norns.script.load(\"code/" script-name ".lua\")")))

(defun norns--load-script-helper (scripts)
  "Prompt user to select one of the SCRIPTS and then ask for norns to launch it."
  (let ((scripts-prompt (--map
                         (if (string= (car it) (nth 1 it))
                             it
                           (cons (s-join "/" it) it))
                         scripts))
        script)
    (if (eq (length scripts) 1)
        (setq script (car scripts))
      (setq script (--> (completing-read "Choose: " scripts-prompt)
                        (cdr (assoc it scripts-prompt)))))
    (if script
        (norns-load-script-raw (s-join "/" script))
      (message "canceled"))))

(defun norns-load-script ()
  "Load script from list.

Prompt user to select any of the existing scripts and then ask
for current norns to launch it.

Current norns is determined with
`norns--location-from-access-policy', depending on the value of
`norns-access-policy'."
  (interactive)
  (norns--load-script-helper (norns-all-scripts)))

(defun norns-load-current-script ()
  "Load currently visited script.
If visiting a script folder, and more than 1 script is found in
it, prompt user to select one."
  (interactive)
  (let ((norns-access-policy :current))
    (norns--load-script-helper (norns-current-scripts))))



;; MAJOR MODE - MAIDEN REPL

(defun norns--maiden-input-sender (_proc input)
  "Send comint INPUT to norns' maiden (via websocket).
Output is processed asyncronously by `norns--maiden-output'."
  (norns-maiden-send input))

(define-derived-mode norns-maiden-repl-mode comint-mode "maiden-repl"
  "Major mode for interracting w/ a monome norns' maiden REPL."
  :keymap (let ((mmap (make-sparse-keymap)))
            mmap)

  (setq comint-prompt-regexp (concat "^" (regexp-quote norns-maiden-repl-prompt)))
  (setq comint-input-sender #'norns--maiden-input-sender)
  (setq comint-process-echoes nil)

  ;; A dummy process to keep comint happy. It will never get any input
  (unless (comint-check-proc (current-buffer))
    ;; Was cat, but on non-Unix platforms that might not exist, so
    ;; use hexl instead, which is part of the Emacs distribution.
    (condition-case nil
        (start-process "maiden" (current-buffer) "hexl")
      (file-error (start-process "maiden" (current-buffer) "cat")))
    (set-process-query-on-exit-flag (norns--comint-process) nil)
    (goto-char (point-max))

    (norns--comint-set-pm (point-max))
    (unless comint-use-prompt-regexp
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
    (comint-output-filter (norns--comint-process) norns-maiden-repl-prompt-internal)
    (set-marker comint-last-input-start (norns--comint-pm))
    (set-process-filter (get-buffer-process (current-buffer)) #'comint-output-filter)))

(defun norns-maiden-repl ()
  "Connect to the maiden REPL for current norns and switch to it.
If already connected, just switch to the buffer.

Current norns is determined depending on the value of `norns-access-policy'."
  (interactive)
  (let* ((default-directory (norns--location-from-access-policy))
         (host (norns--core-curr-host)))
    (switch-to-buffer (norns--ensure-host-maiden-buffer-exists host))))

(defun norns-docker-maiden-repl ()
  "Same as `norns-maiden-repl' but assuming a local docker instance.

See values of `norns-docker-container' and
`norns-local-mdns-domain' for the targeted instance."
  (interactive)
  (unless (assoc "docker" tramp-methods)
    (user-error "Missing \"docker\" TRAMP method, plase install package `docker-tramp'."))
  (let ((norns-access-policy :default)
        (tramp-default-method "docker"))
    (call-interactively #'norns-maiden-repl)))

(defun norns--maiden-repl-after-start (dd)
  "Force reconnection to maiden REPL upon (re)start.

Host is identified by it's path DD."
  (let* ((default-directory dd)
         (host (norns--core-curr-host))
         (frame (selected-frame))
         (win (selected-window))
         (maiden-buff (norns--ensure-host-maiden-buffer-exists host))
         (maiden-visiting-windows (get-buffer-window-list maiden-buff)))
    (when (and norns-repl-switch-on-cmd
               (null maiden-visiting-windows))
      (funcall norns-repl-switch-fn maiden-buff)
      (goto-char (point-max))
      (when norns-repl-switch-no-focus
        (set-frame-selected-window frame win))))

  ;; NB: REPLs seem to need a "ping" to send their startup logs
  (run-at-time
   0.1 nil
   `(lambda ()
      (let ((default-directory ,dd))
        (norns-maiden-send "")))))

(defun norns-maiden-restart ()
  "Restart maiden REPL for current norns."
  (interactive)
  (let* ((default-directory (norns--location-from-access-policy))
         (dd default-directory)
         (host (norns--core-curr-host))
         (port (number-to-string (norns--core-curr-http-port))))
    (request
      (concat "http://" host ":" port "/api/v1/unit/norns-matron.service")
      :params '(("do" . "restart"))
      :parser 'json-read
      :success (cl-function
                (lambda (&key _data &allow-other-keys)
                  (norns--maiden-repl-after-start dd)
                  ;; (message "Got: %S" data)
                  )))))

(defun norns-maiden-install-script (script-url)
  "Install norns SCRIPT-URL."
  (interactive "s> ")
  (let* ((default-directory (norns--location-from-access-policy))
         (host (norns--core-curr-host))
         (port (number-to-string (norns--core-curr-http-port))))
    (request
      (concat "http://" host ":" port "/api/v1/project/install")
      :params `(("url" . ,script-url))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  ;; "catalog_entry.project_name"
                  (message "Installed: %S" data))))))



;; NORNS - SC

(defun norns--sc-output (host txt)
  "Function to forward output TXT from SuperCollider websocket to
the corresponding comint buffer for HOST."
  (norns--comint-async-output-for-host 'norns-sc-buff-alist host norns-sc-repl-prompt-internal txt))

(defun norns--register-sc-buffer (host)
  "Create a new SuperCollider comint buffer for HOST and register
it in `norns-sc-buff-alist'."
  (norns--comint-register-buffer-for-host 'norns-sc-buff-alist host norns-sc-buffer-prefix #'norns-sc-repl-mode))

(defun norns--ensure-host-sc-buffer-exists (host)
  "Ensure that a SuperCollider comint buffer for HOST exists in
`norns-sc-buff-alist'."
  (norns--comint-ensure-buffer-for-host-exists 'norns-sc-buff-alist host #'norns--register-sc-buffer))

(defun norns--ensure-host-sc-ws-open (host)
  "Ensure socket for norns HOST is open.

The REPL (comint buffer) for HOST is stored in `norns-sc-ws-socket-alist'.

Also ensures the existence of SuperCollider output buffer (stored
in `norns-sc-buff-alist')."
  (norns--comint-ensure-host-ws-open 'norns-sc-ws-socket-alist host
                                     norns-sc-ws-port
                                     #'norns--ensure-host-sc-buffer-exists
                                     #'norns--sc-output))

(defun norns-sc-send (cmd)
  "Send CMD to norns via SuperCollider and eventually pop a window
to the REPL buffer."
  (interactive "s> ")

  (cond
   ((string= ";restart" cmd)
    (norns-sc-restart))

   (:default
    (norns--ws-send (concat cmd "")
                    'norns-sc-ws-socket-alist
                    'norns-sc-buff-alist
                    #'norns--ensure-host-sc-ws-open
                    #'norns--ensure-host-sc-buffer-exists)
    ;; NB: sc doesn't usually answer when request is empty, so we simulate it
    (when (string= (s-trim cmd) "")
      (let* ((default-directory (norns--location-from-access-policy))
             (host (norns--core-curr-host)))
        (norns--sc-output host "\n"))))))

(defun norns-sc-send-selection ()
  "Send selected buffer region to SuperCollider REPL."
  (interactive)
  (cond
   ((use-region-p)
    (norns-sc-send (buffer-substring (region-beginning) (region-end)))
    (deactivate-mark))

   (:default (message "no selection"))))



;; MAJOR MODE - SC REPL

(defun norns--sc-input-sender (_proc input)
  "Send comint INPUT to norns' SuperCollider (via websocket).
Output is processed asyncronously by `norns--sc-output'."
  (norns-sc-send input))

(define-derived-mode norns-sc-repl-mode comint-mode "norns-sc-repl"
  "Major mode for interracting w/ a monome norns' SuperCollider REPL."
  :keymap (let ((mmap (make-sparse-keymap)))
            mmap)

  (setq comint-prompt-regexp (concat "^" (regexp-quote norns-sc-repl-prompt)))
  (setq comint-input-sender #'norns--sc-input-sender)
  (setq comint-process-echoes nil)

  ;; A dummy process to keep comint happy. It will never get any input
  (unless (comint-check-proc (current-buffer))
    ;; Was cat, but on non-Unix platforms that might not exist, so
    ;; use hexl instead, which is part of the Emacs distribution.
    (condition-case nil
        (start-process "sc" (current-buffer) "hexl")
      (file-error (start-process "sc" (current-buffer) "cat")))
    (set-process-query-on-exit-flag (norns--comint-process) nil)
    (goto-char (point-max))

    (norns--comint-set-pm (point-max))
    (unless comint-use-prompt-regexp
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
    (comint-output-filter (norns--comint-process) norns-sc-repl-prompt-internal)
    (set-marker comint-last-input-start (norns--comint-pm))
    (set-process-filter (get-buffer-process (current-buffer)) #'comint-output-filter)))

(defun norns-sc-repl ()
  "Connect to the SuperCollider REPL for current norns and switch to it.
If already connected, just switch to the buffer.

Current norns is determined depending on the value of `norns-access-policy'."
  (interactive)
  (let* ((default-directory (norns--location-from-access-policy))
         (host (norns--core-curr-host)))
    (switch-to-buffer (norns--ensure-host-sc-buffer-exists host))))

(defun norns-docker-sc-repl ()
  "Same as `norns-sc-repl' but assuming a local docker instance.

See values of `norns-docker-container' and
`norns-local-mdns-domain' for the targeted instance."
  (interactive)
  (unless (assoc "docker" tramp-methods)
    (user-error "Missing \"docker\" TRAMP method, plase install package `docker-tramp'."))
  (let ((norns-access-policy :default)
        (tramp-default-method "docker"))
    (call-interactively #'norns-sc-repl)))

(defun norns--sc-repl-after-start (dd)
  "Force reconnection to SuperCollider REPL upon (re)start.

Host is identified by it's path DD."
  (let* ((default-directory dd)
         (host (norns--core-curr-host))
         (frame (selected-frame))
         (win (selected-window))
         (sc-buff (norns--ensure-host-sc-buffer-exists host))
         (sc-visiting-windows (get-buffer-window-list sc-buff)))
    (when norns-repl-switch-on-cmd
      (when (null sc-visiting-windows)
        (funcall norns-repl-switch-fn sc-buff)
        (goto-char (point-max))
        (when norns-repl-switch-no-focus
          (set-frame-selected-window frame win)))))

  ;; NB: REPLs seem to need a "ping" to send their startup logs
  (run-at-time
   0.1 nil
   `(lambda ()
      (let ((default-directory ,dd))
        (message (concat "Pinging from " default-directory))
        (norns-sc-send "")))))

(defun norns-sc-restart ()
  "Restart sc REPL for current norns."
  (interactive)
  (let* ((default-directory (norns--location-from-access-policy))
         (dd default-directory)
         (host (norns--core-curr-host))
         (port (number-to-string (norns--core-curr-http-port))))
    (request
      (concat "http://" host ":" port "/api/v1/unit/norns-sclang.service")
      :params '(("do" . "restart"))
      :parser 'json-read
      :success (cl-function
                (lambda (&key _data &allow-other-keys)
                  (norns--sc-repl-after-start dd)
                  ;; (message "Got: %S" data)
                  )))))



;; SOURCE FILE MINOR MODE

(defun norns-send (cmd)
  "Prompt for a command and send it to norns, either to maiden or
to the SuperCollider REPL depending on current buffer mode."
  (interactive "s> ")
  (cond
   ((string= "lua-mode" major-mode)
    (norns-maiden-send cmd))
   ((string= "sclang-mode" major-mode)
    (norns-sc-send cmd))
   (:default
    (user-error "Not a Lua nor SuperCollider source file!"))))

(defun norns-send-selection ()
  "Send selection to norns, either to maiden or to the
SuperCollider REPL depending on current buffer mode."
  (interactive)
  (cond
   ((string= "lua-mode" major-mode)
    (call-interactively #'norns-maiden-send-selection))
   ((string= "sclang-mode" major-mode)
    (call-interactively #'norns-sc-send-selection))
   (:default
    (user-error "Not a Lua nor SuperCollider source file!"))))

(define-minor-mode norns-mode
  "Additional shortcuts for norns lua & sc sources."
  :lighter norns-mode-lighter
  :keymap (let ((mmap (make-sparse-keymap)))
            (define-key mmap (kbd "C-c ! r") #'norns-send-selection)
            (define-key mmap (kbd "C-c ! c") #'norns-send)
            (define-key mmap (kbd "C-c ! R") #'norns-load-current-script)
            mmap)

  (when (string= "lua-mode" major-mode)
    (font-lock-add-keywords nil (norns--lua-make-font-lock-keywords))
    ;; (if (fboundp 'font-lock-flush)
    ;;     (font-lock-flush)
    ;;   (when font-lock-mode
    ;;     (with-no-warnings (font-lock-fontify-buffer))))
    )

  )

(defun norns-mode-maybe-activate ()
  "Helper function to bind to `lua-mode-hook' and
`sclang-mode-hook', to activate `norns-mode' if applicable."
  (when (norns--current-host-norns-p)
    (norns-mode 1)))



;; SYSTEM HELPER COMMANDS

(defun norns-restart ()
  "Restart current norns instance services.

Auto re-spawns maiden and SuperCollider REPLs to catch startup
logs.

Current norns is determined with
`norns--location-from-access-policy', depending on the value of
`norns-access-policy'."
  (interactive)
  (let* ((default-directory (norns--location-from-access-policy))
         (dd default-directory))
    (shell-command "nohup systemctl restart norns-sclang > /dev/null")
    (shell-command "nohup systemctl restart norns-crone > /dev/null")
    (shell-command "nohup systemctl restart norns-matron > /dev/null")
    (norns--sc-repl-after-start dd)
    (norns--maiden-repl-after-start dd)))

(defun norns-reboot ()
  "Reboot current norns instance OS.

Current norns is determined with
`norns--location-from-access-policy', depending on the value of
`norns-access-policy'."
  (interactive)
  (let* ((default-directory (norns--location-from-access-policy))
         (host (norns--core-curr-host)))
    (when (string= host "localhost")
      (user-error "You can't restart norns from within Emacs when it is running from norns!"))
    (shell-command "sudo reboot now")))



;; IO - SCREEN

(defun norns-screen-dump (filename)
  "Make a raw dump of norns screen into a file.

Save it as FILENAME.png inside `norns-screenshot-folder'."
  (interactive "sFileame: ")
  (let ((norns-repl-switch-on-cmd nil))
    (norns-maiden-send (concat "_norns.screen_export_png(\"" norns-screenshot-folder filename ".png\")"))))

(defun norns-screenshot (filename)
  "Take a screenshot of norns screen.

Save it as FILENAME.png inside `norns-screenshot-folder'."
  (interactive "sFileame: ")
  (norns-screen-dump filename)
  (run-at-time
   0.1 nil
   `(lambda ()
      (let ((default-directory (norns--location-from-access-policy))
            (screenshot-file (concat norns-screenshot-folder ,filename ".png")))
        (shell-command (concat "convert " screenshot-file " -gamma 1.25 -filter point -resize 400% -gravity center -background black -extent 120% " screenshot-file))))))




(provide 'norns)
;;; norns.el ends here
