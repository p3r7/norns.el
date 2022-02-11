;;; norns.el --- Control your norns from whithin Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Jordan Besly
;;
;; Version: 0.0.1
;; Keywords: processes, terminals
;; URL: https://github.com/p3r7/norns.el
;; Package-Requires: ((emacs "27.2")(dash "2.17.0")(s "1.12.0")(f "20210624.1103")(websocket "20210110.17"))
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;;
;; For detailed instructions, please look at the README.md at https://github.com/p3r7/norns.el/blob/master/README.md

;;; Code:



;; DEPS

(require 'dash)

(require 's)
(require 'rx)

(require 'f)
(require 'websocket)

(require 'lua-mode)



;; VARS

(defvar norns-access-policy :current-fallback-default)

(defvar norns-user "we")
(defvar norns-host "norns")
(defvar norns-mdns-domain "local")

(defvar norns-maiden-ws-port 5555)
(defvar norns-maiden-ws-socket-alist nil)
(defvar norns-maiden-buffer-prefix "maiden")
(defvar norns-maiden-buff-alist nil)
(defvar norns-maiden-repl-prompt "maiden>> ")
(defvar norns-maiden-repl-prompt-internal "maiden>> ")
(defvar norns-lua-lib-inject-dir "/tmp/")
(defvar norns-lua-lib-inspect-url "https://raw.githubusercontent.com/kikito/inspect.lua/master/inspect.lua")

(defvar norns-sc-ws-port 5556)
(defvar norns-sc-ws-socket-alist nil)
(defvar norns-sc-buffer-prefix "norns-sc")
(defvar norns-sc-buff-alist nil)
(defvar norns-sc-repl-prompt "sc>> ")
(defvar norns-sc-repl-prompt-internal "sc>> ")

(defvar norns-repl-switch-on-cmd t)
(defvar norns-repl-switch-fn #'switch-to-buffer-other-window)
(defvar norns-repl-switch-no-focus t)

(defvar norns-maiden-mode-lighter "maiden-repl")
(defvar norns-sc-mode-lighter "norns-sc-repl")

(defvar norns-mode-lighter " norns")




;; CONST

(defconst norns-script-path-prefix "/home/we/dust/code/")

(defconst norns-script-rx
  (rx bol
      (eval norns-script-path-prefix)
      (group (one-or-more (any "a-z" "A-Z" "0-9" "-" "_")))
      "/"
      (group (one-or-more (any "a-z" "A-Z" "0-9" "-" "_"))) ".lua"
      eol))

(defconst norns-in-script-dir-rx
  (rx bol
      (eval norns-script-path-prefix)
      (group (one-or-more (any "a-z" "A-Z" "0-9" "-" "_")))
      "/"))



;; CORE - PATH

(defun norns--core-curr-fq-path ()
  "Get path (i.e. file-name in Emacs lingo) of current buffer.
it is fully qualified, i.e. w/ a TRAMP prefix if the connexion is remote."
  (if (member major-mode '(dired-mode shell-mode))
      default-directory
    (buffer-file-name)))

(defun norns--core-tramp-extract-path (tramp-path)
  "Remove tramp prefix out of TRAMP-PATH, keeping only the filesystem path."
  (let (vec localname)
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
  "Get current hostame.
Defaults to \"localhost\" if not a TRAMP path."
  (let* ((remote-host (--> (file-remote-p default-directory 'host)
                           (s-chop-suffix (concat "." norns-mdns-domain) it))))
    (or remote-host "localhost")))



;; CORE - COMINT

(defun norns--comint-true-line-beginning-position ()
  "`comint-mode' tricks w/ `line-beginning-position' to make it ignore the prompt."
  (save-excursion
    (let ((pos (line-beginning-position))
          (linum (line-number-at-pos)))
      (beginning-of-line)
      (left-char)
      (if (eq linum (line-number-at-pos)) ; on prompt line
          (line-beginning-position)       ; true position
        pos))))

(defun norns--comint-process ()
  (get-buffer-process (current-buffer)))

(defun norns--comint-set-pm (pos)
  (set-marker (process-mark (get-buffer-process (current-buffer))) pos))

(defun norns--comint-pm nil
  (process-mark (get-buffer-process (current-buffer))))

(defun norns--comint-async-output-for-host (host-buff-alist host prompt txt)
  "Write TXT to comint buffer for HOST (stored in value of symbol HOST-BUFF-ALIST)."
  (let* ((buff (cdr (assoc host (symbol-value host-buff-alist))))
         (visiting-windows (get-buffer-window-list buff 't))
         (eof-visiting-windows (--filter (with-selected-window it
                                           (eq (point) (point-max)))
                                         visiting-windows))
         (output (concat txt prompt))
         prompt-entry)

    (with-current-buffer buff
      (save-excursion
        (end-of-buffer)

        ;; remove active prompt
        (unless (eq (norns--comint-true-line-beginning-position) (line-end-position))
          (unless (eq (line-beginning-position) (line-end-position))
            (setq prompt-entry (buffer-substring (line-beginning-position) (line-end-position))))
          (delete-region (norns--comint-true-line-beginning-position) (line-end-position)))

        ;; insert incoming line + new prompt
        (comint-output-filter (norns--comint-process) output)

        (when prompt-entry
          (end-of-buffer)
          (insert prompt-entry)))

      ;; make visiting windows "follow" (akin to `eshell-scroll-to-bottom-on-output')
      (when visiting-windows
        (message "moving: %s" eof-visiting-windows)
        (--map (set-window-point it (point-max)) eof-visiting-windows)))))

(defun norns--comint-register-buffer-for-host (host-buff-alist host prefix comint-mode)
  (let ((buff (get-buffer-create (concat "*" prefix "/" host "*"))))
    (with-current-buffer buff
      (funcall comint-mode))
    (add-to-list host-buff-alist
                 (cons host buff))
    buff))

(defun norns--comint-ensure-buffer-for-host-exists (host-buff-alist host buff-register-fn)
  (let ((buff (cdr (assoc host (symbol-value host-buff-alist)))))
    (if (buffer-live-p buff)
        buff
      (funcall buff-register-fn host))))

(defun norns--comint-ensure-host-ws-open (host-ws-alist
                                          host ws-port
                                          ensure-host-buffer-exists-fn comint-output-fn)
  "Ensure socket for currently visited host (stored in value of symbol HOST-WS-ALIST) is open.
Also ensures the existence of associated comint output buffer by calling ENSURE-HOST-BUFFER-EXISTS-FN."
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
  "Send CMD to websocket and eventually pop a window to associated comint buffer."
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
        (end-of-buffer)
        (when norns-repl-switch-no-focus
          (set-frame-selected-window frame win))))))



;; NORNS - PATH

(defun norns--current-host-norns-p ()
  "Returns t if host of `default-directory' is a norns."
  (f-directory? (norns--core-trampify-path-maybe "/home/we/dust")))

(defun norns--make-default-norns-tramp-prefix ()
  (concat "/" tramp-default-method ":"
          norns-user "@" norns-host (when norns-mdns-domain (concat "." norns-mdns-domain)) ":"))

(defun norns--location-from-access-policy ()
  (cond
   ((eq norns-access-policy :current-fallback-default)
    (or (and (norns--current-host-norns-p) default-directory)
        (norns--make-default-norns-tramp-prefix)))

   ((eq norns-access-policy :current)
    (unless (norns--current-host-norns-p)
      (user-error "Not visiting a norns!"))
    default-directory)

   (:default
    (norns--make-default-norns-tramp-prefix))))



;; NORNS - PATH - SCRIPTS

(defun norns--script-path-p (fp)
  "Returns t if FP is a script path."
  (s-matches-p norns-script-rx fp))

(defun norns--script-from-path (fp)
  "Extract script name from FP."
  (cdr (s-match norns-script-rx fp)))

(defun norns--in-script-dir-path-p (fp)
  "Returns t if FP is visiting somewhere under a script dir."
  (s-matches-p norns-in-script-dir-rx fp))

(defun norns--script-dir-from-path (fp)
  "Extract script dir name from FP."
  (nth 1 (s-match norns-in-script-dir-rx fp)))

(defun norns-all-scripts ()
  "Get list of scripts on visited norns."
  (let ((default-directory (norns--location-from-access-policy)))
    (--> (f-glob (norns--core-trampify-path-maybe
                  (concat norns-script-path-prefix "*/*.lua")))
         (-map #'norns--core-untrampify-path-maybe it)
         (-map #'norns--script-from-path it))))

(defun norns-current-scripts ()
  "Get list of scripts corresponding to visited buffer."
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
  (norns--comint-async-output-for-host 'norns-maiden-buff-alist host norns-maiden-repl-prompt-internal txt))

(defun norns--register-maiden-buffer (host)
  (norns--comint-register-buffer-for-host 'norns-maiden-buff-alist host norns-maiden-buffer-prefix #'norns-maiden-repl-mode))

(defun norns--ensure-host-maiden-buffer-exists (host)
  (norns--comint-ensure-buffer-for-host-exists 'norns-maiden-buff-alist host #'norns--register-maiden-buffer))

(defun norns--ensure-host-maiden-ws-open (host)
  "Ensure socket for currently visited host (stored in `norns-maiden-ws-socket-alist') is open.
Also ensures the existence of maiden output buffer (stored in `norns-maiden-buff-alist')."
  (norns--comint-ensure-host-ws-open 'norns-maiden-ws-socket-alist host
                                     norns-maiden-ws-port
                                     #'norns--ensure-host-maiden-buffer-exists
                                     #'norns--maiden-output))

(defun norns-maiden-send (cmd)
  "Send CMD to norns via maiden and eventually pop a window to the REPL buffer."
  (interactive "s> ")
  (cond
   ((string= ";restart" cmd)
    (norns-restart))

   (:default
    (norns--ws-send cmd
                    'norns-maiden-ws-socket-alist
                    'norns-maiden-buff-alist
                    #'norns--ensure-host-maiden-ws-open
                    #'norns--ensure-host-maiden-buffer-exists))))

(defun norns-maiden-send-selection ()
  "Send selected buffer region to maiden."
  (interactive)
  (cond
   ((use-region-p)
    (--> (buffer-substring (region-beginning) (region-end))
         (s-replace "\n" "; " it)
         (norns-maiden-send it))
    (deactivate-mark))

   (:default (message "no selection"))))

(defun norns--inject-inspect-lib ()
  "Inject inspect.lua library onto norns."
  (let* ((default-directory (norns--location-from-access-policy))
         (dest-file (norns--core-trampify-path-maybe (concat norns-lua-lib-inject-dir "inspect.lua"))))
    (unless (file-exists-p dest-file)
      (url-copy-file norns-lua-lib-inspect-url dest-file))))

(defun norns-maiden-inspect-symbol (symbol)
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
  "Prompt user to select any of the existing scripts and then ask for norns to launch it.
Tries first on `default-directory' (assuming we're visiting a norns), then fallback to remote `norns-host'."
  (interactive)
  (norns--load-script-helper (norns-all-scripts)))

(defun norns-load-current-script ()
  "Load currently visited script.
If visiting a script folder, and more than 1 script is found in it, prompt user to select one."
  (interactive)
  (let ((norns-access-policy :default))
    (norns--load-script-helper (norns-current-scripts))))



;; MAJOR MODE - MAIDEN REPL

(defun norns--maiden-input-sender (_proc input)
  (norns-maiden-send input))

(define-derived-mode norns-maiden-repl-mode comint-mode norns-maiden-mode-lighter
  "Major mode for interracting w/ a monome norns' maiden repl."
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

    ;; output can include raw characters that confuse comint's
    ;; carriage control code.
    ;; (set (make-local-variable 'comint-inhibit-carriage-motion) t)

    ;; Add a silly header
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
  (interactive)
  (let* ((default-directory (norns--location-from-access-policy))
         (host (norns--core-curr-host)))
    (switch-to-buffer (norns--ensure-host-maiden-buffer-exists host))))



;; NORNS - SC

(defun norns--sc-output (host txt)
  (norns--comint-async-output-for-host 'norns-sc-buff-alist host norns-sc-repl-prompt-internal txt))

(defun norns--register-sc-buffer (host)
  (norns--comint-register-buffer-for-host 'norns-sc-buff-alist host norns-sc-buffer-prefix #'norns-sc-repl-mode))

(defun norns--ensure-host-sc-buffer-exists (host)
  (norns--comint-ensure-buffer-for-host-exists 'norns-sc-buff-alist host #'norns--register-sc-buffer))

(defun norns--ensure-host-sc-ws-open (host)
  "Ensure socket for currently visited host (stored in `norns-sc-ws-socket-alist') is open.
Also ensures the existence of sc output buffer (stored in `norns-sc-buff-alist')."
  (norns--comint-ensure-host-ws-open 'norns-sc-ws-socket-alist host
                                     norns-sc-ws-port
                                     #'norns--ensure-host-sc-buffer-exists
                                     #'norns--sc-output))

(defun norns-sc-send (cmd)
  "Send CMD to norns via sc and eventually pop a window to the REPL buffer."
  (interactive "s> ")
  (norns--ws-send (concat cmd "")
                  'norns-sc-ws-socket-alist
                  'norns-sc-buff-alist
                  #'norns--ensure-host-sc-ws-open
                  #'norns--ensure-host-sc-buffer-exists))

(defun norns-sc-send-selection ()
  "Send selected buffer region to sc REPL."
  (interactive)
  (cond
   ((use-region-p)
    (norns-sc-send (buffer-substring (region-beginning) (region-end)))
    (deactivate-mark))

   (:default (message "no selection"))))



;; MAJOR MODE - SC REPL

(defun norns--sc-input-sender (_proc input)
  (norns-sc-send input))

(define-derived-mode norns-sc-repl-mode comint-mode norns-sc-mode-lighter
  "Major mode for interracting w/ a monome norns' sc repl."
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

    ;; output can include raw characters that confuse comint's
    ;; carriage control code.
    ;; (set (make-local-variable 'comint-inhibit-carriage-motion) t)

    ;; Add a silly header
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
  (interactive)
  (let* ((default-directory (norns--location-from-access-policy))
         (host (norns--core-curr-host)))
    (switch-to-buffer (norns--ensure-host-sc-buffer-exists host))))



;; SOURCE FILE MINOR MODE

(defun norns-send ()
  (interactive)
  (cond
   ((string= "lua-mode" major-mode)
    (call-interactively #'norns-maiden-send))
   ((string= "sclang-mode" major-mode)
    (call-interactively #'norns-sc-send))
   (:default
    (user-error "Not a Lua nor SuperCollider source file!"))))

(defun norns-send-selection ()
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
            (define-key mmap (kbd "C-c e r") #'norns-send-selection)
            (define-key mmap (kbd "C-c e c") #'norns-send)
            (define-key mmap (kbd "C-c e R") #'norns-load-current-script)
            mmap))

(defun norns-mode-hook ()
  (when (norns--current-host-norns-p)
    (norns-mode 1)))



;; SYSTEM HELPER COMMANDS

(defun norns-restart ()
  (interactive)
  (let* ((default-directory (norns--location-from-access-policy))
         (dd default-directory)
         (host (norns--core-curr-host)))
    (shell-command "nohup systemctl restart norns-sclang > /dev/null")
    (shell-command "nohup systemctl restart norns-crone > /dev/null")
    (shell-command "nohup systemctl restart norns-matron > /dev/null")
    (let* ((frame (selected-frame))
           (win (selected-window))
           (buff (norns--ensure-host-maiden-buffer-exists host))
           (visiting-windows (get-buffer-window-list buff)))
      (when (and norns-repl-switch-on-cmd
                 (null visiting-windows))
        (funcall norns-repl-switch-fn buff)
        (end-of-buffer)
        (when norns-repl-switch-no-focus
          (set-frame-selected-window frame win))))

    ;; NB: maiden seems to need a "ping" to send its start logs
    (run-at-time
     0.1 nil
     `(lambda ()
        (let ((default-directory ,dd))
          (norns-maiden-send ""))))))

(defun norns-reboot ()
  (interactive)
  (let* ((default-directory (norns--location-from-access-policy))
         (host (norns--core-curr-host)))
    (when (string= host "localhost")
      (user-error "You can't restart norns from within Emacs when it is running from norns!"))
    (shell-command "sudo reboot now")))




(provide 'norns)
