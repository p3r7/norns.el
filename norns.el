;;; norns.el --- Control your norns from whithin Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Jordan Besly
;;
;; Version: 0.0.1
;; Keywords: processes, terminals
;; URL: https://github.com/p3r7/norns.el
;; Package-Requires: ((emacs "27.2")(dash "2.17.0")(s "1.12.0")(websocket "20210110.17"))
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;;
;; For detailed instructions, please look at the README.md at https://github.com/p3r7/emacs.el/blob/master/README.md

;;; Code:



;; REQUIRES

(require 'dash)

(require 's)
(require 'rx)

(require 'f)
(require 'websocket)



;; VARS & CONSTS

(defvar norns-user "we")
(defvar norns-host "norns.local")
(defvar norns-ws-port 5555)
(defvar norns-ws-socket nil)

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


;; NORNS - WS

(defun norns--ensure-ws-open ()
  (unless (websocket-openp norns-ws-socket)
    (setq norns-ws-socket
          (websocket-open (format "ws://%s:%d" norns-host norns-ws-port)
                          :custom-header-alist '((Sec-WebSocket-Protocol . "bus.sp.nanomsg.org"))))))

(defun norns--send-comand (cmd)
  (norns--ensure-ws-open)
  (websocket-send-text norns-ws-socket (concat cmd "\n"))  )



;; NORNS - PATH - SCRIPTS

(defun norns--core-curr-fq-path ()
  (if (member major-mode '(dired-mode shell-mode))
      default-directory
    (buffer-file-name)))

(defun norns--core-tramp-extract-path (tramp-path)
  (let (vec localname)
    (setq vec (ignore-errors (tramp-dissect-file-name tramp-path)))
    (if vec
        (tramp-file-name-localname vec)
      nil)))

(defun norns--core-untramp-path-maybe (fp)
  (if (file-remote-p fp)
      (norns--core-tramp-extract-path fp)
    fp))

(defun norns--core-tramp-path-maybe (fp)
  (if (file-remote-p default-directory)
      (concat "/ssh:" norns-user "@" norns-host ":" fp)
    fp))

(defun norns--core-curr-path ()
  (norns--core-untramp-path-maybe (norns--core-curr-fq-path)))

(defun norns--script-from-path (fp)
  (cdr (s-match norns-script-rx fp)))

(defun norns--script-path-p (fp)
  (s-matches-p norns-script-rx fp))

(defun norns--script-dir-from-path (fp)
  (nth 1 (s-match norns-in-script-dir-rx fp)))

(defun norns--in-script-dir-path-p (fp)
  (s-matches-p norns-in-script-dir-rx fp))



;; FNS

(defun norns-current-dir-scripts ()
  "Get list of scripts corresponding to visited buffer."
  (let* ((fp (norns--core-curr-path)))
    (unless (s-starts-with? norns-script-path-prefix fp)
      (user-error "Not visiting a script source!"))

    (cond
     ((norns--script-path-p fp)
      (list (norns--script-from-path fp)))

     ((norns--in-script-dir-path-p fp)
      (let ((script-dir (norns--script-dir-from-path fp)))
        ;; TODO: ensure TRAMP prefix
        (--> (f-glob (norns--core-tramp-path-maybe
                      (concat norns-script-path-prefix script-dir "/*.lua")))
             (-map #'norns--core-untramp-path-maybe it)
             (-map #'norns--script-from-path it))))

     (:default (error "Unexpected error")))))



;; COMMANDS - SCRIPT LOAD

(defun norns-load-script-raw (script-name &optional script-dir)
  (interactive "sName: ")
  (let ((script-dir (or script-dir script-name)))
    (norns--send-comand (concat "norns.script.load(\"code/" script-dir "/" script-name ".lua\")"))))

(defun norns--load-script-helper (scripts)
  (let ((scripts-prompt (--map (cons (s-join "/" it) it) scripts))
        script)
    (if (eq (length scripts) 1)
        (setq script (car scripts))
      (setq script (--> (completing-read "Choose: " scripts-prompt)
                        (cdr (assoc it scripts-prompt)))))
    (if script
        (apply #'norns-load-script-raw (nreverse script))
      (message "canceled"))))

(defun norns-load-script ()
  (interactive)
  (let ((scripts (--> (f-glob (norns--core-tramp-path-maybe
                               (concat norns-script-path-prefix "*/*.lua")))
                      (-map #'norns--core-untramp-path-maybe it)
                      (-map #'norns--script-from-path it))))
    (norns--load-script-helper scripts)))

(defun norns-load-current-script ()
  "Load currently visited script.
If visiting a script folder, and more than 1 script is found in it, prompt."
  (interactive)
  (let* ((scripts (norns-current-dir-scripts)))
    (norns--load-script-helper scripts)))



;; COMMANDS - LUA EVAL

(defun norns-eval-selection-or-tap ()
  (interactive)
  (cond
   ((use-region-p)
    (norns--send-comand (buffer-substring (region-beginning) (region-end))))

   (:default (message "no selection"))))




(provide 'norns)
