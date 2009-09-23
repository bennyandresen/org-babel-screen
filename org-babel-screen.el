;;; org-babel-screen.el --- org-babel support for interactive terminal

;; Copyright (C) 2009 Benjamin Andresen

;; Author: Benjamin Andresen
;; Keywords: literate programming, interactive shell
;; Homepage: http://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for interactive terminals. Mostly shell scripts.
;; Heavily inspired by 'eev' from Eduardo Ochs

;;; Code:
(require 'org-babel)

(org-babel-add-interpreter "screen")

(add-to-list 'org-src-lang-modes '("screen" . sh))

(defvar org-babel-screen-terminal "xterm"
  "Terminal process that should be started when in interactive mode.
Must support -T for setting title and -e for executing programs.")

(defun org-babel-execute:screen (body params)
  (message "sending source code block to interactive terminal session...")
  (save-window-excursion
    (let ((socket (org-babel-screen-session-socketname session)))
      (unless socket (progn (org-babel-prep-session:screen session params) (sleep-for 1)))
      (org-babel-screen-session-execute-string session body))))


(defun org-babel-prep-session:screen (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((socket (org-babel-screen-session-socketname session))
         (vars (org-babel-ref-variables params)))
    (apply 'start-process (concat "org-babel: terminal (session " session ")") "*Messages*"
           org-babel-screen-terminal `("-T" ,(concat "session " session) "-e" "screen"
                                      "-c" "/dev/null" "-mS" ,(concat "org-babel-session-" session)
                                      "/bin/zsh"))))

;; helper functions

(defun org-babel-screen-session-execute-string (session body)
  (let ((socket (org-babel-screen-session-socketname session)))
    (when socket
      (let ((tmpfile (org-babel-screen-session-write-temp-file session body)))
        (apply 'start-process (concat "screen (session " session ")") "*Messages*"
               "screen" `("-S" ,socket "-X" "eval" "msgwait 0"
                               ,(concat "readreg z " tmpfile)
                               "paste z"))))))

(defun org-babel-screen-session-socketname (session)
  (let* ((screen-ls (shell-command-to-string "screen -ls"))
         (sockets (remove-if-not
                   '(lambda (x)
                     (string-match (rx (or "(Attached)" "(Detached)")) x))
                   (split-string screen-ls "\n")))
         (match-socket (find-if
                        '(lambda (x)
                          (string-match (concat "org-babel-session-" session) x))
                        sockets)))
    (when match-socket (car (split-string match-socket)))))

(defun org-babel-screen-session-write-temp-file (session body)
  (let ((tmpfile (concat "/tmp/screen.org-babel-session-" session)))
    (with-temp-file tmpfile
      (insert body)

      ;; org-babel has superflous spaces
      (goto-char (point-min))
      (delete-matching-lines "^ +$"))
    tmpfile))

(provide 'org-babel-screen)
;;; org-babel-screen.el ends here
