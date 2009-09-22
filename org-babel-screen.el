(defvar org-screen-terminal "xterm"
  "Terminal process that should be started when in interactive mode.
Must support -T for setting title and -e for executing programs.")

(defun org-screen-channel-socketname (channel)
  (let* ((screen-ls (shell-command-to-string "screen -ls"))
         (sockets (remove-if-not
                   '(lambda (x)
                     (string-match (rx (or "(Attached)" "(Detached)")) x))
                   (split-string screen-ls "\n")))
         (match-socket (find-if
                        '(lambda (x)
                          (string-match (concat "channel-" channel) x))
                        sockets)))
    (when match-socket (car (split-string match-socket)))))

(defun org-screen-channel-start-interactive (channel)
  (apply 'start-process (concat "terminal (channel " channel ")") "*Messages*"
         org-screen-terminal `("-T" ,(concat "channel " channel) "-e" "screen"
         "-c" "/dev/null" "-mS" ,(concat "channel-" channel) "/bin/sh")))

(defun org-screen-channel-write-temp-file (channel input)
  (let ((tmpfile (concat "/tmp/screen.channel-" channel)))
    (with-temp-file tmpfile
      (insert input)
      (insert "\n"))
    tmpfile))

(defun org-screen-channel-execute-string (channel input)
  (let ((socket (org-screen-channel-socketname channel)))
    (when socket
      (let ((tmpfile (org-screen-channel-write-temp-file channel input)))
        (apply 'start-process (concat "screen (channel " channel ")") "*Messages*"
               "screen" `("-S" ,socket "-X" "eval" "msgwait 0"
                               ,(concat "readreg z " tmpfile)
                               "paste z" "slowpaste 0" "msgwait 1"))))))


;;; test case from http://angg.twu.net/eev-current/anim/channels.anim.html
;;; (proof of concept, still needs integration to org-babel)
(org-screen-channel-start-interactive "A") ;; create
(org-screen-channel-start-interactive "B") ;; create

(org-screen-channel-execute-string "B"
"# Listen on port 1234
netcat -l -p 1234")

(org-screen-channel-execute-string "A"
"# Send things to port 1234
{
  echo hi
  sleep 1
  echo bye
  sleep 1
} | netcat localhost 1234")
