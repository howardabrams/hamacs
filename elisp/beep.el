;;; beep.el --- A literate programming for alerting after long projects. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Howard X. Abrams
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams <howard.abrams@gmail.com>
;; Created: December 23, 2020
;;
;; This file is not part of GNU Emacs.
;;
;; *NB:* Do not edit this file. Instead, edit the original literate file at:
;;            ~/website/Technical/Emacs/beep-for-emacs.org
;;       And tangle the file to recreate this one.
;;
;;; Code:

(defvar beep-alert-sound-file
  (expand-file-name "~/other/hamacs/beep-notify.wav")
  "A WAV or AU file used at the completion of a function.")

;; My replacement in case we can't play internal sounds:
(defun beep--beep ()
  "Play a default notification sound file.
Customize the variable, `beep-alert-sound-file' to adjust the sound."
  (if (fboundp 'play-sound-internal)
      (play-sound-file beep-alert-sound-file)
    (call-process-shell-command (format "afplay %s &" beep-alert-sound-file) nil 0)))

(defvar beep-speech-executable "say %s"
  "An OS-dependent shell string to speak. Replaces `%s' with a phrase.")

(defun beep--speak (phrase)
  "Call a program to speak the string, PHRASE.
Customize the variable, `beep-speech-executable'."
  (let ((command (format beep-speech-executable phrase)))
    (shell-command command)))

(defun beep--when-finished (phrase)
  "Notify us with string, PHRASE, to grab our attention.
Useful after a long process has completed, but use sparingly,
as this can be pretty distracting."
  (message phrase)
  (when (functionp 'alert)
    (alert phrase :title "Completed"))
  (beep--beep)
  (beep--speak phrase))

(defun compile-and-notify ()
  "Call `counsel-compile' and notify us when finished.
See `beep--when-finished' for details."
  (interactive)
  (let ((root (projectile-project-root)))
    (counsel-compile root)
    (beep--when-finished "The compile command has finished.")))

(defvar beep-func-too-long-time 5
   "The number of seconds a function runs before it is considered taking too much time, and needing to be alerted when it has finished.")

(defun beep--after-function (func)
  "Call the function, FUNC, interactively, and notify us when completed."
  (let ((start-time (current-time))
        duration)
    (call-interactively func)
    (setq duration (thread-first
                     (current-time)
                     (time-subtract start-time)
                     decode-time
                     first))
    (when (> duration beep-func-too-long-time)
      (beep--when-finished (format "The function, %s, has finished." func)))))

(defun recompile-and-notify ()
  "Call `recompile' and notify us when finished.
See `beep--when-finished' for details."
  (interactive)
  (beep--after-function 'recompile))

(global-set-key (kbd "C-c c") 'recompile-and-notify)
(global-set-key (kbd "C-c C") 'compile-and-notify)

(defun beep-when-runs-too-long (orig-function &rest args)
  "Notifies us about the completion of ORIG-FUNCTION.
  Useful as after advice to long-running functions, for instance:

          (advice-add 'org-publish :around #'beep-when-runs-too-long)"
  (let ((start-time (current-time))
        duration)
    (apply orig-function args)
    (setq duration (thread-first
                     (current-time)
                     (time-subtract start-time)
                     decode-time
                     first))
    (when (> duration beep-func-too-long-time)
      (beep--when-finished (format "The function, %s, has finished."
                                   (beep--extract-function-name orig-function))))))

(defun beep--extract-function-name (expr)
  "Extracts the original function from a lambda expression, EXPR."
  (if (listp expr)
     (if (equal (car expr) 'lambda)
         (car (cadr expr))
       expr)
    expr))

(provide 'beep)
;;; beep.el ends here
