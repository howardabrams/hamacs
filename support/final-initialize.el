;; -*- mode: emacs-lisp-mode -*-
;;; final-initialize.el --- Reinstall my Emacs configuration files -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Howard X. Abrams
;;
;; Author: Howard X. Abrams <http://github/howard>
;; Maintainer: Howard X. Abrams <howard.abrams@gmail.com>
;; Created: September 11, 2020
;; Modified: September 11, 2020
;; Version: 0.0.1
;; Homepage: https://gitlab.com/howardabrams/hamacs
;; Package-Requires: ((emacs 27.1.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Reinstall my Emacs configuration files
;;
;;; Code:

(require 'ob)
(require 'f)

;; The project source is actually defined in ~/.doom.d/hamacs-init.el
;; However, we define it here to get rid of Emacs linter warnings. ;-)
(defvar hamacs-source-dir
  (f-parent (f-parent (or load-file-name (buffer-file-name))))
  "My configuration's source code directory.")

;; Where should the code end up?
(defvar hamacs-private-code-dir (f-join user-emacs-directory "elisp")
  "Location for most of the `hamacs' source code and configuration Lisp files.")

(defun ha/install-by-tangling (source-files)
  (dolist (file source-files)
    (message "Tangling: %s" file)
    (org-babel-tangle-file file)))

(defun ha/install-code-from-essays ()
  "Tangle select website essays into the elisp bucket."
  (ha/install-by-tangling
   '("~/website/Technical/Emacs/getting-more-boxes-done.org"
     "~/website/Technical/Emacs/getting-even-more-boxes-done.org"
     "~/website/Technical/Emacs/beep-for-emacs.org"
     "~/website/Technical/Emacs/focused-work.org")))

(defun ha/install-code-from-hamacs ()
  "Copy Emacs Lisp code without needing to translate."
  (dolist (file (directory-files
		 (f-join hamacs-source-dir "elisp") t (rx bol (not "."))))
    (copy-file file (file-name-as-directory hamacs-private-code-dir) t)))

(defun ha/install-configuration ()
  "Two primary jobs: tangle all source configuration files and link
non-tangled files."
  (interactive)
  (unless (f-exists? user-emacs-directory)
    (make-directory user-emacs-directory))
  (unless (f-exists? hamacs-private-code-dir)
    (make-directory hamacs-private-code-dir))

  (ha/install-code-from-essays)
  ;; (ha/install-code-from-hamacs)


(ha/install-configuration)
(provide 'final-initialize)
;;; final-initialize.el ends here
