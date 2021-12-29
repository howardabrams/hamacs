;;; BOXES-EXTRAS --- Refiling Subtrees to Proper Org Files
;;
;; Author: Howard Abrams <howard@howardabrams.com>
;; Copyright © 2019, Howard Abrams, all rights reserved.
;; Created:  7 January 2019
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;   WARNING: This file is tangled from its original essay.
;;
;;   For a thorough explanation of this code, see the online essay:
;;     http://www.howardism.org/Technical/Emacs/getting-even-more-boxes-done.html
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defun org-subtree-metadata ()
  "Return a list of key aspects of an org-subtree. Includes the
following: header text, body contents, list of tags, region list
of the start and end of the subtree."
  (save-excursion
    ;; Jump to the parent header if not already on a header
    (when (not (org-at-heading-p))
      (org-previous-visible-heading 1))

    (let* ((context (org-element-context))
           (attrs   (second context))
           (props   (org-entry-properties)))

      (list :region     (list (plist-get attrs :begin) (plist-get attrs :end))
            :header     (plist-get attrs :title)
            :tags       (org-get-subtree-tags props)
            :properties (org-get-subtree-properties attrs)
            :body       (org-get-subtree-content attrs)))))

(defun org-get-subtree-tags (&optional props)
  "Given the properties, PROPS, from a call to
`org-entry-properties', return a list of tags."
  (unless props
     (setq props (org-entry-properties)))
  (let ((tag-label (if org-get-subtree-tags-inherited "ALLTAGS" "TAGS")))
    (-some->> props
         (assoc tag-label)
         cdr
         substring-no-properties
         (s-split ":")
         (--filter (not (equalp "" it))))))

(defvar org-get-subtree-tags-inherited t
  "Returns a subtree's tags, and all tags inherited (from tags
  specified in parents headlines or on the file itself). Defaults
  to true.")

(defun org-get-subtree-properties (attributes)
  "Return a list of tuples of a subtrees properties where the keys are strings."

  (defun symbol-upcase? (sym)
    (let ((case-fold-search nil))
      (string-match-p "^:[A-Z]+$" (symbol-name sym))))

  (defun convert-tuple (tup)
    (let ((key (first tup))
          (val (second tup)))
      (list (substring (symbol-name key) 1) val)))

  (->> attributes
       (-partition 2)                         ; Convert plist to list of tuples
       (--filter (symbol-upcase? (first it))) ; Remove lowercase tuples
       (-map 'convert-tuple)))

(defun org-get-subtree-content (attributes)
  "Return the contents of the current subtree as a string."
  (let ((header-components '(clock diary-sexp drawer headline inlinetask
                             node-property planning property-drawer section)))

      (goto-char (plist-get attributes :contents-begin))

      ;; Walk down past the properties, etc.
      (while
          (let* ((cntx (org-element-context))
                 (elem (first cntx))
                 (props (second cntx)))
            (when (member elem header-components)
              (goto-char (plist-get props :end)))))

      ;; At this point, we are at the beginning of what we consider
      ;; the contents of the subtree, so we can return part of the buffer:
      (buffer-substring-no-properties (point) (org-end-of-subtree))))

(defun org-refile-subtree-to-file (dir)
  "Archive the org-mode subtree and create an entry in the
directory folder specified by DIR. It attempts to move as many of
the subtree's properties and other features to the new file."
  (interactive "DDestination: ")
  (let* ((props      (org-subtree-metadata))
         (head       (plist-get props :header))
         (body       (plist-get props :body))
         (tags       (plist-get props :tags))
         (properties (plist-get props :properties))
         (area       (plist-get props :region))
         (filename   (org-filename-from-title head))
         (filepath   (format "%s/%s.org" dir filename)))
    (apply #'delete-region area)
    (org-create-org-file filepath head body tags properties)))

(defun org-create-org-file (filepath header body tags properties)
  "Create a new Org file by FILEPATH. The contents of the file is
pre-populated with the HEADER, BODY and any associated TAGS."
  (find-file-other-window filepath)
  (org-set-file-property "TITLE" header t)
  (when tags
    (org-set-file-property "FILETAGS" (s-join " " tags)))

  ;; Insert any drawer properties as #+PROPERTY entries:
  (when properties
    (goto-char (point-min))
    (or (re-search-forward "^\s*$" nil t) (point-max))
    (--map (insert (format "#+PROPERTY: %s %s" (first it) (second it))) properties))

  ;; My auto-insert often adds an initial headline for a subtree, and in this
  ;; case, I don't want that... Yeah, this isn't really globally applicable,
  ;; but it shouldn't cause a problem for others.
  (when (re-search-forward "^\\* [0-9]$" nil t)
    (replace-match ""))

  (delete-blank-lines)
  (goto-char (point-max))
  (insert "\n")
  (insert body))

(defun org-filename-from-title (title)
  "Creates a useful filename based on a header string, TITLE.
For instance, given the string:    What's all this then?
     This function will return:    whats-all-this-then"
  (let* ((no-letters (rx (one-or-more (not alphanumeric))))
         (init-try (->> title
                        downcase
                        (replace-regexp-in-string "'" "")
                        (replace-regexp-in-string no-letters "-"))))
    (string-trim init-try "-+" "-+")))

(defun org-set-file-property (key value &optional spot)
  "Make sure file contains a top-level, file-wide property.
KEY is something like `TITLE' or `FILETAGS'. This function makes
sure that the property contains the contents of VALUE, and if the
file doesn't have the property, it is inserted at either SPOT, or
if nil,the top of the file."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward (format "^#\\+%s:\s*\\(.*\\)" key) nil t)
          (replace-match value nil nil nil 1)

        (cond
         ;; if SPOT is a number, go to it:
         ((numberp spot) (goto-char spot))
         ;; If SPOT is not given, jump to first blank line:
         ((null spot) (progn (goto-char (point-min))
                             (re-search-forward "^\s*$" nil t)))
         (t (goto-char (point-min))))

        (insert (format "#+%s: %s\n" (upcase key) value))))))

(defun org-refile-to-projects-dir ()
  "Move the current subtree to a file in the `projects' directory."
  (interactive)
  (org-refile-subtree-to-file org-default-projects-dir))

(defun org-refile-to-technical-dir ()
  "Move the current subtree to a file in the `technical' directory."
  (interactive)
  (org-refile-subtree-to-file org-default-technical-dir))

(define-auto-insert "/personal/*\\.org" ["personal.org" ha/autoinsert-yas-expand])

(defun org-refile-to-personal-dir ()
  "Move the current subtree to a file in the `personal' directory."
  (interactive)
  (org-refile-subtree-to-file org-default-personal-dir))

(provide 'boxes-extras)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; boxes-extras.el ends here
