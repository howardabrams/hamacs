;;; BOXES --- An opinionated approach to a GTD workflow
;;
;; Author: Howard Abrams <howard@howardabrams.com>
;; © 2019-2023 Howard Abrams, all rights reserved.
;;   This work is licensed under a Creative Commons Attribution 4.0 International License.
;;   See http://creativecommons.org/licenses/by/4.0/
;; Created:  7 January 2019
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;   WARNING: This file is tangled from its original essay.
;;
;;   For a thorough explanation of this code, see the online essay:
;;     http://www.howardism.org/Technical/Emacs/getting-more-boxes-done.html
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

(defvar org-default-projects-dir   "~/projects"                     "Primary GTD directory")
(defvar org-default-technical-dir  "~/technical"                    "Directory of shareable notes")
(defvar org-default-personal-dir   "~/personal"                     "Directory of un-shareable, personal notes")
(defvar org-default-completed-dir  "~/projects/trophies"            "Directory of completed project files")
(defvar org-default-inbox-file     "~/projects/breathe.org"         "New stuff collects in this file")
(defvar org-default-tasks-file     "~/projects/tasks.org"           "Tasks, TODOs and little projects")
(defvar org-default-incubate-file  "~/projects/incubate.org"        "Ideas simmering on back burner")
(defvar org-default-completed-file nil                              "Ideas simmering on back burner")
(defvar org-default-notes-file     "~/personal/general-notes.org"   "Non-actionable, personal notes")
(defvar org-default-media-file     "~/projects/media.org"           "White papers and links to other things to check out")

(defvar org-capture-templates (list))

(add-to-list 'org-capture-templates
             `("t" "Task Entry"        entry
               (file ,org-default-inbox-file)
               "* %?\n:PROPERTIES:\n:CREATED:%U\n:END:\n\n%i\n\nFrom: %a"
               :empty-lines 1))

(defhydra hydra-org-refiler (org-mode-map "C-c s" :hint nil)
    "
  ^Navigate^      ^Refile^       ^Move^           ^Update^        ^Go To^        ^Dired^
  ^^^^^^^^^^---------------------------------------------------------------------------------------
  _k_: ↑ previous _t_:   tasks     _m X_: projects  _T_: todo task  _g t_: tasks    _g X_: projects
  _j_: ↓ next     _i_/_I_: incubate  _m P_: personal  _S_: schedule   _g i_: incubate _g P_: personal
  _c_: archive    _p_:   personal  _m T_: technical _D_: deadline   _g x_: inbox    _g T_: technical
  _d_: delete     _r_:   refile                   _R_: rename     _g n_: notes    _g C_: completed
  "
    ("<up>" org-previous-visible-heading)
    ("<down>" org-next-visible-heading)
    ("k" org-previous-visible-heading)
    ("j" org-next-visible-heading)
    ("c" org-archive-subtree-as-completed)
    ("d" org-cut-subtree)
    ("t" org-refile-to-task)
    ("i" org-refile-to-incubate)
    ("I" org-refile-to-another-incubator)
    ("p" org-refile-to-personal-notes)
    ("r" org-refile)
    ("m X" org-refile-to-projects-dir)
    ("m P" org-refile-to-personal-dir)
    ("m T" org-refile-to-technical-dir)
    ("T" org-todo)
    ("S" org-schedule)
    ("D" org-deadline)
    ("R" org-rename-header)
    ("g t" (find-file-other-window org-default-tasks-file))
    ("g i" (find-file-other-window org-default-incubate-file))
    ("g x" (find-file-other-window org-default-inbox-file))
    ("g c" (find-file-other-window org-default-completed-file))
    ("g n" (find-file-other-window org-default-notes-file))
    ("g X" (dired org-default-projects-dir))
    ("g P" (dired org-default-personal-dir))
    ("g T" (dired org-default-technical-dir))
    ("g C" (dired org-default-completed-dir))
    ("[\t]" (org-cycle))
    ("s" (org-save-all-org-buffers) "save")
    ("<tab>" (org-cycle) "toggle")
    ("q" nil "quit"))

(setq org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes t
      org-outline-path-complete-in-steps nil)

(setq org-refile-targets
      (append `((,(expand-file-name org-default-media-file) :level . 1)
                (,(expand-file-name org-default-notes-file) :level . 0))
              (->>
               (directory-files org-default-projects-dir nil ".org")
               (-remove-item (file-name-base org-default-media-file))
               (--remove (s-starts-with? "." it))
               (--remove (s-ends-with? "_archive" it))
               (--map (format "%s/%s" (expand-file-name org-default-projects-dir) it))
               (--map `(,it :level . 0)))))

(setq org-refile-target-table nil)

(defun org-subtree-region ()
  "Return a list of the start and end of a subtree."
  (save-excursion
    (list (progn (org-back-to-heading) (point))
          (progn (org-end-of-subtree)  (point)))))

(defun org-refile-directly (file-dest)
  "Move the current subtree to the end of FILE-DEST.
If SHOW-AFTER is non-nil, show the destination window,
otherwise, this destination buffer is not shown."
  (interactive "fDestination: ")

  (defun dump-it (file contents)
    (find-file-other-window file-dest)
    (goto-char (point-max))
    (insert "\n" contents))

  (save-excursion
    (let* ((region (org-subtree-region))
           (contents (buffer-substring (first region) (second region))))
      (apply 'kill-region region)
      (if org-refile-directly-show-after
          (save-current-buffer (dump-it file-dest contents))
        (save-window-excursion (dump-it file-dest contents))))))

(defvar org-refile-directly-show-after nil
  "When refiling directly (using the `org-refile-directly'
function), show the destination buffer afterwards if this is set
to `t', otherwise, just do everything in the background.")

(defun org-refile-to-incubate ()
  "Refile (move) the current Org subtree to `org-default-incubate-fire'."
  (interactive)
  (org-refile-directly org-default-incubate-file))

(defun org-refile-to-task ()
  "Refile (move) the current Org subtree to `org-default-tasks-file'."
  (interactive)
  (org-refile-directly org-default-tasks-file))

(defun org-refile-to-personal-notes ()
  "Refile (move) the current Org subtree to `org-default-notes-file'."
  (interactive)
  (org-refile-directly org-default-notes-file))

(defun org-refile-to-completed ()
  "Refile (move) the current Org subtree to `org-default-completed-file',
unless it doesn't exist, in which case, refile to today's journal entry."
  (interactive)
  (if (and org-default-completed-file (file-exists-p org-default-completed-file))
      (org-refile-directly org-default-completed-file)
    (org-refile-directly (get-journal-file-today))))

(defun org-rename-header (label)
  "Rename the current section's header to LABEL, and moves the
point to the end of the line."
  (interactive (list
                (read-string "Header: "
                             (substring-no-properties (org-get-heading t t t t)))))
  (org-back-to-heading)
  (replace-string (org-get-heading t t t t) label))

(defun org-archive-subtree-as-completed ()
  "Archives the current subtree to today's current journal entry."
  (interactive)
  ;; According to the docs for `org-archive-subtree', the state should be
  ;; automatically marked as DONE, but I don't notice it, so let's force:
  (when (not (equal "DONE" (org-get-todo-state)))
    (org-todo "DONE"))

  (let* ((org-archive-file (or org-default-completed-file
                               (todays-journal-entry)))
         (org-archive-location (format "%s::" org-archive-file)))
     (org-archive-subtree)))

(defun todays-journal-entry ()
  "Return the full pathname to the day's journal entry file.
Granted, this assumes each journal's file entry to be formatted
with year/month/day, as in `20190104' for January 4th.

Note: `org-journal-dir' variable must be set to the directory
where all good journal entries live, e.g. ~/journal."
  (let* ((daily-name   (format-time-string "%Y%m%d"))
         (file-name    (concat org-journal-dir daily-name)))
    (expand-file-name file-name)))

;; Attempt to load the extra library functions tangled from a different essay:
(condition-case nil
    (load-library "boxes-extras")
  (error
   (defun org-refile-to-projects-dir ()
     (interactive)
     (message "Need to load the 'boxes-extra project first."))
   (defun org-refile-to-personal-dir ()
     (interactive)
     (message "Need to load the 'boxes-extra project first."))
   (defun org-refile-to-technical-dir ()
     (interactive)
     (message "Need to load the 'boxes-extra project first."))))

(defun org-boxes-workflow ()
  "Load the default tasks file and start our hydra on the first task shown."
  (interactive)
  (let ((org-startup-folded nil))
    (find-file org-default-inbox-file)
    (delete-other-windows)
    (ignore-errors
      (ha/org-agenda))
    (delete-other-windows)
    (split-window-right-and-focus)
    (pop-to-buffer (get-file-buffer org-default-inbox-file))
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (hydra-org-refiler/body)))

(defun ha/org-agenda ()
  "Displays my favorite agenda perspective."
  (interactive)
  (org-agenda nil "a")
  (get-buffer "*Org Agenda*")
  (execute-kbd-macro (kbd "A t")))

(provide 'boxes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; boxes.el ends here
