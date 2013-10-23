;;; achievements-mode.el --- Achievements for emacs!

;; Copyright (C) 2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: games
;; Created: 22th Oct 2013

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Achievements for emacs.

;; More info at https://github.com/Fuco1/achievements-mode

;;; Code:

(require 'dash)
(require 'f)

;;; data
(defvar amode-achievements nil
  "Alist of achievements.  Achievements are indexed by
achievement ID. This should be a unique string the achievement
will be recognized by.  This is not displayed to the user.

Each achievement id   fshould \"link\" to a plist with following keys:
:desc - General description of this achievement.  This is displayed to the user.
:levels - Number of levels this achievement has.
:level-names - List with name for each level of the achievement.
               These names should include punctuation.
:level-descs - List with description for each level.
:handler - Function that is called in
           `amode-post-command-handler' to generate achievements.
           It should take one argument, the current level of this
           achievement.  If achievement has only one level, it is
           safe to ignore this value.  This function should
           generally be *fast* as it gets called after every
           command.")

(defvar amode-achieved-achievements nil
  "An alist of already achieved achievements and its reached
level.  In addition, achievements can store arbitrary state in
this map.  This is also persisted when exiting an emacs session
and loaded up on start.

The format is: (id :level level :my-key ...)

You can use `update-plist-data-in-alist' to update these values easily.")


;;; macro madness to access the data
(defmacro amode-update-plist-in-alist (alist-id alist plist)
  "Set plist with ALIST-ID in ALIST to PLIST."
  `(-if-let (alist-node (assoc ,alist-id ,alist))
       (setcdr alist-node (copy-sequence ,plist))
     (setq ,alist (push (cons ,alist-id ,plist) ,alist))))

(defmacro amode-update-plist-data-in-alist (alist-id alist prop data)
  "Update PROP with DATA in plist with ALIST-ID in ALIST.

If such ID is not present, do nothing."
  `(-when-let (alist-node (assoc ,alist-id ,alist))
     (setcdr alist-node (copy-sequence (plist-put (cdr alist-node) ,prop ,data)))))

(defmacro amode-set-achievement-data (id prop data)
  "Update PROP with DATA for achievement identified by ID."
  `(amode-update-plist-data-in-alist ,id amode-achieved-achievements ,prop ,data))

(defmacro amode-set-achievement-data-plist (id plist)
  "Set plist for achievement ID to PLIST."
  `(amode-update-plist-in-alist ,id amode-achieved-achievements ,plist))

(defun amode-get-achievement-data (id)
  "Get the data plist associated with achievement ID."
  (cdr (assoc id amode-achieved-achievements)))


;;; adding achievements
(defmacro amode-add-achievement (id desc levels level-names level-descs handler)
  "Register an achievement.

For the description of arguments, see `amode-achievements'."
  `(amode--add-achievement ,id ,desc ,levels ',level-names ',level-descs ',handler))

(defun amode--add-achievement (id desc levels level-names level-descs handler)
  "Register an achievement.

For the description of arguments, see `amode-achievements'."
  (let ((ament (list :desc desc
                     :levels levels
                     :level-names level-names
                     :level-descs level-descs
                     :handler handler)))
    (amode-update-plist-in-alist id amode-achievements ament)
    (unless (assoc id amode-achieved-achievements)
      (amode-set-achievement-data-plist id (list :level 0)))))


;;; the rest :P
(defun amode-post-command-handler ()
  "Run achievement handlers for achievements that aren't already
at the highest level."
  ;; we ignore errors so this hook won't get removed automagically by
  ;; emacs.
  (ignore-errors
    (dolist (ament amode-achievements)
      (let* ((id (car ament))
             (plist (cdr ament))
             (current-level (or (plist-get (amode-get-achievement-data id) :level) 0)))
        (when (funcall (plist-get plist :handler) current-level)
          ;; achievement unlocked!
          (let* ((name (nth current-level (plist-get plist :level-names)))
                 (current-level (1+ current-level)))
            (message "ACHIEVEMENT UNLOCKED: %s Check *achievements* for details." name)
            (with-current-buffer (get-buffer-create "*achievements*")
              (erase-buffer)
              (insert (format "ACHIEVEMENT UNLOCKED: %s [Level: %d/%d]\n\n%s\n\n"
                              name current-level (plist-get plist :levels) (plist-get plist :desc)))
              (dotimes (i current-level)
                (insert (format "[%d] %s - %s\n"
                                (1+ i)
                                (nth i (plist-get plist :level-names))
                                (nth i (plist-get plist :level-descs))))))
            (amode-set-achievement-data id :level current-level)
            ;; remove the achievement from the list if max level is reached.
            (when (= current-level (plist-get plist :levels))
              (setf amode-achievements (delete* id amode-achievements :key (function car))))
            (amode-save-to-disc)))))))

(defgroup achievements-mode nil
  "Achievements for emacs!"
  :prefix "amode-")

(defcustom amode-save-file "~/.emacs.d/achievements"
  "Achievements are stored here upon exiting emacs."
  :group 'achievements-mode
  :type 'file)

;; make sure to remove achievements that has max level already achieved
(defun amode-load-from-disc ()
  (when (file-exists-p amode-save-file)
    (load amode-save-file t)
    ;; remove the max-level achievements
    (dolist (ament amode-achievements)
      (let* ((id (car ament))
             (plist (cdr ament))
             (current-level (plist-get (amode-get-achievement-data id) :level)))
        (when (= current-level (plist-get plist :levels))
          (setf amode-achievements (delete* id amode-achievements :key (function car))))))))

(defun amode-save-to-disc ()
  (with-temp-file amode-save-file
    (insert ";; This file is automatically generated by achievements-mode.el")
    (newline)
    (insert ";; It keeps track of achievements you've already achieved.")
    (newline)
    (insert ";; Do not edit it manually, otherwise you'll spoil yourself the fun!")
    (newline)
    (newline)
    (--each amode-achieved-achievements
      (insert (format "(amode-set-achievement-data-plist %S (list %s))\n"
                      (car it)
                      (mapconcat (lambda (x) (format "%S" x)) (cdr it) " "))))))

(define-minor-mode achievements-mode
  "Turn on achievements mode.  This will install a post-command
hook that will record activity and update the achievements."
  :global t
  :lighter " AchieveMode"
  (if achievements-mode
      (add-hook 'post-command-hook 'amode-post-command-handler)
    (remove-hook 'post-command-hook 'amode-post-command-handler)
    (amode-save-to-disc)))

(add-hook 'kill-emacs-hook 'amode-save-to-disc)

(require 'achievements-list)
(amode-load-from-disc)

(provide 'achievements-mode)

;;; achievements-mode.el ends here
