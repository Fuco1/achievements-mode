;;; achievements-list.el --- Achievements for emacs!

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

;;; Code:

(require 'dash)
(require 'f)

;; Each achievement handler should return non-nil if the achievement
;; was achieved or nil if it wasn't.  If you define multi-level
;; achievement, you get the current level as argument LEVEL

;;; helpers

(defun plist-get-zero (plist key)
  (or (plist-get plist key) 0))


;;;;;;;;;; run tutorial
(defun amode-achievement-run-tutorial (&optional level)
  (eq this-command 'help-with-tutorial))

(amode-add-achievement
 "TUTORIAL"
 "Run the built-in emacs tutorial."
 1
 ("First steps.")
 ("Run the built-in emacs tutorial.")
 amode-achievement-run-tutorial)

;;;;;;;;;; open this file
(defun amode-achievement-cheating-bastard (&optional level)
  ;; add commands that can open files
  (when (memq this-command '(find-file
                             find-file-other-window
                             find-file-other-frame
                             ido-exit-minibuffer
                             ))
    (equal (f-filename (f-this-file)) "achievements-list.el")))

(amode-add-achievement
 "CHEATING_BASTARD"
 "Open achievements-list.el in a buffer."
 1
 ("Cheating Bastard.")
 ("Open achievements-list.el in a buffer.")
 amode-achievement-cheating-bastard)

;;;;;;;;;; start evil mode
(defun amode-achievement-evil (&optional level)
  (eq this-command 'evil-mode))

(amode-add-achievement
 "EVIL_MODE"
 "Enable evil-mode."
 1
 ("Tempted by The Dark Side.")
 ("Enable evil-mode.")
 amode-achievement-evil)

;;;;;;;;;; use "emacs" navigation, not arrow keys
(defun amode-achievement-emacs-navigation (&optional level)
  (let* ((data (amode-get-achievement-data "NO_ARROWS"))
         (cur (plist-get-zero data :data))
         new)
    (when (and (eq this-command 'forward-char) (not (eq last-command-event 'right)))
      (setq new (logior cur 1)))
    (when (and (eq this-command 'backward-char) (not (eq last-command-event 'left)))
      (setq new (logior cur 2)))
    (when (and (eq this-command 'previous-line) (not (eq last-command-event 'up)))
      (setq new (logior cur 4)))
    (when (and (eq this-command 'next-line) (not (eq last-command-event 'down)))
      (setq new (logior cur 8)))
    (if (= 15 new) t
      (amode-set-achievement-data "NO_ARROWS" :data new)
      nil)))

(amode-add-achievement
 "NO_ARROWS"
 "Use the emacs style navigation instead of arrow keys."
 1
 ("Look Ma', no arrowkeys!")
 ("Navigate the cursor up, down, forward and backward without using arrow keys.")
 amode-achievement-emacs-navigation)

;; Here's a definition of a multi-level achievements that also uses
;; state data. This is so silly it's not added by default. Maybe we
;; should add it :D

;; (defun amode-achievement-arrow-in-a-row (&optional level)
;;   (when (memq last-command-event '(left right up down))
;;     (let* ((data (amode-get-achievement-data "ARROW_COMBO"))
;;            (repeat (or (plist-get data :repeat) 0)))
;;       (if (>= repeat (nth level '(5 10)))
;;           t
;;         (amode-set-achievement-data "ARROW_COMBO" :repeat (1+ repeat))
;;         nil))))

;; (amode-add-achievement
;;  "ARROW_COMBO"
;;  "Hit arrow keys several times in a row."
;;  2
;;  ("Sharpshooter." "Master marksman.")
;;  ("Hit the arrow key 5 times in a row."
;;   "Hit the arrow key 10 times in a row.")
;;  amode-achievement-arrow-in-a-row)

(provide 'achievements-list)

;;; achievements-list.el ends here
