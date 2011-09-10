;;;; omm-mode.el
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) pogin 


;; Author: pogin
;; Maintainer: pogin
;; Keywords: convenience, frames
;; Created: 2011/07/04
;; Version: 0.1.2
;; URL: 
;; Site: http://d.hatena.ne.jp/pogin/

;;; License
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the

;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Installation:
;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;; (require 'omm-mode)
;;


;;; Commentary:
;;
;; Tested on Emacs 23.
;;

;;; Commands:
;;
;; `omm-mode-toggle'
;; toggle omm-mode on/off 
;;
;; `omm-mode-style-on'
;; bunish tool-bar, menu-bar, mode-line etc
;;
;; `omm-mode-style-off'
;; omm-mode-style off
;; undo initial status

;;; Keybinds
;;
;; C-c C-o: omm-mode-toggle

;;; Customizable Options:
;;
;; None
;;



;;; Change log:
;; 2011/09/11
;;     omm-mode was globalization
;; 2011/09/05
;;     Add function; omm-mode-linum-toggle 
;;     Add function: omm-mode-change-mode-line
;; 2011/07/04
;;     Created

;;; TODO
;;
;; make play-music function
;; make Change Background Picture function?
;; make Timer function 
;; make defcustom etc

;;; Code
(defvar omm-mode-line-conf-list mode-line-format
  "Save mode-line-format")
;;omm-mode-line-conf-list

(defvar omm-mode-start-var nil
  "If omm-mode-start-var variable is nil, omm-mode is off.
If you eval omm-mode-toggle function, omm-mode is on.
If this variable is t, omm-mode is on.
If you eval omm-mode-toggle, omm-mode-start-var change nil")

(defconst omm-init-list-flag 
  (list 
	 (eq linum-mode t)
	 (eq scroll-bar-mode t)
	 (eq tool-bar-mode t)
	 (eq menu-bar-mode t)
	 omm-mode-line-conf-list
	(if (fboundp 'elscreen-mode)
		(list t (eq elscreen-mode t))
	  (list nil nil))
	(if (fboundp 'tabbar-mode)
		(list t (eq tabbar-mode t))
	  (list nil nil))
	))

(defun omm-mode-change-mode-line (apply-state)
  (save-excursion
	(dolist (buf (buffer-list))
	  (set-buffer buf)
	  (setq mode-line-format apply-state))))

;;test code
;; (omm-mode-change-mode-line omm-mode-line-conf-list)
;; (omm-mode-change-mode-line nil)

(defun omm-mode-linum-toggle (num)
  (save-excursion
	(dolist (buf (buffer-list))
	  (set-buffer buf)
	  (linum-mode num))
	(if (equal num -1)
		(global-linum-mode -1)
	  (global-linum-mode 1))))

;;test code
;; (omm-mode-linum-toggle -1)
;; (omm-mode-linum-toggle 1)


(defun omm-mode-style-on ()
  (interactive)
  (omm-mode-linum-toggle -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (omm-mode-change-mode-line nil)
  (if (fboundp 'elscreen-mode)
      (elscreen-mode -1))
  (if (fboundp 'tabbar-mode)
      (tabbar-mode -1)))

(defun omm-mode-style-off ()
  (interactive)
  (if (eq (nth 0 omm-init-list-flag) t)
      (omm-mode-linum-toggle 1))
  (if (eq (nth 1 omm-init-list-flag) t)
      (scroll-bar-mode 1))
  (if (eq (nth 2 omm-init-list-flag) t)
      (tool-bar-mode 1))
  (if (eq (nth 3 omm-init-list-flag) t)
      (menu-bar-mode 1))
  (omm-mode-change-mode-line omm-mode-line-conf-list)
  (if (and (eq (first (nth 5 omm-init-list-flag)) t)
           (eq (second (nth 5 omm-init-list-flag)) t))
      (elscreen-mode t))
  (if (and (eq (first (nth 6 omm-init-list-flag)) t)
           (eq (second (nth 6 omm-init-list-flag)) t))
      (tabbar-mode t)))

(defun omm-mode-style-off-debug ()
	(omm-mode-linum-toggle 1)
	(scroll-bar-mode -1)
	(tool-bar-mode 1)
	(menu-bar-mode 1)
	(omm-mode-change-mode-line omm-mode-line-conf-list)
	(if (fboundp 'elscreen-mode)
		(elscreen-mode -1))
	(if (fboundp 'tabbar-mode)
		(tabbar-mode 1))
	(setq omm-mode-start-var nil))

;;test code
;; (omm-mode-style-off-debug)

(defun omm-mode-toggle ()
  (interactive)
  (if (eq omm-mode-start-var t)
	  (progn
        (omm-mode-style-off)
        (setq omm-mode-start-var nil))
    (progn
      (omm-mode-style-on)
      (setq omm-mode-start-var t))))

;;test code
;;(omm-mode-toggle)

(defun omm-mode-start ()
  (omm-mode-toggle)
  (set-keymap-parent omm-minor-mode-child-map
                     omm-mode-map)
  (omm-mode-run-hook)
  )

(defun omm-mode-stop ()
  (omm-mode-style-off))

(defun omm-mode-define-keymap ()
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") 'omm-mode-toggle)
    map))

(defvar omm-mode-map
  (omm-mode-define-keymap))

;; (pop omm-mode-map)
(defvar omm-minor-mode-child-map (make-sparse-keymap))
(define-minor-mode omm-mode
  :init-value t
  :global t
  :keymap omm-mode-map
  :group omm-mode
  (if omm-mode
      (omm-mode-start)
 	(omm-mode-stop)))

(defun omm-mode-run-hook ()
  (run-hooks 'omm-mode-hook))

(defun omm-mode-add-hook ()
  (add-hook 'omm-mode-hook
            (if (eq omm-mode-start-var nil)
                (omm-mode-style-on))))

;;(pop omm-mode-hook)

(provide 'omm-mode)

;;; filename ends here