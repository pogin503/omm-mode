;;;; omm-mode.el
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) pogin 


;; Author: pogin
;; Maintainer: pogin
;; Keywords: convenience, frames, emulation
;; Created: 2011/07/04
;; Version: 0.0.5
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
;; (omm-mode)
;;


;;; Commentary:
;;
;; Tested on Emacs 23.
;;

;;; Commands:
;;
;; `omm-toggle'
;; toggle omm-style on/off 
;;
;; `omm-style-on'
;; bunish tool-bar, menu-bar, mode-line etc
;;
;; `omm-style-off'
;; omm-style off
;; undo initial status

;;; Keybinds
;;
;; C-c C-o: omm-toggle

;;; Customizable Options:
;;
;; None
;;



;;; Change log:
;; 2011/09/28
;;    omm-margins-toggle use &optional keyword.
;; 2011/09/24
;;    Add margins functions
;;    Modified :  omm-linum-toggle
;;        When linum-mode is not found, this function does not start process.
;; 2011/09/22
;;    Add fullscreen fuctions
;; 2011/09/13
;;    Add omm-change-mode-line to find-file-hook
;; 2011/09/11
;;    omm-mode was globalization
;; 2011/09/05
;;    Add function; omm-linum-toggle 
;;    Add function: omm-change-mode-line
;; 2011/07/04
;;    Created

;;; TODO
;;
;; make play-music function
;; make Change Background Picture function?
;; make Timer function 
;; make defcustom etc
;; make fullscreen function %50 complete
;; make only current buffer to bunish gui
;; make fringe to narrow function

;;; Code


;;fringe section
(defvar omm-left-margin 25
  "Margin to add to the left side of the screen, 
depends on your resolution and prefered column width")

(defvar omm-right-margin 25
  "Margin to add to the right side of the screen,
 depends on your resolution and prefered column width")

(defvar omm-enable-multi-monitor-support t
  "Whether to enable multi-frame (i.e multiple monitor)
 support. An option since this feature is experimental")

(defvar omm-enable-longline-wrap t
  "If longlines-mode is enabled, should 
longlines-wrap-follows-window-size also be enabled when
 going into omm mode?")


(defun omm-update-window (omm-left-margin-width  omm-right-margin-width)
  (let (
		(left-margin   omm-left-margin-width)
		(right-margin  omm-right-margin-width)
		)
	(set-window-margins (selected-window)
						left-margin
						right-margin)
	))


;; left-margin-width
;; right-margin-width
(current-left-margin)
(set-left-margin 25)
;;test code
;;(omm-update-window  omm-left-margin  omm-right-margin)
;;(omm-update-window 0 0)

(defvar omm-margin-state t)

(defun omm-margins-toggle (&optional omm-arg-margins-state)
  (interactive)
  (let* ((state
		  (if (boundp 'omm-arg-margins-state)
              (progn
                omm-arg-margins-state
                (insert "t"))
            
			(progn 
			  (assert (boundp 'omm-arg-margins-state) nil)
              (assert (fboundp 'omm-arg-margins-state) nil)
              (insert "nil")
			  omm-margin-state)
			  )))
	;; (assert state t)
	(if state
		(progn 
		  (setq omm-margin-state nil)
		  (omm-update-window  omm-left-margin  omm-right-margin))
	  (progn
		(setq omm-margin-state t)
		(omm-update-window 0 0)))))

;;test code 
;;(omm-margins-toggle)
;;(omm-margins-toggle t)
;;(omm-margins-toggle nil)

;;fullscreen function section
(defvar omm-fullscreen-p t "Check if fullscreen is on or off")

(defun omm-non-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND restore #xf120
      (w32-send-sys-command 61728)
    (progn (set-frame-parameter  nil  'width 82)
           (set-frame-parameter  nil  'fullscreen  'fullheight)))
  (omm-fullscreen-state-toggle nil))

;;test code
;;(omm-non-fullscreen)

(defun omm-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND maximaze #xf030
      (w32-send-sys-command 61488)
    (set-frame-parameter nil 'fullscreen 'fullboth))
  (omm-fullscreen-state-toggle t))

;;test code
;;(omm-fullscreen)


(defun omm-fullscreen-state-toggle (state)
  (setq omm-fullscreen-p state))

;;test code
;;(omm-fullscreen-state-toggle omm-fullscreen-p)

(defun omm-toggle-fullscreen ()
  (interactive)
  (if omm-fullscreen-p
      (omm-non-fullscreen)
    (omm-fullscreen)))

;;test code
;;(omm-toggle-fullscreen)

(defvar omm-line-conf-list mode-line-format
  "Save mode-line-format")
;;omm-line-conf-list

(defvar omm-start-var nil
  "If omm-start-var variable is nil, omm-mode is off.
If you eval omm-toggle function, omm-mode is on.
If this variable is t, omm-mode is on.
If you eval omm-toggle, omm-start-var change nil")

(defconst omm-init-list-flag 
  (list 
     (eq linum-mode t)
     (eq scroll-bar-mode t)
     (eq tool-bar-mode t)
     (eq menu-bar-mode t)
     omm-line-conf-list
    (if (fboundp 'elscreen-mode)
        (list t (eq elscreen-mode t))
      (list nil nil))
    (if (fboundp 'tabbar-mode)
        (list t (eq tabbar-mode t))
      (list nil nil))
    ))

(defun omm-change-mode-line (apply-state)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (setq mode-line-format apply-state))
    (setq-default mode-line-format apply-state)))

;;test code
;; (omm-change-mode-line omm-line-conf-list)
;; (omm-change-mode-line nil)

(defun omm-linum-toggle (num)
  (if (fboundp 'linum-mode)
	  (save-excursion
		(dolist (buf (buffer-list))
		  (set-buffer buf)
		  (linum-mode num))
        (if (equal num -1)
            (global-linum-mode -1)
          (global-linum-mode 1)))))

;;test code
;; (omm-linum-toggle -1)
;; (omm-linum-toggle 1)


(defun omm-style-on ()
  (interactive)
  (omm-linum-toggle -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (omm-change-mode-line nil)
  (if (fboundp 'elscreen-mode)
      (elscreen-mode -1))
  (if (fboundp 'tabbar-mode)
      (tabbar-mode -1))
  (setq omm-start-var t))

;;test code
;;(omm-style-on)

(defun omm-style-off ()
  (interactive)
  (if (eq (nth 0 omm-init-list-flag) t)
      (omm-linum-toggle 1))
  (if (eq (nth 1 omm-init-list-flag) t)
      (scroll-bar-mode 1))
  (if (eq (nth 2 omm-init-list-flag) t)
      (tool-bar-mode 1))
  (if (eq (nth 3 omm-init-list-flag) t)
      (menu-bar-mode 1))
  (omm-change-mode-line omm-line-conf-list)
  (if (and (eq (first (nth 5 omm-init-list-flag)) t)
           (eq (second (nth 5 omm-init-list-flag)) t))
      (elscreen-mode t))
  (if (and (eq (first (nth 6 omm-init-list-flag)) t)
           (eq (second (nth 6 omm-init-list-flag)) t))
      (tabbar-mode t))
  (setq omm-start-var nil))

;;test code
;;(omm-style-off)

(defun omm-style-off-debug ()
    (omm-linum-toggle 1)
    (scroll-bar-mode -1)
    (tool-bar-mode 1)
    (menu-bar-mode 1)
    (omm-change-mode-line omm-line-conf-list)
    (if (fboundp 'elscreen-mode)
        (elscreen-mode -1))
    (if (fboundp 'tabbar-mode)
        (tabbar-mode 1))
    (setq omm-start-var nil))

;;test code
;; (omm-style-off-debug)

(defun omm-toggle ()
  (interactive)
  (if (eq omm-start-var nil)
	  (progn 
        (omm-style-on))
		
      (omm-style-off)))

;;test code
;;(omm-toggle)

(defun omm-start ()
  (interactive)
  (omm-style-on)
  (omm-fullscreen)
  (omm-margins-toggle t)
  )

;;test code 
;;(omm-start)

(defun omm-stop ()
  (interactive)
  (omm-style-off)
  (omm-non-fullscreen)
  (omm-margins-toggle nil)
  )

;;test code 
;;(omm-stop)

(defun omm-minor-mode-start ()
  (omm-style-on)
  (set-keymap-parent omm-minor-mode-child-map
                     omm-mode-map)
  (omm-run-hook)
  (omm-fullscreen)
  (omm-margins-toggle t)
  ;; (add-hook 'find-file-hook
  ;;           (lambda ()
  ;;             (omm-change-mode-line nil))
  )

;;test code
;; (omm-minor-mode-start)

(defun omm-minor-mode-stop ()
  ;;  (remove-hook 'after-init-hook)
  ;; (lambda ()
  ;;   omm-change-mode-line nil))
  (omm-style-off)
  (omm-non-fullscreen)
  (omm-margins-toggle nil)
  )

;;test code
;;(omm-minor-mode-stop)

(defun omm-define-keymap ()
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") 'omm-toggle)
    (define-key map (kbd "<f11>") 'omm-toggle-fullscreen)
    map))

;;test code
;;(omm-define-keymap)

(defvar omm-mode-map
  (omm-define-keymap))

;;test code
;;omm-mode-map
;; (pop omm-mode-map)
(defvar omm-minor-mode-child-map (make-sparse-keymap))
(define-minor-mode omm-mode
  "Ommwriter like mode"
  :require 'omm
  :group 'omm
  :global t
  :init-value t
  :keymap omm-mode-map
  (if omm-mode
      (omm-minor-mode-start)
    (omm-minor-mode-stop)))

(defun omm-run-hook ()
  (run-hooks 'omm-hook))


(provide 'omm-mode)

;;; filename ends here

