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
;;


;;; Commentary:
;;
;; Tested on Emacs 23.
;;

;;; Commands:
;;
;; `omm-toggle-layout'
;; toggle omm-layout on/off
;;
;; `omm-layout-on'
;; bunish tool-bar, menu-bar, mode-line etc
;;
;; `omm-layout-off'
;; omm-layout off
;; undo initial status

;;; Keybinds
;;
;; C-c C-o: omm-toggle-layout

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

(eval-when-compile (require 'cl))

;; (require 'frame-local-vars)

;;fringe section
;;margin section
(defcustom omm-left-margin 25
  "Margin to add to the left side of the screen,
depends on your resolution and prefered column width"
  :type 'integer
  :group 'omm)

(defcustom omm-right-margin 25
  "Margin to add to the right side of the screen,
 depends on your resolution and prefered column width"
  :type 'integer
  :group 'omm)

(defcustom omm-enable-multi-monitor-support t
  "Whether to enable multi-frame i.e multiple monitor
 support. An option since this feature is experimental"
  :type 'boolean
  :group 'omm)

(defcustom omm-enable-longline-wrap t
  "If longlines-mode is enabled, should
longlines-wrap-follows-window-size also be enabled when
 going into omm mode?"
  :type 'boolean
  :group 'omm)

(defvar omm-run-unix
  (or (equal system-type 'gnu/linux)
      (equal system-type 'usg-unix-v)
      (equal system-type 'berkeley-unix)
      ))

(defvar omm-run-windows
  (or (equal system-type 'windows-nt)
      (equal system-type 'ms-dos)
      ))
(defvar omm-run-darwin (equal system-type 'darwin))


(defun omm-update-window ()
  (set-window-margins (selected-window)
                      left-margin-width
                      right-margin-width))

;;test code
;;(omm-update-window  omm-left-margin  omm-right-margin)
;;(omm-update-window 0 0)

(defvar omm-mode-path ""
  "For testing omm-mode file path")

;; test-code
;; (setq omm-mode-path (concat user-emacs-directory "plugins/omm-mode"))


(defvar *omm-memtable* (make-hash-table))

(defun* omm-recall (key &optional (frame (selected-frame)))
  (cdr (assoc key (gethash frame *omm-memtable*))))

;;test code
;;((assert (progn (omm-remember 'test (frame-parameter nil 'background-color)) (omm-recall 'test)))

(defun* omm-remember (key val &optional (frame (selected-frame)))
  (let* ((kvlist (gethash frame *omm-memtable*))
         (target (assoc key kvlist)))
    (cond (target
           (setf (cdr target) val))
          (t
           (puthash frame (cons (cons key val)
                                kvlist) *omm-memtable*)))))

;; test code
;; (omm-remember 'test (frame-parameter nil 'background-color))

(defun omm-remember-frame-size ()
  (omm-remember 'frame-width  (frame-parameter nil 'width))
  (omm-remember 'frame-height (frame-parameter nil 'height))
  (omm-remember 'frame-left   (frame-parameter nil 'left))
  (omm-remember 'frame-top    (frame-parameter nil 'top))
  )

;; test-code
;; (omm-remember-frame-size)
(defun omm-recall-frame-size ()
  (interactive)
  (modify-frame-parameters (selected-window)
                           `((left . ,(omm-recall 'frame-left))
                             (top . ,(omm-recall 'frame-top))
                             (width . ,(omm-recall 'frame-width))
                             (height . ,(omm-recall 'frame-height)))))
;; test-code
;; (omm-recall-frame-size)

(defvar omm-layout-margin-state t)

(defun omm-remember-margins ()
  (omm-remember 'left-margin-width left-margin-width)
  (omm-remember 'right-margin-width right-margin-width)
  ;; (cond
  ;;  (omm-enable-multi-monitor-support
  ;;   (setq-default left-margin-width omm-left-margin)
  ;;   (setq-default right-margin-width omm-right-margin))
  ;;  (frame-local-variables-check t)
  ;;  (t
  (setq-default left-margin-width omm-left-margin)
  (setq-default right-margin-width omm-right-margin)
  ;; ))
  (omm-update-window)
  (setq omm-layout-margin-state nil)
  )

;;test code
;;(omm-remember-margins)

(defun omm-recall-margins ()
  ;; (cond
  ;;  (omm-enable-multi-monitor-support
  ;;   (unset-frame-default 'left-margin-width)
  ;;   (unset-frame-default 'right-margin-width)
  ;;   (frame-local-variables-check t))
  ;;  (t
  (setq-default left-margin-width
                (omm-recall 'left-margin-width))
  (setq-default right-margin-width
                (omm-recall 'right-margin-width))
  ;; ))
  (omm-update-window)
  (setq omm-layout-margin-state t)
  )

;;test code
;; (omm-recall-margins)

(defun omm-margins-toggle ()
  (interactive)
  (let* ((state omm-layout-margin-state))
    ;; (if (boundp 'omm-arg-margins-state)
    ;;     (progn
    ;;       omm-arg-margins-state)

    ;;   (progn
    ;;     (assert (boundp 'omm-arg-margins-state) nil)
    ;;     (assert (fboundp 'omm-arg-margins-state) nil)
    ;;     omm-layout-margin-state)
    ;; )))
	;;(assert state t)
	(if state
		  (omm-remember-margins)
		(omm-recall-margins))))

;;test code
;;(omm-margins-toggle)
;;(omm-remember-margins)
;;(omm-recall-margins)
;; left-margin-width
;; right-margin-width

;;---- fullscreen function section
(defvar omm-fullscreen-p t
  "Check if fullscreen is on or off")

(defvar omm-w32toggletitle-exe-path nil
  "To concat `omm-w32fullscreen-toggletitle-cmd' variable")

(defvar omm-w32-fullscreen-toggletitle-cmd
  (let ((str (buffer-file-name (current-buffer))))
    (concat (substring str 0 (string-match "omm-mode.el" str))
            "w32toggletitle.exe"))
  ;; "c:/Users/ki3_user_RO/gnupack_devel-6.00/home/.emacs.d/plugins/omm-mode/w32toggletitle.exe"
  "Path to w32toggletitle.exe command")
;; test-code
;;(assert (string= (expand-file-name (concat omm-mode-path "/w32toggletitle.exe")) (setq omm-w32-fullscreen-toggletitle-cmd  (let ((str (buffer-file-name (current-buffer)))) (concat (substring str 0 (string-match "omm-mode.el" str)) "w32toggletitle.exe")))))

(defun omm-w32-fullscreen-toggle-titlebar ()
  "Toggle display of the titlebar of frame (windows only)"
  (interactive)
  (call-process omm-w32-fullscreen-toggletitle-cmd
		  nil nil nil
		  (frame-parameter (selected-frame) 'window-id))
  (sleep-for 0.2))

(defun omm-w32-fullscreen-on ()
  "Toggle fullscreen display of current frame (windows only)"
  (omm-w32-fullscreen-toggle-titlebar)
  (w32-send-sys-command 61488))


;; test-code
;; (omm-w32-fullscreen-on)
(defun omm-w32-fullscreen-off ()
  (w32-send-sys-command 61728)
  (omm-w32-fullscreen-toggle-titlebar))

;; test-code
;; (omm-w32-fullscreen-off)
(defun omm-non-fullscreen ()
  (interactive)
  (cond ((fboundp 'w32-send-sys-command)
         ;; WM_SYSCOMMAND restore #xf120
         (omm-w32-fullscreen-off))
        ((equal omm-run-darwin t)
         (set-frame-parameter  nil  'fullscreen  nil))
        (t
         ;;(equal omm-run-unix t)
         (set-frame-parameter  nil  'fullscreen  nil)))
  (omm-fullscreen-state-toggle nil))

;;test code
;;(omm-non-fullscreen)

(defun omm-fullscreen ()
  (interactive)
  (cond ((fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND maximaze #xf030
         (omm-w32-fullscreen-on))
        ((equal omm-run-darwin t)
         (set-frame-parameter nil 'fullscreen 'fullboth))
        (t
         ;;(equal omm-run-unix t)
         (set-frame-parameter  nil  'fullscreen  'fullboth)))
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


;;---- other setting

(defvar omm-enable-longline-wrap t
  "If longlines-mode is enabled, should longlines-wrap-follows-window-size
 also be enabled when going into omm mode?")

(defun omm-remember-longline ()
  (omm-remember 'line-spacing (frame-parameter nil 'line-spacing))
  (when (and  (boundp 'longlines-mode)
              longlines-mode
              omm-enable-longline-wrap)
    (omm-remember 'longlines-wrap-follow
                       longlines-wrap-follows-window-size))
  ;; - set
  (modify-frame-parameters (selected-frame)
                           '((line-spacing . 1)))
  (when (and
         (boundp 'longlines-mode)
         longlines-mode
         omm-enable-longline-wrap)
    (longlines-mode 0)
    (setq longlines-wrap-follows-window-size t)
    (longlines-mode 1)))

(defun omm-recall-longline ()
  (modify-frame-parameters
   (selected-frame)
   `((line-spacing . ,(omm-recall 'line-spacing))))
  (when (and
         (boundp 'longlines-mode)
         longlines-mode
         (omm-recall 'longlines-wrap-follow)
         omm-enable-longline-wrap)
    (longlines-mode 0)
    (setq longlines-wrap-follows-window-size
          (omm-recall 'longlines-wrap-follow))
    (longlines-mode 1))
  )

;;---- layout section
(defvar omm-line-conf-list mode-line-format
  "Save mode-line-format")
;;omm-line-conf-list

(defvar omm-start-var nil
  "If omm-start-var variable is nil, omm-mode is off.
If you eval omm-toggle-layout function, omm-mode is on.
If this variable is t, omm-mode is on.
If you eval omm-toggle-layout, omm-start-var change nil")

(defconst omm-init-list-flag
  (list
   (if (boundp 'linum-mode)
       (eq linum-mode t)
     nil)
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


(defun omm-layout-on ()
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
;;(omm-layout-on)

(defun omm-layout-off ()
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
;;(omm-layout-off)

(dont-compile
  (defun omm-layout-off-debug ()
    (omm-linum-toggle 1)
    (scroll-bar-mode -1)
    (tool-bar-mode 1)
    (menu-bar-mode 1)
    (omm-change-mode-line omm-line-conf-list)
    (if (fboundp 'elscreen-mode)
        (elscreen-mode -1))
    (if (fboundp 'tabbar-mode)
        (tabbar-mode 1))
    (setq omm-start-var nil)))

;; test code
;; (omm-layout-off-debug)

(defun omm-toggle-layout ()
  (interactive)
  (if (eq omm-start-var nil)
        (omm-layout-on)
      (omm-layout-off)))

;;test code
;;(omm-toggle-layout)

(defun omm-start ()
  (interactive)
  (omm-remember-margins)
  (omm-layout-on)
  (omm-fullscreen)
  )

;;test code
;;(omm-start)

(defun omm-stop ()
  (interactive)
  (omm-layout-off)
  (omm-non-fullscreen)
  (omm-recall-margins)
  )

;;test code
;;(omm-stop)

(defun omm-minor-mode-start ()
  (omm-layout-on)
  (set-keymap-parent omm-minor-mode-child-map
                     omm-mode-map)
  (omm-run-hook)
  (omm-remember-frame-size)
  (omm-fullscreen)
  (omm-remember-margins)
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
  (omm-layout-off)
  (omm-non-fullscreen)
  (omm-recall-margins)
  (omm-remember-frame-size)
  )

;;test code
;;(omm-minor-mode-stop)

(defun omm-define-keymap ()
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") 'omm-toggle-layout)
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
    (omm-minor-mode-stop)
    ))

;; test-code
;; (omm-mode)
;; (omm-mode 1)
;; (omm-mode -1)

(defun omm-run-hook ()
  (run-hooks 'omm-mode-hook))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
     (expect t
             (omm-mode 1))
     (expect 25
             left-margin-width)
     (expect 25
             right-margin-width)
     (expect nil
             (omm-mode -1))
     (expect 0
             left-margin-width)
     (expect 0
             right-margin-width)))
  )


(provide 'omm-mode)

;;; filename ends here
