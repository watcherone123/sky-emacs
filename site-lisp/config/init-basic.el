;;; init-basic.el --- basic initialization. -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 watcherone123

;; Author: watcherone123 <watcherone123@gmail.com>
;; URL: https://github.com/watcherone123/.emacs.d

;; This file is not part of GNU Emacs.
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

  ;; basic modes
  (setq use-short-answers t)
  (setq confirm-kill-emacs 'y-or-n-p)
  (setq ring-bell-function 'ignore)       ;关闭烦人的出错时的提示声
  (set-default-coding-systems 'utf-8)
  (setq-default indent-tabs-mode nil)

  (setq-default tab-width 4)
  ;; 平滑地进行半屏滚动，避免滚动后recenter操作
  (setq scroll-step 1
        scroll-conservatively 10000)
  (setq completions-detailed t) ;;useful in emacs 28
  (setq use-dialog-box nil)               ;never pop dialog
  (setq mouse-yank-at-point t)   

(add-to-list 'default-frame-alist '(fullscreen . maximized))

  (provide 'init-basic)