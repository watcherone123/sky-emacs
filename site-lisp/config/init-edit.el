;;; init-edit.el --- edit initialization. -*- lexical-binding: t -*-

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

(setup elec-pair
  (:defer
   (electric-pair-mode)))

(setup rainbow-delimiters
  (:hook-into prog-mode))

;; (setup diff-hl
;;   (:hook-into text-mode prog-mode)
;;   (:when-loaded
;;     (diff-hl-margin-mode)))

(setup hl-todo
  (:defer
   (global-hl-todo-mode))
  )

(setup paren
  (:option show-paren-context-when-offscreen 'overlay))

(setq fill-column 100)          ;默认显示 100列就换行
(setq word-wrap t)
(setq word-wrap-by-category t)
;; (add-hook 'org-mode-hook 'turn-on-auto-fill)

(setup so-long
  (:defer
   (global-so-long-mode 1)
   ))

(setup ws-butler
  (:hook-into text-mode prog-mode))

(setup exec-path-from-shell
  (:defer
   (when (memq window-system '(mac ns x))
     (exec-path-from-shell-initialize))))

(setup vundo
  (:bind "l" vundo-forward
         "h" vundo-backward
         "j" vundo-next
         "k" vundo-previous))
           
(provide 'init-edit)