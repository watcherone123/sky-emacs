;;; init-font.el --- font initialization. -*- lexical-binding: t -*-

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

(if (eq system-type 'darwin)
    (defvar sky-font-size 140)
  (defvar sky-font-size 120))
(defvar sky-fixed-ch-en-font "Sarasa Mono SC Nerd")
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(when (display-graphic-p)
  ;; Set default font
  (cl-loop for font in '(
                         ;; "InconsolataGo QiHei NF"
                         ;; "yaheiInconsolata"
                         ;; "Maple Mono"
                         "JetBrainsMono Nerd Font"
                         "JetBrains Mono"
                         "Sarasa Mono SC Nerd"
                         "Monaco"
                         "Consolas")
           when (font-installed-p font)
           return (progn
                    (set-face-attribute 'default nil :family font :height sky-font-size)
                    (set-face-attribute 'fixed-pitch nil :family font :height 1.0)))

  ;; variable-pitch
  (cl-loop for font in '("Noto Sans CJK SC" "Arial" "Helvetica" "Times New Roman")
           when (font-installed-p font)
           return (set-face-attribute 'variable-pitch nil :family font :height 1.0))

  ;; Specify font for all unicode characters
  ;; (cl-loop for font in '("Symbola" "Symbol")
  ;;          when (font-installed-p font)
  ;;          return(set-fontset-font t 'unicode font nil 'prepend))

  ;; ;; Specify font for Chinese characters
  (cl-loop for font in '("Sarasa Mono SC Nerd" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff) font))
  )

(setup mixed-pitch
  ;; (:hook-into text-mode)
  (:when-loaded
    (delete 'org-table mixed-pitch-fixed-pitch-faces)))

;; 中英文严格等宽字体设置
(with-eval-after-load 'org
  (dolist (fixed-chinese-english-face '(org-table))
    (set-face-attribute fixed-chinese-english-face nil :family "Sarasa Mono SC Nerd" :height 1.0)))


(provide 'init-font)