;;; init-gcmh.el --- the Garbage Collector Magic Hack configuration. -*- lexical-binding: t -*-

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

;;; Commentary:
;;
;; the Garbage Collector Magic Hack configuration
;;

;;; Code:
(setup gcmh
  (:option gcmh-idle-delay 'auto
           gcmh-auto-idle-delay-factor 10
           gcmh-high-cons-threshold (* 16 1024 1024))
  (gcmh-mode t))


(defun sky-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun sky-restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold (* 16 1024 1024)))))

(add-hook 'minibuffer-setup-hook #'sky-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'sky-restore-garbage-collection-h)

(provide 'init-gcmh)