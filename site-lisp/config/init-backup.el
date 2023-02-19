;;; init-backup.el --- backup initialization. -*- lexical-binding: t -*-

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

(setq make-backup-files nil)
(setq auto-save-default nil)

(setup super-save
  (:option super-save-auto-save-when-idle t)
  (:defer
   (super-save-mode 1))
  )

(setup savehist
  (:option history-length 10000
           history-delete-duplicates t
           savehist-save-minibuffer-history t)
  (:defer
   (savehist-mode)))

;; clean emacs home files
(setup no-littering
    (require 'no-littering)
    )


(setup recentf
  (:also-load no-littering)
  (:option recentf-max-saved-items 1000
           recentf-exclude `("/tmp/" "/ssh:" ,(concat user-emacs-directory "lib/.*-autoloads\\.el\\'")))
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (:defer
   (recentf-mode)))

(provide 'init-backup)