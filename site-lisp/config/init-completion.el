;;; init-completion.el --- init-completion initialization. -*- lexical-binding: t -*-

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
(setup vertico
  (:option vertico-cycle t)
  (:with-map vertico-map
    (:bind [backspace] vertico-directory-delete-char))
  (:defer
   (vertico-mode)
  ))

(setup orderless
  (:option completion-styles '(orderless)
           completion-category-defaults nil
           completion-category-overrides '((file (styles . (partial-completion))))))

(setup consult
  (:option register-preview-delay 0.1
           register-preview-function #'consult-register-format
           xref-show-xrefs-function #'consult-xref
           xref-show-definitions-function #'consult-xref
           consult-project-root-function (lambda ()
                                           (when-let (project (project-current))
                                             (car (project-roots project)))))
  (:with-map minibuffer-local-map
    (:bind "C-r" consult-history)))

(defcustom my-consult-ripgrep-or-line-limit 1000
  "Buffer size threshold for `my-consult-ripgrep-or-line'.
When the number of characters in a buffer exceeds this threshold,
`consult-ripgrep' will be used instead of `consult-line'."
  :type 'integer)

  (defun consult-ripgrep-one-file ()
  "Call `consult-ripgrep' for the current buffer (a single file)."
  (interactive)
  (let ((consult-project-root-function (lambda nil nil))
        (consult-ripgrep-args
         (concat "rg "
                 "--null "
                 "--line-buffered "
                 "--color=never "
                 "--line-number "
                 "--smart-case "
                 "--no-heading "
                 "--max-columns=1000 "
                 "--max-columns-preview "
                 "--search-zip "
                 "--with-filename "
                 (shell-quote-argument buffer-file-name))))
    (consult-ripgrep)))

(defun sky/ripgrep-search-other-dir()
  (interactive)
  (let ((current-prefix-arg '(-1)))
    (call-interactively 'consult-ripgrep)))

(defun sky/find-file-other-dir()
  (interactive)
  (let ((current-prefix-arg '(-1)))
    (call-interactively 'consult-find)))


(setup marginalia
  (:option marginalia-annotators '(marginalia-annotators-heavy
                                   marginalia-annotators-light
                                   nil))
  (:when-loaded
    (cl-pushnew 'epkg-marginalia-annotate-package
                (alist-get 'package marginalia-annotator-registry)))
  (:hook-into after-init))

(setup embark-consult
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(setup embark
  (:also-load embark-consult)
  (setq prefix-help-command 'embark-prefix-help-command)
  (:global "C-S-a" embark-act))

(setup wgrep)

(setup yasnippet
  (:option yas-snippet-dirs (list (concat sky-emacs-root-dir "snippets")))
  (:defer (yas-global-mode))
 )

(provide 'init-completion)