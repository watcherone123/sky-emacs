(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
(require 'borg)
(setq borg-rewrite-urls-alist
      '(("git@github.com:" . "https://github.com/")
        ("git@gitlab.com:" . "https://gitlab.com/")))
(borg-initialize)