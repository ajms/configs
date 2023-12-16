;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install required packages
(dolist (package '(elpy auctex haskell-mode company))
  (unless (package-installed-p package)
    (package-install package)))

;; Company mode configuration
(add-hook 'after-init-hook 'global-company-mode)

;; Optional: configure company mode further if desired
;; For example, set the delay before suggestions start to appear
(setq company-idle-delay 0.2)

;; Python configuration with elpy
(elpy-enable)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; LaTeX configuration with AUCTeX
;; Ensure that AUCTeX is loaded before setting its variables
(with-eval-after-load 'tex
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)

  ;; Use PDF tools to view PDFs in Emacs
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
)

;; Haskell mode configuration
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; Configure Emacs as a merge tool using ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq-default ediff-highlight-all-diffs 'nil)
(setq ediff-diff-options "-w")

;; Optional: If you want Emacs to handle git merging
;; (global-set-key (kbd "C-c C-g") 'vc-resolve-conflicts)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(auctex elpy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
