;; stack install happy intero apply-refact hlint stylish-haskell hasktags hoogle

;; for SSL issue: https://github.com/davidswelt/aquamacs-emacs/issues/133
;; $ brew install libressl
;; $ echo $(brew --prefix)/etc/libressl/cert.pem
;; /usr/local/etc/libressl/cert.pem

(set-frame-size (selected-frame) 140 55)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/"))
(setq exec-path (append exec-path '("/usr/local/bin/")))

(with-eval-after-load 'tls
    (push "/usr/local/etc/libressl/cert.pem" gnutls-trustfiles))

(global-linum-mode t)

(require 'cl)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/") t))

(package-initialize)
(package-refresh-contents)

;; Install Intero
(package-install 'intero)
;;(add-hook 'haskell-mode-hook 'intero-mode)

(package-install 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(defvar prelude-packages
  '(haskell-mode)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(if (version<= "24.0" emacs-version)
    (unless (prelude-packages-installed-p)
      ;; check for new packages (package versions)
      (message "%s" "Emacs Prelude is now refreshing its package database...")
      (package-refresh-contents)
      (message "%s" " done.")
      ;; install the missing packages
      (dolist (p prelude-packages)
        (when (not (package-installed-p p))
          (package-install p)))))

;; Haskell
(defun my-haskell-hook ()
  (progn
    (interactive-haskell-mode)
    (intero-mode)
    (haskell-doc-mode)
    (haskell-indentation-mode)
))

(add-hook 'haskell-mode-hook 'my-haskell-hook)

(intero-global-mode 1)

(with-eval-after-load 'intero
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint))
)

;; shortcut to set intero targets: Alt-s Alt-t 
(global-set-key (kbd "M-s M-t") #'intero-targets)

;; install stack mode with shortcut: Alt-s Alt-s
(package-install 'hasky-stack)
(global-set-key (kbd "M-s M-s") #'hasky-stack-execute)

;; use Shift-arrow keys to move between windows
(windmove-default-keybindings)

(require 'haskell-mode)
(define-key haskell-mode-map "\C-ch" 'haskell-hoogle)
;(setq haskell-hoogle-command "hoogle")

;; use M-. on a name in a Haskell buffer which will jump directly to its definition
(setq haskell-tags-on-save t)

;; M-x speedbar
(require 'speedbar)
(speedbar-add-supported-extension ".hs")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-stylish-on-save t)
 '(package-selected-packages
   (quote
    (hasky-stack exec-path-from-shell intero haskell-mode flycheck company))))

(require 'linum)

(defvar linum-current-line 1 "Current line number.")
(defvar linum-border-width 1 "Border width for linum.")

(defface linum-current-line
  `((t :inherit linum
       :foreground "goldenrod"
       :weight bold
       ))
  "Face for displaying the current line number."
  :group 'linum)

(defadvice linum-update (before advice-linum-update activate)
  "Set the current line."
  (setq linum-current-line (line-number-at-pos)
        ;; It's the same algorithm that linum dynamic. I only had added one
        ;; space in front of the first digit.
        linum-border-width (number-to-string
                            (+ 1 (length
                                  (number-to-string
                                   (count-lines (point-min) (point-max))))))))

(defun linum-highlight-current-line (line-number)
  "Highlight the current line number using `linum-current-line' face."
  (let ((face (if (= line-number linum-current-line)
                  'linum-current-line
                'linum)))
    (propertize (format (concat "%" linum-border-width "d") line-number)
                'face face)))

(setq linum-format 'linum-highlight-current-line)
