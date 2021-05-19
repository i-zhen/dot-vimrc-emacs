;; _____________________________________________________________________________
;; Aquamacs custom-file warning:
;; Warning: After loading this .emacs file, Aquamacs will also load
;; customizations from `custom-file' (customizations.el). Any settings there
;; will override those made here.
;; Consider moving your startup settings to the Preferences.el file, which
;; is loaded after `custom-file':
;; ~/Library/Preferences/Aquamacs Emacs/Preferences
;; _____________________________________________________________________________

;; stack install happy apply-refact hlint stylish-haskell hasktags hoogle
;; for SSL issue: https://github.com/davidswelt/aquamacs-emacs/issues/133

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
   '("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/") t))

(package-initialize)
;; (package-refresh-contents)

(package-install 'doom-themes)
(load-theme 'doom-nord-light t)

;;; neotree begin
(package-install 'use-package)
(package-install 'all-the-icons)
(require 'all-the-icons)
;; run `M-x all-the-icons-install-fonts` at first time while installed the icons

(use-package neotree
  :bind (("C-x C-n" . neotree-toggle))
  :ensure t
  :commands (neotree-toggle)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))
;;; neotree end

(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 106) ; chars
              (height . 40) ; lines
              (left . 50)
              (top . 50)))
      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 106)
              (height . 40)
              (left . 50)
              (top . 50))))
  (progn
    (setq initial-frame-alist '( (tool-bar-lines . 1)))
    (setq default-frame-alist '( (tool-bar-lines . 1)))))

(set-scroll-bar-mode nil)

;; M-x speedbar
(require 'speedbar)
(speedbar-add-supported-extension ".hs")

;; Prerequisite - begin

(defun hlint-refactor-call-process-region-checked (start end program &optional args)
  "Send text from START to END to PROGRAM with ARGS.
This is a wrapper around `call-process-region' that doesn't replace
the region with the output of PROGRAM if it returned a non-zero
exit code."
  (let ((exit (apply 'call-process-region
                     start end
                     program            ; name of program
                     t                  ; delete region
                     t                  ; send output to buffer
                     nil                ; no redisplay during output
                     args
                     )))
    (unless (eq exit 0) (primitive-undo 1 buffer-undo-list))))


(defun hlint-refactor-call-process-region-preserve-point (start end program &optional args)
  "Send text from START to END to PROGRAM with ARGS preserving the point.
This uses `call-process-region-checked' internally."
  (let ((line (line-number-at-pos))
        (column (current-column)))
    (hlint-refactor-call-process-region-checked start end program args)
    (goto-line line)
    (move-to-column column)))

;;;###autoload
(defun hlint-refactor-refactor-buffer (&optional args)
  "Apply all hlint suggestions in the current buffer.
ARGS specifies additional arguments that are passed to hlint."
  (interactive)
  (hlint-refactor-call-process-region-preserve-point
   (point-min)
   (point-max)
   "hlint"
   (append '("--refactor"
             "-")
           args)))

;;;###autoload
(defun hlint-refactor-refactor-at-point ()
  "Apply the hlint suggestion at point."
  (interactive)
  (let ((col (number-to-string (+ 1 (current-column))))
        (line (number-to-string (line-number-at-pos))))
    (hlint-refactor-refactor-buffer
     (list (concat "--refactor-options=--pos " line "," col)))))

;;;###autoload
(define-minor-mode hlint-refactor-mode
  "Automatically apply hlint suggestions"
  :lighter " hlint-refactor"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map "\C-cb" 'hlint-refactor-refactor-buffer)
            (define-key map "\C-cr" 'hlint-refactor-refactor-at-point)
            map))

(provide 'hlint-refactor)

;;Pre - end

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
(package-install 'flycheck-color-mode-line)
(package-install 'flycheck-pos-tip)
(package-install 'dante)

;; COMPLETION

;; (add-hook 'after-init-hook 'global-company-mode)
;shortcut for completion
(global-set-key (kbd "C-c w") 'company-complete)

;after how many letters do we want to get completion tips? 1 means from the first letter
(setq company-minimum-prefix-length 1)
(setq company-dabbrev-downcase 0)
;after how long of no keys should we get the completion tips? in seconds
(setq company-idle-delay 0.1)

;; ERRORS ON THE FLY

(require 'flycheck)
;; (add-hook 'after-init-hook 'global-flycheck-mode)
(require 'flycheck-color-mode-line)

;tooltip errors
(require 'flycheck-pos-tip)
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

(setq flycheck-pos-tip-timeout 60)

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(require 'flycheck-color-mode-line)
(add-hook 'flycheck-mode-hook
  'flycheck-color-mode-line-mode)

(global-set-key [f9] 'flycheck-list-errors)


(package-install 'flymake-hlint)
(require 'flymake-hlint)

(defun my-haskell-hook ()
  (progn
    (hlint-refactor-mode)
    (interactive-haskell-mode)
    (haskell-doc-mode)
    (haskell-indentation-mode)
	(dante-mode)
	(flycheck-mode)
	(flymake-hlint-load)
	(company-mode)
))

(add-hook 'haskell-mode-hook 'my-haskell-hook)
(setq dante-repl-command-line '("stack" "repl" dante-target))
;; Put following line in your project: .dir-locals.el
;; ((nil . ((dante-methods . (stack)))))

;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
;; (add-hook 'dante-mode-hook
;;   '(lambda () (flycheck-add-next-checker 'haskell-dante
;;                '(warning . haskell-hlint))))


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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-stylish-on-save t)
 '(package-selected-packages
   (quote
    (auto-complete merlin utop tuareg hasky-stack exec-path-from-shell haskell-mode flycheck company))))

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; install OCaml support
(package-install 'tuareg)
(package-install 'utop)
(package-install 'merlin)
(package-install 'auto-complete)

(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(setq merlin-use-auto-complete-mode t)
(setq merlin-error-after-save nil)

;; -- merlin setup ---------------------------------------

(setq opam-share (substring (shell-command-to-string "opam config var share") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
(require 'merlin)

;; Enable Merlin for ML buffers
(add-hook 'tuareg-mode-hook 'merlin-mode)

;; So you can do it on a mac, where `C-<up>` and `C-<down>` are used
;; by spaces.
(define-key merlin-mode-map
  (kbd "C-c <left>") 'merlin-type-enclosing-go-up)
(define-key merlin-mode-map
  (kbd "C-c <right>") 'merlin-type-enclosing-go-down)
(set-face-background 'merlin-type-face "#88FF44")

;; -- enable auto-complete -------------------------------
;; Not required, but useful along with merlin-mode
(require 'auto-complete)
(add-hook 'tuareg-mode-hook 'auto-complete-mode)
(setq merlin-ac-setup 't)

;; Replace tuareg by ocp-indent
(setq opam-share (substring (shell-command-to-string "opam config var share") 0 -1))
(load-file (concat opam-share "/emacs/site-lisp/ocp-indent.el"))

;; Scheme

(package-install 'paredit)
(package-install 'geiser)

(require 'geiser)
(require 'paredit)

(setq scheme-program-name "chez")
(setq geiser-chez-binary "chez")
(setq geiser-active-implementations '(chez))
;; (setq geiser-repl-use-other-window '(nil))

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)

;; gem install taskjuggler
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; M-x package-refresh-contents
;; M-x package-install
;; org-plus-contrib
;; (require 'org-plus-contrib)

;; you can find follow setting in ~/.emacs.d/elpa/org-plus-contrib-YYMMDD/ox-taskjuggler.el

(setq org-taskjuggler-default-reports
  '("textreport report \"Plan\" {
  formats html
  header '== %title =='
  center -8<-
    [#Plan Plan] | [#Resource_Allocation Resource Allocation]
    ----
    === Plan ===
    <[report id=\"plan\"]>
    ----
    === Resource Allocation ===
    <[report id=\"resourceGraph\"]>
  ->8-
}
# A traditional Gantt chart with a project overview.
taskreport plan \"\" {
  headline \"Project Plan\"
  columns bsi, name {width 350}, start, end, effort, chart {scale week width 800}
  loadunit shortauto
  hideresource 1
}
# A graph showing resource allocation. It identifies whether each
# resource is under- or over-allocated for.
resourcereport resourceGraph \"\" {
  headline \"Resource Allocation Graph\"
  columns no, name, effort, annualleave,duties, complete, weekly {width 700}
  loadunit shortauto
  hidetask ~(isleaf() & isleaf_())
  sorttasks plan.start.up
}")
)

(setq org-taskjuggler-default-global-properties
"
shift s40 \"Part time shift\" {
  workinghours wed, thu, fri off
}

leaves holiday \"National Day\" 2021-10-01 +5d,
       holiday \"Dragon Boat Festival\" 2021-06-12 +3d,
       holiday \"Mid-Autumn Festival\" 2021-09-19 +2d

")

(setenv "LC_ALL" "zh_CN.UTF-8")
(setenv "LANG" "zh_CN.UTF-8")
(setenv "LANGUAGE" "zh_CN.UTF-8")
(setenv "LC_COLLATE" "zh_CN.UTF-8")
(setenv "LC_CTYPE" "zh_CN.UTF-8")
(setenv "LC_MESSAGES" "zh_CN.UTF-8")
(setenv "LC_MONETARY" "zh_CN.UTF-8")
(setenv "LC_NUMERIC" "zh_CN.UTF-8")
(setenv "LC_TIME" "zh_CN.UTF-8")
