;; 
;; .emacs for 23.1
;;
;; Naoto ISHIKAWA <toona@seesaa.jp>
;; 

;;
;; lib path
;;

(defvar system-load-path load-path)
(setq my-load-path '("~/.emacs.d/share/emacs/site-lisp"))

(setq load-path (append my-load-path system-load-path))

;;(setenv "RUBYLIB" (concat (getenv "RUBYLIB") ":" (expand-file-name "~/.emacs.d/lib/ruby/site_ruby")))
(setenv "PATH"    (concat (getenv "PATH") ":" (expand-file-name "~/.emacs.d/bin")))

;;
;; lang
;;

(set-language-environment "Japanese")
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq file-name-coding-system 'utf-8)

;;
;; appearance / behavior
;;
(setq auto-compression-mode t)
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)
(setq auto-save-mode nil)
(setq default-major-mode 'text-mode)
(setq fill-column 200)
(setq indent-tabs-mode nil) 
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq minibuffer-max-depth nil)
(setq next-line-add-newlines nil)
(setq ps-paper-type 'a4)
(setq sentence-end-double-space t)
(setq text-mode-hook 'turn-on-auto-fill)
(setq use-dialog-box nil)
(setq-default transient-mark-mode t)

(menu-bar-mode nil)
(show-paren-mode t) 

(if (= (user-uid) 0) (setq backup-inhibited t))

;;
;; mode
;;

(setq auto-mode-alist (append (list (cons "\\.pl$" 'cperl-mode)
				    (cons "\\.cgi$" 'cperl-mode)
				    (cons "\\.pm$" 'cperl-mode)
                                    (cons "\\.prl$" 'cperl-mode)
                                    (cons "\\.html$" 'yahtml-mode)
                                    (cons "\\.inc$" 'yahtml-mode)
                                    (cons "\\.js$" 'js2-mode)
                                    )
                              auto-mode-alist))


(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;
;; yahtml-mode
;;

(delete 'sgml-html-meta-auto-coding-function auto-coding-functions)
(autoload 'yahtml-mode "yahtml" "Yet Another HTML mode" t)
(add-hook 'yahtml-mode-hook
	  #'(lambda ()
	      (auto-fill-mode -1)))


;;
;; cperl-mode
;;

(setq cperl-indent-parens-as-block t)
(setq cperl-close-paren-offset -4)
(setq cperl-indent-level 4)
(setq cperl-label-offset -4)
(setq cperl-continued-statement-offset 4)
(add-hook 'cperl-mode-hook
	  '(lambda ()
             (require 'perl-debug)
;;             (perl-debug-set-coding-system)
	     (setq perl-debug-command-name "~/.emacs.d/perl-debug.sh")
             (define-key cperl-mode-map "\C-cc" 'perl-debug-lint)
	     (define-key cperl-mode-map "\C-cd" 'perl-debug)))

;;
;; c-mode
;;

(defun my-c-mode ()
  (interactive)
  (c-mode)
  
  ;; Basic indent is 4 spaces
  (setq c-basic-offset 4)
  
  ;; Continuation lines are indented 2 spaces
  (c-set-offset 'statement-cont 2 t)
  (c-set-offset 'arglist-cont 2 t)
  (c-set-offset 'arglist-cont-nonempty 2 t)
  
  ;; Labels are flush to the left
  (c-set-offset 'label -1000 t))


;;
;; js2-mode
;;

(autoload 'js2-mode "js2-mode" nil t)

;;
;; sdic
;;
;; [toona@toona ~/tmp/sdic-2.1.3 ] ./configure --prefix ~/.emacs.d && make install && make dict && make install-dict
;; dict は prefix指定できないっぽいので local/share/dict のものをコピー。また、gene についてはインストーラがおかしいので 
;; 展開した gene.txt を euc にして cat gene.txt| /usr/bin/perl contrib/gene.perl > gene.sdic 
;;
(global-set-key "\C-cw" 'sdic-describe-word)
(setq sdic-window-height 10 sdic-disable-select-window t)
(setq sdic-eiwa-dictionary-list '((sdicf-client "~/.emacs.d/share/dict/gene.sdic"))
      sdic-waei-dictionary-list '((sdicf-client "~/.emacs.d/share/dict/jedict.sdic"
						(add-keys-to-headword t))))
(load-library "sdic")

;;
;; migemo
;; 
;; sudo rpm -Uvh ftp://fr.rpmfind.net/linux/fedora/linux/releases/19/Everything/x86_64/os/Packages/c/cmigemo-1.3-0.10.date20110227.fc19.1.x86_64.rpm
;;

(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(if (file-exists-p "/usr/share/cmigemo/utf-8/migemo-dict")
    (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict"))
(if (file-exists-p "/usr/local/share/migemo/utf-8/migemo-dict")
    (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"))
(migemo-init)

;;
;; svn
;;

(load-library "psvn")

;;
;; git
;;

(load-library "magit")

;; ----------------------------------------------------------------------
;; if file bigin "#!" +x - Script with shebang line
;; ----------------------------------------------------------------------
(add-hook 'after-save-hook
          '(lambda ()
             (save-restriction
               (widen)
               (and (string= "#!" (buffer-substring 1 (min 3 (point-max))))
                    (let* ((name (buffer-file-name))
                           (mode (file-modes name)))
                      (if (not (string-match "/\\.[^/]+$" name))
                          (progn
                            (set-file-modes name
                                            (logior mode (logand (/ mode 4) 73))
					    )
                            (message (concat "Wrote " name " (+x)"))))
                      )))))


;;
;; for osx
;;
(if (eq system-type 'darwin)
    (keyboard-translate ?\C-h ?\C-?)
    (setq ns-command-modifier (quote meta))
    (setq ns-alternate-modifier (quote super)))

;;
;; die, when this file have wrong.
;;
(put 'eval-expression 'disabled nil)
