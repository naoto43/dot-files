;;; -*- Emacs-Lisp -*-
;;; $Id: perl-debug.el,v 1.5 1999/01/20 10:15:11 tsuchiya Exp $

;;; Author: Tsuchiya Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;; Keywords: perl

;;; Commentary:

;; Perl スクリプトをデバッグしたり文法検査をするためのコマンドを定義し
;; たライブラリです。利用及び再配布の際は、GNU 一般公用許諾書の適当な
;; バージョンにしたがって下さい。

;; 一次配布元
;;    http://www-nagao.kuee.kyoto-u.ac.jp/member/tsuchiya/elisp/perl-debug.el

;;; Install:

;; 適当な場所に perl-debug.el をコピーしてから、次の指定を ~/.emacs
;; に付け加えてください。
;;
;;    (add-hook 'perl-mode-hook
;;              '(lambda ()
;;                 (require 'perl-debug)
;;                 (perl-debug-set-coding-system)
;;                 (define-key perl-mode-map "\C-cc" 'perl-debug-lint)
;;                 (define-key perl-mode-map "\C-cd" 'perl-debug)))
	     

;;; 依存関係の宣言
(provide 'perl-debug)
(require 'gud)

(eval-when-compile
  (require 'comint)
  (require 'shell))


;;; 変数 / 定数の宣言
(defvar perl-debug-coding-system (if (>= emacs-major-version 20) 'euc-japan-unix *euc-japan*unix)
  "*Perl スクリプトの漢字コード")

(defvar perl-debug-window-height 10
  "*結果を出力するためのウインドウの高さ")

(defvar perl-debug-command-name (if (boundp 'perldb-command-name) perldb-command-name "perl")
  "*Perl のファイル名")

(defvar perl-debug-lint-option "-wc"
  "*文法チェックを行なうために Perl に与えるオプション")

(defconst perl-debug-buffer-name "*perl-debug*" "結果を表示するバッファの名前")

(defvar perl-debug-history '() "perl-debug の入力の履歴")
(make-variable-buffer-local 'perl-debug-history)

(defvar perl-debug-complete-functions
  '(perl-debug-dynamic-complete-filename)
  "*Function list to complete arguments for perl-debug")


;;;----------------------------------------------------------------------
;;;		漢字コードを設定
;;;----------------------------------------------------------------------

(if (coding-system-p perl-debug-coding-system)
    (if (>= emacs-major-version 20)
	(setq process-coding-system-alist
	      (cons (cons (format ".*%s" perl-debug-command-name) perl-debug-coding-system)
		    (if (boundp 'process-coding-system-alist) process-coding-system-alist)))
      (define-program-coding-system nil (format ".*%s" perl-debug-command-name) perl-debug-coding-system)))


(defun perl-debug-set-coding-system () "\
Set coding system of current buffer for perl
カレントバッファの漢字コードを perl-debug-coding-system の値に設定する関数
"
  (if (coding-system-p perl-debug-coding-system)
      (cond
       ((boundp 'MULE)
	(set-file-coding-system perl-debug-coding-system))
       ((fboundp 'coding-system-equal)
	(let ((mp (buffer-modified-p))
	      (ud (memq buffer-file-coding-system
			'(undecided undecided-unix undecided-dos undecided-mac))))
	  (if (coding-system-equal perl-debug-coding-system buffer-file-coding-system)
	      nil ; if coding-system is the same, do nothing
	    (set-buffer-file-coding-system perl-debug-coding-system)
	    (if ud (set-buffer-modified-p mp)))))
       ((featurep 'mule)
	(set-file-coding-system perl-debug-coding-system)))
    (message "漢字コードの設定が間違っています。")))



;;;----------------------------------------------------------------------
;;;		Debugger
;;;----------------------------------------------------------------------

(defsubst perl-debug-get-command-name () "\
Function to get file name fo executing Perl.
ファイルの先頭に
	#!/usr/local/bin/jperl
などの記述があれば、そこで指定されている perl の名前を返す。
指定されていない場合は perl-debug-command-name の値を返す。
"
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (if (and (re-search-forward "^#!\\s-*\\(\\(\\(/[^/]+\\)*/\\)?j?perl\\)\\b"
				  (save-excursion (end-of-line) (point)) t)
	       (file-executable-p (match-string 1)))
	  (match-string 1)
	perl-debug-command-name))))


(defun perl-debug-dynamic-complete-filename () "\
Dynamically complete the filename at point."
  (interactive)
  (require 'comint)
  (require 'shell)
  (let ((p (point)))
    (skip-chars-backward shell-file-name-chars)
    (if (= ?~ (char-after (point)))
	(insert (prog1 (expand-file-name (buffer-substring (point) p))
		  (delete-region (point) p)))
      (goto-char p)))
  (comint-dynamic-complete-filename))


(defun perl-debug () "\
Debug Perl script in current buffer.
カレントバッファの Perl スクリプトをデバッグするためのコマンド
"
  (interactive)
  (and (buffer-modified-p)
       (y-or-n-p (concat "Save file " buffer-file-name "? "))
       (basic-save-buffer))
  (let ((keymap (copy-keymap minibuffer-local-map))
	(h perl-debug-history) s)
    (setq s (unwind-protect
		(progn
		  (define-key minibuffer-local-map "\t"
		    '(lambda ()
		       (interactive)
		       (run-hook-with-args-until-success 'perl-debug-complete-functions)))
		  (read-from-minibuffer (format "Input arguments [%s]: "
						(substring default-directory 0 -1))
					(car h) nil nil '(h . 1)))
	      (setq minibuffer-local-map keymap)))
    (setq perl-debug-history (if (string= s (car h)) h (cons s h)))
    (perldb (format "%s %s %s" (perl-debug-get-command-name) buffer-file-name s))))



;;;----------------------------------------------------------------------
;;;		Checker
;;;----------------------------------------------------------------------

(defun perl-debug-lint () "\
Check script in the current buffer.
カレントバッファの Perl スクリプトの文法的正確さを検査するコマンド
"
  (interactive)
  (and (buffer-modified-p)
       (y-or-n-p (concat "Save file " buffer-file-name "? "))
       (basic-save-buffer))
  (let ((file buffer-file-name)
	(perl (perl-debug-get-command-name)))
    (set-buffer (get-buffer-create perl-debug-buffer-name))
    (setq buffer-read-only nil)
    (erase-buffer)
    (call-process perl nil t nil perl-debug-lint-option file)
    (setq buffer-read-only t)
    (set-buffer-modified-p nil))
  (let ((w (get-buffer-window perl-debug-buffer-name)))
    (if (= 1 (count-lines (point-min) (point-max)))
	;; 文法チェックの結果が1行のみの場合は、minibuffer への出力のみに留め、
	;; もし既に window が表示されている場合はそれを削除する
	(progn
	  (if w (if (= (window-height w) perl-debug-window-height)
		    (delete-window w)
		  (set-window-buffer w (other-buffer))))
	  (message (buffer-substring (point-min)
				     (progn
				       (goto-char (point-max))
				       (skip-chars-backward " \t\n")
				       (point)))))
      ;; 文法チェックの結果が複数行にわたる場合は、window を表示する
      (or w (set-window-buffer (setq w (if (one-window-p)
					   (split-window (selected-window)
							 (- (window-height)
							    perl-debug-window-height))
					 (next-window)))
			       perl-debug-buffer-name))
      (set-window-start w (point-min))
      )))



;;; match-string
(or (fboundp 'match-string)
    ;; Introduced in Emacs 19.29.
    (defun match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num))))))
