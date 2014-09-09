;;; -*- Emacs-Lisp -*-
;;; $Id: perl-debug.el,v 1.5 1999/01/20 10:15:11 tsuchiya Exp $

;;; Author: Tsuchiya Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;; Keywords: perl

;;; Commentary:

;; Perl ������ץȤ�ǥХå�������ʸˡ�����򤹤뤿��Υ��ޥ�ɤ������
;; ���饤�֥��Ǥ������ѵڤӺ����ۤκݤϡ�GNU ���̸��ѵ������Ŭ����
;; �С������ˤ������äƲ�������

;; �켡���۸�
;;    http://www-nagao.kuee.kyoto-u.ac.jp/member/tsuchiya/elisp/perl-debug.el

;;; Install:

;; Ŭ���ʾ��� perl-debug.el �򥳥ԡ����Ƥ��顢���λ���� ~/.emacs
;; ���դ��ä��Ƥ���������
;;
;;    (add-hook 'perl-mode-hook
;;              '(lambda ()
;;                 (require 'perl-debug)
;;                 (perl-debug-set-coding-system)
;;                 (define-key perl-mode-map "\C-cc" 'perl-debug-lint)
;;                 (define-key perl-mode-map "\C-cd" 'perl-debug)))
	     

;;; ��¸�ط������
(provide 'perl-debug)
(require 'gud)

(eval-when-compile
  (require 'comint)
  (require 'shell))


;;; �ѿ� / ��������
(defvar perl-debug-coding-system (if (>= emacs-major-version 20) 'euc-japan-unix *euc-japan*unix)
  "*Perl ������ץȤδ���������")

(defvar perl-debug-window-height 10
  "*��̤���Ϥ��뤿��Υ�����ɥ��ι⤵")

(defvar perl-debug-command-name (if (boundp 'perldb-command-name) perldb-command-name "perl")
  "*Perl �Υե�����̾")

(defvar perl-debug-lint-option "-wc"
  "*ʸˡ�����å���Ԥʤ������ Perl ��Ϳ���륪�ץ����")

(defconst perl-debug-buffer-name "*perl-debug*" "��̤�ɽ������Хåե���̾��")

(defvar perl-debug-history '() "perl-debug �����Ϥ�����")
(make-variable-buffer-local 'perl-debug-history)

(defvar perl-debug-complete-functions
  '(perl-debug-dynamic-complete-filename)
  "*Function list to complete arguments for perl-debug")


;;;----------------------------------------------------------------------
;;;		���������ɤ�����
;;;----------------------------------------------------------------------

(if (coding-system-p perl-debug-coding-system)
    (if (>= emacs-major-version 20)
	(setq process-coding-system-alist
	      (cons (cons (format ".*%s" perl-debug-command-name) perl-debug-coding-system)
		    (if (boundp 'process-coding-system-alist) process-coding-system-alist)))
      (define-program-coding-system nil (format ".*%s" perl-debug-command-name) perl-debug-coding-system)))


(defun perl-debug-set-coding-system () "\
Set coding system of current buffer for perl
�����ȥХåե��δ��������ɤ� perl-debug-coding-system ���ͤ����ꤹ��ؿ�
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
    (message "���������ɤ����꤬�ְ�äƤ��ޤ���")))



;;;----------------------------------------------------------------------
;;;		Debugger
;;;----------------------------------------------------------------------

(defsubst perl-debug-get-command-name () "\
Function to get file name fo executing Perl.
�ե��������Ƭ��
	#!/usr/local/bin/jperl
�ʤɤε��Ҥ�����С������ǻ��ꤵ��Ƥ��� perl ��̾�����֤���
���ꤵ��Ƥ��ʤ����� perl-debug-command-name ���ͤ��֤���
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
�����ȥХåե��� Perl ������ץȤ�ǥХå����뤿��Υ��ޥ��
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
�����ȥХåե��� Perl ������ץȤ�ʸˡŪ���Τ��򸡺����륳�ޥ��
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
	;; ʸˡ�����å��η�̤�1�ԤΤߤξ��ϡ�minibuffer �ؤν��ϤΤߤ�α�ᡢ
	;; �⤷���� window ��ɽ������Ƥ�����Ϥ����������
	(progn
	  (if w (if (= (window-height w) perl-debug-window-height)
		    (delete-window w)
		  (set-window-buffer w (other-buffer))))
	  (message (buffer-substring (point-min)
				     (progn
				       (goto-char (point-max))
				       (skip-chars-backward " \t\n")
				       (point)))))
      ;; ʸˡ�����å��η�̤�ʣ���Ԥˤ錄����ϡ�window ��ɽ������
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
