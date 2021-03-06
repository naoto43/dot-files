;;; Magit -- control Git from Emacs.

;; Copyright (C) 2008, 2009  Marius Vollmer
;; Copyright (C) 2008  Linh Dang
;; Copyright (C) 2008  Alex Ott
;; Copyright (C) 2008  Marcin Bachry
;; Copyright (C) 2009  Alexey Voinov
;; Copyright (C) 2009  John Wiegley
;;
;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; Invoking the magit-status function will show a buffer with the
;; status of the current git repository and its working tree.  That
;; buffer offers key bindings for manipulating the status in simple
;; ways.
;;
;; The status buffer mainly shows the difference between the working
;; tree and the index, and the difference between the index and the
;; current HEAD.  You can add individual hunks from the working tree
;; to the index, and you can commit the index.
;;
;; See the Magit User Manual for more information.

;;; TODO

;; - Queuing of asynchronous commands.
;; - Good email integration.
;; - Showing tags.
;; - Visiting from staged hunks doesn't always work since the line
;;   numbers don't refer to the working tree.  Fix that somehow.
;; - Get current defun from removed lines in a diff
;; - Amending commits other than HEAD.
;; - 'Subsetting', only looking at a subset of all files.

(require 'cl)
(require 'parse-time)
(require 'log-edit)
(require 'easymenu)
(require 'diff-mode)

(defgroup magit nil
  "Controlling Git from Emacs."
  :prefix "magit-"
  :group 'tools)

(defcustom magit-git-executable "git"
  "The name of the Git executable."
  :group 'magit
  :type 'string)

(defcustom magit-git-standard-options '("--no-pager")
  "Standard options when running Git."
  :group 'magit
  :type '(repeat string))

(defcustom magit-save-some-buffers t
  "Non-nil means that \\[magit-status] will save modified buffers before running.
Setting this to t will ask which buffers to save, setting it to 'dontask will
save all modified buffers without asking."
  :group 'magit
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Ask" t)
		 (const :tag "Save without asking" dontask)))

(defcustom magit-commit-all-when-nothing-staged 'ask
  "Determines what \\[magit-log-edit] does when nothing is staged.
Setting this to nil will make it do nothing, setting it to t will arrange things so that the actual commit command will use the \"--all\" option, setting it to 'ask will first ask for confirmation whether to do this, and setting it to 'ask-stage will cause all changes to be staged, after a confirmation."
  :group 'magit
  :type '(choice (const :tag "No" nil)
		 (const :tag "Always" t)
		 (const :tag "Ask" ask)
		 (const :tag "Ask to stage everything" ask-stage)))

(defcustom magit-commit-signoff nil
  "When performing git commit adds --signoff"
  :group 'magit
  :type 'boolean)

(defcustom magit-log-cutoff-length 100
  "The maximum number of commits to show in the log and whazzup buffers"
  :group 'magit
  :type 'integer)

(defcustom magit-process-popup-time -1
  "Popup the process buffer if a command takes longer than this many seconds."
  :group 'magit
  :type '(choice (const :tag "Never" -1)
		 (const :tag "Immediately" 0)
		 (integer :tag "After this many seconds")))

(defcustom magit-log-edit-confirm-cancellation nil
  "Require acknowledgement before cancelling the log edit buffer."
  :group 'magit
  :type 'boolean)

(defface magit-header
  '((t))
  "Face for generic header lines.

Many Magit faces inherit from this one by default."
  :group 'magit)

(defface magit-section-title
  '((t :weight bold :inherit magit-header))
  "Face for section titles."
  :group 'magit)

(defface magit-branch
  '((t :weight bold :inherit magit-header))
  "Face for the current branch."
  :group 'magit)

(defface magit-diff-file-header
  '((t :inherit magit-header))
  "Face for diff file header lines."
  :group 'magit)

(defface magit-diff-hunk-header
  '((t :slant italic :inherit magit-header))
  "Face for diff hunk header lines."
  :group 'magit)

(defface magit-diff-add
  '((((class color) (background light))
     :foreground "blue1")
    (((class color) (background dark))
     :foreground "white"))
  "Face for lines in a diff that have been added."
  :group 'magit)

(defface magit-diff-none
  '((t))
  "Face for lines in a diff that are unchanged."
  :group 'magit)

(defface magit-diff-del
  '((((class color) (background light))
     :foreground "red")
    (((class color) (background dark))
     :foreground "OrangeRed"))
  "Face for lines in a diff that have been deleted."
  :group 'magit)

(defface magit-item-highlight
  '((((class color) (background light))
     :background "gray95")
    (((class color) (background dark))
     :background "dim gray"))
  "Face for highlighting the current item."
  :group 'magit)

(defface magit-item-mark
  '((((class color) (background light))
     :foreground "red")
    (((class color) (background dark))
     :foreground "orange"))
  "Face for highlighting marked item."
  :group 'magit)

(defface magit-log-tag-label
  '((((class color) (background light))
     :background "LightGoldenRod")
    (((class color) (background dark))
     :background "DarkGoldenRod"))
  "Face for git tag labels shown in log buffer."
  :group 'magit)

(defface magit-log-head-label
  '((((class color) (background light))
     :background "spring green")
    (((class color) (background dark))
     :background "DarkGreen"))
  "Face for branch head labels shown in log buffer."
  :group 'magit)

;;; Macros

(defmacro magit-with-refresh (&rest body)
  (declare (indent 0))
  `(magit-refresh-wrapper (lambda () ,@body)))

;;; Utilities

(defun magit-use-region-p ()
  (if (fboundp 'use-region-p)
      (use-region-p)
    (and transient-mark-mode mark-active)))

(defun magit-goto-line (line)
  ;; Like goto-line but doesn't set the mark.
  (save-restriction
    (widen)
    (goto-char 1)
    (forward-line (1- line))))

(defun magit-format-shell-command (fmt args)
  (apply 'format fmt (mapcar #'magit-escape-for-shell args)))

(defun magit-format-git-command (fmt args)
  (concat (magit-concat-with-delim
	   " "
	   (mapcar #'magit-escape-for-shell
		   (cons magit-git-executable
			 magit-git-standard-options)))
	  " "
	  (magit-format-shell-command fmt args)))

(defun magit-shell-lines (command)
  (let ((str (magit-shell-command-to-string command)))
    (if (string= str "")
	nil
      (let ((lines (nreverse (split-string str "\n"))))
	(if (string= (car lines) "")
	    (setq lines (cdr lines)))
	(nreverse lines)))))

(defun magit-shell-command-to-string (command)
  (with-output-to-string
    (with-current-buffer
        standard-output
      (process-file shell-file-name nil t nil shell-command-switch command))))

(defun magit-shell (command)
  (let ((str (magit-shell-command-to-string command)))
    (if (string= str "")
	nil
      (if (equal (elt str (- (length str) 1)) ?\n)
	  (substring str 0 (- (length str) 1))
	str))))

(defun magit-git-lines (fmt &rest args)
  (magit-shell-lines (magit-format-git-command fmt args)))

(defun magit-git-string (fmt &rest args)
  (magit-shell (magit-format-git-command fmt args)))

(defun magit-git-exit-code (fmt &rest args)
  (call-process shell-file-name nil nil nil
		shell-command-switch
		(magit-format-git-command fmt args)))

(defun magit-file-lines (file)
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((rev (nreverse (split-string (buffer-string) "\n"))))
	(nreverse (if (equal (car rev) "")
		      (cdr rev)
		    rev))))))

(defun magit-write-file-lines (file lines)
  (with-temp-buffer
    (dolist (l lines)
      (insert l "\n"))
    (write-file file)))

(defun magit-concat-with-delim (delim seqs)
  (cond ((null seqs)
	 nil)
	((null (cdr seqs))
	 (car seqs))
	(t
	 (concat (car seqs) delim (magit-concat-with-delim delim (cdr seqs))))))

(defun magit-get (&rest keys)
  (magit-git-string "config %s" (magit-concat-with-delim "." keys)))

(defun magit-set (val &rest keys)
  (if val
      (magit-git-string "config %s %s" (magit-concat-with-delim "." keys) val)
    (magit-git-string "config --unset %s" (magit-concat-with-delim "." keys))))

(defun magit-get-top-dir (cwd)
  (let ((cwd (expand-file-name cwd)))
    (and (file-directory-p cwd)
	 (let* ((default-directory cwd)
		(magit-dir
		 (magit-git-string "rev-parse --git-dir 2>/dev/null")))
	   (and magit-dir
		(file-name-as-directory
		 (or (file-name-directory magit-dir) cwd)))))))

(defun magit-get-ref (ref)
  (magit-git-string "symbolic-ref -q %s" ref))

(defun magit-get-current-branch ()
  (let* ((head (magit-get-ref "HEAD"))
	 (pos (and head (string-match "^refs/heads/" head))))
    (if pos
	(substring head 11)
      nil)))

(defun magit-read-top-dir ()
  (file-name-as-directory
   (read-directory-name "Git repository: "
			(or (magit-get-top-dir default-directory) default-directory))))

(defun magit-name-rev (rev)
  (and rev
       (let ((name (magit-git-string "name-rev --name-only %s" rev)))
	 (if (or (not name) (string= name "undefined"))
	     rev
	   name))))

(defun magit-put-line-property (prop val)
  (put-text-property (line-beginning-position) (line-beginning-position 2)
		     prop val))

(defun magit-escape-for-shell (str)
  (concat "'" (replace-regexp-in-string "'" "'\\''" str) "'"))

(defun magit-format-commit (commit format)
  (magit-git-string "log --max-count=1 --pretty=format:%s %s" format commit))

(defun magit-current-line ()
  (buffer-substring-no-properties (line-beginning-position)
				  (line-end-position)))

(defun magit-insert-region (beg end buf)
  (let ((text (buffer-substring-no-properties beg end)))
    (with-current-buffer buf
      (insert text))))

(defun magit-insert-current-line (buf)
  (let ((text (buffer-substring-no-properties
	       (line-beginning-position) (line-beginning-position 2))))
    (with-current-buffer buf
      (insert text))))

(defun magit-file-uptodate-p (file)
  (eq (magit-git-exit-code "diff --quiet -- %s" file) 0))

(defun magit-anything-staged-p ()
  (not (eq (magit-git-exit-code "diff --quiet --cached") 0)))

(defun magit-everything-clean-p ()
  (and (not (magit-anything-staged-p))
       (eq (magit-git-exit-code "diff --quiet") 0)))

(defun magit-commit-parents (commit)
  (cdr (magit-git-lines "rev-list -1 --parents %s | tr ' ' '\n'"
			commit)))

;; XXX - let the user choose the parent

(defun magit-choose-parent (commit op)
  (let* ((parents (magit-commit-parents commit)))
    (if (> (length parents) 1)
	(error "Can't %s merge commits." op)
      (car parents))))

(defun magit-choose-parent-id (commit op)
  (let* ((parents (magit-commit-parents commit)))
    (if (> (length parents) 1)
	(error "Can't %s merge commits." op)
      nil)))

;;; Revisions and ranges

(defun magit-list-interesting-revisions ()
  (append (magit-git-lines "branch -a | cut -c3-")
	  (magit-git-lines "tag")))

(defun magit-read-rev (prompt &optional def)
  (let* ((prompt (if def
		     (format "%s (default %s): " prompt def)
		   (format "%s: " prompt)))
	 (rev (completing-read prompt (magit-list-interesting-revisions)
			       nil nil nil nil def)))
    (if (string= rev "")
	nil
      rev)))

(defun magit-read-rev-range (op &optional def-beg def-end)
  (let ((beg (magit-read-rev (format "%s start" op)
			     def-beg)))
    (if (not beg)
	nil
      (let ((end (magit-read-rev (format "%s end" op) def-end)))
	(cons beg end)))))

(defun magit-rev-to-git (rev)
  (or rev
      (error "No revision specified"))
  (if (string= rev ".")
      (magit-marked-commit)
    rev))

(defun magit-rev-range-to-git (range)
  (or range
      (error "No revision range specified"))
  (if (stringp range)
      range
    (if (cdr range)
	(format "%s..%s"
		(magit-rev-to-git (car range))
		(magit-rev-to-git (cdr range)))
      (format "%s" (magit-rev-to-git (car range))))))

(defun magit-rev-describe (rev)
  (or rev
      (error "No revision specified"))
  (if (string= rev ".")
      "mark"
    (magit-name-rev rev)))

(defun magit-rev-range-describe (range things)
  (or range
      (error "No revision range specified"))
  (if (stringp range)
      (format "%s in %s" things range)
    (if (cdr range)
	(format "%s from %s to %s" things
		(magit-rev-describe (car range))
		(magit-rev-describe (cdr range)))
      (format "%s at %s" things (magit-rev-describe (car range))))))

(defun magit-default-rev ()
  (magit-name-rev (magit-commit-at-point t)))

;;; Sections

;; A buffer in magit-mode is organized into hierarchical sections.
;; These sections are used for navigation and for hiding parts of the
;; buffer.
;;
;; Most sections also represent the objects that Magit works with,
;; such as files, diffs, hunks, commits, etc.  The 'type' of a section
;; identifies what kind of object it represents (if any), and the
;; parent and grand-parent, etc provide the context.

(defstruct magit-section
  parent title beginning end children hidden type info)

(defvar magit-top-section nil)
(make-variable-buffer-local 'magit-top-section)
(put 'magit-top-section 'permanent-local t)

(defvar magit-old-top-section nil)

(defvar magit-section-hidden-default nil)

(defun magit-new-section (title type)
  (let* ((s (make-magit-section :parent magit-top-section
				:title title
				:type type
				:hidden magit-section-hidden-default))
	 (old (and magit-old-top-section
		   (magit-find-section (magit-section-path s)
				       magit-old-top-section))))
    (if magit-top-section
	(setf (magit-section-children magit-top-section)
	      (cons s (magit-section-children magit-top-section)))
      (setq magit-top-section s))
    (if old
	(setf (magit-section-hidden s) (magit-section-hidden old)))
    s))

(defun magit-cancel-section (section)
  (delete-region (magit-section-beginning section)
		 (magit-section-end section))
  (let ((parent (magit-section-parent section)))
    (if parent
	(setf (magit-section-children parent)
	      (delq section (magit-section-children parent)))
      (setq magit-top-section nil))))

(defmacro magit-with-section (title type &rest body)
  (declare (indent 2))
  (let ((s (gensym)))
    `(let* ((,s (magit-new-section ,title ,type))
	    (magit-top-section ,s))
       (setf (magit-section-beginning ,s) (point))
       ,@body
       (setf (magit-section-end ,s) (point))
       (setf (magit-section-children ,s)
	     (nreverse (magit-section-children ,s)))
       ,s)))

(defun magit-set-section-info (info &optional section)
  (setf (magit-section-info (or section magit-top-section)) info))

(defmacro magit-create-buffer-sections (&rest body)
  (declare (indent 0))
  `(let ((inhibit-read-only t))
     (erase-buffer)
     (let ((magit-old-top-section magit-top-section))
       (setq magit-top-section nil)
       ,@body
       (when (null magit-top-section)
	 (magit-with-section 'top nil
	   (insert "(empty)\n")))
       (magit-propertize-section magit-top-section)
       (magit-section-set-hidden magit-top-section
				 (magit-section-hidden magit-top-section)))))

(defun magit-propertize-section (section)
  (put-text-property (magit-section-beginning section)
		     (magit-section-end section)
		     'magit-section section)
  (dolist (s (magit-section-children section))
    (magit-propertize-section s)))

(defun magit-find-section (path top)
  (if (null path)
      top
    (let ((sec (find-if (lambda (s) (equal (car path)
					   (magit-section-title s)))
			(magit-section-children top))))
      (if sec
	  (magit-find-section (cdr path) sec)
	nil))))

(defun magit-section-path (section)
  (if (not (magit-section-parent section))
      '()
    (append (magit-section-path (magit-section-parent section))
	    (list (magit-section-title section)))))

(defun magit-find-section-at (pos secs)
  (while (and secs
	      (not (and (<= (magit-section-beginning (car secs)) pos)
			(<  pos (magit-section-end (car secs))))))
    (setq secs (cdr secs)))
  (if secs
      (or (magit-find-section-at pos (magit-section-children (car secs)))
	  (car secs))
    nil))

(defun magit-find-section-after (pos secs)
  (while (and secs
	      (not (> (magit-section-beginning (car secs)) pos)))
    (setq secs (cdr secs)))
  (car secs))

(defun magit-find-section-before (pos secs)
  (let ((prev nil))
    (while (and secs
		(not (> (magit-section-beginning (car secs)) pos)))
      (setq prev (car secs))
      (setq secs (cdr secs)))
    prev))

(defun magit-current-section ()
  (or (get-text-property (point) 'magit-section)
      magit-top-section))

(defun magit-insert-section (section-title-and-type
			     buffer-title washer cmd &rest args)
  (let* ((body-beg nil)
	 (section-title (if (consp section-title-and-type)
			    (car section-title-and-type)
			  section-title-and-type))
	 (section-type (if (consp section-title-and-type)
			   (cdr section-title-and-type)
			 nil))
	 (section
	  (magit-with-section section-title section-type
	    (if buffer-title
		(insert (propertize buffer-title 'face 'magit-section-title)
			"\n"))
	    (setq body-beg (point))
	    (apply 'process-file cmd nil t nil args)
	    (if (not (eq (char-before) ?\n))
		(insert "\n"))
	    (if washer
		(save-restriction
		  (narrow-to-region body-beg (point))
		  (goto-char (point-min))
		  (funcall washer)
		  (goto-char (point-max)))))))
    (if (= body-beg (point))
	(magit-cancel-section section)
      (insert "\n"))
    section))

(defun magit-git-section (section-title-and-type
			  buffer-title washer &rest args)
  (apply #'magit-insert-section
	 section-title-and-type
	 buffer-title
	 washer
	 magit-git-executable
	 (append magit-git-standard-options args)))

(defun magit-next-section (section)
  (let ((parent (magit-section-parent section)))
    (if parent
	(let ((next (cadr (memq section
				(magit-section-children parent)))))
	  (or next
	      (magit-next-section parent))))))

(defun magit-goto-next-section ()
  (interactive)
  (let* ((section (magit-current-section))
	 (next (or (and (not (magit-section-hidden section))
			(magit-section-children section)
			(magit-find-section-after (point)
						  (magit-section-children
						   section)))
		   (magit-next-section section))))

    (if next
	(progn
	  (goto-char (magit-section-beginning next))
	  (if (memq magit-submode '(log reflog))
	      (magit-show-commit next))
	  (if (not (magit-section-hidden next))
	      (let ((offset (- (line-number-at-pos
				(magit-section-beginning next))
			       (line-number-at-pos
				(magit-section-end next)))))
		(if (< offset (window-height))
		    (recenter offset)))))
      (message "No next section"))))

(defun magit-prev-section (section)
  (let ((parent (magit-section-parent section)))
    (if parent
	(let ((prev (cadr (memq section
				(reverse (magit-section-children parent))))))
	  (cond (prev
		 (while (and (not (magit-section-hidden prev))
			     (magit-section-children prev))
		   (setq prev (car (reverse (magit-section-children prev)))))
		 prev)
		(t
		 parent))))))

(defun magit-goto-previous-section ()
  (interactive)
  (let ((section (magit-current-section)))
    (cond ((= (point) (magit-section-beginning section))
	   (let ((prev (magit-prev-section (magit-current-section))))
	     (if prev
		 (progn
		   (if (memq magit-submode '(log reflog))
		       (magit-show-commit (or prev section)))
		   (goto-char (magit-section-beginning prev)))
	       (message "No previous section"))))
	  (t
	   (let ((prev (magit-find-section-before (point)
						  (magit-section-children
						   section))))
	     (if (memq magit-submode '(log reflog))
		 (magit-show-commit (or prev section)))
	     (goto-char (magit-section-beginning (or prev section))))))))

(defun magit-goto-section (path)
  (let ((sec (magit-find-section path magit-top-section)))
    (if sec
	(goto-char (magit-section-beginning sec))
      (message "No such section"))))

(defun magit-for-all-sections (func &optional top)
  (let ((section (or top magit-top-section)))
    (when section
      (funcall func section)
      (dolist (c (magit-section-children section))
	(magit-for-all-sections func c)))))

(defun magit-section-set-hidden (section hidden)
  (setf (magit-section-hidden section) hidden)
  (let ((inhibit-read-only t)
	(beg (save-excursion
	       (goto-char (magit-section-beginning section))
	       (forward-line)
	       (point)))
	(end (magit-section-end section)))
    (put-text-property beg end 'invisible hidden))
  (if (not hidden)
      (dolist (c (magit-section-children section))
	(magit-section-set-hidden c (magit-section-hidden c)))))

(defun magit-section-any-hidden (section)
  (or (magit-section-hidden section)
      (some #'magit-section-any-hidden (magit-section-children section))))

(defun magit-section-collapse (section)
  (dolist (c (magit-section-children section))
    (setf (magit-section-hidden c) t))
  (magit-section-set-hidden section nil))

(defun magit-section-expand (section)
  (dolist (c (magit-section-children section))
    (setf (magit-section-hidden c) nil))
  (magit-section-set-hidden section nil))

(defun magit-section-expand-all-aux (section)
  (dolist (c (magit-section-children section))
    (setf (magit-section-hidden c) nil)
    (magit-section-expand-all-aux c)))

(defun magit-section-expand-all (section)
  (magit-section-expand-all-aux section)
  (magit-section-set-hidden section nil))

(defun magit-section-hideshow (flag-or-func)
  (let ((section (magit-current-section)))
    (cond ((magit-section-parent section)
	   (goto-char (magit-section-beginning section))
	   (if (functionp flag-or-func)
	       (funcall flag-or-func section)
	     (magit-section-set-hidden section flag-or-func))))))

(defun magit-show-section ()
  (interactive)
  (magit-section-hideshow nil))

(defun magit-hide-section ()
  (interactive)
  (magit-section-hideshow t))

(defun magit-collapse-section ()
  (interactive)
  (magit-section-hideshow #'magit-section-collapse))

(defun magit-expand-section ()
  (interactive)
  (magit-section-hideshow #'magit-section-expand))

(defun magit-toggle-section ()
  (interactive)
  (magit-section-hideshow
   (lambda (s)
     (magit-section-set-hidden s (not (magit-section-hidden s))))))

(defun magit-expand-collapse-section ()
  (interactive)
  (magit-section-hideshow
   (lambda (s)
     (cond ((magit-section-any-hidden s)
	    (magit-section-expand-all s))
	   (t
	    (magit-section-collapse s))))))

(defun magit-cycle-section ()
  (interactive)
  (magit-section-hideshow
   (lambda (s)
     (cond ((magit-section-hidden s)
	    (magit-section-collapse s))
	   ((notany #'magit-section-hidden (magit-section-children s))
	    (magit-section-set-hidden s t))
	   (t
	    (magit-section-expand s))))))

(defun magit-section-lineage (s)
  (and s (cons s (magit-section-lineage (magit-section-parent s)))))

(defun magit-section-show-level (section level threshold path)
  (magit-section-set-hidden section (>= level threshold))
  (when (< level threshold)
    (if path
	(magit-section-show-level (car path) (1+ level) threshold (cdr path))
      (dolist (c (magit-section-children section))
	(magit-section-show-level c (1+ level) threshold nil)))))

(defun magit-show-level (level all)
  (if all
      (magit-section-show-level magit-top-section 0 level nil)
    (let ((path (reverse (magit-section-lineage (magit-current-section)))))
      (magit-section-show-level (car path) 0 level (cdr path)))))

(defun magit-show-only-files ()
  (interactive)
  (if (eq magit-submode 'status)
      (call-interactively 'magit-show-level-2)
    (call-interactively 'magit-show-level-1)))

(defun magit-show-only-files-all ()
  (interactive)
  (if (eq magit-submode 'status)
      (call-interactively 'magit-show-level-2-all)
    (call-interactively 'magit-show-level-1-all)))

(defmacro magit-define-level-shower-1 (level all)
  (let ((fun (intern (format "magit-show-level-%s%s"
			     level (if all "-all" ""))))
	(doc (format "Show sections on level %s." level)))
    `(defun ,fun ()
       ,doc
       (interactive)
       (magit-show-level ,level ,all))))

(defmacro magit-define-level-shower (level)
  `(progn
     (magit-define-level-shower-1 ,level nil)
     (magit-define-level-shower-1 ,level t)))

(defmacro magit-define-section-jumper (sym title)
  (let ((fun (intern (format "magit-jump-to-%s" sym)))
	(doc (format "Jump to section `%s'." title)))
    `(defun ,fun ()
       ,doc
       (interactive)
       (magit-goto-section '(,sym)))))

(defvar magit-highlight-overlay nil)

(defvar magit-highlighted-section nil)

(defun magit-highlight-section ()
  (let ((section (magit-current-section)))
    (when (not (eq section magit-highlighted-section))
      (setq magit-highlighted-section section)
      (if (not magit-highlight-overlay)
	  (let ((ov (make-overlay 1 1)))
	    (overlay-put ov 'face 'magit-item-highlight)
	    (setq magit-highlight-overlay ov)))
      (if (and section (magit-section-type section))
	  (move-overlay magit-highlight-overlay
			(magit-section-beginning section)
			(magit-section-end section)
			(current-buffer))
	(delete-overlay magit-highlight-overlay)))))

(defun magit-section-context-type (section)
  (if (null section)
      '()
    (let ((c (or (magit-section-type section)
		 (if (symbolp (magit-section-title section))
		     (magit-section-title section)))))
      (if c
	  (cons c (magit-section-context-type
		   (magit-section-parent section)))
	'()))))

(defun magit-prefix-p (prefix list)
  ;;; Very schemish...
  (or (null prefix)
      (if (eq (car prefix) '*)
	  (or (magit-prefix-p (cdr prefix) list)
	      (and (not (null list))
		   (magit-prefix-p prefix (cdr list))))
	(and (not (null list))
	     (equal (car prefix) (car list))
	     (magit-prefix-p (cdr prefix) (cdr list))))))

(defmacro magit-section-case (head &rest clauses)
  (declare (indent 1))
  (let ((section (car head))
	(info (cadr head))
	(type (gensym))
	(context (gensym))
	(opname (caddr head)))
    `(let* ((,section (magit-current-section))
	    (,info (magit-section-info ,section))
	    (,type (magit-section-type ,section))
	    (,context (magit-section-context-type ,section)))
       (cond ,@(mapcar (lambda (clause)
			 (if (eq (car clause) t)
			     clause
			   (let ((prefix (reverse (car clause)))
				 (body (cdr clause)))
			     `((magit-prefix-p ',prefix ,context)
			       ,@body))))
		       clauses)
	     ,@(if opname
		   `(((not ,type)
		      (error "Nothing to %s here." ,opname))
		     (t
		      (error "Can't %s a %s."
			     ,opname
			     (or (get ,type 'magit-description)
				 ,type)))))))))

(defmacro magit-section-action (head &rest clauses)
  (declare (indent 1))
  `(magit-with-refresh
     (magit-section-case ,head ,@clauses)))

(defun magit-wash-sequence (func)
  (while (and (not (eobp))
	      (funcall func))))

;;; Running commands

(defun magit-set-mode-line-process (str)
  (let ((pr (if str (concat " " str) "")))
    (save-excursion
      (magit-for-all-buffers (lambda ()
			       (setq mode-line-process pr))))))

(defun magit-process-indicator-from-command (comps)
  (if (magit-prefix-p (cons magit-git-executable magit-git-standard-options)
		      comps)
      (setq comps (nthcdr (+ (length magit-git-standard-options) 1) comps)))
  (cond ((or (null (cdr comps))
	     (not (member (car comps) '("remote"))))
	 (car comps))
	(t
	 (concat (car comps) " " (cadr comps)))))

(defvar magit-process nil)
(defvar magit-process-client-buffer nil)

(defun magit-run* (cmd-and-args
		   &optional logline noerase noerror nowait input)
  (let ((cmd (car cmd-and-args))
	(args (cdr cmd-and-args))
	(dir default-directory)
	(buf (get-buffer-create "*magit-process*"))
	(successp nil))
    (or (not magit-process)
	(error "Git is already running."))
    (magit-set-mode-line-process
     (magit-process-indicator-from-command cmd-and-args))
    (setq magit-process-client-buffer (current-buffer))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
	(setq default-directory dir)
	(if noerase
	    (goto-char (point-max))
	  (erase-buffer))
	(insert "$ " (or logline
			 (magit-concat-with-delim " " cmd-and-args))
		"\n")
	(cond (nowait
	       (setq magit-process
		     (apply 'start-process cmd buf cmd args))
	       (set-process-sentinel magit-process 'magit-process-sentinel)
	       (set-process-filter magit-process 'magit-process-filter)
	       (when input
		 (with-current-buffer input
		   (process-send-region magit-process
					(point-min) (point-max)))
		 (process-send-eof magit-process)
		 (sit-for 0.1 t))
	       (cond ((= magit-process-popup-time 0)
		      (pop-to-buffer (process-buffer magit-process)))
		     ((> magit-process-popup-time 0)
		      (run-with-timer
		       magit-process-popup-time nil
		       (function
			(lambda (buf)
			  (with-current-buffer buf
			    (when magit-process
			      (display-buffer (process-buffer magit-process))
			      (goto-char (point-max))))))
		       (current-buffer))))
	       (setq successp t))
	      (input
	       (with-current-buffer input
		 (setq default-directory dir)
		 (setq successp
		       (equal (apply 'call-process-region
				     (point-min) (point-max)
				     cmd nil buf nil args) 0)))
	       (magit-set-mode-line-process nil)
	       (magit-need-refresh magit-process-client-buffer))
	      (t
	       (setq successp
		     (equal (apply 'call-process cmd nil buf nil args) 0))
	       (magit-set-mode-line-process nil)
	       (magit-need-refresh magit-process-client-buffer))))
      (or successp
	  noerror
	  (error "Git failed."))
      successp)))

(defun magit-process-sentinel (process event)
  (let ((msg (format "Git %s." (substring event 0 -1)))
	(successp (string-match "^finished" event)))
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t))
	(goto-char (point-max))
	(insert msg "\n")
	(message msg)))
    (setq magit-process nil)
    (magit-set-mode-line-process nil)
    (magit-refresh-buffer magit-process-client-buffer)))

(defun magit-process-filter (proc string)
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t))
      (goto-char (process-mark proc))
      ;; Find last ^M in string.  If one was found, ignore everything
      ;; before it and delete the current line.
      (let ((ret-pos (position ?\r string :from-end t)))
	(cond (ret-pos
	       (goto-char (line-beginning-position))
	       (delete-region (point) (line-end-position))
	       (insert (substring string (+ ret-pos 1))))
	      (t
	       (insert string))))
      (set-marker (process-mark proc) (point)))))

(defun magit-run (cmd &rest args)
  (magit-with-refresh
    (magit-run* (cons cmd args))))

(defun magit-run-git (&rest args)
  (magit-with-refresh
    (magit-run* (append (cons magit-git-executable
			      magit-git-standard-options)
			args))))

(defun magit-run-with-input (input cmd &rest args)
  (magit-with-refresh
    (magit-run* (cons cmd args) nil nil nil nil input)))

(defun magit-run-git-with-input (input &rest args)
  (magit-with-refresh
    (magit-run* (append (cons magit-git-executable
			    magit-git-standard-options)
			args)
		nil nil nil nil input)))

(defun magit-run-shell (fmt &rest args)
  (let ((cmd (apply #'format fmt (mapcar #'magit-escape-for-shell args))))
    (magit-with-refresh
      (magit-run* (list shell-file-name shell-command-switch cmd)
		  cmd))))

(defun magit-run-git-async (&rest args)
  (magit-run* (append (cons magit-git-executable
			    magit-git-standard-options)
		      args)
	      nil nil nil t))

(defun magit-run-async-with-input (input cmd &rest args)
  (magit-run* (cons cmd args) nil nil nil t input))

(defun magit-display-process ()
  (interactive)
  (display-buffer "*magit-process*"))

;;; Mode

;; We define individual functions (instead of using lambda etc) so
;; that the online help can show something meaningful.

(magit-define-section-jumper untracked "Untracked files")
(magit-define-section-jumper unstaged  "Unstaged changes")
(magit-define-section-jumper staged    "Staged changes")
(magit-define-section-jumper unpushed  "Unpushed commits")
(magit-define-section-jumper svn-unpushed  "Unpushed commits (SVN)")

(magit-define-level-shower 1)
(magit-define-level-shower 2)
(magit-define-level-shower 3)
(magit-define-level-shower 4)

(defvar magit-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "n") 'magit-goto-next-section)
    (define-key map (kbd "p") 'magit-goto-previous-section)
    (define-key map (kbd "TAB") 'magit-toggle-section)
    (define-key map (kbd "<backtab>") 'magit-expand-collapse-section)
    (define-key map (kbd "1") 'magit-show-level-1)
    (define-key map (kbd "2") 'magit-show-level-2)
    (define-key map (kbd "3") 'magit-show-level-3)
    (define-key map (kbd "4") 'magit-show-level-4)
    (define-key map (kbd "M-1") 'magit-show-level-1-all)
    (define-key map (kbd "M-2") 'magit-show-level-2-all)
    (define-key map (kbd "M-3") 'magit-show-level-3-all)
    (define-key map (kbd "M-4") 'magit-show-level-4-all)
    (define-key map (kbd "M-h") 'magit-show-only-files)
    (define-key map (kbd "M-H") 'magit-show-only-files-all)
    (define-key map (kbd "M-s") 'magit-show-level-4)
    (define-key map (kbd "M-S") 'magit-show-level-4-all)
    (define-key map (kbd "g") 'magit-refresh)
    (define-key map (kbd "G") 'magit-refresh-all)
    (define-key map (kbd "s") 'magit-stage-item)
    (define-key map (kbd "S") 'magit-stage-all)
    (define-key map (kbd "u") 'magit-unstage-item)
    (define-key map (kbd "U") 'magit-unstage-all)
    (define-key map (kbd "i") 'magit-ignore-item)
    (define-key map (kbd "I") 'magit-ignore-item-locally)
    (define-key map (kbd "?") 'magit-describe-item)
    (define-key map (kbd ".") 'magit-mark-item)
    (define-key map (kbd "=") 'magit-diff-with-mark)
    (define-key map (kbd "-") 'magit-diff-smaller-hunks)
    (define-key map (kbd "+") 'magit-diff-larger-hunks)
    (define-key map (kbd "0") 'magit-diff-default-hunks)
    (define-key map (kbd "l") 'magit-log)
    (define-key map (kbd "L") 'magit-log-long)
    (define-key map (kbd "h") 'magit-reflog-head)
    (define-key map (kbd "H") 'magit-reflog)
    (define-key map (kbd "d") 'magit-diff-working-tree)
    (define-key map (kbd "D") 'magit-diff)
    (define-key map (kbd "a") 'magit-apply-item)
    (define-key map (kbd "A") 'magit-cherry-pick-item)
    (define-key map (kbd "v") 'magit-revert-item)
    (define-key map (kbd "x") 'magit-reset-head)
    (define-key map (kbd "X") 'magit-reset-working-tree)
    (define-key map (kbd "k") 'magit-discard-item)
    (define-key map (kbd "RET") 'magit-visit-item)
    (define-key map (kbd "SPC") 'magit-show-item-or-scroll-up)
    (define-key map (kbd "DEL") 'magit-show-item-or-scroll-down)
    (define-key map (kbd "C-w") 'magit-copy-item-as-kill)
    (define-key map (kbd "b") 'magit-checkout)
    (define-key map (kbd "B") 'magit-create-branch)
    (define-key map (kbd "m") 'magit-manual-merge)
    (define-key map (kbd "M") 'magit-automatic-merge)
    (define-key map (kbd "e") 'magit-interactive-resolve-item)
    (define-key map (kbd "N r") 'magit-svn-rebase)
    (define-key map (kbd "N c") 'magit-svn-dcommit)
    (define-key map (kbd "R") 'magit-rebase-step)
    (define-key map (kbd "r s") 'magit-rewrite-start)
    (define-key map (kbd "r t") 'magit-rewrite-stop)
    (define-key map (kbd "r a") 'magit-rewrite-abort)
    (define-key map (kbd "r f") 'magit-rewrite-finish)
    (define-key map (kbd "r *") 'magit-rewrite-set-unused)
    (define-key map (kbd "r .") 'magit-rewrite-set-used)
    (define-key map (kbd "P") 'magit-push)
    (define-key map (kbd "f") 'magit-remote-update)
    (define-key map (kbd "F") 'magit-pull)
    (define-key map (kbd "c") 'magit-log-edit)
    (define-key map (kbd "C") 'magit-add-log)
    (define-key map (kbd "t") 'magit-tag)
    (define-key map (kbd "T") 'magit-annotated-tag)
    (define-key map (kbd "z") 'magit-stash)
    (define-key map (kbd "Z") 'magit-stash-snapshot)
    (define-key map (kbd "w") 'magit-wazzup)
    (define-key map (kbd "$") 'magit-display-process)
    (define-key map (kbd "E") 'magit-interactive-rebase)
    (define-key map (kbd "V") 'magit-show-branches)
    (define-key map (kbd "q") 'quit-window)
    map))

(easy-menu-define magit-mode-menu magit-mode-map
  "Magit menu"
  '("Magit"
    ["Refresh" magit-refresh t]
    ["Refresh all" magit-refresh-all t]
    "---"
    ["Stage" magit-stage-item t]
    ["Stage all" magit-stage-all t]
    ["Unstage" magit-unstage-item t]
    ["Unstage all" magit-unstage-all t]
    ["Commit" magit-log-edit t]
    ["Add log entry" magit-add-log t]
    ["Tag" magit-tag t]
    ["Annotated tag" magit-annotated-tag t]
    "---"
    ["Diff working tree" magit-diff-working-tree t]
    ["Diff" magit-diff t]
    ["Log" magit-log t]
    ["Long Log" magit-log-long t]
    ["Reflog head" magit-reflog-head t]
    ["Reflog" magit-reflog t]
    "---"
    ["Cherry pick" magit-cherry-pick-item t]
    ["Apply" magit-apply-item t]
    ["Revert" magit-revert-item t]
    "---"
    ["Ignore" magit-ignore-item t]
    ["Ignore locally" magit-ignore-item-locally t]
    ["Discard" magit-discard-item t]
    ["Reset head" magit-reset-head t]
    ["Reset working tree" magit-reset-working-tree t]
    ["Stash" magit-stash t]
    ["Snapshot" magit-stash-snapshot t]
    "---"
    ["Switch branch" magit-checkout t]
    ["Create branch" magit-create-branch t]
    ["Merge" magit-automatic-merge t]
    ["Merge (no commit)" magit-manual-merge t]
    ["Interactive resolve" magit-interactive-resolve-item t]
    ["Rebase" magit-rebase-step t]
    ("Git SVN"
     ["Rebase" magit-svn-rebase (magit-svn-enabled)]
     ["Commit" magit-svn-dcommit (magit-svn-enabled)]
     )
    ("Rewrite"
     ["Start" magit-rewrite-start t]
     ["Stop" magit-rewrite-stop t]
     ["Finish" magit-rewrite-finish t]
     ["Abort" magit-rewrite-abort t]
     ["Set used" magit-rewrite-set-used t]
     ["Set unused" magit-rewrite-set-unused t])
    "---"
    ["Push" magit-push t]
    ["Pull" magit-pull t]
    ["Remote update" magit-remote-update t]
    "---"
    ["Display Git output" magit-display-process t]
    ["Quit Magit" quit-window t]))

(defvar magit-mode-hook nil)

(put 'magit-mode 'mode-class 'special)

(defvar magit-submode nil)
(make-variable-buffer-local 'magit-submode)
(put 'magit-submode 'permanent-local t)

(defvar magit-refresh-function nil)
(make-variable-buffer-local 'magit-refresh-function)
(put 'magit-refresh-function 'permanent-local t)

(defvar magit-refresh-args nil)
(make-variable-buffer-local 'magit-refresh-args)
(put 'magit-refresh-args 'permanent-local t)

(defvar last-point)

(defun magit-remember-point ()
  (setq last-point (point)))

(defun magit-invisible-region-end (pos)
  (while (and (not (= pos (point-max))) (invisible-p pos))
    (setq pos (next-char-property-change pos)))
  pos)

(defun magit-invisible-region-start (pos)
  (while (and (not (= pos (point-min))) (invisible-p pos))
    (setq pos (1- (previous-char-property-change pos))))
  pos)

(defun magit-correct-point-after-command ()
  ;; Emacs often leaves point in invisible regions, it seems.  To fix
  ;; this, we move point ourselves and never let Emacs do its own
  ;; adjustements.
  ;;
  ;; When point has to be moved out of an invisible region, it can be
  ;; moved to its end or its beginning.  We usually move it to its
  ;; end, except when that would move point back to where it was
  ;; before the last command.
  ;;
  (if (invisible-p (point))
      (let ((end (magit-invisible-region-end (point))))
	(goto-char (if (= end last-point)
		       (magit-invisible-region-start (point))
		     end))))
  (setq disable-point-adjustment t))

(defun magit-post-command-hook ()
  (magit-correct-point-after-command)
  (magit-highlight-section))

(defun magit-mode ()
  "Review the status of a git repository and act on it.

Please see the manual for a complete description of Magit.

\\{magit-mode-map}"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq buffer-read-only t)
  (setq major-mode 'magit-mode
	mode-name "Magit"
	mode-line-process ""
	truncate-lines t
	line-move-visual nil)
  (add-hook 'pre-command-hook #'magit-remember-point nil t)
  (add-hook 'post-command-hook #'magit-post-command-hook t t)
  (use-local-map magit-mode-map)
  (run-mode-hooks 'magit-mode-hook))

(defun magit-mode-init (dir submode refresh-func &rest refresh-args)
  (setq default-directory dir
	magit-submode submode
	magit-refresh-function refresh-func
	magit-refresh-args refresh-args)
  (magit-mode)
  (magit-refresh-buffer))

(defun magit-find-buffer (submode &optional dir)
  (let ((topdir (magit-get-top-dir (or dir default-directory))))
    (dolist (buf (buffer-list))
      (if (save-excursion
	    (set-buffer buf)
	    (and default-directory
		 (equal (expand-file-name default-directory) topdir)
		 (eq major-mode 'magit-mode)
		 (eq magit-submode submode)))
	  (return buf)))))

(defun magit-find-status-buffer (&optional dir)
  (magit-find-buffer 'status dir))

(defun magit-for-all-buffers (func &optional dir)
  (dolist (buf (buffer-list))
    (save-excursion
      (set-buffer buf)
      (if (and (eq major-mode 'magit-mode)
	       (or (null dir)
		   (equal default-directory dir)))
	  (funcall func)))))

(defun magit-refresh-buffer (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let* ((old-line (line-number-at-pos))
	   (old-section (magit-current-section))
	   (old-path (and old-section
			  (magit-section-path (magit-current-section))))
	   (section-line (and old-section
			      (count-lines
			       (magit-section-beginning old-section)
			       (point)))))
      (if magit-refresh-function
	  (apply magit-refresh-function
		 magit-refresh-args))
      (magit-refresh-marked-commits-in-buffer)
      (let ((s (and old-path (magit-find-section old-path magit-top-section))))
	(cond (s
	       (goto-char (magit-section-beginning s))
	       (forward-line section-line))
	      (t
	       (magit-goto-line old-line)))
	(dolist (w (get-buffer-window-list (current-buffer)))
	  (set-window-point w (point)))
	(magit-highlight-section)))))

(defun magit-revert-buffers ()
  (dolist (buffer (buffer-list))
    (when (and buffer
	       (not (verify-visited-file-modtime buffer))
	       (not (buffer-modified-p buffer)))
      (with-current-buffer buffer
	(ignore-errors
	  (revert-buffer t t t))))))

(defvar magit-refresh-needing-buffers nil)
(defvar magit-refresh-pending nil)

(defun magit-refresh-wrapper (func)
  (if magit-refresh-pending
      (funcall func)
    (let ((status-buffer (magit-find-buffer 'status default-directory))
	  (magit-refresh-needing-buffers nil)
	  (magit-refresh-pending t))
      (unwind-protect
	  (funcall func)
	(when magit-refresh-needing-buffers
	  (magit-revert-buffers)
	  (dolist (b (adjoin status-buffer
			     magit-refresh-needing-buffers))
	    (magit-refresh-buffer b)))))))

(defun magit-need-refresh (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (when (not (memq buffer magit-refresh-needing-buffers))
      (setq magit-refresh-needing-buffers
	    (cons buffer magit-refresh-needing-buffers)))))

(defun magit-refresh ()
  (interactive)
  (magit-with-refresh
    (magit-need-refresh)))

(defun magit-refresh-all ()
  (interactive)
  (magit-for-all-buffers #'magit-refresh-buffer default-directory))

;;; Untracked files

(defun magit-wash-untracked-file ()
  (if (looking-at "^? \\(.*\\)$")
      (let ((file (match-string-no-properties 1)))
	(delete-region (point) (+ (line-end-position) 1))
	(magit-with-section file 'file
	  (magit-set-section-info file)
	  (insert "\t" file "\n"))
	t)
    nil))

(defun magit-wash-untracked-files ()
  (magit-wash-sequence #'magit-wash-untracked-file))

(defun magit-insert-untracked-files ()
  (magit-git-section 'untracked "Untracked files:"
		     'magit-wash-untracked-files
		     "ls-files" "-t" "--others" "--exclude-standard"))

;;; Diffs and Hunks

(defvar magit-diff-context-lines 3)

(defun magit-diff-U-arg ()
  (format "-U%d" magit-diff-context-lines))

(defun magit-diff-smaller-hunks (&optional count)
  (interactive "p")
  (setq magit-diff-context-lines (max 0 (- magit-diff-context-lines count)))
  (magit-refresh))

(defun magit-diff-larger-hunks (&optional count)
  (interactive "p")
  (setq magit-diff-context-lines (+ magit-diff-context-lines count))
  (magit-refresh))

(defun magit-diff-default-hunks ()
  (interactive "")
  (setq magit-diff-context-lines 3)
  (magit-refresh))

(defun magit-diff-line-file ()
  (cond ((looking-at "^diff --git ./\\(.*\\) ./\\(.*\\)$")
	 (match-string-no-properties 2))
	((looking-at "^diff --cc +\\(.*\\)$")
	 (match-string-no-properties 1))
	(t
	 nil)))

(defun magit-wash-diffs ()
  (magit-wash-sequence #'magit-wash-diff-or-other-file))

(defun magit-wash-diff-or-other-file ()
  (or (magit-wash-diff)
      (magit-wash-other-file)))

(defun magit-wash-other-file ()
  (if (looking-at "^? \\(.*\\)$")
      (let ((file (match-string-no-properties 1)))
	(delete-region (point) (+ (line-end-position) 1))
	(magit-with-section file 'file
	  (magit-set-section-info file)
	  (insert "\tNew      " file "\n"))
	t)
    nil))

(defvar magit-hide-diffs nil)

(defun magit-wash-diff ()
  (cond ((looking-at "^diff")
	 (let ((magit-section-hidden-default magit-hide-diffs))
	   (magit-with-section (magit-current-line) 'diff
	     (let ((file (magit-diff-line-file))
		   (end (save-excursion
			  (forward-line) ;; skip over "diff" line
			  (if (search-forward-regexp "^diff\\|^@@" nil t)
			      (goto-char (match-beginning 0))
			    (goto-char (point-max)))
			  (point-marker))))
	       (let* ((status (cond
			       ((looking-at "^diff --cc")
				'unmerged)
			       ((save-excursion
				  (search-forward-regexp "^new file" end t))
				'new)
			       ((save-excursion
				  (search-forward-regexp "^new mode" end t))
				'mode)
			       ((save-excursion
				  (search-forward-regexp "^deleted" end t))
				'deleted)
			       ((save-excursion
				  (search-forward-regexp "^rename" end t))
				'renamed)
			       (t
				'modified)))
		      (file2 (cond
			      ((save-excursion
				 (search-forward-regexp "^rename from \\(.*\\)"
							end t))
			       (match-string-no-properties 1))))
		      (status-text (case status
				     ((unmerged)
				      (format "Unmerged %s" file))
				     ((new)
				      (format "New      %s" file))
				     ((mode)
				      (format "New mode %s" file))
				     ((deleted)
				      (format "Deleted  %s" file))
				     ((renamed)
				      (format "Renamed  %s   (from %s)"
					      file file2))
				     ((modified)
				      (format "Modified %s" file))
				     (
				      (format "?        %s" file)))))
		 (magit-set-section-info (list status file file2))
		 (insert "\t" status-text "\n")
		 (goto-char end)
		 (let ((magit-section-hidden-default nil))
		   (magit-wash-sequence #'magit-wash-hunk))
		 t)))))
	(t
	 nil)))

(defun magit-diff-item-kind (diff)
  (car (magit-section-info diff)))

(defun magit-diff-item-file (diff)
  (cadr (magit-section-info diff)))

(defun magit-diff-item-file2 (diff)
  (caddr (magit-section-info diff)))

(defun magit-wash-hunk ()
  (cond ((looking-at "\\(^@+\\)[^@]*@+")
	 (let ((n-columns (1- (length (match-string 1))))
	       (head (match-string 0)))
	   (magit-with-section head 'hunk
	     (magit-put-line-property 'face 'magit-diff-hunk-header)
	     (forward-line)
	     (while (not (or (eobp)
			     (looking-at "^diff\\|^@@")))
	       (let ((prefix (buffer-substring-no-properties
			      (point) (min (+ (point) n-columns) (point-max)))))
		 (cond ((string-match "\\+" prefix)
			(magit-put-line-property 'face 'magit-diff-add))
		       ((string-match "-" prefix)
			(magit-put-line-property 'face 'magit-diff-del))
		       (t
			(magit-put-line-property 'face 'magit-diff-none))))
	       (forward-line))))
	 t)
	(t
	 nil)))

(defun magit-hunk-item-diff (hunk)
  (let ((diff (magit-section-parent hunk)))
    (or (eq (magit-section-type diff) 'diff)
	(error "Huh?  Parent of hunk not a diff."))
    diff))

(defun magit-diff-item-insert-header (diff buf)
  (let ((beg (save-excursion
	       (goto-char (magit-section-beginning diff))
	       (forward-line)
	       (point)))
	(end (if (magit-section-children diff)
		 (magit-section-beginning (car (magit-section-children diff)))
	       (magit-section-end diff))))
    (magit-insert-region beg end buf)))

(defun magit-insert-diff-item-patch (diff buf)
  (let ((beg (save-excursion
	       (goto-char (magit-section-beginning diff))
	       (forward-line)
	       (point)))
	(end (magit-section-end diff)))
    (magit-insert-region beg end buf)))

(defun magit-insert-hunk-item-patch (hunk buf)
  (magit-diff-item-insert-header (magit-hunk-item-diff hunk) buf)
  (magit-insert-region (magit-section-beginning hunk) (magit-section-end hunk)
		       buf))

(defun magit-insert-hunk-item-region-patch (hunk reverse beg end buf)
  (magit-diff-item-insert-header (magit-hunk-item-diff hunk) buf)
  (save-excursion
    (goto-char (magit-section-beginning hunk))
    (magit-insert-current-line buf)
    (forward-line)
    (let ((copy-op (if reverse "+" "-")))
      (while (< (point) (magit-section-end hunk))
	(if (and (<= beg (point)) (< (point) end))
	    (magit-insert-current-line buf)
	  (cond ((looking-at " ")
		 (magit-insert-current-line buf))
		((looking-at copy-op)
		 (let ((text (buffer-substring-no-properties
			      (+ (point) 1) (line-beginning-position 2))))
		   (with-current-buffer buf
		     (insert " " text))))))
	(forward-line))))
  (with-current-buffer buf
    (diff-fixup-modifs (point-min) (point-max))))

(defun magit-hunk-item-is-conflict-p (hunk)
  ;;; XXX - Using the title is a bit too clever...
  (string-match "^diff --cc"
		(magit-section-title (magit-hunk-item-diff hunk))))

(defun magit-hunk-item-target-line (hunk)
  (save-excursion
    (beginning-of-line)
    (let ((line (line-number-at-pos)))
      (if (looking-at "-")
	  (error "Can't visit removed lines."))
      (goto-char (magit-section-beginning hunk))
      (if (not (looking-at "@@+ .* \\+\\([0-9]+\\),[0-9]+ @@+"))
	  (error "Hunk header not found."))
      (let ((target (parse-integer (match-string 1))))
	(forward-line)
	(while (< (line-number-at-pos) line)
	  ;; XXX - deal with combined diffs
	  (if (not (looking-at "-"))
	      (setq target (+ target 1)))
	  (forward-line))
	target))))

(defun magit-apply-diff-item (diff &rest args)
  (when (zerop magit-diff-context-lines)
    (setq args (cons "--unidiff-zero" args)))
  (with-current-buffer (get-buffer-create "*magit-tmp*")
    (erase-buffer))
  (magit-insert-diff-item-patch diff "*magit-tmp*")
  (apply #'magit-run-git "apply" (append args (list "-"))))

(defun magit-apply-hunk-item* (hunk reverse &rest args)
  (when (zerop magit-diff-context-lines)
    (setq args (cons "--unidiff-zero" args)))
  (let ((tmp (get-buffer-create "*magit-tmp*")))
    (with-current-buffer tmp
      (erase-buffer))
    (if (magit-use-region-p)
	(magit-insert-hunk-item-region-patch
	 hunk reverse (region-beginning) (region-end) tmp)
      (magit-insert-hunk-item-patch hunk tmp))
    (apply #'magit-run-git-with-input tmp
	   "apply" (append args (list "-")))))

(defun magit-apply-hunk-item (hunk &rest args)
  (apply #'magit-apply-hunk-item* hunk nil args))

(defun magit-apply-hunk-item-reverse (hunk &rest args)
  (apply #'magit-apply-hunk-item* hunk t (cons "--reverse" args)))

(defun magit-insert-unstaged-changes (title)
  (let ((magit-hide-diffs t))
    (magit-git-section 'unstaged title 'magit-wash-diffs
		       "diff" (magit-diff-U-arg))))

(defun magit-insert-staged-changes (no-commit)
  (let ((magit-hide-diffs t))
    (if no-commit
        (let ((null-tree (magit-git-string "mktree </dev/null")))
          (magit-git-section 'staged "Staged changes:" 'magit-wash-diffs
			     "diff" "--cached" (magit-diff-U-arg)
			     null-tree))
      (magit-git-section 'staged "Staged changes:" 'magit-wash-diffs
			 "diff" "--cached" (magit-diff-U-arg)))))

;;; Logs and Commits

(defun magit-parse-log-ref (refname)
  "Return shortened and propertized version of full REFNAME, like
\"refs/remotes/origin/master\"."
  (let ((face 'magit-log-head-label))
    (cond ((string-match "^\\(tag: +\\)?refs/tags/\\(.+\\)" refname)
	   (setq refname (match-string 2 refname)
		 face 'magit-log-tag-label))
	  ((string-match "^refs/remotes/\\(.+\\)" refname)
	   (setq refname (match-string 1 refname)))
	  ((string-match "[^/]+$" refname)
	   (setq refname (match-string 0 refname))))
    (propertize refname 'face face)))

(defun magit-parse-log-refs (refstring)
  "Parse REFSTRING annotation from `git log --decorate'
output (for example: \"refs/remotes/origin/master,
refs/heads/master\") and return prettified string for displaying
in log buffer."
  (mapconcat 'identity
	     (mapcar 'magit-parse-log-ref
		     (remove-if (lambda (refname)
				  (string-match "/HEAD$" refname))
				(reverse (split-string refstring ", *" t))))
	     " - "))

(defun magit-wash-log-line ()
  (if (and (search-forward-regexp "[0-9a-fA-F]\\{40\\}" (line-end-position) t)
	   (goto-char (match-beginning 0))
	   (not (looking-back "commit ")))
      (let ((commit (match-string-no-properties 0)))
	(delete-region (match-beginning 0) (match-end 0))
	(fixup-whitespace)
	(goto-char (line-beginning-position))
	(when (search-forward-regexp "^[|*\\/ ]+\\((\\(tag:.+?\\|refs/.+?\\))\\)"
				     (line-end-position) t)
	  (let ((refstring (match-string-no-properties 2)))
	    (delete-region (match-beginning 1) (match-end 1))
	    (insert (magit-parse-log-refs refstring)))
	  (goto-char (line-beginning-position)))
	(magit-with-section commit 'commit
	  (magit-set-section-info commit)
	  (forward-line)))
    (forward-line))
  t)

(defun magit-wash-log ()
  (let ((magit-old-top-section nil))
    (magit-wash-sequence #'magit-wash-log-line)))

(defvar magit-currently-shown-commit nil)

(defun magit-wash-commit ()
  (cond ((search-forward-regexp "^diff" nil t)
	 (goto-char (match-beginning 0))
	 (magit-wash-diffs))))

(defun magit-refresh-commit-buffer (commit)
  (magit-create-buffer-sections
    (magit-git-section nil nil
		       'magit-wash-commit
		       "log" "--max-count=1"
		       "--pretty=medium"
		       "--cc" "-p" commit)))

(defun magit-show-commit (commit &optional scroll)
  (when (magit-section-p commit)
    (setq commit (magit-section-info commit)))
  (let ((dir default-directory)
	(buf (get-buffer-create "*magit-commit*")))
    (cond ((equal magit-currently-shown-commit commit)
	   (let ((win (get-buffer-window buf)))
	     (cond ((not win)
		    (display-buffer buf))
		   (scroll
		    (with-selected-window win
		      (funcall scroll))))))
	  (t
	   (setq magit-currently-shown-commit commit)
	   (display-buffer buf)
	   (with-current-buffer buf
	     (set-buffer buf)
	     (goto-char (point-min))
	     (magit-mode-init dir 'commit
			      #'magit-refresh-commit-buffer commit))))))

(defvar magit-marked-commit nil)

(defvar magit-mark-overlay nil)
(make-variable-buffer-local 'magit-mark-overlay)
(put 'magit-mark-overlay 'permanent-local t)

(defun magit-refresh-marked-commits ()
  (magit-for-all-buffers #'magit-refresh-marked-commits-in-buffer))

(defun magit-refresh-marked-commits-in-buffer ()
  (if (not magit-mark-overlay)
      (let ((ov (make-overlay 1 1)))
	(overlay-put ov 'face 'magit-item-mark)
	(setq magit-mark-overlay ov)))
  (delete-overlay magit-mark-overlay)
  (magit-for-all-sections
   (lambda (section)
     (when (and (eq (magit-section-type section) 'commit)
		(equal (magit-section-info section)
		       magit-marked-commit))
       (move-overlay magit-mark-overlay
		     (magit-section-beginning section)
		     (magit-section-end section)
		     (current-buffer))))))

(defun magit-set-marked-commit (commit)
  (setq magit-marked-commit commit)
  (magit-refresh-marked-commits))

(defun magit-marked-commit ()
  (or magit-marked-commit
      (error "No commit marked")))

(defun magit-insert-unpulled-commits (remote branch)
  (magit-git-section 'unpulled
		     "Unpulled commits:" 'magit-wash-log
		     "log" "--pretty=format:* %H %s"
		     (format "HEAD..%s/%s" remote branch)))

(defun magit-insert-unpushed-commits (remote branch)
  (magit-git-section 'unpushed
		     "Unpushed commits:" 'magit-wash-log
		     "log" "--pretty=format:* %H %s"
		     (format "%s/%s..HEAD" remote branch)))

(defun magit-insert-unpulled-svn-commits ()
  (magit-git-section 'svn-unpulled
		     "Unpulled commits (SVN):" 'magit-wash-log
		     "log" "--pretty=format:* %H %s"
		     (format "HEAD..remotes/%s" (magit-get-svn-branch-name))))

(defun magit-insert-unpushed-svn-commits ()
  (magit-git-section 'svn-unpushed
		     "Unpushed commits (SVN):" 'magit-wash-log
		     "log" "--pretty=format:* %H %s"
		     (format "remotes/%s..HEAD" (magit-get-svn-branch-name))))

;;; Status

(defun magit-refresh-status ()
  (magit-create-buffer-sections
    (magit-with-section 'status nil
      (let* ((branch (magit-get-current-branch))
	     (remote (and branch (magit-get "branch" branch "remote")))
             (svn-enabled (magit-svn-enabled))
	     (head (magit-git-string
		    "log --max-count=1 --abbrev-commit --pretty=oneline"))
	     (no-commit (string-match "fatal: bad default revision" head)))
	(if remote
	    (insert (format "Remote: %s %s\n"
			    remote (magit-get "remote" remote "url"))))
	(insert (format "Local:  %s %s\n"
			(propertize (or branch "(detached)")
				    'face 'magit-branch)
			(abbreviate-file-name default-directory)))
        (insert (format "Head:   %s\n"
                        (if no-commit "nothing commited (yet)" head)))
	(let ((merge-heads (magit-file-lines ".git/MERGE_HEAD")))
	  (if merge-heads
	      (insert (format "Merging: %s\n"
			      (magit-concat-with-delim
			       ", "
			       (mapcar 'magit-name-rev merge-heads))))))
	(let ((rebase (magit-rebase-info)))
	  (if rebase
	      (insert (apply 'format "Rebasing: %s (%s of %s)\n" rebase))))
	(insert "\n")
	(magit-insert-untracked-files)
	(magit-insert-stashes)
	(magit-insert-pending-changes)
	(magit-insert-pending-commits)
	(when remote
	  (magit-insert-unpulled-commits remote branch))
        (when svn-enabled
          (magit-insert-unpulled-svn-commits))
	(let ((staged (or no-commit (magit-anything-staged-p))))
	  (magit-insert-unstaged-changes
	   (if staged "Unstaged changes:" "Changes:"))
	  (if staged
	      (magit-insert-staged-changes no-commit)))
	(when remote
	  (magit-insert-unpushed-commits remote branch))
        (when svn-enabled
          (magit-insert-unpushed-svn-commits))))))

(defun magit-init (dir)
  "Initialize git repository in specified directory"
  (interactive (list (read-directory-name "Directory for Git repository: ")))
  (let ((topdir (magit-get-top-dir dir)))
    (when (or (not topdir)
	      (yes-or-no-p
	       (format
		(if (string-equal topdir (expand-file-name dir))
		    "There is already a Git repository in %s. Reinitialize? "
		  "There is a Git repository in %s. Create another in %s? ")
		topdir dir)))
      (unless (file-directory-p dir)
	(and (y-or-n-p (format "Directory %s does not exists. Create it? " dir))
	     (make-directory dir)))
      (let ((default-directory dir))
	(magit-run* (list "git" "init"))))))

(defun magit-status (dir)
  (interactive (list (or (and (not current-prefix-arg)
			      (magit-get-top-dir default-directory))
			 (magit-read-top-dir))))
  (if magit-save-some-buffers
      (save-some-buffers (eq magit-save-some-buffers 'dontask)))
  (let ((topdir (magit-get-top-dir dir)))
    (unless topdir
      (when (y-or-n-p (format "There is no Git repository in %S. Create one? "
			      dir))
	(magit-init dir)
	(setq topdir (magit-get-top-dir dir))))
    (when topdir
      (let ((buf (or (magit-find-buffer 'status topdir)
                     (switch-to-buffer
                      (get-buffer-create
                       (concat "*magit: "
                               (file-name-nondirectory
                                (directory-file-name topdir)) "*"))))))
        (switch-to-buffer buf)
        (magit-mode-init topdir 'status #'magit-refresh-status)))))


;;; Staging and Unstaging

(defun magit-stage-item ()
  "Add the item at point to the staging area."
  (interactive)
  (magit-section-action (item info "stage")
    ((untracked file)
     (magit-run-git "add" info))
    ((unstaged diff hunk)
     (if (magit-hunk-item-is-conflict-p item)
	 (error (concat "Can't stage individual resolution hunks.  "
			"Please stage the whole file.")))
     (magit-apply-hunk-item item "--cached"))
    ((unstaged diff)
     (magit-run-git "add" "-u" (magit-diff-item-file item)))
    ((staged *)
     (error "Already staged"))
    ((hunk)
     (error "Can't stage this hunk"))
    ((diff)
     (error "Can't stage this diff"))))

(defun magit-unstage-item ()
  "Remove the item at point from the staging area."
  (interactive)
  (magit-section-action (item info "unstage")
    ((staged diff hunk)
     (magit-apply-hunk-item-reverse item "--cached"))
    ((staged diff)
     (magit-run-git "reset" "-q" "HEAD" "--" (magit-diff-item-file item)))
    ((unstaged *)
     (error "Already unstaged"))
    ((hunk)
     (error "Can't unstage this hunk"))
    ((diff)
     (error "Can't unstage this diff"))))

(defun magit-stage-all ()
  (interactive)
  (magit-run-git "add" "-u" "."))

(defun magit-unstage-all ()
  (interactive)
  (magit-run-git "reset" "HEAD"))

;;; Branches

(defun magit-checkout (rev)
  (interactive (list (magit-read-rev "Switch to" (magit-default-rev))))
  (if rev
      (magit-run-git "checkout" (magit-rev-to-git rev))))

(defun magit-read-create-branch-args ()
  (let* ((cur-branch (magit-get-current-branch))
	 (branch (read-string "Create branch: "))
	 (parent (magit-read-rev "Parent" cur-branch)))
    (list branch parent)))

(defun magit-create-branch (branch parent)
  (interactive (magit-read-create-branch-args))
  (if (and branch (not (string= branch ""))
	   parent)
      (magit-run-git "checkout" "-b"
		     branch
		     (magit-rev-to-git parent))))

;;; Merging

(defun magit-guess-branch ()
  (let ((sec (magit-current-section)))
    (if (and sec (eq (magit-section-type sec) 'wazzup))
	(magit-section-info sec))))

(defun magit-manual-merge (rev)
  (interactive (list (magit-read-rev "Manually merge" (magit-guess-branch))))
  (if rev
      (magit-run-git "merge" "--no-ff" "--no-commit"
		     (magit-rev-to-git rev))))

(defun magit-automatic-merge (rev)
  (interactive (list (magit-read-rev "Merge" (magit-guess-branch))))
  (if rev
      (magit-run-git "merge" (magit-rev-to-git rev))))

;;; Rebasing

(defun magit-rebase-info ()
  (cond ((file-exists-p ".dotest")
	 (list (magit-name-rev (car (magit-file-lines ".dotest/onto")))
	       (car (magit-file-lines ".dotest/next"))
	       (car (magit-file-lines ".dotest/last"))))
	((file-exists-p ".git/.dotest-merge")
	 (list (car (magit-file-lines ".git/.dotest-merge/onto_name"))
	       (car (magit-file-lines ".git/.dotest-merge/msgnum"))
	       (car (magit-file-lines ".git/.dotest-merge/end"))))
	(t
	 nil)))

(defun magit-rebase-step ()
  (interactive)
  (let ((info (magit-rebase-info)))
    (if (not info)
	(let ((rev (magit-read-rev "Rebase to")))
	  (if rev
	      (magit-run-git "rebase" (magit-rev-to-git rev))))
      (let ((cursor-in-echo-area t)
	    (message-log-max nil))
	(message "Rebase in progress.  Abort, Skip, or Continue? ")
	(let ((reply (read-event)))
	  (case reply
	    ((?A ?a)
	     (magit-run-git "rebase" "--abort"))
	    ((?S ?s)
	     (magit-run-git "rebase" "--skip"))
	    ((?C ?c)
	     (magit-run-git "rebase" "--continue"))))))))

;; git svn commands

(defun magit-svn-rebase ()
  (interactive)
  (magit-run-git-async "svn" "rebase"))

(defun magit-svn-dcommit ()
  (interactive)
  (magit-run-git-async "svn" "dcommit"))

(defun magit-svn-enabled ()
   (not (null (magit-get-svn-branch-name))))

(defun magit-get-svn-branch-name ()
  (or (find "git-svn" (magit-list-interesting-revisions)
	    :test 'equal)
      (find "trunk" (magit-list-interesting-revisions)
	    :test 'equal)))

;;; Resetting

(defun magit-reset-head (rev &optional hard)
  (interactive (list (magit-read-rev (format "%s head to"
					     (if current-prefix-arg
						 "Hard reset"
					       "Reset"))
				     (or (magit-default-rev)
					 "HEAD^"))
		     current-prefix-arg))
  (if rev
      (magit-run-git "reset" (if hard "--hard" "--soft")
		     (magit-rev-to-git rev))))

(defun magit-reset-working-tree ()
  (interactive)
  (if (yes-or-no-p "Discard all uncommitted changes? ")
      (magit-run-git "reset" "--hard")))

;;; Rewriting

(defun magit-read-rewrite-info ()
  (when (file-exists-p ".git/magit-rewrite-info")
    (with-temp-buffer
      (insert-file-contents ".git/magit-rewrite-info")
      (goto-char (point-min))
      (read (current-buffer)))))

(defun magit-write-rewrite-info (info)
  (with-temp-file ".git/magit-rewrite-info"
    (prin1 info (current-buffer))
    (princ "\n" (current-buffer))))

(defun magit-insert-pending-commits ()
  (let* ((info (magit-read-rewrite-info))
	 (pending (cdr (assq 'pending info))))
    (when pending
      (magit-with-section 'pending nil
	(insert (propertize "Pending commits:\n"
			    'face 'magit-section-title))
	(dolist (p pending)
	  (let* ((commit (car p))
		 (properties (cdr p))
		 (used (plist-get properties 'used)))
	    (magit-with-section commit 'commit
	      (magit-set-section-info commit)
	      (insert (magit-git-string
		       "log --max-count=1 --pretty=format:%s %s --"
		       (if used ". %s" "* %s")
		       commit)
		      "\n")))))
      (insert "\n"))))

(defun magit-rewrite-set-commit-property (commit prop value)
  (let* ((info (magit-read-rewrite-info))
	 (pending (cdr (assq 'pending info)))
	 (p (assoc commit pending)))
    (when p
      (setf (cdr p) (plist-put (cdr p) prop value))
      (magit-write-rewrite-info info)
      (magit-need-refresh))))

(defun magit-rewrite-set-used ()
  (interactive)
  (magit-section-action (item info)
    ((pending commit)
     (magit-rewrite-set-commit-property info 'used t))))

(defun magit-rewrite-set-unused ()
  (interactive)
  (magit-section-action (item info)
    ((pending commit)
     (magit-rewrite-set-commit-property info 'used nil))))

(defun magit-insert-pending-changes ()
  (let* ((info (magit-read-rewrite-info))
	 (orig (cadr (assq 'orig info))))
    (when orig
      (let ((magit-hide-diffs t))
	(magit-git-section 'pending-changes
			   "Pending changes"
			   'magit-wash-diffs
			   "diff" (magit-diff-U-arg) "-R" orig)))))

(defun magit-rewrite-start (from &optional onto)
  (interactive (list (magit-read-rev "Rewrite from" (magit-default-rev))))
  (or (magit-everything-clean-p)
      (error "You have uncommitted changes."))
  (or (not (magit-read-rewrite-info))
      (error "Rewrite in progress."))
  (let* ((orig (magit-git-string "rev-parse HEAD"))
	 (base (magit-git-string "rev-parse %s^" from))
	 (pending (magit-git-lines "rev-list %s.." base)))
    (magit-write-rewrite-info `((orig ,orig)
				(pending ,@(mapcar #'list pending))))
    (magit-run-git "reset" "--hard" base)))

(defun magit-rewrite-stop (&optional noconfirm)
  (interactive)
  (let* ((info (magit-read-rewrite-info)))
    (or info
	(error "No rewrite in progress."))
    (when (or noconfirm
	      (yes-or-no-p "Stop rewrite? "))
      (magit-write-rewrite-info nil)
      (magit-need-refresh))))

(defun magit-rewrite-abort ()
  (interactive)
  (let* ((info (magit-read-rewrite-info))
	 (orig (cadr (assq 'orig info))))
    (or info
	(error "No rewrite in progress."))
    (or (magit-everything-clean-p)
	(error "You have uncommitted changes."))
    (when (yes-or-no-p "Abort rewrite? ")
      (magit-write-rewrite-info nil)
      (magit-run-git "reset" "--hard" orig))))

(defun magit-rewrite-finish ()
  (interactive)
  (magit-with-refresh
    (magit-rewrite-finish-step t)))

(defun magit-rewrite-finish-step (first-p)
  (let ((info (magit-read-rewrite-info)))
    (or info
	(error "No rewrite in progress."))
    (let* ((pending (cdr (assq 'pending info)))
	   (first-unused (find-if (lambda (p)
				    (not (plist-get (cdr p) 'used)))
				  pending
				  :from-end t))
	   (commit (car first-unused)))
      (cond ((not first-unused)
	     (magit-rewrite-stop t))
	    ((magit-apply-commit commit t (not first-p))
	     (magit-rewrite-set-commit-property commit 'used t)
	     (magit-rewrite-finish-step nil))))))

;;; Updating, pull, and push

(defun magit-remote-update ()
  (interactive)
  (if (magit-svn-enabled)
      (magit-run-git-async "svn" "fetch")
    (magit-run-git-async "remote" "update")))

(defun magit-pull ()
  (interactive)
  (magit-run-git-async "pull" "-v"))

(defun magit-read-remote (prompt def)
  (completing-read (if def
		       (format "%s (default %s): " prompt def)
		     (format "%s: " prompt))
		   (magit-git-lines "remote")
		   nil nil nil nil def))

(defun magit-push ()
  (interactive)
  (let* ((branch (or (magit-get-current-branch)
		     (error "Don't push a detached head. That's gross.")))
	 (branch-remote (magit-get "branch" branch "remote"))
	 (push-remote (if (or current-prefix-arg
			      (not branch-remote))
			  (magit-read-remote (format "Push %s to" branch)
					     branch-remote)
			branch-remote)))
    (if (and (not branch-remote)
	     (not current-prefix-arg))
	(magit-set push-remote "branch" branch "remote"))
    (magit-run-git-async "push" "-v" push-remote branch)))

;;; Log edit mode

(defvar magit-log-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'magit-log-edit-commit)
    (define-key map (kbd "C-c C-a") 'magit-log-edit-toggle-amending)
    (define-key map (kbd "M-p") 'log-edit-previous-comment)
    (define-key map (kbd "M-n") 'log-edit-next-comment)
    (define-key map (kbd "C-c C-k") 'magit-log-edit-cancel-log-message)
    map))

(defvar magit-pre-log-edit-window-configuration nil)

(defun magit-log-fill-paragraph (&optional justify)
  "Fill the paragraph, but preserve open parentheses at beginning of lines.
Prefix arg means justify as well."
  (interactive "P")
  ;; Add lines starting with a left paren or an asterisk.
  (let ((paragraph-start (concat paragraph-start "\\|*\\|(")))
    (let ((end (progn (forward-paragraph) (point)))
	  (beg (progn (backward-paragraph) (point)))
	  (adaptive-fill-mode nil))
      (fill-region beg end justify)
      t)))

(define-derived-mode magit-log-edit-mode text-mode "Magit Log Edit"
  (set (make-local-variable 'fill-paragraph-function)
       'magit-log-fill-paragraph)
  (use-local-map magit-log-edit-map))

(defun magit-log-edit-cleanup ()
  (save-excursion
    (goto-char (point-min))
    (flush-lines "^#")
    (goto-char (point-min))
    (if (re-search-forward "[ \t\n]*\\'" nil t)
	(replace-match "\n" nil nil))))

(defun magit-log-edit-append (str)
  (save-excursion
    (set-buffer (get-buffer-create "*magit-log-edit*"))
    (goto-char (point-max))
    (insert str "\n")))

(defconst magit-log-header-end "-- End of Magit header --\n")

(defun magit-log-edit-get-fields ()
  (let ((buf (get-buffer "*magit-log-edit*"))
	(result nil))
    (if buf
	(save-excursion
	  (set-buffer buf)
	  (goto-char (point-min))
	  (while (looking-at "^\\([A-Za-z0-9-_]+\\): *\\(.*\\)$")
	    (setq result (acons (intern (downcase (match-string 1)))
				(match-string 2)
				result))
	    (forward-line))
	  (if (not (looking-at (regexp-quote magit-log-header-end)))
	      (setq result nil))))
    (nreverse result)))

(defun magit-log-edit-set-fields (fields)
  (let ((buf (get-buffer-create "*magit-log-edit*")))
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (if (search-forward-regexp (format "^\\([A-Za-z0-9-_]+:.*\n\\)+%s"
					 (regexp-quote magit-log-header-end))
				 nil t)
	  (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (when fields
	(while fields
	  (insert (capitalize (symbol-name (caar fields))) ": "
		  (cdar fields) "\n")
	  (setq fields (cdr fields)))
	(insert magit-log-header-end)))))

(defun magit-log-edit-set-field (name value)
  (let* ((fields (magit-log-edit-get-fields))
	 (cell (assq name fields)))
    (cond (cell
	   (if value
	       (rplacd cell value)
	     (setq fields (delq cell fields))))
	  (t
	   (if value
	       (setq fields (append fields (list (cons name value)))))))
    (magit-log-edit-set-fields fields)))

(defun magit-log-edit-get-field (name)
  (cdr (assq name (magit-log-edit-get-fields))))

(defun magit-log-edit-setup-author-env (author)
  (cond (author
	 ;; XXX - this is a bit strict, probably.
	 (or (string-match "\\(.*\\) <\\(.*\\)>, \\(.*\\)" author)
	     (error "Can't parse author string."))
	 ;; Shucks, setenv destroys the match data.
	 (let ((name (match-string 1 author))
	       (email (match-string 2 author))
	       (date  (match-string 3 author)))
	   (setenv "GIT_AUTHOR_NAME" name)
	   (setenv "GIT_AUTHOR_EMAIL" email)
	   (setenv "GIT_AUTHOR_DATE" date)))
	(t
	 (setenv "GIT_AUTHOR_NAME")
	 (setenv "GIT_AUTHOR_EMAIL")
	 (setenv "GIT_AUTHOR_DATE"))))

(defun magit-log-edit-push-to-comment-ring (comment)
  (when (or (ring-empty-p log-edit-comment-ring)
	    (not (equal comment (ring-ref log-edit-comment-ring 0))))
    (ring-insert log-edit-comment-ring comment)))

(defun magit-log-edit-commit ()
  (interactive)
  (let* ((fields (magit-log-edit-get-fields))
	 (amend (equal (cdr (assq 'amend fields)) "yes"))
	 (commit-all (equal (cdr (assq 'commit-all fields)) "yes"))
	 (tag (cdr (assq 'tag fields)))
	 (author (cdr (assq 'author fields))))
    (magit-log-edit-push-to-comment-ring (buffer-string))
    (magit-log-edit-setup-author-env author)
    (magit-log-edit-set-fields nil)
    (magit-log-edit-cleanup)
    (if (= (buffer-size) 0)
	(insert "(Empty description)\n"))
    (let ((commit-buf (current-buffer)))
      (with-current-buffer (magit-find-buffer 'status default-directory)
	(cond (tag
	       (magit-run-git-with-input commit-buf "tag" tag "-a" "-F" "-"))
	      (t
	       (apply #'magit-run-async-with-input commit-buf
		      magit-git-executable
		      (append magit-git-standard-options
			      (list "commit" "-F" "-")
			      (if commit-all '("--all") '())
			      (if amend '("--amend") '())
			      (if magit-commit-signoff
				  '("--signoff") '())))))))
    (erase-buffer)
    (bury-buffer)
    (when magit-pre-log-edit-window-configuration
      (set-window-configuration magit-pre-log-edit-window-configuration)
      (setq magit-pre-log-edit-window-configuration nil))))

(defun magit-log-edit-cancel-log-message ()
  (interactive)
  (when (or (not magit-log-edit-confirm-cancellation)
	    (yes-or-no-p
	     "Really cancel editing the log (any changes will be lost)?"))
    (erase-buffer)
    (bury-buffer)
    (when magit-pre-log-edit-window-configuration
      (set-window-configuration magit-pre-log-edit-window-configuration)
      (setq magit-pre-log-edit-window-configuration nil))))

(defun magit-log-edit-toggle-amending ()
  (interactive)
  (let* ((fields (magit-log-edit-get-fields))
	 (cell (assq 'amend fields)))
    (if cell
	(rplacd cell (if (equal (cdr cell) "yes") "no" "yes"))
      (setq fields (acons 'amend "yes" fields))
      (magit-log-edit-append
       (magit-format-commit "HEAD" "%s%n%n%b")))
    (magit-log-edit-set-fields fields)))

(defun magit-pop-to-log-edit (operation)
  (let ((dir default-directory)
	(buf (get-buffer-create "*magit-log-edit*")))
    (setq magit-pre-log-edit-window-configuration
	  (current-window-configuration))
    (pop-to-buffer buf)
    (setq default-directory dir)
    (magit-log-edit-mode)
    (message "Type C-c C-c to %s (C-c C-k to cancel)." operation)))

(defun magit-log-edit ()
  (interactive)
  (magit-log-edit-set-field 'tag nil)
  (when (and magit-commit-all-when-nothing-staged
	     (not (magit-anything-staged-p)))
    (cond ((eq magit-commit-all-when-nothing-staged 'ask-stage)
	   (if (and (not (magit-everything-clean-p))
		    (y-or-n-p "Nothing staged. Stage everything now? "))
	       (magit-stage-all)))
	  ((not (magit-log-edit-get-field 'commit-all))
	   (magit-log-edit-set-field
	    'commit-all
	    (if (or (eq magit-commit-all-when-nothing-staged t)
		    (y-or-n-p "Nothing staged. Commit all unstaged changes? "))
		"yes" "no")))))
  (magit-pop-to-log-edit "commit"))

(defun magit-add-log ()
  (interactive)
  (let ((section (magit-current-section)))
    (let ((fun (if (eq (magit-section-type section) 'hunk)
		   (save-window-excursion
		     (save-excursion
		       (magit-visit-item)
		       (add-log-current-defun)))
		 nil))
	  (file (magit-diff-item-file
		 (cond ((eq (magit-section-type section) 'hunk)
			(magit-hunk-item-diff section))
		       ((eq (magit-section-type section) 'diff)
			section)
		       (t
			(error "No change at point"))))))
      (magit-log-edit)
      (goto-char (point-min))
      (cond ((not (search-forward-regexp (format "^\\* %s" (regexp-quote file))
					 nil t))
	     ;; No entry for file, create it.
	     (goto-char (point-max))
	     (insert (format "\n* %s" file))
	     (if fun
		 (insert (format " (%s)" fun)))
	     (insert ": "))
	    (fun
	     ;; found entry for file, look for fun
	     (let ((limit (or (save-excursion
				(and (search-forward-regexp "^\\* " nil t)
				     (match-beginning 0)))
			      (point-max))))
	       (cond ((search-forward-regexp (format "(.*\\<%s\\>.*):"
						     (regexp-quote fun))
					     limit t)
		      ;; found it, goto end of current entry
		      (if (search-forward-regexp "^(" limit t)
			  (backward-char 2)
			(goto-char limit)))
		     (t
		      ;; not found, insert new entry
		      (goto-char limit)
		      (if (bolp)
			  (open-line 1)
			(newline))
		      (insert (format "(%s): " fun))))))))))

;;; Tags

(defun magit-tag (name)
  (interactive "sNew tag name: ")
  (magit-run-git "tag" name))

(defun magit-annotated-tag (name)
  (interactive "sNew tag name: ")
  (magit-log-edit-set-field 'tag name)
  (magit-pop-to-log-edit "tag"))

;;; Stashing

(defun magit-wash-stash ()
  (if (search-forward-regexp "stash@{\\(.*\\)}" (line-end-position) t)
      (let ((stash (match-string-no-properties 0))
	    (name (match-string-no-properties 1)))
	(delete-region (match-beginning 0) (match-end 0))
	(goto-char (match-beginning 0))
	(fixup-whitespace)
	(goto-char (line-beginning-position))
	(insert name)
	(goto-char (line-beginning-position))
	(magit-with-section stash 'stash
	  (magit-set-section-info stash)
	  (forward-line)))
    (forward-line))
  t)

(defun magit-wash-stashes ()
  (let ((magit-old-top-section nil))
    (magit-wash-sequence #'magit-wash-stash)))

(defun magit-insert-stashes ()
  (magit-git-section 'stashes
		     "Stashes:" 'magit-wash-stashes
		     "stash" "list"))

(defun magit-stash (description)
  (interactive "sStash description: ")
  (magit-run-git "stash" "save" description))

(defun magit-stash-snapshot ()
  (interactive)
  (magit-with-refresh
    (magit-run-git "stash" "save"
		   (format-time-string "Snapshot taken at %Y-%m-%d %H:%M:%S"
				       (current-time)))
    (magit-run-git "stash" "apply" "stash@{0}")))

(defvar magit-currently-shown-stash nil)

(defun magit-show-stash (stash &optional scroll)
  (when (magit-section-p stash)
    (setq stash (magit-section-info stash)))
  (let ((dir default-directory)
	(buf (get-buffer-create "*magit-stash*")))
    (cond ((equal magit-currently-shown-stash stash)
	   (let ((win (get-buffer-window buf)))
	     (cond ((not win)
		    (display-buffer buf))
		   (scroll
		    (with-selected-window win
		      (funcall scroll))))))
	  (t
	   (setq magit-currently-shown-stash stash)
	   (display-buffer buf)
	   (with-current-buffer buf
	     (set-buffer buf)
	     (goto-char (point-min))
	     (let* ((range (cons (concat stash "^2^") stash))
		    (args (magit-rev-range-to-git range)))
	       (magit-mode-init dir 'diff #'magit-refresh-diff-buffer
				range args)))))))

;;; Commits

(defun magit-commit-at-point (&optional nil-ok-p)
  (let* ((section (magit-current-section))
	 (commit (and (eq (magit-section-type section) 'commit)
		      (magit-section-info section))))
    (if nil-ok-p
	commit
      (or commit
	  (error "No commit at point.")))))

(defun magit-apply-commit (commit &optional docommit noerase revert)
  (let* ((parent-id (magit-choose-parent-id commit "cherry-pick"))
	 (success (magit-run* `(,magit-git-executable
				,@magit-git-standard-options
				,(if revert "revert" "cherry-pick")
				,@(if parent-id
				      (list "-m" (number-to-string parent-id)))
				,@(if (not docommit) (list "--no-commit"))
				,commit)
			      nil noerase)))
    (when (or (not docommit) success)
      (cond (revert
	     (magit-log-edit-append
	      (magit-format-commit commit "Reverting \"%s\"")))
	    (t
	     (magit-log-edit-append
	      (magit-format-commit commit "%s%n%n%b"))
	     (magit-log-edit-set-field
	      'author
	      (magit-format-commit commit "%an <%ae>, %ai")))))
    success))

(defun magit-apply-item ()
  (interactive)
  (magit-section-action (item info "apply")
    ((pending commit)
     (magit-apply-commit info)
     (magit-rewrite-set-commit-property info 'used t))
    ((commit)
     (magit-apply-commit info))
    ((unstaged *)
     (error "Change is already in your working tree"))
    ((staged *)
     (error "Change is already in your working tree"))
    ((hunk)
     (magit-apply-hunk-item item))
    ((diff)
     (magit-apply-diff-item item))
    ((stash)
     (magit-run-git "stash" "apply" info))))

(defun magit-cherry-pick-item ()
  (interactive)
  (magit-section-action (item info "cherry-pick")
    ((pending commit)
     (magit-apply-commit info t)
     (magit-rewrite-set-commit-property info 'used t))
    ((commit)
     (magit-apply-commit info t))
    ((stash)
     (magit-run-git "stash" "pop" info))))

(defun magit-revert-item ()
  (interactive)
  (magit-section-action (item info "revert")
    ((pending commit)
     (magit-apply-commit info nil nil t)
     (magit-rewrite-set-commit-property info 'used nil))
    ((commit)
     (magit-apply-commit info nil nil t))
    ((hunk)
     (magit-apply-hunk-item-reverse item))
    ((diff)
     (magit-apply-diff-item item "--reverse"))))

(defvar magit-have-graph 'unset)
(defvar magit-have-decorate 'unset)
(make-variable-buffer-local 'magit-have-graph)
(put 'magit-have-graph 'permanent-local t)
(make-variable-buffer-local 'magit-have-decorate)
(put 'magit-have-decorate 'permanent-local t)

(defun magit-configure-have-graph ()
  (if (eq magit-have-graph 'unset)
      (let ((res (magit-git-exit-code "log --graph --max-count=0")))
	(setq magit-have-graph (eq res 0)))))

(defun magit-configure-have-decorate ()
  (if (eq magit-have-decorate 'unset)
      (let ((res (magit-git-exit-code "log --decorate --max-count=0")))
	(setq magit-have-decorate (eq res 0)))))

(defun magit-refresh-log-buffer (range style args)
  (magit-configure-have-graph)
  (magit-configure-have-decorate)
  (magit-create-buffer-sections
    (apply #'magit-git-section nil
	   (magit-rev-range-describe range "Commits")
	   'magit-wash-log
	   `("log"
	     ,(format "--max-count=%s" magit-log-cutoff-length)
	     ,style
	     ,@(if magit-have-decorate (list "--decorate"))
	     ,@(if magit-have-graph (list "--graph"))
	     ,args "--"))))

(defun magit-log (&optional arg)
  (interactive "P")
  (let* ((range (if arg
		    (magit-read-rev-range "Log" "HEAD")
		  "HEAD"))
	 (topdir (magit-get-top-dir default-directory))
	 (args (magit-rev-range-to-git range)))
    (switch-to-buffer "*magit-log*")
    (magit-mode-init topdir 'log #'magit-refresh-log-buffer range
		     "--pretty=oneline" args)))

(defun magit-log-long (&optional arg)
  (interactive "P")
  (let* ((range (if arg
		    (magit-read-rev-range "Long log" "HEAD")
		  "HEAD"))
	 (topdir (magit-get-top-dir default-directory))
	 (args (magit-rev-range-to-git range)))
    (switch-to-buffer "*magit-log*")
    (magit-mode-init topdir 'log #'magit-refresh-log-buffer range
		     "--stat" args)))

;;; Reflog

(defun magit-refresh-reflog-buffer (head args)
  (magit-create-buffer-sections
    (magit-git-section 'reflog
		       (format "Local history of head %s" head)
		       'magit-wash-log
		       "log" "--walk-reflogs"
		       (format "--max-count=%s" magit-log-cutoff-length)
		       "--pretty=oneline"
		       args)))

(defun magit-reflog (head)
  (interactive (list (magit-read-rev "Reflog of" "HEAD")))
  (if head
      (let* ((topdir (magit-get-top-dir default-directory))
	     (args (magit-rev-to-git head)))
	(switch-to-buffer "*magit-reflog*")
	(magit-mode-init topdir 'reflog
			 #'magit-refresh-reflog-buffer head args))))

(defun magit-reflog-head ()
  (interactive)
  (magit-reflog "HEAD"))

;;; Diffing

(defun magit-refresh-diff-buffer (range args)
  (magit-create-buffer-sections
    (magit-git-section 'diffbuf
		       (magit-rev-range-describe range "Changes")
		       'magit-wash-diffs
		       "diff" (magit-diff-U-arg) args)))

(defun magit-diff (range)
  (interactive (list (magit-read-rev-range "Diff")))
  (if range
      (let* ((dir default-directory)
	     (args (magit-rev-range-to-git range))
	     (buf (get-buffer-create "*magit-diff*")))
	(display-buffer buf)
	(save-excursion
	  (set-buffer buf)
	  (magit-mode-init dir 'diff #'magit-refresh-diff-buffer range args)))))

(defun magit-diff-working-tree (rev)
  (interactive (list (magit-read-rev "Diff with (default HEAD)")))
  (magit-diff (or rev "HEAD")))

(defun magit-diff-with-mark ()
  (interactive)
  (magit-diff (cons (magit-marked-commit)
		    (magit-commit-at-point))))

;;; Wazzup

(defun magit-wazzup-toggle-ignore (branch edit)
  (let ((ignore-file ".git/info/wazzup-exclude"))
    (if edit
	(setq branch (read-string "Branch to ignore for wazzup: " branch)))
    (let ((ignored (magit-file-lines ignore-file)))
      (cond ((member branch ignored)
	     (when (or (not edit)
		       (y-or-n-p "Branch %s is already ignored. Unignore?"))
	       (setq ignored (delete branch ignored))))
	    (t
	     (setq ignored (append ignored (list branch)))))
      (magit-write-file-lines ignore-file ignored)
      (magit-need-refresh))))

(defun magit-refresh-wazzup-buffer (head all)
  (magit-create-buffer-sections
    (magit-with-section 'wazzupbuf nil
      (insert (format "Wazzup, %s\n\n" head))
      (let* ((excluded (magit-file-lines ".git/info/wazzup-exclude"))
	     (all-branches (magit-git-lines "branch -a | cut -c3-"))
	     (branches (if all all-branches
			 (remove-if (lambda (b) (member b excluded))
				    all-branches)))
	     (reported (make-hash-table :test #'equal)))
	(dolist (b branches)
	  (let* ((hash (magit-git-string "rev-parse %s" b))
		 (reported-branch (gethash hash reported)))
	    (unless (or (and reported-branch
			     (string= (file-name-nondirectory b)
				      reported-branch))
			(not (magit-git-string "merge-base %s %s" head b)))
	      (puthash hash (file-name-nondirectory b) reported)
	      (let* ((n (magit-git-string "log --pretty=oneline %s..%s | wc -l"
					  head b))
		     (section
		      (let ((magit-section-hidden-default t))
			(magit-git-section
			 (cons b 'wazzup)
			 (format "%s unmerged commits in %s%s"
				 n b
				 (if (member b excluded)
				     " (normally ignored)"
				   ""))
			 'magit-wash-log
			 "log"
			 (format "--max-count=%s" magit-log-cutoff-length)
			 "--pretty=oneline"
			 (format "%s..%s" head b)
			 "--"))))
		(magit-set-section-info b section)))))))))

(defun magit-wazzup (&optional all)
  (interactive "P")
  (let* ((topdir (magit-get-top-dir default-directory)))
    (switch-to-buffer "*magit-wazzup*")
    (magit-mode-init topdir 'wazzup
		     #'magit-refresh-wazzup-buffer
		     (magit-get-current-branch) all)))

;;; Miscellaneous

(defun magit-ignore-file (file edit local)
  (let ((ignore-file (if local ".git/info/exclude" ".gitignore")))
    (if edit
	(setq file (read-string "File to ignore: " file)))
    (append-to-file (concat "/" file "\n") nil ignore-file)
    (magit-need-refresh)))

(defun magit-ignore-item ()
  (interactive)
  (magit-section-action (item info "ignore")
    ((untracked file)
     (magit-ignore-file info current-prefix-arg nil))
    ((wazzup)
     (magit-wazzup-toggle-ignore info current-prefix-arg))))

(defun magit-ignore-item-locally ()
  (interactive)
  (magit-section-action (item info "ignore")
    ((untracked file)
     (magit-ignore-file info current-prefix-arg t))))

(defun magit-discard-diff (diff)
  (let ((kind (magit-diff-item-kind diff))
	(file (magit-diff-item-file diff)))
    (cond ((eq kind 'deleted)
	   (when (yes-or-no-p (format "Resurrect %s? " file))
	     (magit-run-git "reset" "-q" "--" file)
	     (magit-run-git "checkout" "--" file)))
	  ((eq kind 'new)
	   (if (yes-or-no-p (format "Delete %s? " file))
	       (magit-run-git "rm" "-f" "--" file)))
	  (t
	   (if (yes-or-no-p (format "Discard changes to %s? " file))
	       (magit-run-git "checkout" "--" file))))))

(defun magit-discard-item ()
  (interactive)
  (magit-section-action (item info "discard")
    ((untracked file)
     (if (yes-or-no-p (format "Delete %s? " info))
	 (magit-run "rm" info)))
    ((untracked)
     (if (yes-or-no-p "Delete all untracked files and directories? ")
	 (magit-run "git" "clean" "-df")))
    ((unstaged diff hunk)
     (when (yes-or-no-p (if (magit-use-region-p)
			    "Discard changes in region? "
			  "Discard hunk? "))
       (magit-apply-hunk-item-reverse item)))
    ((staged diff hunk)
     (if (magit-file-uptodate-p (magit-diff-item-file
				 (magit-hunk-item-diff item)))
	 (when (yes-or-no-p (if (magit-use-region-p)
				"Discard changes in region? "
			      "Discard hunk? "))
	   (magit-apply-hunk-item-reverse item "--index"))
       (error "Can't discard this hunk.  Please unstage it first.")))
    ((unstaged diff)
     (magit-discard-diff item))
    ((staged diff)
     (magit-discard-diff item))
    ((hunk)
     (error "Can't discard this hunk"))
    ((diff)
     (error "Can't discard this diff"))
    ((stash)
     (when (yes-or-no-p "Discard stash? ")
       (magit-run-git "stash" "drop" info)))))

(defun magit-visit-item ()
  (interactive)
  (magit-section-action (item info "visit")
    ((untracked file)
     (find-file info))
    ((diff)
     (find-file (magit-diff-item-file item)))
    ((hunk)
     (let ((file (magit-diff-item-file (magit-hunk-item-diff item)))
	   (line (magit-hunk-item-target-line item)))
       (find-file file)
       (goto-line line)))
    ((commit)
     (magit-show-commit info)
     (pop-to-buffer "*magit-commit*"))
    ((stash)
     (magit-show-stash info)
     (pop-to-buffer "*magit-diff*"))))

(defun magit-show-item-or-scroll-up ()
  (interactive)
  (magit-section-action (item info)
    ((commit)
     (magit-show-commit info #'scroll-up))
    ((stash)
     (magit-show-stash info #'scroll-up))
    (t
     (scroll-up))))

(defun magit-show-item-or-scroll-down ()
  (interactive)
  (magit-section-action (item info)
    ((commit)
     (magit-show-commit info #'scroll-down))
    ((stash)
     (magit-show-stash info #'scroll-down))
    (t
     (scroll-down))))

(defun magit-mark-item (&optional unmark)
  (interactive "P")
  (if unmark
      (magit-set-marked-commit nil)
    (magit-section-action (item info "mark")
      ((commit)
       (magit-set-marked-commit (if (eq magit-marked-commit info)
				    nil
				  info))))))

(defun magit-describe-item ()
  (interactive)
  (let ((section (magit-current-section)))
    (message "Section: %s %s-%s %S %S %S"
	     (magit-section-type section)
	     (magit-section-beginning section)
	     (magit-section-end section)
	     (magit-section-title section)
	     (magit-section-info section)
	     (magit-section-context-type section))))

(defun magit-copy-item-as-kill ()
  "Copy sha1 of commit at point into kill ring."
  (interactive)
  (magit-section-action (item info "copy")
    ((commit)
     (kill-new info)
     (message "%s" info))))

(defun magit-interactive-rebase ()
  "Start a git rebase -i session, old school-style."
  (interactive)
  (server-start)
  (let* ((section (get-text-property (point) 'magit-section))
	 (commit (and (member 'commit (magit-section-context-type section))
		      (magit-section-info section)))
	 (old-editor (getenv "GIT_EDITOR")))
    (setenv "GIT_EDITOR" (expand-file-name "emacsclient" exec-directory))
    (unwind-protect
	(shell-command (concat magit-git-executable " rebase -i "
			       (or (and commit (concat commit "^"))
				   (read-string "Interactively rebase to: "))
			       " &"))
      (if old-editor
	  (setenv "GIT_EDITOR" old-editor)))))

(defun magit-show-branches ()
  "Show all of the current branches in other-window."
  (interactive)
  (save-selected-window
    (switch-to-buffer-other-window "*magit-branches*")
    (erase-buffer)
    (shell-command (concat magit-git-executable " branch -va") t t)))

(defvar magit-ediff-file)
(defvar magit-ediff-windows)

(defun magit-interactive-resolve (file)
  (let ((merge-status (magit-git-string "ls-files -u -- %s" file))
	(base-buffer (generate-new-buffer (concat file ".base")))
	(our-buffer (generate-new-buffer (concat file ".current")))
	(their-buffer (generate-new-buffer (concat file ".merged")))
	(windows (current-window-configuration)))
    (if (null merge-status)
	(error "Cannot resolge %s" file))
    (with-current-buffer base-buffer
      (if (string-match "^[0-9]+ [0-9a-f]+ 1" merge-status)
	  (insert (magit-git-string "cat-file blob :1:%s" file))))
    (with-current-buffer our-buffer
      (if (string-match "^[0-9]+ [0-9a-f]+ 2" merge-status)
	  (insert (magit-git-string "cat-file blob :2:%s" file))))
    (with-current-buffer their-buffer
      (if (string-match "^[0-9]+ [0-9a-f]+ 3" merge-status)
	  (insert (magit-git-string "cat-file blob :3:%s" file))))
    ;; We have now created the 3 buffer with ours, theirs and the ancestor files
    (with-current-buffer (ediff-merge-buffers-with-ancestor our-buffer their-buffer base-buffer)
      (make-local-variable 'magit-ediff-file)
      (setq magit-ediff-file file)
      (make-local-variable 'magit-ediff-windows)
      (setq magit-ediff-windows windows)
      (make-local-variable 'ediff-quit-hook)
      (add-hook 'ediff-quit-hook
		(lambda ()
		  (let ((buffer-A ediff-buffer-A)
			(buffer-B ediff-buffer-B)
			(buffer-C ediff-buffer-C)
			(buffer-Ancestor ediff-ancestor-buffer)
			(file magit-ediff-file)
			(windows magit-ediff-windows))
		    (ediff-cleanup-mess)
		    (find-file file)
		    (erase-buffer)
		    (insert-buffer-substring buffer-C)
		    (kill-buffer buffer-A)
		    (kill-buffer buffer-B)
		    (kill-buffer buffer-C)
		    (when (bufferp buffer-Ancestor) (kill-buffer buffer-Ancestor))
		    (set-window-configuration windows)
		    (message "Conflict resolution finished; you may save the buffer")))))))


(defun magit-interactive-resolve-item ()
  (interactive)
  (magit-section-action (item info "resolv")
    ((diff)
     (magit-interactive-resolve (cadr info)))))


(provide 'magit)
