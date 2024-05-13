;;; yynt.el --- simple org export manager -*- lexical-binding: t; -*-

;; Copyright (C) 2024 include-yy <yy@egh0bww1.com>

;; Author: include-yy <yy@egh0bww1.com>
;; Maintainer: include-yy <yy@egh0bww1.com>
;; Created: 22 Apr 2024

;; Package-Version: 0.1
;; Package-Requires: ((emacs "29.2") (emacsql "3.1.1"))
;; Keywords: manager
;; URL: https://github.com/include-yy/yynt

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:

(require 'cl-lib)
(require 'emacsql)

(defvar yynt-project-list nil
  "list of all yynt project")
(defvar yynt-current-project nil
  "(name dir objs) list of current project")

(cl-defstruct (yynt-project (:conc-name yynt-project--)
			    (:constructor yynt-project--make)
			    (:copier nil))
  "Struct for yynt projects"
  (name nil ; symbol of project name
   :documentation "Symbol of project name")
  (dir nil ; directory of project
       :documentation "directory of project")
  (pubdir nil ; directory for publish
	  :documentation "directory for publish")
  (cache nil ; cache file path
	 :documentation "cache directory of project")
  (builds nil; list of yynt-build objects
   :documentation "list of `yynt-build' objects"))

(defun yynt-create-project (name &optional directory)
  "Create a new yynt project.

NAME must be a symbol.
This function also set current project to the created project"
  (when (or (null name) (not (symbolp name)))
    (error "yynt's project name must be a non-nil symbol"))
  (ignore (or directory (setq directory default-directory)))
  (let ((project (yynt-project--make :name name :dir directory)))
    (setq yynt-project-list (cons project (cl-remove name yynt-project-list
						     :key #'yynt-project--name)))
    (setq yynt-current-project project)))

(defun yynt-choose-project (name)
  "interactively choose a project as current project

NAME must be symbol type"
  (interactive (list (intern (completing-read "Choose a project: "
					      yynt-project-list nil t))))
  (let ((project (assq name yynt-project-list)))
    (setq yynt-current-project project)))

(defun yynt-in-project-p (file &optional project)
  "determine if FILE is in `yynt-current-project' or not."
  (file-in-directory-p (file-truename file) (or project (yynt-project--dir yynt-current-project))))

(cl-defstruct (yynt-build (:conc-name yynt-build--)
			  (:constructor yynt-build--make)
			  (:copier nil))
  "Struct that contains build info of a series of files"
  (type 0 ; type of build object
	:documentation "type of build object. It can be 0, 1 or 2")
  (path nil ; path to build object
	:documentation "full path to this build-object")
  (collect #'ignore
	   :documentation ; collect all items
	   "function that take build object as arg and  return items need to be built")
  (export #'ignore ; actual export function
	  :documentation "Function that 1st arg must be file")
  (extra nil ; files need to be generated after build '((infiles) . (outfiles))
	 :documentation "files need to be generated after build")
  (info nil ; plist holding contextual information
	:documentation "plist that pass to export function's ext-plist arg")
  (info-extra nil ; additional plist for certain usage.
	      :documentation "additional plist for certain usage"))

(cl-defun yynt-create-build (&key type path collect export extra info info-extra)
  "create `yynt-build' object with `yynt-current-project' as belonged project."
  (when (not (yynt-project-p yynt-current-project))
    (error "seems not a valid yynt-project: %s" yynt-current-project))
  (let* ((full-path (expand-file-name path (yynt-project--dir yynt-current-project)))
	 (extra-full-path (cons (mapcar (lambda (x) (expand-file-name x full-path)) (car extra))
				(mapcar (lambda (x) (expand-file-name x (yynt-project--dir
								     yynt-current-project)))
					(cdr extra))))
	 (obj (yynt-build--make
	       :type type :path full-path
	       :collect collect :export export
	       :extra extra-full-path :info info :info-extra info-extra))
	 (builds (yynt-project--builds yynt-current-project)))
    (setf (yynt-project--builds yynt-current-project)
	  (cons obj (cl-remove full-path builds :test #'string= :key #'yynt-build--path)))))

(defun yynt-in-build-p (bobj file)
  "determine if File is in BOBJ build object
file may have some constrains (WIP)"
  (let ((bpath (yynt-build--path bobj)))
    (pcase (yynt-build--type bobj)
      (0 (file-equal-p bpath file))
      (1 (file-equal-p bpath (file-name-directory file)))
      (2 (or (file-equal-p bpath (file-name-directory file))
	     (file-equal-p bpath (file-name-directory (file-name-directory file)))))
      (_ (error "seems not a valid build object type")))))

(defun yynt-get-file-build (file)
  "get the corresponding build object from filename FILE"
  (if-let* ((project (cl-find-if (lambda (p) (yynt-in-project-p file p)) yynt-project-list))
	    (bobj (cl-find-if (lambda (b) (yynt-in-build-p file b)) (yynt-project--builds project))))
      bobj
    (error "file %s may not belongs to any exist projects or build objects" file)))


;;; Caching functions
;;; taken from ox-publish.el
(defvar yynt-current-cache nil
    "This will cache timestamps and titles for files in publishing projects.
Blocks could hash sha1 values here.")

(defun yynt-write-cache-file (&optional project free-cache)
  "Write project PROJECT's cache to file
If FREE-CACHE, empty the cache"
  (unless yynt-current-cache
    (error "`yynt-publish-write-chache-file' called, but no cache present"))
  (unless (or project yynt-current-project)
    (error "not in any project"))
  (let* ((project (or project yynt-current-project))
	 (cache-file (yynt-project--cache project)))
    (unless cache-file
      (error "Cannot find cache-file in `yynt-write-cache-file'"))
    (with-temp-file cache-file
      (let (print-level print-length)
	(insert "(setq yynt-current-cache \
(make-hash-table :test 'equal))")
	(maphash (lambda (k v)
		   (insert
		    (format "(puthash %S %s%S yynt-current-cache)\n"
			    k (if (or (listp v) (symbolp v)) "'" "") v)))
		 yynt-current-cache)))
    (when free-cache (yynt-reset-cache))))

(defun yynt-initialize-cache (project)
  "Initialize the projects cache if not initialized yet and return it."
  (unless (yynt-project-p project)
    (error "Seems not a valid `yynt-project' object: %s" project))
  (let* ((cache-file (yynt-project--cache project))
	 (cexists (file-exists-p cache-file)))
    (when yynt-current-cache (yynt-reset-cache))
    (if cexists (load-file cache-file)
      (setq yynt-current-cache
	    (make-hash-table :test #'equal)))
    (unless cexists (yynt-write-cache-file project))))

(defun yynt-get-cache (key)
  "Return the value stored in `org-publish-cache' for key KEY.
Return nil, if no value or nil is found.  Raise an error if the
cache does not exist."
  (unless yynt-current-cache
    (error "`yynt-get-cache' called, but no cache present"))
  (gethash key yynt-current-cache))

(defun yynt-set-cache (key value)
  "Store KEY VALUE pair in `org-publish-cache'.
Returns value on success, else nil.  Raise an error if the cache
does not exist."
  (unless yynt-current-cache
    (error "`yynt-set-cache' called, but no cache present"))
  (puthash key value yynt-current-cache))

(defun yynt-reset-cache ()
  "Empty `yynt-current-cache' and reset it nil."
  (message "%s" "Resetting org-publish-cache")
  (when (hash-table-p yynt-current-cache)
    (clrhash yynt-current-cache))
  (setq yynt-current-cache nil))

(defun yynt-set-cache-file-property
    (filename property value &optional project)
  "Set the VALUE for a PROPERTY of file FILENAME in publishing cache to VALUE.
If the entry does not exist, it will be created.  Return VALUE."
  ;; Evtl. load the requested cache file:
  (when project (yynt-initialize-cache project))
  (let ((pl (yynt-get-cache filename)))
    (if pl (progn (plist-put pl property value) value)
      (yynt-get-cache-file-property
       filename property value nil project))))

(defun yynt-get-cache-file-property
    (filename property &optional default no-create project)
  "Return the value for a PROPERTY of file FILENAME in publishing cache.
Use cache file of PROJECT-NAME.  Return the value of that PROPERTY,
or DEFAULT, if the value does not yet exist.  Create the entry,
if necessary, unless NO-CREATE is non-nil."
  (when project (org-publish-initialize-cache project))
  (let ((properties (yynt-get-cache filename)))
    (cond ((null properties)
	   (unless no-create
	     (org-publish-cache-set filename (list property default)))
	   default)
	  ((plist-member properties property) (plist-get properties property))
	  (t default))))

(defun yynt-get-filename-key (filename)
  "Return path to timestamp file for filename FILENAME."
  (setq filename (concat filename "::" (or pub-dir "") "::"
			 (format "%s" (or pub-func ""))))
  (concat "X" (if (fboundp 'sha1) (sha1 filename) (md5 filename))))


(defun yynt-cache-file-needs-publishing (filename)
  "Check the timestamp of the last publishing of FILENAME.
Return non-nil if the file needs publishing.  Also check if
any included files have been more recently published, so that
the file including them will be republished as well."
  (unless org-publish-cache
    (error "`yynt-cache-file-needs-publishing' called, but no cache present"))
  (let* ((key (yynt-get-filename-key filename))
	 (pstamp (yynt-get-cache key))
	 (org-inhibit-startup t)
	 included-files-mtime)
    (when (equal (file-name-extension filename) "org")
      (let ((case-fold-search t))
	(with-temp-buffer
          (delay-mode-hooks
            (org-mode)
            (insert-file-contents filename)
	    (goto-char (point-min))
	    (while (re-search-forward "^[ \t]*#\\+INCLUDE:" nil t)
	      (let ((element (org-element-at-point)))
	        (when (eq 'keyword (org-element-type element))
		  (let* ((value (org-element-property :value element))
		         (include-filename
			  (and (string-match "\\`\\(\".+?\"\\|\\S-+\\)" value)
			       (let ((m (org-strip-quotes
				         (match-string 1 value))))
			         ;; Ignore search suffix.
			         (if (string-match "::.*?\\'" m)
				     (substring m 0 (match-beginning 0))
				   m)))))
		    (when include-filename
		      (push (org-publish-cache-mtime-of-src
			     (expand-file-name include-filename (file-name-directory filename)))
			    included-files-mtime))))))))))
    (or (null pstamp)
	(let ((mtime (org-publish-cache-mtime-of-src filename)))
	  (or (time-less-p pstamp mtime)
	      (cl-some (lambda (ct) (time-less-p mtime ct))
		       included-files-mtime))))))



(defun yynt-export-extfile (bobj)
  "build all extra files"
  (let* ((export-func (yynt-build--export bobj))
	 (extra (yynt-build--extra bobj))
	 (info (yynt-build--info bobj))
	 (extinfo (yynt-build--info-extra bobj))
	 (infiles (car extra))
	 (outfiles (cdr extra)))
    (mapc (lambda (f) (funcall export-func f nil nil nil nil
			   (org-combine-plists info extinfo)))
	  infiles)
    (mapc #'yynt-export-onefile outfiles)))

(defun yynt-export-onefile (file &optional no-extra)
  "export one file"
  (let* ((tname (file-truename file))
	 (bobj (yynt-get-file-build tname))
	 (extfile (car (yynt-build--extra bobj)))
	 (export-fn (yynt-build--export bobj))
	 (info (yynt-build--info bobj)))
    (if (eq (yynt-build--type bobj) 0)
	(if (string-match-p "\\.org\\'" tname)
	    (funcall export-fn tname nil nil nil nil info)
	  (funcall export-fn tname))
      (let ((pls (if (not (member tname extfile)) info
		   (org-combine-plists info (yynt-build--info-extra bobj)))))
	(funcall export-fn tname nil nil nil nil pls)))
    (when (and (not (member tname extfile))
	       (not no-extra)
	       (not (eq (yynt-build--type bobj) 1)))
      (yynt-export-extfile bobj))))

(defun yynt-export-buildobj (bobj &optional no-extra)
  "build all files in build object BOBJ"
  (let* ((export-func (yynt-build--export bobj))
	 (collect-func (yynt-build--collect bobj))
	 (info (yynt-build--info bobj))
	 (files (funcall collect-func bobj)))
    (pcase (yynt-build--type bobj)
      (0 (let* ((path (yynt-build--path bobj)))
	   (if (string-match-p "\\.org\\'" path)
	       (funcall export-func path nil nil nil nil info)
	     (funcall export-func path))))
      (1 (mapc (lambda (f) (funcall export-func f nil nil nil nil info)) files))
      (2 (mapc (lambda (d) (mapc (lambda (f) (funcall export-func f nil nil nil nil info))
			     (directory-files d t "\\.org\\'")))
	       files))
      (_ (error "unknwon build object type")))
    ;; build extra files
    (when (and (not no-extra)
	       (not (eq (yynt-build--type bobj) 0)))
      (yynt-export-extfile bobj))))


;;; Utils

(defun yynt-wrap-export-function (fn)
  "wrap a xxx-export-to-html function to take a filename argument

the export function's prototype must be:"
  (lambda (file &rest args)
    (if (not (file-exists-p file))
	(error "yynt: file not exists: %s" file)
      (if-let ((buf (get-file-buffer file)))
	  (with-current-buffer buf
	    (apply fn args))
	(with-current-buffer (find-file-noselect file)
	  (unwind-protect
	      (apply fn args)
	    (kill-buffer)))))))

(defun yynt-get-org-keywords (file infos)
  "get (keyword . value) alist from the head of FILE

INFOS is a list of keywords, keywords are case-insensitive
format of keywords is #+KEY: VALUE.
FILE must be a valid full path.

take from https://github.com/bastibe/org-static-blog"
  (if (not (file-exists-p file))
      (error "yynt: file not exists: %s" file)

    (let ((case-fold-search t))
      (with-temp-buffer
	(insert-file-contents file nil 0 4096)
	(let (res)
	  (dolist (a infos)
	    (goto-char (point-min))
	    (when (search-forward-regexp
		   (concat "^\\#\\+" a ":[ ]*\\(.+\\)$") nil t)
	      (push (cons a (match-string 1)) res)))
	  res)))))

(defun yynt-directory-files (dir &optional full)
  (directory-files dir full directory-files-no-dot-files-regexp))

(defun yynt-filter-dir-items (pred &optional dir full)
  "return a list of subdirs under DIR that satisfy PRED.

If specified, DIR must be full path.
PRED takes base name as only arg.
If FULL is non-nil, return full path."
  (if (not dir) (setq dir default-directory)
    (cond
     ((not (file-exists-p dir))
      (error "yynt: file not exists: %s" dir))
     ((not (file-directory-p dir))
      (error "yynt: not a directory: %s" dir))))
  (let* ((default-directory dir)
	 (files (yynt-directory-files dir))
	 (res0 (cl-remove-if-not pred files))
	 (res1 (if full (mapcar #'expand-file-name res0) res0)))
    res1))

(defvar yynt--logger "*yynt*"
  "buffer for logging")
(defun yynt--create-log-buffer ()
  "Create or get log buffer"
  (let ((buf (get-buffer-create yynt--logger)))
    (with-current-buffer buf
      (when (not (equal major-mode 'messages-buffer-mode))
	(messages-buffer-mode)))
    buf))
(defun yynt-log (message &optional no-newline switch)
  "Write a log message to buffer named `yynt--logger'

When called interactively, it shows the log buffer."
  (interactive "i")
  (if (called-interactively-p 'interactive)
      (switch-to-buffer (yynt--create-log-buffer))
    (with-current-buffer (yynt--create-log-buffer)
      (let ((inhibit-read-only t))
	(goto-char (point-max))
	(insert (concat message (if no-newline "" "\n")))))
    (when switch
      (switch-to-buffer (yynt--create-log-buffer)))))

;; | pathname | name | type | tag | pub-time | build-time | export-time | export |
;;   PRIMARY

;; Consider using ox-publish.el's cache and emacs's builtin SQLite3 support, and prefer the latter.
;; I need a uniform interface for caching build time and something else.
;; Caching migrated from ox-publish.el is not completed.

(provide 'yynt)
;;; yynt.el ends here
