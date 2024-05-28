;;; yynt.el --- simple org export manager -*- lexical-binding: t; -*-

;; Copyright (C) 2024 include-yy <yy@egh0bww1.com>

;; Author: include-yy <yy@egh0bww1.com>
;; Maintainer: include-yy <yy@egh0bww1.com>
;; Created: 22 Apr 2024

;; Package-Version: 0.1
;; Package-Requires: ((emacs "29.2") (emacsql "3.1"))
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
(require 'org)
(require 'emacsql-sqlite)

(defvar yynt-project-list nil
  "list of all yynt project")
(defvar yynt-current-project nil
  "current project's `yynt-project' object")

(cl-defstruct (yynt-project (:conc-name yynt-project--)
			    (:constructor yynt-project--make)
			    (:copier nil))
  "Struct for yynt projects, create it using `yynt-create-project'"
  (name nil ; symbol of project name
   :documentation "Symbol of project name")
  (dir nil ; directory of project
       :documentation "directory of project")
  (pubdir nil ; directory for publish
	  :documentation "directory for publish")
  (cache nil ; cache file path
	 :documentation "cache file path of project")
  (cache-items nil
	       :documentation "items need to be cached")
  (builds nil; list of yynt-build objects
   :documentation "list of `yynt-build' objects"))

(defun yynt-create-project (name pubdir cache &optional directory)
  "Create a new yynt project.

NAME must be a non-nil symbol. PUBDIR is path to publication
directory, and CACHE is path to CACHE file. relative path means
parent directory is DIRECTORY.

If DIRECTORY is not supplied, it is the value of
`default-directory'.

This function also set `yynt-current-project' to the created
project."
  (cl-assert (and (symbolp name) (not (null name))))
  (cl-assert (stringp pubdir))
  (cond
   ((null directory) (setq directory default-directory))
   ((and (file-exists-p directory)
	 (file-directory-p directory))
    (setq directory (file-name-as-directory directory)))
   (t (error "directory may not exist or not a true directory: %s" directory)))
  (unless (file-name-absolute-p pubdir)
    (setq pubdir (expand-file-name pubdir directory)))
  (unless (or (null cache) (file-name-absolute-p cache))
    (setq cache (expand-file-name cache directory)))
  (let ((project (yynt-project--make :name name :dir directory
				     :pubdir pubdir :cache cache)))
    (setq yynt-project-list
	  (cons project (cl-remove name yynt-project-list
				   :key #'yynt-project--name)))
    (setq yynt-current-project project)))

(defun yynt-choose-project (name)
  "interactively choose a project as current project

NAME must be symbol type"
  (interactive (list (intern (completing-read
			      "Choose a project: "
			      (mapcar #'yynt-project--name yynt-project-list)
			      nil t))))
  (let ((project (car-safe (cl-member name yynt-project-list
				      :key #'yynt-project--name))))
    (setq yynt-current-project project)))

(defun yynt-in-project-p (file &optional project)
  "determine if FILE is in PROJECT or not."
  (file-in-directory-p (file-truename file)
		       (or project (yynt-project--dir yynt-current-project))))

(cl-defstruct (yynt-build (:conc-name yynt-build--)
			  (:constructor yynt-build--make)
			  (:copier nil))
  "Struct that contains build info of a series of files"
  (project nil ; project object that belongs to
	   :documentation "project object that belongs to")
  (type 0 ; type of build object
	:documentation "type of build object. It can be 0, 1 or 2")
  (path nil ; path to build object
	:documentation "full path to this build-object")
  (collect #'ignore ; collect all items
	   :documentation "function that return list of item need to be built")
  (info nil ; plist holding contextual information
	:documentation "plist that pass to export function's ext-plist arg")
  (collect-ex #'ignore
	      :documentation "function that collect extra files")
  (info-ex nil ; additional plist for certain usage.
	      :documentation "additional plist for certain usage")
  (fn #'ignore ; actual export function
      :documentation "Function take arglist (PLIST &optional force)")
  (attrs nil ; list of meta info needs to be taken from item
	 :documentation "list of meta info needs to be taken from item")
  (no-cache-files-ht nil :documentation "hashtable that stores file not need cache")
  (ext-files nil :documentation "external files depend on this build object")
  (included-resources nil :documentation "resources need to be exported")
  (excluded-resources-2 nil :documentation "resource need to be excluded, use for type 2"))

(cl-defun yynt-create-build (&key type path collect info collect-ex info-ex
				  fn attrs no-cache-files external-files
				  included-resources excluded-resources-2)
  "create `yynt-build' object with `yynt-current-project' as belonged project."
  (when (not (yynt-project-p yynt-current-project))
    (error "seems not a valid yynt-project: %s" yynt-current-project))
  (let* ((project-dir (yynt-project--dir yynt-current-project))
	 (full-path (expand-file-name path project-dir))
	 (final-path (if (not (file-directory-p full-path)) full-path
		       (file-name-as-directory full-path)))
	 (nocache-hl (let ((ht (make-hash-table :test #'equal)))
		       (prog1 ht (mapc (lambda (x) (puthash x t ht)) no-cache-files))))
	 (obj (yynt-build--make
	       :project yynt-current-project
	       :type type :path final-path
	       :collect collect :info info
	       :collect-ex collect-ex :info-ex info-ex
	       :fn fn :attrs attrs
	       :no-cache-files-ht nocache-hl
	       :ext-files external-files
	       :included-resources included-resources
	       :excluded-resources-2 excluded-resources-2))
	 (builds (yynt-project--builds yynt-current-project)))
    (setf (yynt-project--builds yynt-current-project)
	  (cons obj (cl-remove full-path builds
			       :test #'string=
			       :key #'yynt-build--path)))))

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

(defun yynt-no-cache-p (bobj file)
  "determine if FILE is in no-cache-files-ht"
  (let ((ht (yynt-build--no-cache-files-ht bobj))
	(path (yynt-build--path bobj))
	(type (yynt-build--type bobj)))
    (pcase type
      (0 (gethash (file-name-nondirectory path) ht))
      ((or 1 2)
       (let ((name (substring file (length path))))
	 (gethash name ht)))
      (_ (error "not a valid build-object type: %s" type)))))

(defun yynt-get-file-build (file)
  "get the corresponding build object from filename FILE"
  (if-let* ((project (cl-find-if (lambda (p) (yynt-in-project-p file p))
				 yynt-project-list))
	    (bobj (cl-find-if (lambda (b) (yynt-in-build-p file b))
			      (yynt-project--builds project))))
      bobj
    nil))


(defun yynt-export-onefile (bobj file &optional ex force)
  "export one file under BOBJ"
  (unless (file-exists-p file)
    (error "file not exists: %s" file))
  (let ((fn (yynt-build--fn bobj))
	(info (if ex (org-combine-plists
		      (yynt-build--info bobj)
		      (yynt-build--info-ex bobj))
		(yynt-build--info bobj)))
	(force (or force (yynt-no-cache-p bobj file))))
    (if-let ((buf (get-file-buffer file)))
	(with-current-buffer buf
	  (funcall fn info force))
      (with-current-buffer (find-file-noselect file)
	(unwind-protect (funcall fn info force)
	  (kill-buffer))))))

(defun yynt-export-extra-files (bobj)
  "export whole build object's external files"
  (let* ((extfiles (yynt-build--ext-files bobj))
	 (base-path (yynt-project--dir (yynt-build--project bobj)))
	 (full-files (mapcar (lambda (x) (file-name-concat base-path x)) extfiles)))
    (mapc (lambda (x)
	    (if-let ((obj (yynt-get-file-build x)))
		(yynt-export-onefile obj x t t)
	      (error "file %s not belongs to any build object" x)))
	  full-files)))

(defun yynt-export-build-object (bobj &optional force noexternal)
  "export whole build object's files"
  (let ((type (yynt-build--type bobj)))
    (pcase type
      (0 (let ((file (yynt-build--path bobj)))
	   (yynt-export-onefile bobj file nil force)))
      ((or 1 2)
       (let* ((co-fn (yynt-build--collect bobj))
	      (co-fn-ex (yynt-build--collect-ex bobj))
	      (files-1 (funcall co-fn bobj))
	      (files-2 (funcall co-fn-ex bobj)))
	 (mapc (lambda (x) (yynt-export-onefile bobj x nil force)) files-1)
	 (mapc (lambda (x) (yynt-export-onefile bobj x t force)) files-2)))
      (_ (error "not a valid build object type: %s" type)))
    (unless noexternal
      (yynt-export-extra-files bobj))))

(defun yynt-export-build-object-list (bobjs &optional force)
  "export a list of build object"
  (let ((ext-files (cl-delete-duplicates
		    (apply #'append (mapcar #'yynt-build--ext-files bobjs))
		    :test #'equal)))
    (mapc (lambda (x) (yynt-export-build-object x force t)))
    (mapc (lambda (x)
	    (if-let ((obj (yynt-get-file-build x)))
		(yynt-export-onefile obj x t t)
	      (error "file %s not belong to any build object" x)))
	  ext-files)))

(defun yynt-export-file (filename &optional force)
  "User Command, export current buffer
If called interactively, use current's buffer-file-name as FILENAME

If invoked with C-u, force export"
  (interactive (list (buffer-file-name) current-prefix-arg))
  (if (null filename)
      (user-error "buffer seems not have `buffer-file-name'")
    (let ((bobj (yynt-get-file-build filename)))
      (if (null bobj)
	  (user-error "file %s seems not belong to one build object" filename)
	(if (eq 0 (yynt-build--type bobj))
	    (yynt-export-onefile bobj filename nil force)
	  (let* ((len (length (yynt-build--path bobj)))
		 (basename (substring filename len)))
	    (cond
	     ((cl-member basename (funcall (yynt-build--collect bobj) bobj)
			 :key (lambda (x) (substring x len)))
	      (yynt-export-onefile bobj filename nil force))
	     ((cl-member basename (funcall (yynt-build--collect-ex bobj) bobj)
			 :key (lambda (x) (substring x len)))
	      (yynt-export-onefile bobj filename t force))
	     (t (user-error "file seems not a exportable file"))))
	  (yynt-export-extfile bobj))))))

(defun yynt-get-project-build-relative-path (&optional project)
  "return alist of bobj's relative path name and bobj"
  (ignore (or project (setq project yynt-current-project)))
  (let* ((bobjs (yynt-project--builds project))
	 (len (length (yynt-project--dir project))))
    (mapcar (lambda (x) (cons (substring (yynt-build--path x) len) x))
	    bobjs)))

(defun yynt-export-buildobj (bobj &optional force)
  "User Command, export selected build object"
  (interactive (list (completing-read
		      "Select a build object:> "
		      (cons '("t" . t)
			    (yynt-get-project-build-relative-path))
		      nil t)
		     current-prefix-arg))
  (cond
   ((eq bobj t)
    (yynt-export-build-object-list
     (yynt-project--builds yynt-current-project)))
   (yynt-export-build-object bobj force)))


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


;;; Utils

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

;; | project-name | pathname | name | type | tag | pub-time | build-time | export-time | export |
;;   PRIMARY

;; Consider using ox-publish.el's cache and emacs's builtin SQLite3 support, and prefer the latter.
;; I need a uniform interface for caching build time and something else.
;; Caching migrated from ox-publish.el is not completed.

(provide 'yynt)
;;; yynt.el ends here
