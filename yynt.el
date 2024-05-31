;;; yynt.el --- simple org export manager -*- lexical-binding: t; -*-

;; Copyright (C) 2024 include-yy <yy@egh0bww1.com>

;; Author: include-yy <yy@egh0bww1.com>
;; Maintainer: include-yy <yy@egh0bww1.com>
;; Created: 22 Apr 2024

;; Package-Version: 0.1
;; Package-Requires: ((emacs "29.2"))
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
(require 'sqlite)

(unless (sqlite-available-p)
  (error "Please ensure your emacs supports builtin sqlite utilities"))

(defgroup yynt nil
  "a simple Org publish manager"
  :group 'applications)

(defcustom yynt-use-logger t
  "use logger or not"
  :type 'boolean)


;;; Definition of yynt-project and some helper functions.

(defvar yynt-project-list nil
  "list of all yynt project.")
(defvar yynt-current-project nil
  "current project's `yynt-project' object.

Set it using `yynt-choose-project'.")

(defvar yynt--temp-project nil
  "Temporary variable used for the creation phase,
representing the current project.")

(defconst yynt-project-fixed-fields
  '("build_time" "publish_time")
  "Inherent fields in the cache database.")

(cl-defstruct (yynt-project (:conc-name yynt-project--)
			    (:constructor yynt-project--make)
			    (:copier nil))
  "Struct definition for yynt projects. Create it with `yynt-create-project'.

Create a build object belonging to the project by calling
`yynt-create-build' after calling `yynt-create-project'.

If cache is nil, the caching mechanism is not used."
  (name nil :documentation "Symbol of the project name.")
  (dir nil :documentation "Full path of the project.")
  (pubdir nil :documentation "Full path of the publish directory.")
  (cache nil :documentation "Full path of the cache file.")
  (cache-items nil :documentation "Items that need to be cached.")
  (builds nil :documentation "list of `yynt-build' objects."))

(defun yynt--string-nw-p (s)
  "Return t if S is a string containing a non-blank character.
Otherwise, return nil.

Get from `org-string-nw-p'."
  (and (stringp s) (string-match-p "[^ \r\t\n]" s)))

(defun yynt--cache-item-p (s)
  "Return t if S is a valid cache-item. Otherwise return nil."
  (and (yynt--string-nw-p s)
       (string-match-p "[A-Za-z][0-9A-Za-z_]*" s)
       (not (member s (cons "path" yynt-project-fixed-fields)))))

(defun yynt-create-project (name pubdir cache cache-items &optional directory)
  "Create a new yynt project.

NAME is the name symbol of the project, which must be a non-nil and
non-keyword symbol.

PUBDIR is the publish directory, which must be of `string' type. If it
is a relative path, it is relative to DIRECTORY.

CACHE can be nil or the path to the cache file. If it is a relative
path, it is relative to the DIRECTORY. If CACHE is nil, it indicates
that the caching mechanism is not used.

CACHE-ITEMS is a list of strings that specifies the fields that need to
be present in the database for the creation of the cache database.

Items in CACHE-ITEMS must satisfy the `yynt--cache-item-p' predicate,
which means they must start with a letter and match the regular
expression \"[0-9A-Za-z_]\". Additionally, they cannot contain items with
the same names as those in `yynt-project-fixed-fields' (plus \"path\").

DIRECTORY is the root directory of the project. If DIRECTORY is not
provided, it will use `default-directory' as the default value."
  ;; Check arguments
  (when (or (not (symbolp name)) (null name) (keywordp name))
    (error "not a valid project name: %s" name))
  (unless (yynt--string-nw-p pubdir)
    (error "not a valid pubdir: %s" pubdir))
  (unless (or (null cache) (yynt--string-nw-p cache))
    (error "not a valid cache: %s" cache))
  (unless (cl-every #'yynt--cache-item-p cache-items)
    (error "not a valid cache-items %s" cache-items))
  (unless (or (null directory) (yynt--string-nw-p directory))
    (error "directory's type is not correct: %s" directory))
  ;; Normalization of the directory
  (cond
   ((null directory) (setq directory default-directory))
   ((and (file-exists-p directory)
	 (file-directory-p directory))
    (setq directory (file-name-as-directory (expand-file-name directory))))
   (t (error "directory may not exist or not a true directory: %s" directory)))
  ;; Normalization of the publish directory
  (unless (file-name-absolute-p pubdir)
    (setq pubdir (expand-file-name pubdir directory)))
  ;; Create Publish directory if not exists
  (unless (file-exists-p pubdir)
    (make-directory pubdir t))
  ;; Normalization of the cache file path
  (unless (or (null cache) (file-name-absolute-p cache))
    (setq cache (expand-file-name cache directory)))
  ;; ;; Create Cache file if necessary
  ;; (when (and (stringp cache) (not (file-exists-p cache)))
  ;;   (with-temp-file cache))
  ;; Normalization of cache-items, converting all to lowercase
  (setq cache-items (append yynt-project-fixed-fields
			    (mapcar #'downcase cache-items)))
  ;; Create Project object
  (let ((project (yynt-project--make :name name :dir directory
				     :pubdir pubdir :cache cache
				     :cache-items cache-items)))
    ;; Replace object with the same name if exists
    (setq yynt-project-list
	  (cons project (cl-delete name yynt-project-list
				   :key #'yynt-project--name)))
    ;; Initialize cache if necessary
    (when cache
      (yynt-initialize-cache project))
    (setq yynt--temp-project project)))

(defun yynt-choose-project (name)
  "Interactively select one from the existing project objects as the
current project.

NAME is the name of the project and is of symbol type."
  (interactive (list (intern (completing-read
			      "Choose a project: "
			      (mapcar #'yynt-project--name yynt-project-list)
			      nil t))))
  (if-let ((project (car-safe (cl-member name yynt-project-list
				      :key #'yynt-project--name))))
      (setq yynt-current-project project)
    (user-error "Seems not a exist project object's name: %s" name)))

(defun yynt-in-project-p (file &optional project)
  "Determine if a file is located within the project.

If PROJECT is not provided, use `yynt-current-project'."
  (if-let ((project (or project yynt-current-project)))
    (file-in-directory-p (file-truename file)
			 (yynt-project--dir project))
    (error "project not specified: %s" project)))

(defun yynt-project-has-cache-p (project)
  "Determine whether the PROJECT object uses the caching mechanism."
  (yynt-project--cache project))

(defun yynt-get-file-project-basename (file project)
  "Get the relative path of a FILE with respect to its PROJECT."
  (declare (pure t))
  (if (not (yynt-in-project-p file project))
      (error "file %s not in project %s" file project)
    (let ((dir (yynt-project--dir project)))
      (substring file (length dir)))))


;;; Implementation of the caching functionality.

(defvar yynt--sqlite-obj nil)
(defun yynt-open-sqlite (file)
  (sqlite-open file))
(defun yynt-close-sqlite (obj)
  (when (sqlitep obj) (sqlite-close obj)))
(defmacro yynt-with-sqlite (project &rest body)
  "Open the project's cache file, evaluate the BODY, close the file.
The value returned is the value of the last form in the body."
  (declare (indent 1))
  (let ((proj (gensym)))
    `(let* ((,proj ,project)
	    (cache (yynt-project--cache ,proj))
	    (yynt--sqlite-obj (yynt-open-sqlite cache)))
       (unwind-protect ,(cons 'progn body)
	 (yynt-close-sqlite yynt--sqlite-obj)))))

(defun yynt-initialize-cache (project)
  "Initialize the cache database, creating table if not exist."
  (let* ((fields (yynt-project--cache-items project)))
    (yynt-with-sqlite project
      (if-let* ((info (sqlite-select yynt--sqlite-obj
				     "PRAGMA table_info(yynt)"))
		(old-fields (mapcar (lambda (ls) (nth 1 ls)) info)))
	  (unless (equal fields old-fields)
	    (let* ((new (mapconcat #'identity fields ","))
		   (int (mapconcat
			 #'identity
			 (cl-intersection fields old-fields :test #'string=)
			 ",")))
	      (with-sqlite-transaction yynt--sqlite-obj
		(sqlite-execute yynt--sqlite-obj "DROP TABLE IF EXISTS temp;")
		(sqlite-execute
		 yynt--sqlite-obj
		 (format "CREATE TABLE temp (path PRIMARY KEY,%s);" new))
		(sqlite-execute
		 yynt--sqlite-obj
		 (format "INSERT INTO temp (path,%s) SELECT path,%s FROM yynt;"
			 int int))
		(sqlite-execute yynt--sqlite-obj "DROP TABLE yynt;")
		(sqlite-execute yynt--sqlite-obj
				"ALTER TABLE temp RENAME to yynt;"))))
	(sqlite-execute yynt--sqlite-obj
			(format "CREATE TABLE yynt (path PRIMARY KEY,%s)"
				(mapconcat #'identity fields ",")))))))

(defun yynt-clear-cache (project)
  "Clear the database."
  (yynt-with-sqlite project
    (sqlite-execute yynt--sqlite-obj "DELETE FROM yynt;")))

(defun yynt-upsert-cache-1 (project file keys values)
  "Update or insert a row: update if the file already exists, otherwise
insert."
  (let* ((items (yynt-project--cache-items project))
	 (base (yynt-get-file-project-basename file project))
	 (k-fields (mapconcat #'identity (cons "path" keys) ","))
	 (v-fields (mapconcat (lambda (x) (format "'%s'" x))
			      (cons base values) ","))
	 (kv-seq (mapconcat (lambda (x) (format "%s='%s'" (car x) (cdr x)))
			    (cl-mapcar #'cons keys values) ",")))
    (if (not (cl-subsetp keys items :test #'string=))
	(error "some keys not belong to cache-items: (%s, %s)"
	       keys items)
      (sqlite-execute
       yynt--sqlite-obj
       (format "\
INSERT INTO yynt (%s) VALUES (%s)
ON CONFLICT(path) DO UPDATE SET %s"
	       k-fields v-fields kv-seq)))))

(defun yynt-upsert-cache (project file keys values)
  (yynt-with-sqlite project
    (yynt-upsert-cache-1 project file keys values)))

(defun yynt-delete-cache-1 (project file)
  "Delete a row."
  (let ((key (yynt-get-file-project-basename file project)))
    (sqlite-execute
     yynt--sqlite-obj
     "DELETE FROM yynt WHERE path=?" (list key))))

(defun yynt-delete-cache (project file)
  (yynt-with-sqlite project
    (yynt-delete-cache-1 project file)))

(defun yynt-select-cache-1 (project file keys)
  (car-safe
   (sqlite-execute
    yynt--sqlite-obj
    (format "SELECT %s FROM yynt where path=%s"
	    (mapconcat #'identity keys ",")
	    (yynt-get-file-project-basename file)))))

(defun yynt-select-cache (project file keys)
  (yynt-with-sqlite project
    (yynt-select-cache-1 project file keys)))

;;; Impl of logger
(defvar yynt--log-buffer "*yynt*"
  "buffer for logging")
(defun yynt--create-log-buffer ()
  "Create or get log buffer"
  (let ((buf (get-buffer-create yynt--log-buffer)))
    (with-current-buffer buf
      (when (not (equal major-mode 'messages-buffer-mode))
	(messages-buffer-mode)))
    buf))
(defun yynt-log (message &optional newline)
  "Write a log message to buffer named `yynt--log-buffer'"
  (when yynt-use-logger
    (with-current-buffer (yynt--create-log-buffer)
      (let ((inhibit-read-only t))
	(goto-char (point-max))
	(insert (concat message (and newline "\n")))))))
(defun yynt-logger ()
  "Show logger buffer"
  (interactive)
  (switch-to-buffer-other-window (yynt--create-log-buffer)))

;;; Definition of yynt-build and some helper functions.
(cl-defstruct (yynt-build (:conc-name yynt-build--)
			  (:constructor yynt-build--make)
			  (:copier nil))
  "Struct that contains build info of a series of files"
  (project nil :documentation "project object that belongs to")
  (name nil :documentation "name of this build object")
  (path nil :documentation "full path to this build-object")
  (type 0 :documentation "type of build object. It can be 0, 1 or 2")
  (collect #'ignore :documentation "(bobj) => list of files need build")
  (info nil :documentation "org export info plist")
  (collect-ex #'ignore :documentation "function that collect extra files")
  (info-ex nil :documentation "additional info plist")
  (fn #'ignore :documentation "export funtion (PLIST) -> nil")
  (attrs nil :documentation "keywords extract from source file")
  (no-cache-files-ht nil :documentation "files without caching")
  (ext-files nil :documentation "external files depend on this build object")
  (included-resources nil :documentation "resources need to be published")
  (collect-2 #'ignore :documentation "(bobj) -> list of dir for pub")
  (excluded-fn-2 nil :documentation "(bobj path) -> list of excluded res"))

(cl-defun yynt-create-build (&key type path collect info collect-ex info-ex
				  fn attrs no-cache-files external-files
				  included-resources collect-2
				  excluded-fn-2)
  "create `yynt-build' object with `yynt--temp-project' as belonged project."
  (when (not (yynt-project-p yynt--temp-project))
    (error "seems not a valid yynt-project: %s" yynt--temp-project))
  (let* ((project-dir (yynt-project--dir yynt--temp-project))
	 (full-path (expand-file-name path project-dir))
	 (final-path (if (not (file-directory-p full-path)) full-path
		       (file-name-as-directory full-path)))
	 (ht (let ((ht (make-hash-table :test #'equal)))
	       (prog1 ht (mapc (lambda (x) (puthash x t ht)) no-cache-files))))
	 (obj (yynt-build--make
	       :project yynt--temp-project
	       :name path :path final-path :type type
	       :collect collect :info info
	       :collect-ex collect-ex :info-ex info-ex
	       :fn fn :attrs attrs :no-cache-files-ht ht
	       :ext-files external-files
	       :included-resources included-resources
	       :collect-2 collect-2 :excluded-fn-2 excluded-fn-2))
	 (builds (yynt-project--builds yynt--temp-project)))
    (setf (yynt-project--builds yynt--temp-project)
	  (cons obj (cl-remove full-path builds
			       :test #'string=
			       :key #'yynt-build--path)))))

(defun yynt-get-file-project-basename (file project)
  "Get the relative path of a FILE with respect to its PROJECT."
  (declare (pure t))
  (if (not (yynt-in-project-p file project))
      (error "file %s not in project %s" file project)
    (let ((dir (yynt-project--dir project)))
      (substring file (length dir)))))

(defun yynt-get-file-build-basename (file bobj)
  (if (not (yynt-in-build-p bobj file))
      (error "file %s not in build object %s" file bobj)
    (let ((dir (yynt-build--path bobj)))
      (substring file (length dir)))))

(defun yynt-in-build-p (bobj file)
  "determine if File is in BOBJ build object
file may have some constrains (WIP)"
  (let ((bpath (yynt-build--path bobj)))
    (pcase (yynt-build--type bobj)
      (0 (file-equal-p bpath file))
      (1 (file-equal-p bpath (file-name-directory file)))
      (2 (or (file-equal-p bpath (file-name-directory file))
	     (file-equal-p bpath (file-name-directory
				  (file-name-directory file)))))
      (_ (error "seems not a valid build object type")))))

(defun yynt-file-no-cache-p (bobj file)
  "determine if FILE is in no-cache-files-ht"
  (if (not (yynt-project-has-cache-p (yynt-build--project bobj))) nil
    (let ((ht (yynt-build--no-cache-files-ht bobj))
	  (path (yynt-build--path bobj))
	  (type (yynt-build--type bobj)))
      (pcase type
	(0 (gethash (file-name-nondirectory path) ht))
	((or 1 2)
	 (let ((name (substring file (length path))))
	   (gethash name ht)))
	(_ (error "not a valid build-object type: %s" type))))))

(defun yynt-get-file-build-object (file &optional project)
  "get the corresponding build object from filename FILE"
  (if-let* ((project (or project
			 (cl-find-if
			  (lambda (p) (yynt-in-project-p file p))
			  yynt-project-list)))
	    (bobj (cl-find-if (lambda (b) (yynt-in-build-p file b))
			      (yynt-project--builds project))))
      bobj
    nil))

;;; Implementation of the build functionality.

(defun yynt-combine-plists (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting properties
from the other lists.  Settings in the last list are the most significant
ones and overrule settings in the other lists.

Copied from `org-combine-plists'"
  (let ((rtn (copy-sequence (pop plists)))
	p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
	(setq p (pop ls) v (pop ls))
	(setq rtn (plist-put rtn p v))))
    rtn))

(defun yynt-get-org-keywords (infos)
  "get (keyword . value) alist from the head of FILE

INFOS is a list of keywords, keywords are case-insensitive
format of keywords is #+KEY: VALUE.
FILE must be a valid full path.

take from https://github.com/bastibe/org-static-blog"
  (let ((case-fold-search t)
	res-key res-val)
    (save-excursion
      (dolist (a infos)
	(goto-char (point-min))
	(when (search-forward-regexp
	       (concat "^\\#\\+" a ":[ ]*\\(.+\\)$") 4096 t)
	  (push a res-key)
	  (push (match-string 1) res-val))))
    (cons res-key res-val)))

(defun yynt-get-file-ctime (filepath)
  "get file's ctime, utc+0"
  (format-time-string
   "%Y-%m-%d %T"
   (nth 5 (file-attributes filepath)) t))

(defun yynt-time-less-p (st1 st2)
  "tell if st1's time is less than st2,
time format must be YYYY-MM-DD hh:mm:ss"
  (time-less-p (date-to-time st1)
	       (date-to-time st2)))

(defun yynt--export-current-buffer (fn plist)
  "inner function"
  (condition-case-unless-debug nil
      (prog1 t (funcall fn plist))
    (error nil)))

(defun yynt--do-export (bobj plist file &optional force)
  "export and log, cache"
  (let* ((project (yynt-build--project bobj))
	 (basename (yynt-get-file-project-basename file project))
	 (project-name (yynt-project--name project))
	 (build-name (yynt-build--name bobj))
	 (attrs (yynt-build--attrs bobj))
	 (fn (yynt-build--fn bobj)))
    (yynt-log (format "[%s → %s] %s exporting... "
		      project-name build-name basename))
    (if (yynt-file-no-cache-p bobj file)
	(let ((buf0 (get-file-buffer file))
	      (buf (if buf0 buf0 (find-file-noselect file))))
	  (with-current-buffer buf
	    (unwind-protect
		(if (yynt--export-current-buffer fn plist)
		    (yynt-log "ok" t) (yynt-log "fail" t))
	      (unless buf0 (kill-buffer)))))
      (let ((btime (or (car (yynt-select-cache-1
			     project file '("build_time")))
		       "1900-01-01 00:00"))
	    (ctime (yynt-get-file-ctime file)))
	(if (and (not force) (yynt-time-less-p ctime btime))
	    (yynt-log "skip" t)
	  (let* ((buf0 (get-file-buffer file))
		 (buf (if buf0 buf0 (find-file-noselect file))))
	    (with-current-buffer buf
	      (unwind-protect
		  (let* ((props (yynt-get-org-keywords attrs))
			 (res (yynt--export-current-buffer fn plist))
			 (time (format-time-string
				"%Y-%m-%d %T" (current-time) t)))
		    (if (not res) (yynt-log "fail" t)
		      (yynt-upsert-cache
		       project file
		       (cons "build_time" (car props))
		       (cons time (cdr props)))))
		(unless buf0 (kill-buffer))))))))))

(defun yynt-export-files (bobj files &optional ex force)
  "export one file under BOBJ"
  (let ((info (if ex (yynt-combine-plists
		      (yynt-build--info bobj)
		      (yynt-build--info-ex bobj))
		(yynt-build--info bobj))))
    (dolist (f files)
      (yynt--do-export bobj info file force))))

(defun yynt-export-extra-files (project files)
  "export whole build object's external files"
  (let* ((base-path (yynt-project--dir project))
	 (full-files (mapcar (lambda (x) (file-name-concat base-path x))
			     files)))
    (mapc (lambda (x)
	    (if-let ((obj (yynt-get-file-build-object x project)))
		(yynt-export-files obj (list x) t t)
	      (error "file %s not belongs to any build object" x)))
	  full-files)))

(defun yynt-export-build-object (bobj &optional force noexternal)
  "export whole build object's files"
  (let ((type (yynt-build--type bobj)))
    (pcase type
      (0 (let ((file (yynt-build--path bobj)))
	   (yynt-export-files bobj (list file) nil force)))
      ((or 1 2)
       (let* ((co-fn (yynt-build--collect bobj))
	      (co-fn-ex (yynt-build--collect-ex bobj))
	      (files-1 (funcall co-fn bobj))
	      (files-2 (funcall co-fn-ex bobj)))
	 (yynt-export-files bobj files-1 nil force)
	 (yynt-export-files bobj files-2 t force)))
      (_ (error "not a valid build object type: %s" type)))
    (unless noexternal
      (yynt-export-extra-files
       (yynt-build--project bobj)
       (yynt-build--ext-files bobj)))))

(defun yynt-export-build-object-list (bobjs &optional force)
  "export a list of build object"
  (when bobjs
    (let ((ext-files (cl-delete-duplicates
		      (apply #'append
			     (mapcar #'yynt-build--ext-files bobjs))
		      :test #'equal)))
      (dolist (b bobjs) (yynt-export-build-object b force))
      (yynt-export-extra-files (yynt-build--project (car bobjs))
			       ext-files))))

(defun yynt-export-file (filename &optional force)
  "User Command, export current buffer
If called interactively, use current's buffer-file-name as FILENAME

If invoked with C-u, force export"
  (interactive (list (buffer-file-name) current-prefix-arg))
  (if (null filename)
      (user-error "buffer seems not have `buffer-file-name'")
    (let ((bobj (yynt-get-file-build-object filename)))
      (if (null bobj)
	  (user-error "file %s seems not belong to one build object" filename)
	(if (eq 0 (yynt-build--type bobj))
	    (yynt-export-build-object bobj force)
	  (let* ((len (yynt-build--path bobj))
		 (basename (yynt-get-file-build-basename filename bobj)))
	    (cond
	     ((cl-member basename (funcall (yynt-build--collect bobj) bobj)
			 :key (lambda (x) (substring x len)))
	      (yynt-export-files bobj (list filename) nil force))
	     ((cl-member basename (funcall (yynt-build--collect-ex bobj) bobj)
			 :key (lambda (x) (substring x len)))
	      (yynt-export-files bobj (list filename) t force))
	     (t (user-error "file seems not a exportable file"))))
	  (yynt-export-extra-files project (yynt-build--ext-files bobj)))))))

(defun yynt-get-project-build-relative-path (&optional project)
  "return alist of bobj's relative path name and bobj"
  (ignore (or project (setq project yynt-current-project)))
  (let* ((bobjs (yynt-project--builds project))
	 (len (length (yynt-project--dir project))))
    (mapcar (lambda (x) (cons (substring (yynt-build--path x) len) x))
	    bobjs)))

(defun yynt-export-build (bobj &optional force)
  "User Command, export selected build object"
  (interactive (list (completing-read
		      "Select a build object:> "
		      (cons '("t" . t)
			    (mapcar (lambda (x) (cons (yynt-build--name x) x))
				    (yynt-project--builds
				     yynt-current-project)))
		      nil t)
		     current-prefix-arg))
  (if (eq bobj t)
      (yynt-export-build-object-list
       (yynt-project--builds yynt-current-project))
    (yynt-export-build-object bobj force)))


;;; Utils

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


(provide 'yynt)
;;; yynt.el ends here
