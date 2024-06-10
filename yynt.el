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

;; TODO:
;; - Better error signal
;; - Consider make yynt-build's fn more generic(MAYBE)
;; - Consider add dired support for export and publish
;; - Consider add a transient interface
;; - Improve documentation and comments

;;; Code:

(require 'cl-lib)
(require 'sqlite)

(unless (sqlite-available-p)
  (error "Please ensure your emacs supports builtin sqlite utilities"))

(unless (version<= "3.38" (sqlite-version))
  (error "Please use sqlite version>= 3.38"))

(defgroup yynt nil
  "A simple Org publish manager."
  :group 'applications)

(defcustom yynt-use-logger t
  "Use logger or not.

The logger will be used to output export messages and file publish messages."
  :type 'boolean)

;;; Definition of yynt-project and some helper functions.

(defvar yynt-project-list nil
  "List of all yynt project.")
(defvar yynt-current-project nil
  "Current project's `yynt-project' object.

Set it using `yynt-choose-project'.")

(defconst yynt-project-fixed-fields
  '("file_name" "build_name" "ex" "export_time" "publish_time")
  "Inherent fields in the cache database.

All tables use the relative \"PATH\" of the file, relative to the
PROJECT directory, as the primary key.

FILE_NAME is the name of the file, without any directory prefixes.

BUILD_NAME is the name of the BUILD_OBJECT to which the file belongs.

EX indicates whether the file comes from the collect-ex function of the
BUILD OBJECT. If it is 1, it comes from collect-ex; otherwise(0), it
comes from the collect function.

EXPORT_TIME records the export time of the file in UTC+0.

PUBLISH_TIME records the publication time of the file, only used for
exported files and resources that need to be published.")

(cl-defstruct (yynt-project (:conc-name yynt-project--)
			    (:constructor yynt-project--make)
			    (:copier nil))
  "Struct definition for yynt projects. Create it with `yynt-create-project'.

Please create a build object belonging to the project by calling
`yynt-create-build' after calling `yynt-create-project'."
  (name nil :documentation "Symbol of the project name.")
  (dir nil :documentation "Full path of the project.")
  (pubdir nil :documentation "Full path of the publish directory.")
  (cache nil :documentation "Full path of the cache file.")
  (cache-items nil :documentation "Items that need to be cached.")
  (builds nil :documentation "List of `yynt-build' objects."))

(defun yynt--string-nw-p (s)
  "Return t if S is a string containing a non-blank character.
Otherwise, return nil.

Get from `org-string-nw-p'."
  (and (stringp s) (string-match-p "[^ \r\t\n]" s)))

(defun yynt--cache-item-p (s)
  "Return t if S is a valid cache-item name. Otherwise return nil."
  (and (yynt--string-nw-p s)
       (string-match-p "[A-Za-z][0-9A-Za-z_]*" s)
       (not (member s (cons "path" yynt-project-fixed-fields)))))

(defun yynt--valid-filename-p (file)
  "Determine if FILE is a valid path string."
  (and (yynt--string-nw-p file)
       (not (string-match-p file-name-invalid-regexp file))))

(defun yynt--valid-rela-filename-p (file)
  "Determine if FILE is a valid relative path string.

A relative path cannot start with \"/\"."
  (and (yynt--valid-filename-p file)
       (not (string-match-p "^/" file))))

(defun yynt-create-project (name pubdir cache cache-items &optional directory)
  "Create a new `yynt-project' object.

NAME is the name symbol of the project, which must be a non-nil and
non-keyword symbol.

PUBDIR is the publish directory, which must be of `string' type or
nil (means no need to publish). If it is a relative path, it is relative
to DIRECTORY. PUBDIR and DIRECTORY cannot be the same directory. If
PUBDIR is nil, the entire project will not be exported.

CACHE can be nil or the path to the cache file. If it is a relative
path, it is relative to the DIRECTORY. If CACHE is nil, it indicates
that the caching mechanism is not used.

CACHE-ITEMS is a list of strings that specifies the fields that need to
be present in the database for the creation of the cache database.

Items in CACHE-ITEMS must satisfy the `yynt--cache-item-p' predicate,
which means they must start with a letter and match the regular
expression \"[0-9A-Za-z_]\". Additionally, they cannot contain items
with the same names as those in `yynt-project-fixed-fields' (plus
\"path\").

DIRECTORY is the root directory of the project. If DIRECTORY is not
provided, it will use `default-directory' as the default value."
  ;; Check arguments
  (when (or (not (symbolp name)) (null name) (keywordp name))
    (error "not a valid project name: %s" name))
  (unless (or (null pubdir) (yynt--valid-filename-p pubdir))
    (error "not a valid pubdir: %s" pubdir))
  (unless (or (null cache) (yynt--valid-filename-p cache))
    (error "not a valid cache: %s" cache))
  (unless (cl-every #'yynt--cache-item-p cache-items)
    (error "not a valid cache-items %s" cache-items))
  (unless (or (null directory) (yynt--valid-filename-p directory))
    (error "directory's type is not correct: %s" directory))
  ;; Normalization of the directory
  (cond
   ((null directory) (setq directory default-directory))
   ((and (file-exists-p directory)
	 (file-directory-p directory))
    (setq directory (file-name-as-directory (expand-file-name directory))))
   (t (error "directory may not exist or not a true directory: %s" directory)))
  ;; Normalization of the publish directory
  (unless (or (null pubdir) (file-name-absolute-p pubdir))
    (setq pubdir (expand-file-name pubdir directory)))
  (when (and pubdir (file-equal-p directory pubdir)) ; pubdir could be nil
    (error "pubdir and directory are the same directory: %s" directory))
  ;; Create Publish directory if pub is non-nil and not exists
  (unless (or (null pubdir) (file-exists-p pubdir))
    (make-directory pubdir t))
  ;; Normalization of the cache file path
  (unless (or (null cache) (file-name-absolute-p cache))
    (setq cache (expand-file-name cache directory)))

  ;; Since sqlite-open will create the file, we do not need to create the cache
  ;; file ourselves here.
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
      (yynt--initialize-cache project))
    project))

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

(defun yynt--in-project-p (file project)
  "Determine if a file is located within the project."
  ;; `file-in-directory-p' is too slow
  ;;(file-in-directory-p file (yynt-project--dir project))
  (let* ((pdir (yynt-project--dir project))
	 (len (length pdir)))
    (and (>= (length file) len)
	 (string= pdir (substring file 0 len)))))

(defun yynt--project-has-cache-p (project)
  "Determine whether PROJECT object uses the caching mechanism."
  (yynt-project--cache project))

(defun yynt--project-has-pubdir-p (project)
  "Determine whether PROJECT has pubdir"
  (yynt-project--pubdir project))

(defun yynt-get-file-project-basename (file project)
  "Get the relative path of a FILE with respect to its PROJECT."
  (if (not (yynt--in-project-p file project))
      (error "file %s not in project %s" file project)
    (let ((dir (yynt-project--dir project)))
      (substring file (length dir)))))

(defun yynt-get-file-project-fullname (file project)
  "Get the full path of FILE with respect to its PROJECT."
  (let ((dir (yynt-project--dir project)))
    (file-name-concat dir file)))

;;; Implementation of the caching functionality.

;; In yynt, the database is primarily used for caching during export or publish,
;; specifically to check if the modification time of the source files is later
;; than the export time or publish time.

;; Additionally, this cache database can store some file-related metadata during
;; export, such as tags, article titles, or summary content. This allows for
;; quick generation of summary content without having to read each file
;; individually(Instead, we can read from database).

;; The database has the following format:
;; | path | fixed_field | ... | attrs  | ... |

(defvar yynt--sqlite-obj nil
  "Temporary database object, used in conjunction with `yynt-with-sqlite'.")
(defun yynt--open-sqlite (file)
  "Call `sqlite-open' to open FILE and return an sqlite3 database object."
  (sqlite-open file))
(defun yynt--close-sqlite (obj)
  "Call `sqlite-close' to close the database object."
  (when (sqlitep obj) (sqlite-close obj)))
(defmacro yynt-with-sqlite (project &rest body)
  "Open the project's cache file, evaluate the BODY, close the file.
The value returned is the value of the last form in the body.

If PROJECT does not support the cache mechanism, the effect of this
macro is equivalent to progn."
  (declare (indent 1))
  (let ((proj (gensym)))
    `(let* ((,proj ,project)
	    (cache (yynt-project--cache ,proj))
	    (yynt--sqlite-obj (when cache (yynt--open-sqlite cache))))
       (unwind-protect ,(cons 'progn body)
	 (yynt--close-sqlite yynt--sqlite-obj)))))

(defun yynt--initialize-cache (project)
  "Initialize the cache database, creating table YYNT if not exist.

If the table YYNT already exists, this function will check if the fields
in the project's cache-items match exactly with the fields in the
table. If they do not match, the table YYNT will be recreated.

The data with the same names from the old table will be copied to the
new table, and the old table will be replaced by the new table with the
same name."
  (let* ((fields (yynt-project--cache-items project)))
    (yynt-with-sqlite project
      (if-let* ((info (sqlite-select yynt--sqlite-obj
				     "PRAGMA table_info(yynt)"))
		;; https://www.sqlite.org/pragma.html#pragma_table_info
		;; | cid | name | type | notnull | dflt_value | pk |
		(old-fields (mapcar (lambda (ls) (nth 1 ls)) info)))
	  (unless (equal fields (cdr old-fields)) ; ignore path
	    (let* ((new (mapconcat #'identity fields ","))
		   (shr (mapconcat
			 #'identity
			 ;; retrieve common fields
			 (cl-intersection fields old-fields :test #'string=)
			 ",")))
	      (with-sqlite-transaction yynt--sqlite-obj
		;; drop the temporary table TEMP if exists
		(sqlite-execute yynt--sqlite-obj "DROP TABLE IF EXISTS temp;")
		;; create temporary table TEMP with new items
		(sqlite-execute
		 yynt--sqlite-obj
		 (format "CREATE TABLE temp (path PRIMARY KEY,%s);" new))
		;; insert rows from old table to new table
		(sqlite-execute
		 yynt--sqlite-obj
		 (format "INSERT INTO temp (path,%s) SELECT path,%s FROM yynt;"
			 shr shr))
		;; drop old table YYNT
		(sqlite-execute yynt--sqlite-obj "DROP TABLE yynt;")
		;; rename table TEMP to YYNT
		(sqlite-execute yynt--sqlite-obj
				"ALTER TABLE temp RENAME to yynt;"))))
	;; table yynt not ever exists
	(sqlite-execute yynt--sqlite-obj
			(format "CREATE TABLE yynt (path PRIMARY KEY,%s)"
				(mapconcat #'identity fields ",")))))))

(defun yynt--clear-cache ()
  "Clear the table YYNT."
  (sqlite-execute yynt--sqlite-obj "DELETE FROM yynt;"))

(defun yynt--upsert-cache (project file keys values)
  "Update or insert a row: update if the file already exists, otherwise insert.

Ensure that `yynt--sqlite-obj' belongs to the PROJECT."
  (let* ((base (yynt-get-file-project-basename file project))
	 (k-fields (mapconcat #'identity (cons "path" keys) ","))
	 (v-fields (mapconcat (lambda (x) (format "'%s'" x))
			      (cons base values) ","))
	 (kv-seq (mapconcat (lambda (x) (format "%s='%s'" (car x) (cdr x)))
			    (cl-mapcar #'cons keys values) ",")))
    (sqlite-execute
     yynt--sqlite-obj
     (format "\
INSERT INTO yynt (%s) VALUES (%s)
ON CONFLICT(path) DO UPDATE SET %s"
	     k-fields v-fields kv-seq))))

(defun yynt--delete-cache (project file)
  "Delete a row using FILE as the primary key, if it exists.

Ensure that `yynt--sqlite-obj' belongs to the PROJECT."
  (let ((key (yynt-get-file-project-basename file project)))
    (sqlite-execute
     yynt--sqlite-obj
     "DELETE FROM yynt WHERE path=?" (list key))))

(defun yynt--select-cache (project file keys)
  "Retrieve a record from the database.

Ensure that `yynt--sqlite-obj' belongs to the PROJECT."
  (car-safe
   (sqlite-execute
    yynt--sqlite-obj
    (format "SELECT %s FROM yynt where path='%s'"
	    (mapconcat #'identity keys ",")
	    (yynt-get-file-project-basename file project)))))

;; We don't provide `yynt-execute', it is dangerous.
;; If you really want to manually modify the database, please use the following
;; functions with caution.

;; (defun yynt-execute (project query context &optional values)
;;   (unless (yynt-project-p project)
;;     (error "not a valid yynt-project: %s" project))
;;   (unless (yynt--project-has-cache-p project)
;;     (error "project has no cache file: %s" (yynt-project--name project)))
;;   (if (null context)
;;       (sqlite-execute yynt--sqlite-obj query values)
;;     (yynt-with-sqlite project
;;       (sqlite-execute yynt--sqlite-obj query values))))

(defun yynt-select (project query context &optional values return-type)
  "Select data from PROJECT cache database that matches QUERY.

If context is non-nil, call `sqlite-select' with sqlite context.

For the argument VALUES and RETURN-TYPE, see `sqlite-select' docstring."
  (unless (yynt-project-p project)
    (error "not a valid yynt-project: %s" project))
  (unless (yynt--project-has-cache-p project)
    (error "project has no cache file: %s" (yynt-project--name project)))
  (if (null context)
      (sqlite-select yynt--sqlite-obj query values return-type)
    (yynt-with-sqlite project
      (sqlite-select yynt--sqlite-obj query values return-type))))

(defun yynt-delete-missing-cache (project)
  "Delete the rows in the PROJECT cache where the files do not exist."
  (interactive (list yynt-current-project))
  (if (not (yynt-project-p project))
      (user-error "not a project object")
    (if (not (yynt--project-has-cache-p project))
	(user-error "project doesn't have cache file")
      (yynt-with-sqlite project
	(let ((files (car (sqlite-select yynt--sqlite-obj
					 "SELECT path FROM yynt")))
	      nofiles)
	  (dolist (f files)
	    (let ((full (yynt-get-file-project-fullname f project)))
	      (unless (file-exists-p full)
		(push full nofiles))))
	  (dolist (n nofiles)
	    (yynt--delete-cache project n))
	  (message "delete %s rows" (length nofiles))
	  (length nofiles))))))


;;; Definition of yynt-build and some helper functions.

;; The build structure includes all the information I believe is needed for
;; export and publish. I have classified the subprojects that need to be
;; exported and published into three categories: single file, single-level file
;; directory, and two-level file directory. They are:

;; - single file (type-0): just a file and related resources.
;; - single-level directory (type-1): {dir}/*.org and related resources.
;; - two-level directory (type-2): {dir}/*/*.org and related resources.

;; Type-2 is quite special; For publishing, it requires specifying the
;; directories to be exported and the exclusion items within each
;; directory. This requires the user to fill in the collect-2 and excluded-fn-2
;; fields.

;; Please read the docstrings of the yynt-build structure and the
;; `yynt-create-build' function to understand their usage in detail.

(cl-defstruct (yynt-build (:conc-name yynt-build--)
			  (:constructor yynt-build--make)
			  (:copier nil))
  "Struct that contains build info of a series of files.

Create it with `yynt-create-build'."
  (project nil :documentation "Project that belongs to.")
  (name nil :documentation "Name of this build object.")
  (path nil :documentation "Relative path to project's root.")
  (type 0 :documentation "Type number of build object: [012].")
  (collect #'ignore :documentation "(bobj) => list of export files.")
  (info nil :documentation "(Org) export info plist.")
  (collect-ex #'ignore :documentation "(bobj) => list of other export files.")
  (info-ex nil :documentation "Export info plist for collect-ex.")
  (fn #'ignore :documentation "Export funtion: (PLIST IN OUT) => nil.")
  (attrs nil :documentation "Keywords extract from source file.")
  (no-cache-files-ht nil :documentation "Files without caching.")
  (ext-files nil :documentation "External files depend on this build object.")
  (published nil :documentation "Whether to publish.")
  (convert-fn nil :documentation "Convert input file to output file name.")
  (included-resources nil :documentation "Resources to be published.")
  (collect-2 #'ignore :documentation "(bobj) => list of dir for pub.")
  (excluded-fn-2 nil :documentation "(bobj path) => excluded pred."))

(cl-defun yynt-create-build (&key project path type collect info collect-ex
				  info-ex fn attrs no-cache-files ext-files
				  published convert-fn included-resources
				  collect-2 excluded-fn-2)
  "Create a new `yynt-build' object.

PROJECT is used as the project object pointed to by the build object.

PATH is a path string relative to the project path.

TYPE is a numeric identifier for the build object' type. It can be 0, 1,
or 2, representing a single file, one-level directory, and two-level
directory, respectively.

COLLECT is a function that accepts a build object and returns all the
files in the build object that need to be exported. Type 0 projects do
not require this parameter.

INFO is a plist containing export configuration information. For
details, refer to the EXT-PLIST docstring in `org-export-as'.

COLLECT-EX accepts a build object as a parameter and returns additional
files that need to be exported. Type 0 projects do not require this
parameter. For a type-0 build object, if is t, then the file type is EX
and COLLECT-EX will return list of this file.

INFO-EX provides export information for the files obtained from
COLLECT-EX. It will layer over INFO and may override some of INFO's
information.

FN is the export function that will export the current buffer. It
accepts INFO plist and OUTFILE path as parameters. If the function does
not signal an error, the export is considered successful.

ATTRS is a list of keywords used to obtain metadata from the files to be
exported. They must belong to the project's cache-items and cannot
include keywords from `yynt-project-fixed-fields'.

NO-CACHE-FILES specifies files that do not need to be cached. They are
paths relative to the build object path. For type 0 projects, set to nil
to indicate none exist, or t to cache the single file.

EXT-FILES specifies a list of files that depend on the build object's
content. They are paths relative to the project object's
directory. EXT-FILES must be files from function COLLECT-EX.

CONVERT-FN converts the name of the file to be exported into the name of
the resulting exported file.

INCLUDED-RESOURCES is a list of resource files that the build object
needs to publsih. They are paths relative to the build object's
directory. For type 0 projects, they are relative to the project's
directory.

COLLECT-2 accepts bobj as a parameter and returns a list of directories
that need to be published. The returned directories are paths relative
to the bobj directory. It is only used for type 2 build objects.

EXCLUDED-FN-2 is a function that accepts bobj and PATH and returns a
predicate function. PATH is the path relative to the bobj directory that
contains resources. The returned predicate function returns nil for file
names that need to be published and t for file names that do not need to
be published."
  ;; FIXME: Add external-files checker
  (unless (yynt-project-p project)
    (error "seems not a valid yynt-project: %s" project))
  (unless (and (integerp type) (<= 0 type 2))
    (error "seems not a valid yynt-object type: %s" type))
  (unless (and (yynt--valid-rela-filename-p path)
	       (file-exists-p (file-name-concat
			       (yynt-project--dir project)
			       path)))
    (error "path not valid or not exist: %s" path))
  (unless (or (eq type 0) (functionp collect))
    (error "not a valid collect function: %s" collect))
  (unless (plistp info)
    (error "not a valid info plist: %s" info))
  (unless (or (eq type 0) (functionp collect-ex))
    (error "not a valid collect-ex function: %s" collect-ex))
  (unless (or (eq type 0) (plistp info-ex))
    (error "not a valid info-ex plist: %s" info-ex))
  (unless (functionp fn)
    (error "not a valid export function: %s" fn))
  (unless (and (cl-every #'yynt--cache-item-p attrs)
	       (cl-subsetp attrs (yynt-project--cache-items project)
			   :test #'string=))
    (error "not a valid attrs %s" attrs))
  (unless (or (and (eq type 0) (eq no-cache-files t))
	      (cl-every #'yynt--valid-rela-filename-p no-cache-files))
    (error "not a valid no-cache-file list: %s" no-cache-files))
  (unless (cl-every #'yynt--valid-rela-filename-p ext-files)
    (error "not a valid external file list: %s" ext-files))
  (unless (functionp convert-fn)
    (error "not a valid convert-fn: %s" convert-fn))
  (unless (cl-every #'yynt--valid-rela-filename-p included-resources)
    (error "not a valid included-resources: %s" included-resources))
  (when (and (eq type 2) published (not (functionp collect-2)))
    (error "not a valid collect-2 function: %s" collect-2))
  (when (and (eq type 2) published (not (functionp excluded-fn-2)))
    (error "not a valid excluded-fn-2 function: %s" excluded-fn-2))
  (let* ((project-dir (yynt-project--dir project))
	 (full-path (expand-file-name path project-dir))
	 (build-path (if (not (file-directory-p full-path)) full-path
		       (file-name-as-directory full-path)))
	 (name (if (not (file-directory-p build-path)) path
		 (directory-file-name path)))
	 (ht (let ((ht (make-hash-table :test #'equal)))
	       (if (eq type 0) ; type 0 just check itself
		   (if (null no-cache-files) ht
		     (prog1 ht (puthash path t ht)))
		 (dolist (x no-cache-files ht) (puthash x t ht)))))
	 ;; translate include-resources to full path
	 ;; note for type-0 build object, we use project's dir as base dir
	 (i-res (if (eq type 0)
		    (mapcar (lambda (x) (yynt-get-file-project-fullname x project))
			    included-resources)
		  (mapcar (lambda (x) (expand-file-name x build-path))
			  included-resources))))
    (when (eq type 0)
      (let ((co-func (let ((x `(,build-path))) (lambda (_bobj) x))))
	(if (null collect-ex)
	    (setq collect co-func
		  collect-ex #'ignore)
	  (setq collect-ex co-func
		collect #'ignore))))
    (unless (cl-every #'file-exists-p i-res)
      (error "seems some resources not exist: %s" i-res))
    (let ((obj (yynt-build--make
		:project project
		:name name :path build-path :type type
		:collect collect :info info
		:collect-ex collect-ex :info-ex info-ex
		:fn fn :attrs attrs :no-cache-files-ht ht
		:ext-files ext-files
		:published published :convert-fn convert-fn
		:included-resources i-res
		:collect-2 collect-2 :excluded-fn-2 excluded-fn-2))
	  (builds (yynt-project--builds project)))
      (setf (yynt-project--builds project)
	    (cons obj (cl-remove build-path builds
				 :test #'string=
				 :key #'yynt-build--path)))
      obj)))

(defun yynt-get-file-build-basename (file bobj)
  "Get the relative path of FILE with respect to its BOBJ's base directory."
  (if (not (yynt--in-build-p bobj file))
      (error "file %s not in build object %s" file bobj)
    (let ((dir (yynt-build--path bobj)))
      (substring file (length dir)))))

(defun yynt-get-file-build-fullname (file bobj)
  "Get the full path of FILE under BOBJ."
  (let ((dir (yynt-build--path bobj)))
    (file-name-concat dir file)))

(defun yynt--in-build-p (bobj file)
  "Determine if FILE is located within BOBJ.

Used only for files that need to be exported."
  (let ((bpath (yynt-build--path bobj)))
    (pcase (yynt-build--type bobj)
      (0 (string= bpath file))
      (1 (string= bpath (file-name-directory file)))
      (2 (or (string= bpath (file-name-directory file))
	     (string= bpath (file-name-directory
			     (directory-file-name
			      (file-name-directory file))))))
      (_ (error "seems not a valid build object type")))))

(defun yynt--file-no-cache-p (bobj file)
  "Determine if FILE is in no-cache-files-ht.

For type 0, the FILE parameter is not mandatory."
  (if (not (yynt--project-has-cache-p (yynt-build--project bobj))) nil
    (let ((ht (yynt-build--no-cache-files-ht bobj))
	  (type (yynt-build--type bobj)))
      (pcase type
	(0 (= (hash-table-count ht) 1))
	((or 1 2)
	 (let ((name (yynt-get-file-build-basename file bobj)))
	   (gethash name ht)))
	(_ (error "not a valid build-object type: %s" type))))))

(defun yynt--build-can-publish-p (bobj)
  "Determine whether BOBJ can be publish.

If the project to which BOBJ belongs does not have a non-nil pubdir, its
published attribute does not take effect and it will never be published."
  (let ((project (yynt-build--project bobj)))
    (and (yynt--project-has-pubdir-p project)
	 (yynt-build--published bobj))))

(defun yynt-get-file-build-object (file &optional project)
  "Get the corresponding build object based on FILE.

If the PROJECT parameter is not provided, `yynt-current-project' will be
used as the project for lookup."
  (when-let ((project (or project yynt-current-project)))
    (cl-find-if (lambda (b) (yynt--in-build-p b file))
		(yynt-project--builds project))))

;;; Implementation of the build functionality.

;; In this section, the main functions implemented include logging, extracting
;; keyword information from the files to be exported, individual export of Build
;; Objects, batch export of Build Objects, and single file export.

;; The files collected by function collect-ex depend on the files collected by
;; function collect. If the former needs to gather summary information from the
;; latter, the cache database YYNT might be useful.

(defconst yynt--keywords-extract-bound 2048
  "Place bound for finding keywords from the start of buffer.")

;; logger used for export and publish.
(defvar yynt--log-buffer "*yynt*"
  "Buffer for logging.")
(defun yynt--create-log-buffer ()
  "Create or get log buffer."
  (let ((buf (get-buffer-create yynt--log-buffer)))
    (with-current-buffer buf
      (when (not (equal major-mode 'messages-buffer-mode))
	(messages-buffer-mode)))
    buf))
(defun yynt--log (message &optional newline)
  "Write a log message to buffer named `yynt--log-buffer'.

If NEWLINE is non-nil, this function will add a newline at the
end of the MESSAGE."
  (when yynt-use-logger
    (with-current-buffer (yynt--create-log-buffer)
      (let ((inhibit-read-only t))
	(goto-char (point-max))
	(insert (concat message (and newline "\n"))))))
  nil)

(defun yynt-logger ()
  "Show logger buffer."
  (interactive)
  (switch-to-buffer-other-window (yynt--create-log-buffer)))

(defun yynt-combine-plists (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting
properties from the other lists. Settings in the last list are
the most significant ones and overrule settings in the other lists.

Copied from `org-combine-plists'"
  (let ((rtn (copy-sequence (pop plists)))
	p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
	(setq p (pop ls) v (pop ls))
	(setq rtn (plist-put rtn p v))))
    rtn))

(defun yynt--get-org-keywords (infos)
  "Get (keyword . value) alist from the head of current buffer.

INFOS is a list of keywords, keywords are
case-insensitive. Format of keywords is #+KEY: VALUE.

Get from https://github.com/bastibe/org-static-blog."
  (let ((case-fold-search t)
	res-key res-val)
    (save-excursion
      (dolist (a infos)
	(goto-char (point-min))
	(when (search-forward-regexp
	       (concat "^\\#\\+" a ":[ ]*\\(.+\\)$")
	       (min yynt--keywords-extract-bound (point-max))
	       t)
	  (push a res-key)
	  (push (match-string 1) res-val))))
    (cons res-key res-val)))

;; Time functions.
(defun yynt--get-file-ctime (filepath)
  "Get file's ctime, utc+0."
  (format-time-string
   "%Y-%m-%d %T"
   (nth 5 (file-attributes filepath)) t))

(defun yynt--get-current-time ()
  "Get the current UTC+0 time as a string in the format
YYYY-MM-DD hh:mm:ss."
  (format-time-string "%Y-%m-%d %T" (current-time) t))

(defun yynt--time-less-p (st1 st2)
  "Tell if st1's time is less than st2. Time format must be
 YYYY-MM-DD hh:mm:ss"
  (time-less-p (date-to-time st1)
	       (date-to-time st2)))

(define-inline yynt-buildm-collect (bobj)
  "Call the collect function of BOBJ to retrieve the files."
  (inline-letevals (bobj)
    (inline-quote (funcall (yynt-build--collect ,bobj) ,bobj))))

(define-inline yynt-buildm-collect-ex (bobj)
  "Call the collect-ex function of BOBJ to retrieve the files."
  (inline-letevals (bobj)
    (inline-quote (funcall (yynt-build--collect-ex ,bobj) ,bobj))))

(define-inline yynt-buildm-project-name (bobj)
  "Retrieve the name of the PROJECT to which BOBJ points."
  (inline-quote (yynt-project--name (yynt-build--project ,bobj))))

(define-inline yynt-buildm-convert (bobj file)
  "Retrieve the export file name of FILE under BOBJ."
  (inline-letevals (bobj file)
    (inline-quote (funcall (yynt-build--convert-fn ,bobj) ,file))))

(define-inline yynt-buildm-collect-2 (bobj)
  "Call the collect-2 function of BOBJ to retrieve type-2 publish directories."
  (inline-letevals (bobj)
    (inline-quote (funcall (yynt-build--collect-2 ,bobj) ,bobj))))

(defun yynt--export-current-buffer (fn plist in out)
  "Call the export function. If the function does not signal an
error, the export is considered successful and returns t; otherwise, it
returns nil.

If debug-on-error is enabled through toggle-debug-on-error, errors will
not be caught."
  (condition-case-unless-debug nil
      (prog1 t (funcall fn plist in out))
    (error nil)))

(defun yynt--do-export ( project bobj pname bname attrs
			 fn plist file out-file ex &optional force)
  "Execute the specific export function, write log messages, and
update the cache if necessary.

For files that need to be cached, this function requires an sqlite
context. If the file needs to be cached and its ctime is later than the
cached export time, the file will be exported.

If the parameter FORCE is non-nil, the file will always be exported."
  (let* ((basename (yynt-get-file-project-basename file project)))
    (yynt--log (format "[%s → %s] %s exporting... " pname bname basename))
    (with-temp-buffer
      (insert-file-contents file)
      (if (yynt--file-no-cache-p bobj file) ; don't need cache
	  (if (yynt--export-current-buffer fn plist file out-file)
	      (yynt--log "ok" t) (yynt--log "fail" t))
	(let ((btime (or (car (yynt--select-cache
			       project file '("export_time")))
			 "2000-01-01 00:00"))
	      (ctime (yynt--get-file-ctime file)))
	  (if (and (not force) (yynt--time-less-p ctime btime))
	      (yynt--log "skip" t)
	    (let* ((props (yynt--get-org-keywords attrs))
		   (res (yynt--export-current-buffer fn plist file out-file))
		   (time (yynt--get-current-time)))
	      (if (not res) (yynt--log "fail" t)
		(yynt--upsert-cache
		 project file
		 (append '("file_name" "build_name" "ex" "export_time")
			 (car props))
		 (append (list (file-name-nondirectory file)
			       bname (if ex 1 0) time)
			 (cdr props)))
		(yynt--log "ok" t)))))))))

(defun yynt--export-files (bobj files &optional ex force)
  "Export the list of files FILES located under BOBJ.
Return list of generated-file.

If the EX parameter is non-nil, the plist used for export will combine
info and info-ex."
  (let* ((project (yynt-build--project bobj))
	 (pname (yynt-project--name project))
	 (bname (yynt-build--name bobj))
	 (attrs (yynt-build--attrs bobj))
	 (fn (yynt-build--fn bobj))
	 (info (if ex (yynt-combine-plists
		       (yynt-build--info bobj)
		       (yynt-build--info-ex bobj))
		 (yynt-build--info bobj)))
	 (convert-fn (yynt-build--convert-fn bobj))
	 (out-files (mapcar convert-fn files)))
    (cl-mapc (lambda (f g)
	       (yynt--do-export project bobj pname bname attrs
				fn info f g ex force))
	     files out-files)))

(defun yynt--export-external-files (project files)
  "Export the external file list FILES in PROJECT. Return the list
of generated files."
  (let* ((full-files (mapcar (lambda (x) (yynt-get-file-project-fullname x project))
			     files))
	 res)
    (dolist (f full-files res)
      (if-let* ((obj (yynt-get-file-build-object f project))
		(cov-fn (yynt-build--convert-fn obj)))
	  (progn (yynt--export-files obj (list f) t t)
		 (push (cons (funcall cov-fn f) obj) res))
	(error "file %s not belongs to any build object" f)))))

(defun yynt--collect-external-files (bobjs)
  "Retrieve all ext-files from the BOBJS list.

BOBJS must belong to the same project."
  (let* ((all-list (mapcar #'yynt-build--ext-files bobjs))
	 (rtn (copy-sequence (pop all-list)))
	 ls e)
    (while all-list
      (setq ls (pop all-list))
      (while ls
	(setq e (pop ls))
	(cl-pushnew e rtn :test #'string=)))
    rtn))

(defun yynt-export-build-object-list (bobjs &optional force)
  "Export all files in the build object list BOBJS.

BOBJS must belong to the same project."
  (when bobjs
    (let* ((ext-files (yynt--collect-external-files bobjs))
	   (project (yynt-build--project (car bobjs)))
	   (ext-builds (mapcar (lambda (x)
				 (yynt-get-file-build-object
				  (yynt-get-file-project-fullname x project)
				  project))
			       ext-files))
	   (exts (cl-mapcar #'cons ext-files ext-builds))
	   (necessary-ext-files (mapcar #'car (cl-remove-if
					       (lambda (x) (member (cdr x) bobjs))
					       exts))))
      (yynt--log (format "<<<EXPORTING %s object in {%s} --- %s\n"
			 (length bobjs)
			 (yynt-project--name project)
			 (yynt--get-current-time)))
      (yynt-with-sqlite project
	(dolist (b bobjs)
	  (let ((files-1 (yynt-buildm-collect b)))
	    (yynt--log (format "<EXPORTING {%s → %s} --- %s\n"
			       (yynt-project--name project)
			       (yynt-build--name b)
			       (yynt--get-current-time)))
	    (yynt--export-files b files-1 nil force)))
	(yynt--log (format "<<EXPORTING %s object's ex files --- %s\n"
			   (length bobjs)
			   (yynt--get-current-time)))
	(dolist (b bobjs)
	  (let ((files-2 (yynt-buildm-collect-ex b)))
	    (yynt--export-files b files-2 t force)))
	(if (null necessary-ext-files)
	    (yynt--log "<<NO EXTERNAL FILES\n")
	  (yynt--log (format "<<EXPORTING EXTERNAL FILES --- %s\n"
			     (yynt--get-current-time)))
	  (yynt--export-external-files project necessary-ext-files))))))

(defun yynt-export-build (bname &optional force)
  "Export selected build object."
  (interactive (list (completing-read
		      "Select a build object:> "
		      (cons "*t*"
			    (mapcar #'yynt-build--name
				    (yynt-project--builds
				     yynt-current-project)))
		      nil t)
		     current-prefix-arg))
  (let ((start-time (float-time)))
    (if (equal bname "*t*")
	(yynt-export-build-object-list
	 (yynt-project--builds yynt-current-project) force)
      (let ((bobj (car (cl-member
			bname (yynt-project--builds yynt-current-project)
			:test #'string= :key #'yynt-build--name))))
	(yynt-with-sqlite yynt-current-project
	  (yynt-export-build-object-list (list bobj) force))))
    (message "export [%s] in %ss" bname (- (float-time) start-time))))

(defun yynt-export-file (file &optional force)
  "Export current buffer.

If called interactively, use current's `buffer-file-name' as FILE. If
invoked with C-u, force export."
  (interactive (list (buffer-file-name) current-prefix-arg))
  (if (null file)
      (user-error "buffer seems not have `buffer-file-name'")
    (let* ((bobj (yynt-get-file-build-object file)))
      (if (null bobj)
	  (user-error "file %s seems not belong to any build object" file)
	(let* ((project (yynt-build--project bobj))
	       (conv-fn (yynt-build--convert-fn bobj))
	       res res-ex)
	  (yynt--log (format "<<<EXPORTING %s in {%s → %s} --- %s\n"
			     (yynt-get-file-project-basename file project)
			     (yynt-buildm-project-name bobj)
			     (yynt-build--name bobj)
			     (yynt--get-current-time)))
	  (yynt-with-sqlite project
	    (let* ((basename (yynt-get-file-build-basename file bobj))
		   (get-fn (lambda (x) (yynt-get-file-build-basename x bobj))))
	      (cond
	       ((cl-member basename (yynt-buildm-collect bobj)
			   :key get-fn :test #'string=)
		(yynt--export-files bobj (list file) nil force)
		(push (funcall conv-fn file) res)
		;; export rest ex files
		(let ((ex-files (yynt-buildm-collect-ex bobj)))
		  (yynt--export-files bobj ex-files t force)
		  (dolist (f ex-files) (push (funcall conv-fn f) res-ex))))
	       ;; file belongs to ex fileset
	       ((cl-member basename (yynt-buildm-collect-ex bobj)
			   :key get-fn :test #'string=)
		(yynt--export-files bobj (list file) t force)
		(push (funcall conv-fn file) res-ex))
	       (t (user-error "file seems not a exportable file"))))
	    ;; return (bobj res res-ex res-ext)
	    ;; used by `yynt-publish-file'
	    (cl-values bobj res res-ex
		       (mapcar #'car
			       (yynt--export-external-files
				project (yynt-build--ext-files bobj))))))))))

(defun yynt-export-current-buffer (buffer &optional project)
  "Accept a buffer as a parameter and export that buffer."
  (or project (setq project yynt-current-project))
  (let* ((file (buffer-file-name buffer))
	 (bobj (yynt-get-file-build-object file project)))
    (unless file
      (user-error "buffer seems not has a file"))
    (unless (yynt-build-p bobj)
      (user-error "file may not belong to any yynt's any build object"))
    (yynt-with-sqlite project
      (cond
       ((member file (yynt-buildm-collect bobj))
	(yynt--export-files bobj (list file) nil t))
       ((member file (yynt-buildm-collect-ex bobj))
	(yynt--export-files bobj (list file) t t))
       (t (user-error "file may not belong to any yynt's any build object"))))
    (yynt-buildm-convert bobj file)))

;;; Impl of publisher.

;; The main function of publishing is to export the artifacts from each build
;; object and attach their associated resources to the pubdir of the PROJECT.

;; Publishing will also use caching to eliminate unnecessary file copying.

(defun yynt--publish-attachment (file pub-dir)
  "Copy the file FILE to the publish directory PUB-DIR.

See `org-publish-attachment'"
  (unless (file-directory-p pub-dir)
    (make-directory pub-dir t))
  (let ((output (expand-file-name (file-name-nondirectory file) pub-dir)))
    (copy-file file output t)
    output))

(defun yynt--publish-attach-file-cached (project file &optional force)
  "Publish FILE to PROJECT pubdir, write log messages,
and update the cache if necessary.

If FORCE is non-nil, FILE will always be published."
  (when (and (file-exists-p file)
	     (yynt--in-project-p file project))
    (let* ((pname (yynt-project--name project))
	   (rela-file (yynt-get-file-project-basename file project))
	   (rela-dir (file-name-directory rela-file))
	   (pub-dir (file-name-concat (yynt-project--pubdir project)
				      rela-dir)))
      (if (not (yynt--project-has-cache-p project))
	  (progn (yynt--publish-attachment file pub-dir)
		 (yynt--log (format "(%s) %s published" pname file) t))
	(let ((ptime (or (car (yynt--select-cache project file
						  '("publish_time")))
			 "2000-01-01 00:00"))
	      (ctime (yynt--get-file-ctime file)))
	  (if (and (not force) (yynt--time-less-p ctime ptime))
	      (yynt--log (format "(%s) %s skipped"
				 pname rela-file)
			 t)
	    (yynt--publish-attachment file pub-dir)
	    (yynt--upsert-cache project file '("publish_time")
				(list (yynt--get-current-time)))
	    (yynt--log (format "(%s) %s published" pname rela-file) t)))))))

(defun yynt--publish-attach-dir-cached (project dir &optional force)
  "Publish DIR recursively to PROJECT pubdir."
  (when (file-exists-p dir)
    (dolist (x (directory-files-recursively dir ".*"))
      (yynt--publish-attach-file-cached project x force))))

(defun yynt-publish-attach-cached (project file-or-dir-ls &optional force)
  "Publish items in FILE-OR-DIR-LS to PROJECT pubdir."
  (dolist (x file-or-dir-ls)
    (if (file-directory-p x)
	(yynt--publish-attach-dir-cached project x force)
      (yynt--publish-attach-file-cached project x force))))

(defun yynt--directory-files (dir &optional full)
  "Retrieve the list of directories and files excluding \".\" and \"..\"."
  (directory-files dir full directory-files-no-dot-files-regexp))

(defun yynt-publish-build-object-list (bobjs &optional force)
  "Export and Publish all files in BOBJS list"
  (let* ((externals (yynt-export-build-object-list bobjs force))
	 (pub-bobjs (cl-remove-if-not #'yynt--build-can-publish-p bobjs)))
    (if (null pub-bobjs)
	(yynt--log (format ">>>PUBLISH zero build object --- %s\n"
			   (yynt--get-current-time)))
      (yynt--log (format ">>>PUBLISH %s build objects in {%s} --- %s\n"
			 (length pub-bobjs)
			 (yynt-buildm-project-name (car pub-bobjs))
			 (yynt--get-current-time)))
      (let ((project (yynt-build--project (car pub-bobjs))))
	(yynt-with-sqlite project
	  (dolist (p pub-bobjs)
	    (yynt--log (format ">PUBLISH {%s → %s} --- %s\n"
			       (yynt-buildm-project-name p)
			       (yynt-build--name p)
			       (yynt--get-current-time)))
	    (let* ((convert-fn (yynt-build--convert-fn p))
		   (type (yynt-build--type p)))
	      (pcase type
		((or 0 1)
		 (let* ((files (mapcar convert-fn
				       (append (yynt-buildm-collect p)
					       (yynt-buildm-collect-ex p))))
			(res (yynt-build--included-resources p)))
		   (yynt-publish-attach-cached
		    project (append files res) force)))
		(2 (let* ((dirs (yynt-buildm-collect-2 p))
			  (excl-fn (yynt-build--excluded-fn-2 p)))
		     (dolist (d dirs)
		       (let* ((pred (funcall excl-fn p d))
			      (full-dir (yynt-get-file-build-fullname d p))
			      (files (yynt--directory-files full-dir))
			      (final (mapcar
				      (lambda (x) (file-name-concat full-dir x))
				      (cl-remove-if pred files))))
			 (yynt-publish-attach-cached project final force)))
		     (let ((files (mapcar convert-fn
					  (yynt-buildm-collect-ex p))))
		       (yynt-publish-attach-cached project files force)))))))
	  (let ((pub-exts (cl-remove-if-not
			   #'yynt-build--published
			   externals :key #'cdr)))
	    (if (null pub-exts)
		(yynt--log ">>NO EXTERNAL FILES\n")
	      (yynt--log (format ">>PUBLISH EXTERNAL FILES --- %s\n"
				 (yynt--get-current-time)))
	      (let ((files (mapcar #'car pub-exts)))
		(yynt-publish-attach-cached project files force)))))))))

(defun yynt-publish-build (bname &optional force)
  "Interactively choose and publish a BOBJ."
  (interactive (list (completing-read
		      "Select a project:>"
		      (cons "*t*"
			    (mapcar #'yynt-build--name
				    (yynt-project--builds yynt-current-project)))
		      nil t)
		     current-prefix-arg))
  (let ((start-time (float-time)))
    (if (equal bname "*t*")
	(let ((objs (cl-remove-if-not
		     #'yynt--build-can-publish-p
		     (yynt-project--builds yynt-current-project))))
	  (yynt-publish-build-object-list objs force)
	  (message "publish project [%s] in %ss"
		   t (- (float-time) start-time)))
      (let ((bobj (car (cl-member
			bname (yynt-project--builds yynt-current-project)
			:test #'string= :key #'yynt-build--name))))
	(if (not (yynt--build-can-publish-p bobj))
	    (message "seems not a publish-able bobj: [%s]"
		     (yynt-build--name bobj))
	  (yynt-with-sqlite (yynt-build--project bobj)
	    (yynt-publish-build-object-list (list bobj) force))
	  (message "publish project [%s] in %ss"
		   (yynt-build--name bobj)
		   (- (float-time) start-time)))))))

(defun yynt--get-publish-file-2 (file bobj)
  (let* ((bdir (yynt-build--path bobj))
	 (dir-0 (file-name-directory file))
	 (dir (file-name-base
	       (directory-file-name
		(file-name-directory file))))
	 (pred (funcall (yynt-build--excluded-fn-2 bobj)
			bobj dir))
	 (files (let ((default-directory bdir))
		  (yynt--directory-files dir)))
	 (files-1 (cl-remove-if pred files)))
    (mapcar (lambda (x) (file-name-concat dir-0 x))
	    files-1)))

(defun yynt-publish-file (file &optional force)
  "Publish current buffer.

If called interactively, use current’s ‘buffer-file-name’ as FILE.
If invoked with C-u, force publish."
  (interactive (list (buffer-file-name) current-prefix-arg))
  (let ((start-time (float-time))
	(export-res (yynt-export-file file force)))
    (cl-multiple-value-bind (bobj res res-ex res-ext) export-res
      (let ((proj (yynt-build--project bobj))
	    (resource (yynt-build--included-resources bobj))
	    (export-files (append res res-ex res-ext)))
	(if (not (yynt--build-can-publish-p bobj))
	    (message "seems file not in a publish-able build object")
	  (yynt--log (format ">>>PUBLISHING %s in %s--- %s\n"
			     (yynt-get-file-project-basename file proj)
			     (yynt-project--name proj)
			     (yynt--get-current-time)))
	  (yynt-with-sqlite proj
	    (pcase (yynt-build--type bobj)
	      ((or 0 1)
	       (yynt-publish-attach-cached
		proj (append export-files resource) force))
	      (2 (let ((res-2 (and res (yynt--get-publish-file-2 file bobj))))
		   (if (not res-2)
		       (yynt-publish-attach-cached
			proj (append export-files resource) force)
		     (yynt-publish-attach-cached
		      proj (append res-2 res-ex res-ext resource) force))))
	      (_ (error "never happends"))))
	  (message (format "publish file in [%s] fin in %ss"
			   (yynt-build--name bobj)
			   (- (float-time) start-time))))))))

;;; Miscellaneous Utils

(defun yynt-p1 (regexp)
  "Accept a regular expression as input, and return a function that takes
BOBJ as a parameter and returns a list of absolute paths of files under
the BOBJ root directory that match the regular expression.

The function can be used to generate collect member in type-1 object."
  (lambda (bobj)
    (directory-files (yynt-build--path bobj) t regexp)))

(defun yynt-p1s (files)
  "Accept a list of files and return a function that takes BOBJ as a
parameter and returns the list of absolute paths of these files under
the BOBJ directory.

The function can be used to generate collect-ex member in type-1 and
type-2 object."
  (lambda (bobj)
    (mapcar (lambda (x) (file-name-concat (yynt-build--path bobj) x))
	    files)))

(defun yynt-p2 (reg1 reg2)
  "Accept REG1 and REG2 as parameters and return a function that takes BOBJ
as a parameter and returns a list of absolute paths of all files in
type-2 that meet the conditions.

Here, REG1 is used to match all directories in the BOBJ directory that
meet the conditions, and REG2 is used to match the files in each
directory that meet the conditions.

The function can be used to generate the collect member function for type-2."
  (lambda (bobj)
    (let ((dirs (cl-remove-if-not
		 #'file-directory-p
		 (directory-files (yynt-build--path bobj) t reg1)))
	  ret)
      (dolist (d dirs ret)
	(dolist (f (directory-files d t reg2))
	  (push f ret))))))

(defun yynt-c2 (reg)
  "Accept a regular expression as a parameter and return a function that
takes BOBJ as a parameter and returns a list of relative paths of files
in the root directory that match REG.

This function can be used to generate the collect-2 member function for
type-2 objects."
  (lambda (bobj)
    (directory-files (yynt-build--path bobj) nil reg)))

(defun yynt-e2 (reg)
  "Accept a regular expression as a parameter and return a function that
takes BOBJ and relative directory path as parameters, and then returns a
function that takes a file as a parameter and checks if the file matches
the regular expression REG.

This function can be used to generate the excluded-fn-2 member function
for type-2 objects."
  (lambda (_bobj _path)
    (lambda (file)
      (string-match-p reg file))))

(provide 'yynt)
;;; yynt.el ends here

;; Local Variables:
;; emacs-lisp-docstring-fill-column: 72
;; fill-column: 80
;; End:
