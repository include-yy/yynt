;;; yynt.el --- simple org export manager -*- lexical-binding: t; -*-

;; Copyright (C) 2024 include-yy <yy@egh0bww1.com>

;; Author: include-yy <yy@egh0bww1.com>
;; Maintainer: include-yy <yy@egh0bww1.com>
;; Created: 22 Apr 2024

;; Package-Version: 0.1
;; Package-Requires: ((emacs "29.2") (emacsql "3.1.1") (f "0.20.0"))
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

(defgroup yynt nil
  "Custom group of yynt")

(defcustom yynt-project-directory nil
  "base directory of project"
  :type 'sexp)

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
  (if (called-interactively-p)
      (switch-to-buffer (yynt--create-log-buffer))
    (with-current-buffer (yynt--create-log-buffer)
      (let ((inhibit-read-only t))
	(goto-char (point-max))
	(insert (concat message (if no-newline "" "\n")))))
    (when switch
      (switch-to-buffer (yynt--create-log-buffer)))))

(defun yynt-wrap-export-function (fn)
  "wrap a xxx-export-to-html function to take a filename argument

the export function's prototype must be:
(&optional async subtreep visible-only body-only ext-plist)"
  (lambda (file &optional async subtreep visible-only body-only ext-plist)
    (if (not (file-exists-p file))
	(error "yynt: file not exists: %s" file)
      (if-let ((buf (get-file-buffer file)))
	  (with-current-buffer buf
	    (funcall fn async subtreep visible-only body-only ext-plist))
	(with-current-buffer (find-file-noselect file)
	  (unwind-protect
	      (funcall fn async subtreep visible-only body-only ext-plist)
	    (kill-buffer)))))))

(cl-defstruct (yynt-build (:conc-name yynt-build--)
			  (:constructor yynt-build-make)
			  (:copier nil))
  "Struct that contains build info of a series of files"
  (type 0 ; type of build object
	:documentation "type of build object. It can be 0, 1 or 2")
  (path nil ; path to build object
	:documentation "relative path to base build directory")
  (collect #'ignore
	   :documentation ; collect all items
	   "function that take build object as arg and  return items need to be built")
  (export #'ignore ; actual export function
	  :documentation "Function returned by `yynt-wrap-export-function'")
  (extra nil ; files need to be generated after build
	 :documentation "files need to be generated after build")
  (info nil ; plist holding contextual information
	:documentation "plist that pass to export function's ext-plist arg"))

;; | pathname | name | tag | pub-time | build-time |
;;   PRIMARY
;; | pathname | export-time |
;;

(provide 'yynt)
;;; yynt.el ends here
