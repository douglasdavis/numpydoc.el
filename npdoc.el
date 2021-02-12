;;; npdoc.el --- Insert NumPy style docstring.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Doug Davis

;; Author: Doug Davis <ddavis@ddavis.io>
;; Package-Version: "0.1.0"
;; Package-Requires: ((emacs "26.1") (cl-lib "1.0") (python "0.27") (dash "2.17.0") (s "1.12.0"))
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Insert NumPy style docstrings into Python function definitions.

;;; Code:

(require 'cl-lib)
(require 'python)
(require 'dash)
(require 's)

(cl-defstruct (npdoc--def (:constructor npdoc--def-create)
                          (:copier nil))
  args
  rtype)

(cl-defstruct (npdoc--arg (:constructor npdoc--arg-create)
                          (:copier nil))
  name
  type
  default)

(defun npdoc--indented-insert (n s)
  "Insert S with indentation N."
  (insert (format "%s%s" (make-string n ?\s) s)))

(defun npdoc--str-to-arg (s)
  "Convert S to arg structure."
  (cond (;; typehint and default value
         (and (s-contains-p ":" s) (s-contains-p "=" s))
         (let* ((comps1 (s-split ":" s))
                (comps2 (s-split "=" (nth 1 comps1)))
                (name (s-trim (nth 0 comps1)))
                (type (s-trim (nth 0 comps2)))
                (default (s-trim (nth 1 comps2))))
           (npdoc--arg-create :name name :type type :default default)))
        ;; only a typehint
        ((s-contains-p ":" s)
         (let* ((comps1 (s-split ":" s))
                (name (s-trim (nth 0 comps1)))
                (type (s-trim (nth 1 comps1))))
           (npdoc--arg-create :name name :type type :default nil)))
        ;; only a default value
        ((s-contains-p "=" s)
         (let* ((comps1 (s-split "=" s))
                (name (s-trim (nth 0 comps1)))
                (default (s-trim (nth 1 comps1))))
           (npdoc--arg-create :name name :type nil :default default)))
        ;; only a name
        (t (npdoc--arg-create :name s :type nil :default nil))))

(defun npdoc--split-args (sig)
  "Split SIG on comma while ignoring commas in type hint brackets."
  (let ((bc 0)
        (cursor -1)
        (strings '()))
    (dotimes (i (length sig))
      (let ((char (aref sig i)))
        (cond
         ((= char ?\[)
          (setq bc (1+ bc)))
         ((= char ?\])
          (setq bc (1- bc)))
         ((and (= char ?,) (= bc 0))
          (setq strings (append strings (list (substring sig (1+ cursor) i))))
          (setq cursor i))
         )))
    (setq strings (append strings (list (substring sig (1+ cursor)))))))

(defun npdoc--parse-def ()
  "Parse a Python function definition; return instance of npdoc--def."
  (let* ((start (progn
                  (python-nav-beginning-of-defun)
                  (point)))
         (stop (progn
                 (python-nav-end-of-statement)
                 (point)))
         ;; trimmed string of the function signature
         (trimmed (s-collapse-whitespace (buffer-substring-no-properties start stop)))
         ;; split into parts (args and return type)
         (parts (s-split "->" trimmed))
         ;; save return type as a string (or nil)
         (rtype (if (nth 1 parts)
                    (s-chop-suffix ":" (s-trim (nth 1 parts)))
                  nil))
         ;; raw signature without return type as a string
         (rawsig (cond (rtype (substring (s-trim (nth 0 parts)) 0 -1))
                       (t (substring (s-trim (nth 0 parts)) 0 -2))))
         ;; function args as strings
         (rawargs (-map (lambda (x) (s-trim x))
                        (npdoc--split-args (substring rawsig
                                                      (1+ (s-index-of "(" rawsig))))))
         ;; function args as a list of structures (remove if "" or "self"
         (args
          (-remove
           (lambda (x) (or (string= "" (npdoc--arg-name x))
                           (string= "self" (npdoc--arg-name x))))
           (-map (lambda (x) (npdoc--str-to-arg x)) rawargs))))
    (goto-char stop)
    (npdoc--def-create :args args :rtype rtype)))


(defun npdoc--insert (fndef indent)
  "Insert FNDEF with indentation level INDENT."
  (progn
    (insert "\n")
    (npdoc--indented-insert indent "\"\"\"SHORT-SUMMARY\n\n")
    (npdoc--indented-insert indent "LONG-SUMMARY\n")
    ;; parameters
    (when (npdoc--def-args fndef)
      (insert "\n")
      (npdoc--indented-insert indent "Parameters\n")
      (npdoc--indented-insert indent "----------\n")
      (dolist (element (npdoc--def-args fndef))
        (let* ((name (npdoc--arg-name element))
               (type (npdoc--arg-type element)))
          (npdoc--indented-insert indent
                                  (if type
                                      (format "%s : %s\n"
                                              name type)
                                    (format "%s\n" name)))
          (npdoc--indented-insert indent "   ADD\n"))))
    ;; return
    (when (npdoc--def-rtype fndef)
      (insert "\n")
      (npdoc--indented-insert indent "Returns\n")
      (npdoc--indented-insert indent "-------\n")
      (npdoc--indented-insert indent (npdoc--def-rtype fndef))
      (insert "\n")
      (npdoc--indented-insert indent "    ADD\n"))
    ;; done
    (insert "\n")
    (npdoc--indented-insert indent "\"\"\"")))

;;;###autoload
(defun npdoc ()
  "Generate NumPy style docstring for Python function."
  (interactive)
  (npdoc--insert (npdoc--parse-def) 4))

(provide 'npdoc)
;;; npdoc.el ends here
