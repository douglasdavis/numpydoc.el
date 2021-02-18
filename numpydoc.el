;;; numpydoc.el --- Insert NumPy style docstring     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Doug Davis

;; Author: Doug Davis <ddavis@ddavis.io>
;; URL: https://github.com/douglasdavis/numpydoc.el
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (dash "2.17.0"))
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
(require 'subr-x)
(require 'python)

(require 'dash)


(cl-defstruct numpydoc--def
  args
  rtype)

(cl-defstruct numpydoc--arg
  name
  type
  defval)

(defun numpydoc--indented-insert (n s)
  "Insert S with indentation N."
  (insert (format "%s%s" (make-string n ?\s) s)))

(defun numpydoc--str-to-arg (s)
  "Convert S to a `numpydoc--arg' struct instance."
  (cond ((and (string-match-p ":" s) (string-match-p "=" s))
         (let* ((comps1 (split-string s ":"))
                (comps2 (split-string (nth 1 comps1) "="))
                (name (string-trim (car comps1)))
                (type (string-trim (car comps2)))
                (defval (string-trim (nth 1 comps2))))
           (make-numpydoc--arg :name name
                               :type type
                               :defval defval)))
        ;; only a typehint
        ((string-match-p ":" s)
         (let* ((comps1 (split-string s ":"))
                (name (string-trim (car comps1)))
                (type (string-trim (nth 1 comps1))))
           (make-numpydoc--arg :name name
                               :type type
                               :defval nil)))
        ;; only a default value
        ((string-match-p "=" s)
         (let* ((comps1 (split-string s "="))
                (name (string-trim (car comps1)))
                (defval (string-trim (nth 1 comps1))))
           (make-numpydoc--arg :name name
                               :type nil
                               :defval defval)))
        ;; only a name
        (t (make-numpydoc--arg :name s
                               :type nil
                               :defval nil))))

(defun numpydoc--split-args (all-args)
  "Split ALL-ARGS on comma delimiter but ignore commas in type brackets."
  (let ((bc 0)
        (cursor -1)
        (strs '()))
    (dotimes (i (length all-args))
      (let ((ichar (aref all-args i)))
        (cond ((= ichar ?\[) (setq bc (1+ bc)))
              ((= ichar ?\]) (setq bc (1- bc)))
              ((and (= ichar ?,) (= bc 0))
               (setq strs (append strs (list (substring all-args (1+ cursor) i))))
               (setq cursor i)))))
    (setq strs (append strs (list (substring all-args (1+ cursor)))))))

(defun numpydoc--parse-def ()
  "Parse a Python function definition; return instance of numpydoc--def.

This function terminates with the cursor on the end of the python
function definition (`python-nav-end-of-statement')."
  (let* ((fnsig (buffer-substring-no-properties
                 (progn
                   (python-nav-beginning-of-defun)
                   (point))
                 (progn
                   (python-nav-end-of-statement)
                   (point))))
         ;; trimmed string of the function signature
         (trimmed (replace-regexp-in-string "[ \t\n\r]+" " " fnsig))
         ;; split into parts (args and return type)
         (parts (split-string trimmed "->"))
         ;; raw return
         (rawret (if (nth 1 parts)
                     (string-trim (nth 1 parts))
                   nil))
         ;; save return type as a string (or nil)
         (rtype (when rawret
                  (substring rawret 0 (1- (length rawret)))))
         ;; raw signature without return type as a string
         (rawsig (cond (rtype (substring (string-trim (car parts)) 0 -1))
                       (t (substring (string-trim (car parts)) 0 -2))))
         ;; function args as strings
         (rawargs (mapcar #'string-trim
                          (numpydoc--split-args
                           (substring rawsig
                                      (1+ (string-match-p (regexp-quote "(")
                                                          rawsig))))))
         ;; function args as a list of structures (remove some special cases)
         (args (-remove (lambda (x)
                          (-contains-p (list "" "self" "*" "/")
                                       (numpydoc--arg-name x)))
                        (mapcar #'numpydoc--str-to-arg rawargs))))
    (make-numpydoc--def :args args :rtype rtype)))

(defun numpydoc--has-existing-docstring-p ()
  "Return non-nil if an existing docstring is detected."
  (let* ((cp (point))
         (ret nil))
    (python-nav-beginning-of-defun)
    (python-nav-end-of-statement)
    (end-of-line)
    (right-char)
    (back-to-indentation)
    (right-char 1)
    (setq ret (and (eq ?\" (preceding-char))
                   (eq ?\" (following-char))
                   (progn
                     (right-char)
                     (eq ?\" (preceding-char)))
                   t))
    (goto-char cp)
    ret))

(defun numpydoc--detect-indent ()
  "Detect indentation level of current position's function."
  (let* ((cp (point))
         (beg (progn
                (python-nav-beginning-of-defun)
                (point)))
         (ind (progn
                (back-to-indentation)
                (point))))
    (goto-char cp)
    (+ python-indent-offset (- ind beg))))

(defun numpydoc--insert (fndef indent)
  "Insert FNDEF with indentation level INDENT."
  (progn
    (insert "\n")
    (numpydoc--indented-insert indent "\"\"\"SHORT-SUMMARY\n\n")
    (numpydoc--indented-insert indent "LONG-SUMMARY\n")
    ;; parameters
    (when (numpydoc--def-args fndef)
      (insert "\n")
      (numpydoc--indented-insert indent "Parameters\n")
      (numpydoc--indented-insert indent "----------\n")
      (dolist (element (numpydoc--def-args fndef))
        (let* ((name (numpydoc--arg-name element))
               (type (numpydoc--arg-type element)))
          (numpydoc--indented-insert indent
                                     (if type
                                         (format "%s : %s\n"
                                                 name type)
                                       (format "%s\n" name)))
          (numpydoc--indented-insert indent "    ADD\n"))))
    ;; return if non-nil and not "None"
    (when (and (numpydoc--def-rtype fndef)
               (not (string= (numpydoc--def-rtype fndef) "None")))
      (insert "\n")
      (numpydoc--indented-insert indent "Returns\n")
      (numpydoc--indented-insert indent "-------\n")
      (numpydoc--indented-insert indent (numpydoc--def-rtype fndef))
      (insert "\n")
      (numpydoc--indented-insert indent "    ADD\n"))
    ;; examples
    (insert "\n")
    (numpydoc--indented-insert indent "Examples\n")
    (numpydoc--indented-insert indent "--------\n")
    (numpydoc--indented-insert indent "ADD\n")
    ;; done
    (insert "\n")
    (numpydoc--indented-insert indent "\"\"\"")))

(defun numpydoc--existing-docstring-beg-end-chars ()
  "Find the beginning and ending characters of existing docstring."
  (let ((cp (point))
        (end (progn
               (python-nav-beginning-of-defun)
               (python-nav-end-of-statement)
               (python-nav-forward-sexp)
               (point)))
        (beg (progn
               (left-char 4)
               (search-backward "\"\"\"")
               (point))))
    (goto-char cp)
    (vector (+ 3 beg) (- end 3))))

(defun numpydoc--parse-existing ()
  "Parse existing docstring."
  (let* ((cp (point))
         (id (numpydoc--detect-indent))
         (be (numpydoc--existing-docstring-beg-end-chars))
         (ds (buffer-substring-no-properties (elt be 0) (elt be 1)))
         (parts (-remove #'string-empty-p (split-string ds "\n")))
         (short-sum (pop parts))
         (long-sum (substring (pop parts) id)))
    (goto-char cp)
    `(,short-sum ,long-sum ,(mapcar (lambda (x) (substring x id)) parts))))

;;;###autoload
(defun numpydoc-generate ()
  "Generate NumPy style docstring for Python function."
  (interactive)
  (let* ((cp (point))
         (has-ds (numpydoc--has-existing-docstring-p))
         (indent (numpydoc--detect-indent)))
    (goto-char cp)
    (if has-ds
        (message "Docstring already exists for this function.")
      (numpydoc--insert (numpydoc--parse-def) indent))))

(provide 'numpydoc)
;;; numpydoc.el ends here
