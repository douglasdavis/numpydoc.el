;;; numpydoc.el --- Insert NumPy style docstring     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Doug Davis

;; Author: Doug Davis <ddavis@ddavis.io>
;; Maintainer: Doug Davis
;; URL: https://github.com/douglasdavis/numpydoc.el
;; License: GPL-3.0-or-later
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (s "1.12.0") (dash "2.18.0"))
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
(require 's)

;;; customization code.

(defgroup numpydoc nil
  "NumPy docstrings."
  :group 'convenience
  :prefix "numpydoc-")

(defcustom numpydoc-template-short "SHORT-DESCRIPTION"
  "Template text for the short description in a docstring."
  :group 'numpydoc
  :type 'string)

(defcustom numpydoc-template-long "LONG-DESCRIPTION"
  "Template text for the long description in a docstring."
  :group 'numpydoc
  :type 'string)

(defcustom numpydoc-template-desc "ADD"
  "Template for individual component descriptions.
This will be added for individual argument and return description
text, and below the Examples section."
  :group 'numpydoc
  :type 'string)

(defcustom numpydoc-prompt-for-input t
  "If t you will be prompted to enter a description of each template."
  :group 'numpydoc
  :type 'boolean)

(defcustom numpydoc-quote-char ?\"
  "Character for docstring quoting style (double or single quote)"
  :group 'numpydoc
  :type 'character)

(defcustom numpydoc-insert-examples-block t
  "Flag to control if Examples section is inserted into the buffer."
  :group 'numpydoc
  :type 'boolean)

;; private implementation code.

(cl-defstruct numpydoc--def
  args
  rtype)

(cl-defstruct numpydoc--arg
  name
  type
  defval)

(defun numpydoc--str-to-arg (s)
  "Convert S to an instance of `numpydoc--arg'.
The style of the argument can take on three four:
1. First we check for a typed argument with a default value, so it
   contains both ':' and '='. Example would be 'x: int = 5'.
2. Then we check for a typed argument without a default value,
   containing only ':'. Example would be 'x: int'.
3. Then we check for an untyped argument with a default value,
   containing only '='. Example would be 'x=5'.
4. Finally the default is an untyped argument without a default
   value. Example would be `x`."
  (cond ((and (s-contains-p ":" s) (s-contains-p "=" s))
         (let* ((comps1 (s-split ":" s))
                (comps2 (s-split "=" (nth 1 comps1)))
                (name (s-trim (car comps1)))
                (type (s-trim (car comps2)))
                (defval (s-trim (nth 1 comps2))))
           (make-numpydoc--arg :name name
                               :type type
                               :defval defval)))
        ;; only a typehint
        ((string-match-p ":" s)
         (let* ((comps1 (s-split ":" s))
                (name (s-trim (car comps1)))
                (type (s-trim (nth 1 comps1))))
           (make-numpydoc--arg :name name
                               :type type
                               :defval nil)))
        ;; only a default value
        ((s-contains-p "=" s)
         (let* ((comps1 (s-split "=" s))
                (name (s-trim (car comps1)))
                (defval (s-trim (nth 1 comps1))))
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
               (setq strs (append strs (list (substring all-args
                                                        (1+ cursor)
                                                        i))))
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
         (trimmed (s-collapse-whitespace fnsig))
         ;; split into parts (args and return type)
         (parts (s-split "->" trimmed))
         ;; raw return
         (rawret (if (nth 1 parts)
                     (s-trim (nth 1 parts))
                   nil))
         ;; save return type as a string (or nil)
         (rtype (when rawret
                  (substring rawret 0 (1- (length rawret)))))
         ;; raw signature without return type as a string
         (rawsig (cond (rtype (substring (s-trim (car parts)) 0 -1))
                       (t (substring (s-trim (car parts)) 0 -2))))
         ;; function args as strings
         (rawargs (-map #'s-trim
                        (numpydoc--split-args
                         (substring rawsig
                                    (1+ (string-match-p (regexp-quote "(")
                                                        rawsig))))))
         ;; function args as a list of structures (remove some special cases)
         (args (-remove (lambda (x)
                          (-contains-p (list "" "self" "*" "/")
                                       (numpydoc--arg-name x)))
                        (-map #'numpydoc--str-to-arg rawargs))))
    (make-numpydoc--def :args args :rtype rtype)))

(defun numpydoc--has-existing-docstring-p ()
  "Return non-nil if an existing docstring is detected."
  (let ((cp (point))
        (ret nil))
    (python-nav-beginning-of-defun)
    (python-nav-end-of-statement)
    (end-of-line)
    (right-char)
    (back-to-indentation)
    (right-char 1)
    (setq ret (and (eq numpydoc-quote-char (preceding-char))
                   (eq numpydoc-quote-char (following-char))
                   (progn
                     (right-char)
                     (eq numpydoc-quote-char (preceding-char)))
                   t))
    (goto-char cp)
    ret))

(defun numpydoc--detect-indent ()
  "Detect indentation level of current position's function."
  (let ((cp (point))
        (beg (progn
               (python-nav-beginning-of-defun)
               (point)))
        (ind (progn
               (back-to-indentation)
               (point))))
    (goto-char cp)
    (+ python-indent-offset (- ind beg))))

(defun numpydoc--insert (n s)
  "Insert S with indentation N."
  (insert (format "%s%s" (make-string n ?\s) s)))

(defun numpydoc--insert-short-and-long-desc (indent)
  "Insert short description with INDENT level."
  (let ((ld nil))
    (insert "\n")
    (numpydoc--insert indent (concat (make-string 3 numpydoc-quote-char)
                                     (if numpydoc-prompt-for-input
                                         (read-string
                                          (format "Short description: "))
                                       numpydoc-template-short)
                                     "\n\n"))
    (numpydoc--insert indent (make-string 3 numpydoc-quote-char))
    (forward-line -1)
    (beginning-of-line)
    (if numpydoc-prompt-for-input
        (progn
          (setq ld (read-string "Long description: " nil nil "" nil))
          (when (length> ld 1)
            (insert "\n")
            (numpydoc--insert indent (concat ld "\n"))))
      (insert "\n")
      (numpydoc--insert indent (concat numpydoc-template-long "\n")))))

(defun numpydoc--insert-arguments (fnargs indent)
  "Insert FNARGS (function arguments) at INDENT level."
  (when fnargs
    (insert "\n")
    (numpydoc--insert indent "Parameters\n")
    (numpydoc--insert indent "----------\n")
    (dolist (element fnargs)
      (let* ((name (numpydoc--arg-name element))
             (type (numpydoc--arg-type element)))
        (numpydoc--insert indent (if type
                                     (format "%s : %s\n"
                                             name type)
                                   (format "%s\n" name)))
        (numpydoc--insert indent
                          (concat (make-string 4 ?\s)
                                  (if numpydoc-prompt-for-input
                                      (read-string
                                       (format
                                        "Description for %s: "
                                        name))
                                    numpydoc-template-desc)
                                  "\n"))))))

(defun numpydoc--insert-return (fnret indent)
  "Insert FNRET (return) description (if exists) at INDENT level."
  (when (and fnret (not (string= fnret "None")))
    (insert "\n")
    (numpydoc--insert indent "Returns\n")
    (numpydoc--insert indent "-------\n")
    (numpydoc--insert indent fnret)
    (insert "\n")
    (numpydoc--insert indent
                      (concat (make-string 4 ?\s)
                              (if numpydoc-prompt-for-input
                                  (read-string "Description for return: ")
                                numpydoc-template-desc)
                              "\n"))))

(defun numpydoc--insert-examples (indent)
  "Insert function examples block at INDENT level."
  (when numpydoc-insert-examples-block
    (insert "\n")
    (numpydoc--insert indent "Examples\n")
    (numpydoc--insert indent "--------\n")
    (numpydoc--insert indent (concat numpydoc-template-desc "\n"))))

(defun numpydoc--insert-docstring (fndef indent)
  "Insert FNDEF with indentation level INDENT."
  (numpydoc--insert-short-and-long-desc indent)
  (numpydoc--insert-arguments (numpydoc--def-args fndef) indent)
  (numpydoc--insert-return (numpydoc--def-rtype fndef) indent)
  (numpydoc--insert-examples indent))

;; (defun numpydoc--existing-docstring-beg-end-chars ()
;;   "Find the beginning and ending characters of existing docstring."
;;   (let ((cp (point))
;;         (end (progn
;;                (python-nav-beginning-of-defun)
;;                (python-nav-end-of-statement)
;;                (python-nav-forward-sexp)
;;                (point)))
;;         (beg (progn
;;                (left-char 4)
;;                (search-backward "\"\"\"")
;;                (point))))
;;     (goto-char cp)
;;     (vector (+ 3 beg) (- end 3))))

;; (defun numpydoc--parse-existing ()
;;   "Parse existing docstring."
;;   (let* ((cp (point))
;;          (id (numpydoc--detect-indent))
;;          (be (numpydoc--existing-docstring-beg-end-chars))
;;          (ds (buffer-substring-no-properties (elt be 0) (elt be 1)))
;;          (parts (-remove #'string-empty-p (split-string ds "\n")))
;;          (short-sum (pop parts))
;;          (long-sum (substring (pop parts) id)))
;;     (goto-char cp)
;;     `(,short-sum ,long-sum ,(mapcar (lambda (x) (substring x id)) parts))))

;; In Emacs 28 we are able to tag interactive functions to specific
;; modes; this macro allows us to use the feature and still be
;; compatible with older versions of GNU Emacs.
(defmacro future-interactive (arg-descriptor &rest modes)
  (if (< emacs-major-version 28)
      `(interactive ,arg-descriptor)
    `(interactive ,arg-descriptor ,@modes)))

;;; public API

;;;###autoload
(defun numpydoc-generate ()
  "Generate NumPy style docstring for Python function."
  (future-interactive nil python-mode)
  (let* ((cp (point))
         (has-ds (numpydoc--has-existing-docstring-p))
         (indent (numpydoc--detect-indent)))
    (goto-char cp)
    (if has-ds
        (message "Docstring already exists for this function.")
      (numpydoc--insert-docstring (numpydoc--parse-def) indent))))

(provide 'numpydoc)
;;; numpydoc.el ends here
