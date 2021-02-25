;;; numpydoc.el --- NumPy style docstring insertion -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Doug Davis

;; Author: Doug Davis <ddavis@ddavis.io>
;; Maintainer: Doug Davis <ddavis@ddavis.io>
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

;; This package provides a single public function to automatically
;; generate NumPy style docstrings for Python functions:
;; `numpydoc-generate'. The NumPy docstring style guide can be found
;; at https://numpydoc.readthedocs.io/en/latest/format.html
;;
;; Customizations include opting in or out of a minibuffer prompt for
;; entering various components of the docstring, templates for when
;; opting out of the prompt, the quoting style used, and whether or
;; not to include an Examples block. See the `numpydoc' customization
;; group.

;;; Code:

(require 'cl-lib)
(require 'python)
(require 'subr-x)

(require 'dash)
(require 's)

;;; customization code.

(defgroup numpydoc nil
  "NumPy docstrings."
  :group 'convenience
  :prefix "numpydoc-")

(defcustom numpydoc-prompt-for-input t
  "If t you will be prompted to enter necessary descriptions."
  :group 'numpydoc
  :type 'boolean)

(defcustom numpydoc-template-short "FIXME: Short description."
  "Template text for the short description in a docstring."
  :group 'numpydoc
  :type 'string)

(defcustom numpydoc-template-long "FIXME: Long description."
  "Template text for the long description in a docstring."
  :group 'numpydoc
  :type 'string)

(defcustom numpydoc-template-desc "FIXME: Add docs."
  "Template text for individual component descriptions.
This will be added for individual argument and return description
text, and below the Examples section."
  :group 'numpydoc
  :type 'string)

(defcustom numpydoc-quote-char ?\"
  "Character for docstring quoting style (double or single quote)."
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

(defun numpydoc--arg-str-to-struct (argstr)
  "Convert ARGSTR to an instance of `numpydoc--arg'.
The argument takes on one of four possible styles:
1. First we check for a typed argument with a default value, so it
   contains both ':' and '='. Example would be 'x: int = 5'.
2. Then we check for a typed argument without a default value,
   containing only ':'. Example would be 'x: int'.
3. Then we check for an untyped argument with a default value,
   containing only '='. Example would be 'x=5'.
4. Finally the default is an untyped argument without a default
   value. Example would be `x`."
  (cond (;; type hint and default value
         (and (s-contains-p ":" argstr) (s-contains-p "=" argstr))
         (let* ((comps1 (s-split ":" argstr))
                (comps2 (s-split "=" (nth 1 comps1)))
                (name (s-trim (car comps1)))
                (type (s-trim (car comps2)))
                (defval (s-trim (nth 1 comps2))))
           (make-numpydoc--arg :name name
                               :type type
                               :defval defval)))
        ;; only a typehint
        ((string-match-p ":" argstr)
         (let* ((comps1 (s-split ":" argstr))
                (name (s-trim (car comps1)))
                (type (s-trim (nth 1 comps1))))
           (make-numpydoc--arg :name name
                               :type type
                               :defval nil)))
        ;; only a default value
        ((s-contains-p "=" argstr)
         (let* ((comps1 (s-split "=" argstr))
                (name (s-trim (car comps1)))
                (defval (s-trim (nth 1 comps1))))
           (make-numpydoc--arg :name name
                               :type nil
                               :defval defval)))
        ;; only a name
        (t (make-numpydoc--arg :name argstr
                               :type nil
                               :defval nil))))

(defun numpydoc--split-args (fnargs)
  "Split FNARGS on comma but ignore those in type [brackets]."
  (let ((bc 0)
        (cursor -1)
        (strs '()))
    (dotimes (i (length fnargs))
      (let ((ichar (aref fnargs i)))
        (cond ((= ichar ?\[) (setq bc (1+ bc)))
              ((= ichar ?\]) (setq bc (1- bc)))
              ((and (= ichar ?,) (= bc 0))
               (setq strs (append strs (list (substring fnargs
                                                        (1+ cursor)
                                                        i))))
               (setq cursor i)))))
    (setq strs (append strs (list (substring fnargs (1+ cursor)))))))

(defun numpydoc--parse-def ()
  "Parse a Python function definition; return instance of numpydoc--def."
  (save-excursion
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
                          (-map #'numpydoc--arg-str-to-struct rawargs))))
      (make-numpydoc--def :args args :rtype rtype))))

(defun numpydoc--has-existing-docstring-p ()
  "Check if an existing docstring is detected."
  (save-excursion
    (python-nav-beginning-of-defun)
    (python-nav-end-of-statement)
    (end-of-line)
    (right-char)
    (back-to-indentation)
    (right-char 1)
    (and (eq numpydoc-quote-char (preceding-char))
         (eq numpydoc-quote-char (following-char))
         (progn
           (right-char)
           (eq numpydoc-quote-char (preceding-char)))
         t)))

(defun numpydoc--detect-indent ()
  "Detect necessary indent for current function docstring."
  (save-excursion
    (let ((beg (progn
                 (python-nav-beginning-of-defun)
                 (point)))
          (ind (progn
                 (back-to-indentation)
                 (point))))
      (+ python-indent-offset (- ind beg)))))

(defun numpydoc--insert (indent &rest lines)
  "Insert all elements of LINES at indent level INDENT."
  (dolist (s lines)
    (insert (format "%s%s" (make-string indent ?\s) s))))

(defun numpydoc--fill-last-insertion ()
  "Fill paragraph on last inserted text."
  (save-excursion
    (move-beginning-of-line nil)
    (back-to-indentation)
    (set-mark-command nil)
    (move-end-of-line nil)
    (fill-paragraph nil t)
    (deactivate-mark)))

(defun numpydoc--insert-short-and-long-desc (indent)
  "Insert short description with INDENT level."
  (let ((ld nil))
    (insert "\n")
    (numpydoc--insert indent
                      (concat (make-string 3 numpydoc-quote-char)
                              (if numpydoc-prompt-for-input
                                  (read-string
                                   (format "Short description: "))
                                numpydoc-template-short)
                              "\n\n")
                      (make-string 3 numpydoc-quote-char))
    (forward-line -1)
    (beginning-of-line)
    (if numpydoc-prompt-for-input
        (progn
          (setq ld (read-string "Long description: " nil nil "" nil))
          (when (not (string-empty-p ld))
            (insert "\n")
            (numpydoc--insert indent ld)
            (numpydoc--fill-last-insertion)
            (insert "\n")))
      (insert "\n")
      (numpydoc--insert indent numpydoc-template-long)
      (insert "\n"))))

(defun numpydoc--insert-parameter (indent element)
  "Insert ELEMENT parameter name and type at level INDENT."
  (let ((name (numpydoc--arg-name element))
        (type (numpydoc--arg-type element)))
    (numpydoc--insert indent
                      (if type
                          (format "%s : %s\n" name type)
                        (format "%s\n" name)))))

(defun numpydoc--insert-parameter-desc (indent element)
  "Insert ELEMENT parameter description at level INDENT."
  (let* ((name (numpydoc--arg-name element))
         (desc (concat (make-string 4 ?\s)
                       (if numpydoc-prompt-for-input
                           (read-string (format "Description for %s: "
                                                name))
                         numpydoc-template-desc))))
    (numpydoc--insert indent desc)
    (numpydoc--fill-last-insertion)
    (insert "\n")))

(defun numpydoc--insert-parameters (indent fnargs)
  "Insert FNARGS (function arguments) at INDENT level."
  (when fnargs
    (insert "\n")
    (numpydoc--insert indent
                      "Parameters\n"
                      "----------\n")
    (dolist (element fnargs)
      (numpydoc--insert-parameter indent element)
      (numpydoc--insert-parameter-desc indent element))))

(defun numpydoc--insert-return (indent fnret)
  "Insert FNRET (return) description (if exists) at INDENT level."
  (when (and fnret (not (string= fnret "None")))
    (insert "\n")
    (numpydoc--insert indent
                      "Returns\n"
                      "-------\n"
                      fnret)
    (insert "\n")
    (numpydoc--insert indent
                      (concat (make-string 4 ?\s)
                              (if numpydoc-prompt-for-input
                                  (read-string "Description for return: ")
                                numpydoc-template-desc)))
    (numpydoc--fill-last-insertion)
    (insert "\n")))

(defun numpydoc--insert-examples (indent)
  "Insert function examples block at INDENT level."
  (when numpydoc-insert-examples-block
    (insert "\n")
    (numpydoc--insert indent
                      "Examples\n"
                      "--------\n"
                      (concat numpydoc-template-desc "\n"))))

(defun numpydoc--insert-docstring (indent fndef)
  "Insert FNDEF with indentation level INDENT."
  (numpydoc--insert-short-and-long-desc indent)
  (numpydoc--insert-parameters indent (numpydoc--def-args fndef))
  (numpydoc--insert-return indent (numpydoc--def-rtype fndef))
  (numpydoc--insert-examples indent))

(defun numpydoc--delete-existing ()
  "Delete existing docstring."
  (let ((3q (make-string 3 numpydoc-quote-char)))
    (when (numpydoc--has-existing-docstring-p)
      (python-nav-beginning-of-defun)
      (python-nav-end-of-statement)
      (re-search-forward 3q)
      (left-char 3)
      (set-mark-command nil)
      (re-search-forward 3q)
      (re-search-forward 3q)
      (right-char 1)
      (delete-region (region-beginning) (region-end))
      (deactivate-mark)
      (indent-for-tab-command))))

(defmacro numpydoc--interactive (arg-descriptor &rest modes)
  "Use interactive tagging only when possible.
In GNU Emacs 28 we are able to tag interactive functions to
specific modes; this macro allows us to use the feature and still
be compatible with older versions of GNU Emacs. ARG-DESCRIPTOR is
passed to `interactive' like expected in versions pre-28, while
MODES is the list of modes where the function is interactive in
28-and-later versions."
  (if (< emacs-major-version 28)
      `(interactive ,arg-descriptor)
    `(interactive ,arg-descriptor ,@modes)))

;;; public API

;;;###autoload
(defun numpydoc-generate ()
  "Generate NumPy style docstring for Python function."
  (numpydoc--interactive nil python-mode)
  (let ((good-to-go t))
    (when (numpydoc--has-existing-docstring-p)
      (if (y-or-n-p "Docstring exists; destroy and start new? ")
          (numpydoc--delete-existing)
        (setq good-to-go nil)))
    (when good-to-go
      (python-nav-beginning-of-defun)
      (python-nav-end-of-statement)
      (numpydoc--insert-docstring (numpydoc--detect-indent)
                                  (numpydoc--parse-def)))))

(provide 'numpydoc)
;;; numpydoc.el ends here
