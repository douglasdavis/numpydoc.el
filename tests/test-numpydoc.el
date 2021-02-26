;;; test-numpydoc.el --- Testing for numpydoc.el     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Doug Davis

;; Author: Doug Davis <ddavis@ddavis.io>
;; Keywords: testing

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

;; Testing code for numpydoc.el

;;; Code:

(require 'buttercup)
(require 'numpydoc)

(defvar fsig1
  "def f(a: int, b: float = 5.5, c: Optional[Union[str, int]] = None) -> float:")
(defvar fsig2 "def f(x, y=5, z=None):")
(defvar fsig3 "\
def somelongerfunc(
    a1: np.ndarray,
    a2: Optional[np.ndarray] = None,
    a3: Optional[Sequence[float]] = None,
) -> Tuple[int, float]:")

(describe "Arg check"
  (it "Checks arg parsing 1"
    (let ((a (make-numpydoc--arg :name "a" :type "int" :defval nil))
          (b (make-numpydoc--arg :name "b" :type "float" :defval "5.5"))
          (c (make-numpydoc--arg :name "c"
                                 :type "Optional[Union[str, int]]"
                                 :defval "None"))
          (args (numpydoc--def-args (numpydoc--parse-def fsig1)))
          (ret (numpydoc--def-rtype (numpydoc--parse-def fsig1))))
      (expect a :to-equal (car args))
      (expect b :to-equal (nth 1 args))
      (expect c :to-equal (nth 2 args))
      (expect ret :to-equal "float")))

  (it "Checks arg parsing 2"
    (let ((x (make-numpydoc--arg :name "x" :type nil :defval nil))
          (y (make-numpydoc--arg :name "y" :type nil :defval "5"))
          (z (make-numpydoc--arg :name "z" :type nil :defval "None"))
          (args (numpydoc--def-args (numpydoc--parse-def fsig2)))
          (ret (numpydoc--def-rtype (numpydoc--parse-def fsig2))))
      (expect x :to-equal (car args))
      (expect y :to-equal (nth 1 args))
      (expect z :to-equal (nth 2 args))
      (expect nil :to-be ret)))

  (it "Checks arg parsing 3"
    (let ((a1 (make-numpydoc--arg :name "a1"
                                  :type "np.ndarray"
                                  :defval nil))
          (a2 (make-numpydoc--arg :name "a2"
                                  :type "Optional[np.ndarray]"
                                  :defval "None"))
          (a3 (make-numpydoc--arg :name "a3"
                                  :type "Optional[Sequence[float]]"
                                  :defval "None"))
          (args (numpydoc--def-args (numpydoc--parse-def fsig3)))
          (ret (numpydoc--def-rtype (numpydoc--parse-def fsig3))))
      (expect a1 :to-equal (car args))
      (expect a2 :to-equal (nth 1 args))
      (expect a3 :to-equal (nth 2 args))
      (expect ret :to-equal "Tuple[int, float]"))))

(provide 'test-numpydoc)
;;; test-numpydoc.el ends here
