(require 'buttercup)
(require 'numpydoc)

(defvar fsig1
  "def f(a: int, b: float = 5.5, c: Optional[Union[str, int]] = None) -> float:")

(defvar fsig2 "def f(x, y=5, z=None):")

(describe "Arg check"
  (it "Checks arg parsing 1"
    (let ((manual-a (make-numpydoc--arg :name "a" :type "int" :defval nil))
          (manual-b (make-numpydoc--arg :name "b" :type "float" :defval "5.5"))
          (manual-c (make-numpydoc--arg :name "c"
                                        :type "Optional[Union[str, int]]"
                                        :defval "None"))
          (parsed (numpydoc--parse-def fsig1)))
      (expect manual-a :to-equal (car (numpydoc--def-args parsed)))
      (expect manual-b :to-equal (nth 1 (numpydoc--def-args parsed)))
      (expect manual-c :to-equal (nth 2 (numpydoc--def-args parsed)))
      (expect "float" :to-equal (numpydoc--def-rtype parsed))))

  (it "Checks arg parsing 2"
    (let ((manual-x (make-numpydoc--arg :name "x" :type nil :defval nil))
          (manual-y (make-numpydoc--arg :name "y" :type nil :defval "5"))
          (manual-z (make-numpydoc--arg :name "z" :type nil :defval "None"))
          (parsed (numpydoc--parse-def fsig2)))
      (expect manual-x :to-equal (car (numpydoc--def-args parsed)))
      (expect manual-y :to-equal (nth 1 (numpydoc--def-args parsed)))
      (expect manual-z :to-equal (nth 2 (numpydoc--def-args parsed)))
      (expect (numpydoc--def-rtype parsed) :to-be nil))))
