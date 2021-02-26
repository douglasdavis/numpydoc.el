EMACS ?= emacs

# A space-separated list of required package names
NEEDED_PACKAGES = package-lint s dash

INIT_PACKAGES="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (dolist (pkg '(${NEEDED_PACKAGES})) \
    (unless (package-installed-p pkg) \
      (unless (assoc pkg package-archive-contents) \
        (package-refresh-contents)) \
      (package-install pkg))) \
  )"

all: package-lint compile clean-elc version

version:
	${EMACS} -Q --eval "(message (emacs-version))" -batch

package-lint:
	${EMACS} -Q --eval ${INIT_PACKAGES} -batch -f package-lint-batch-and-exit numpydoc.el

compile: clean-elc
	${EMACS} -Q --eval ${INIT_PACKAGES} -L . -batch -f batch-byte-compile *.el

clean-elc:
	rm -f f.elc

.PHONY: all compile clean-elc package-lint
