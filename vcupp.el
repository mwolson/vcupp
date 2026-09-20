;;; vcupp.el --- Better use-package :vc support -*- lexical-binding: t -*-

;; Author: Michael Olson
;; Version: 0.4.0
;; Package-Requires: ((emacs "30.1") (use-package "2.4"))
;; Keywords: convenience, lisp
;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://github.com/mwolson/vcupp

;;; Commentary:

;; This package extends Emacs's built-in `package-vc' and `use-package :vc'
;; behavior for larger repositories and explicit compile targets.

;;; Code:

(eval-when-compile
  (require 'package-vc)
  (require 'project))

(require 'cl-lib)
(require 'package)
(require 'package-vc nil t)
(require 'seq)
(require 'use-package)

(defvar native-comp-jit-compilation)
(defvar package-vc-selected-packages)
(defvar warning-minimum-level)

(defgroup vcupp nil
  "Extensions for `use-package :vc'."
  :group 'convenience)

(defun vcupp-preload-package (sym)
  "Load the autoloads for package SYM without initializing all packages.
Looks up SYM in `package-user-dir' (checking both VC-style and
versioned directory names) and loads its autoloads file if found.

This is useful in early-init files where you want specific packages
available (e.g., for `:init' forms or `eval-and-compile' blocks)
without paying the cost of a full `package-initialize'."
  (when-let* ((pkg-dirs (and (file-directory-p package-user-dir)
                             (directory-files package-user-dir t "\\`[^.]")))
              (sym-name (symbol-name sym))
              (pkg-dir (or (let ((vc-dir (expand-file-name sym-name
                                                           package-user-dir)))
                             (and (file-directory-p vc-dir) vc-dir))
                           (let ((regexp (format "\\`%s-[0-9]"
                                                 (regexp-quote sym-name))))
                             (cl-find-if
                              (lambda (d)
                                (string-match-p
                                 regexp (file-name-nondirectory d)))
                              pkg-dirs))))
              ((file-directory-p pkg-dir))
              (autoload-name (expand-file-name
                              (format "%s-autoloads" sym-name)
                              pkg-dir)))
    (load autoload-name t t)))

(defun vcupp-activate-package (sym)
  "Activate installed package SYM when it is present in `package-alist'."
  (when-let* ((pkg (cadr (assq sym package-alist))))
    (package-activate-1 pkg)))

(defun vcupp--generated-el-file-p (path)
  "Return non-nil if PATH names a generated `-autoloads' or `-pkg' file."
  (string-match-p "-\\(?:autoloads\\|pkg\\)\\.el\\'" path))

(defun vcupp--expand-el-file-specs (base-dir files)
  "Expand glob patterns in FILES relative to BASE-DIR.
Returns a deduplicated list of existing `.el' paths, excluding
generated files."
  (delete-dups
   (delq nil
         (apply #'append
                (mapcar
                 (lambda (file)
                   (mapcar (lambda (path)
                             (and (file-regular-p path)
                                  (not (vcupp--generated-el-file-p path))
                                  path))
                           (file-expand-wildcards
                            (expand-file-name file base-dir) t)))
                 files)))))

(defun vcupp--pkg-spec-parts (pkg-desc)
  "Return (BASE-DIR MAIN-FILE COMPILE-FILES) for PKG-DESC, or nil."
  (let* ((pkg-spec (and (package-vc-p pkg-desc)
                        (package-vc--desc->spec pkg-desc))))
    (when pkg-spec
      (let* ((dir (package-desc-dir pkg-desc))
             (lisp-dir (plist-get pkg-spec :lisp-dir))
             (base-dir (if lisp-dir (expand-file-name lisp-dir dir) dir)))
        (list base-dir
              (plist-get pkg-spec :main-file)
              (plist-get pkg-spec :compile-files))))))

(defun vcupp--selected-el-files (pkg-desc)
  "Return the selected `.el' file paths for PKG-DESC, or nil.
Includes both `:main-file' and `:compile-files'."
  (pcase (vcupp--pkg-spec-parts pkg-desc)
    (`(,base-dir ,main-file ,compile-files)
     (let ((selected-files
            (cond
             (compile-files
              (append (and main-file (list main-file))
                      compile-files))
             (main-file
              (list main-file)))))
       (when selected-files
         (vcupp--expand-el-file-specs base-dir selected-files))))))

(defun vcupp--dep-scan-files (pkg-desc)
  "Return `.el' file paths for dependency scanning of PKG-DESC, or nil.
Returns only `:main-file' when set, falling back to `:compile-files'.
This avoids self-dependencies from extension files that declare
the main package in their own `Package-Requires' header."
  (pcase (vcupp--pkg-spec-parts pkg-desc)
    (`(,base-dir ,main-file ,compile-files)
     (let ((dep-files (cond
                       (main-file (list main-file))
                       (compile-files compile-files))))
       (when dep-files
         (vcupp--expand-el-file-specs base-dir dep-files))))))

(defun vcupp--compile-targets (pkg-desc)
  "Return a compile target plist for PKG-DESC, or nil."
  (let* ((pkg-spec (and (package-vc-p pkg-desc)
                        (package-vc--desc->spec pkg-desc))))
    (when pkg-spec
      (let* ((dir (package-desc-dir pkg-desc))
             (lisp-dir (plist-get pkg-spec :lisp-dir))
             (full-dir (if lisp-dir (expand-file-name lisp-dir dir) dir))
             (selected-files (vcupp--selected-el-files pkg-desc)))
        (cond
         (selected-files
          (list :type 'files :paths selected-files))
         (lisp-dir
          (and (file-directory-p full-dir)
               (list :type 'dir :path full-dir))))))))

(defun vcupp--save-spec-early (pkg-desc pkg-spec &optional _rev)
  "Save PKG-SPEC for PKG-DESC before unpack so upgrades can find it."
  (when-let* ((name (package-desc-name pkg-desc))
              ((not (alist-get name package-vc-selected-packages nil nil #'string=))))
    (push (cons name pkg-spec) package-vc-selected-packages)))

(defun vcupp--call-unpack-1 (orig-fn pkg-desc &optional pkg-dir)
  "Call ORIG-FN for PKG-DESC, forwarding PKG-DIR only on Emacs 30.
Emacs 31 dropped the PKG-DIR argument from `package-vc--unpack-1'."
  (if pkg-dir
      (funcall orig-fn pkg-desc pkg-dir)
    (funcall orig-fn pkg-desc)))

(defun vcupp--selected-file-deps (orig-fn pkg-desc &optional pkg-dir)
  "Limit dependency scanning for PKG-DESC to selected files.
ORIG-FN is the original `package-vc--unpack-1' function.
PKG-DIR is the checkout directory on Emacs 30; Emacs 31 dropped this
argument and reads it from PKG-DESC.
Uses only `:main-file' for scanning to avoid self-dependencies
from extension files."
  (let ((selected-files (vcupp--dep-scan-files pkg-desc)))
    (if (not selected-files)
        (vcupp--call-unpack-1 orig-fn pkg-desc pkg-dir)
      (cl-letf* ((orig-directory-files (symbol-function 'directory-files))
                 ((symbol-function 'directory-files)
                  (lambda (dir &optional full match nosort count)
                    (if (and full (equal match "\\.el\\'"))
                        selected-files
                      (funcall orig-directory-files dir full match nosort count)))))
        (vcupp--call-unpack-1 orig-fn pkg-desc pkg-dir)))))

(defun vcupp--skip-elpa (orig-fn dir &rest args)
  "Prevent `project-remember-projects-under' from indexing `elpa/' checkouts.
ORIG-FN is called with DIR and ARGS only when DIR is outside `package-user-dir'."
  (unless (string-prefix-p (expand-file-name package-user-dir)
                           (expand-file-name dir))
    (apply orig-fn dir args)))

(defun vcupp--handle-pre-release (orig-fn str)
  "Handle version headers that `package-strip-rcs-id' cannot parse.
ORIG-FN is the original function, STR is the version string.
First normalizes known pre-release suffixes (DEV, SNAPSHOT) to forms
that `version-to-list' understands, then strips any remaining
unrecognized suffixes so that packages with non-standard version
headers still get a usable version number in their `-pkg.el'
descriptor."
  (or (condition-case nil (funcall orig-fn str) (error nil))
      (when str
        (let ((normalized (replace-regexp-in-string
                           "-\\(?:DEV\\|SNAPSHOT\\)[^.]*\\'" "snapshot" str)))
          (or (condition-case nil (funcall orig-fn normalized) (error nil))
              (condition-case nil
                  (funcall orig-fn (replace-regexp-in-string
                                    "-\\(?:alpha\\|beta\\|rc\\)[^.]*\\'" "" str))
                (error nil)))))))

(defun vcupp--default-test-ignores (pkg-dir)
  "Return regexp patterns to exclude test directories under PKG-DIR."
  (when pkg-dir
    (let (patterns)
      (dolist (name '("test" "tests"))
        (let ((subdir (expand-file-name name pkg-dir)))
          (when (file-directory-p subdir)
            (push (concat "\\`" (regexp-quote (file-name-as-directory subdir)))
                  patterns))))
      patterns)))

(defun vcupp--vc-pkg-ignore-patterns (pkg-desc)
  "Return combined ignore patterns for VC package PKG-DESC.
Merges `.elpaignore' patterns with default test directory exclusions."
  (append (when (fboundp 'package--parse-elpaignore)
            (package--parse-elpaignore pkg-desc))
          (vcupp--default-test-ignores (package-desc-dir pkg-desc))))

(defun vcupp--byte-compile-targets (orig-fn pkg-desc)
  "Byte-compile only selected files for PKG-DESC.
ORIG-FN is the original `package--compile' function.  For VC
packages without explicit compile targets, default test directory
exclusions are applied alongside any `.elpaignore' patterns."
  (let ((target (vcupp--compile-targets pkg-desc)))
    (cond
     (target
      (let ((warning-minimum-level :error))
        (pcase (plist-get target :type)
          ('files
           (dolist (path (plist-get target :paths))
             (byte-compile-file path)))
          ('dir
           (byte-recompile-directory (plist-get target :path) 0 'force)))))
     ((package-vc-p pkg-desc)
      (let ((warning-minimum-level :error)
            (byte-compile-ignore-files
             (append (vcupp--vc-pkg-ignore-patterns pkg-desc)
                     byte-compile-ignore-files))
            (load-path load-path))
        (byte-recompile-directory (package-desc-dir pkg-desc) 0 t)))
     (t
      (funcall orig-fn pkg-desc)))))

(defun vcupp--native-compile-targets (orig-fn pkg-desc)
  "Native-compile only selected files for PKG-DESC.
ORIG-FN is the original `package--native-compile-async' function.
For VC packages without explicit compile targets, default test
directory exclusions are applied alongside any `.elpaignore' patterns."
  (when (native-comp-available-p)
    (let ((target (vcupp--compile-targets pkg-desc)))
      (cond
       (target
        (let ((warning-minimum-level :error))
          (pcase (plist-get target :type)
            ('files
             (native-compile-async (plist-get target :paths)))
            ('dir
             (native-compile-async
              (directory-files-recursively (plist-get target :path) "\\.el\\'"))))))
       ((package-vc-p pkg-desc)
        (let* ((warning-minimum-level :error)
               (pkg-dir (package-desc-dir pkg-desc))
               (ignores (vcupp--vc-pkg-ignore-patterns pkg-desc))
               (files (directory-files-recursively pkg-dir "\\.el\\'")))
          (when ignores
            (setq files (seq-remove
                         (lambda (f)
                           (cl-some (lambda (re) (string-match-p re f))
                                    ignores))
                         files)))
          (when files
            (native-compile-async files))))
       (t
        (funcall orig-fn pkg-desc))))))

(defun vcupp--bare-symbol (s)
  "Return S without reader position information.
If S is not a symbol, return it unchanged.  Uses `symbol-with-pos-p'
so this still works when `symbols-with-pos-enabled' is nil."
  (if (symbol-with-pos-p s)
      (bare-symbol s)
    s))

(defun vcupp--sanitize-selected-packages (orig-fn &optional value)
  "Strip symbol positions before persisting `package-selected-packages'.
ORIG-FN is the original `package--save-selected-packages'.
VALUE is the optional new list of selected packages.
Emacs 31 sorts that list with `string<', which signals if any
entry is a symbol-with-pos left behind by compiling a VC checkout."
  (when value
    (setq value (mapcar #'vcupp--bare-symbol value)))
  (setq package-selected-packages
        (mapcar #'vcupp--bare-symbol package-selected-packages))
  (funcall orig-fn value))

(advice-add 'package--save-selected-packages :around
            #'vcupp--sanitize-selected-packages)

(advice-add 'project-remember-projects-under :around #'vcupp--skip-elpa)

(with-eval-after-load 'project
  (when (file-directory-p package-user-dir)
    (let ((elpa-prefix (file-name-as-directory
                        (expand-file-name package-user-dir))))
      (dolist (root (project-known-project-roots))
        (when (string-prefix-p elpa-prefix (expand-file-name root))
          (project-forget-project root))))))

(with-eval-after-load 'package-vc
  (advice-add 'package-vc--unpack :before #'vcupp--save-spec-early)
  (advice-add 'package-vc--unpack-1 :around #'vcupp--selected-file-deps)
  (advice-add 'package-strip-rcs-id :around #'vcupp--handle-pre-release)
  (advice-add 'package--compile :around #'vcupp--byte-compile-targets)
  (advice-add 'package--native-compile-async :around #'vcupp--native-compile-targets))

(unless (memq :compile-files use-package-vc-valid-keywords)
  (add-to-list 'use-package-vc-valid-keywords :compile-files))

(defun vcupp--normalize-vc-arg (orig-fn arg)
  "Normalize a `use-package :vc' ARG that may contain `:compile-files'.
ORIG-FN is the original `use-package-normalize--vc-arg' function."
  (if (not (member :compile-files arg))
      (funcall orig-fn arg)
    (cl-flet* ((ensure-string (s)
                              (if (and s (stringp s)) s (symbol-name s)))
               (ensure-symbol (s)
                              (if (and s (stringp s)) (intern s) s))
               (ensure-list (value)
                            (pcase value
                              (`(quote ,items) items)
                              ((pred listp) value)
                              (_ (list value))))
               (normalize (k v)
                          (pcase k
                            (:rev (pcase v
                                    ('nil (if use-package-vc-prefer-newest
                                              nil
                                            :last-release))
                                    (:last-release :last-release)
                                    (:newest nil)
                                    (_ (ensure-string v))))
                            (:vc-backend (ensure-symbol v))
                            ((or :compile-files :ignored-files)
                             (ensure-list v))
                            (_ (ensure-string v)))))
      (pcase-let* ((`(,name . ,opts) arg))
        (if (stringp opts)
            (list name opts)
          (let ((opts (use-package-split-when
                       (lambda (el)
                         (seq-contains-p use-package-vc-valid-keywords el))
                       opts)))
            (cl-loop for (k . _) in opts
                     if (not (member k use-package-vc-valid-keywords))
                     do (use-package-error
                         (format "Keyword :vc received unknown argument: %s. Supported keywords are: %s"
                                 k use-package-vc-valid-keywords)))
            (list name
                  (cl-loop for (k . v) in opts
                           if (not (eq k :rev))
                           nconc (list k (normalize k (if (length= v 1)
                                                          (car v)
                                                        v))))
                  (normalize :rev (car (alist-get :rev opts))))))))))

(advice-add 'use-package-normalize--vc-arg :around #'vcupp--normalize-vc-arg)

(defun vcupp--vc-handler-always-runtime (name _keyword arg rest state)
  "Ensure `:vc' install call for NAME survives byte-compilation.
The built-in `use-package-handler/:vc' calls `use-package-vc-install'
at compile time and omits it from the byte-compiled body.  When
compile-angel or a similar tool byte-compiles an init file, packages
that were missing at compile time stay missing at runtime because the
install call is absent from the `.elc'.  This override always includes
the install call in the body while preserving the compile-time install
for symbol resolution.  _KEYWORD is ignored.  ARG, REST, and STATE
are forwarded from `use-package-handler/:vc'."
  (let ((body (use-package-process-keywords name rest state))
        (local-path (car (plist-get state :load-path))))
    (when (bound-and-true-p byte-compile-current-file)
      (funcall #'use-package-vc-install arg local-path))
    (push `(use-package-vc-install ',arg ,local-path) body)
    body))

(advice-add 'use-package-handler/:vc :override
            #'vcupp--vc-handler-always-runtime)

(defun vcupp-suppress-native-comp-jit ()
  "Configure native-comp for configs that use `vcupp-native-comp-all'.
Silences async native-comp warnings and disables JIT compilation,
since the batch native-comp flow handles compilation ahead of time.
Has no effect when `vcupp-native-comp-active-p' is non-nil."
  (unless (bound-and-true-p vcupp-native-comp-active-p)
    (eval-when-compile (require 'comp-run))
    (require 'comp-run)
    (setq native-comp-async-report-warnings-errors 'silent
          native-comp-jit-compilation nil)))

(defun vcupp-ensure-packages-on-install ()
  "Enable `use-package-always-ensure' during batch package installation.
Call this from your early-init file after loading vcupp.  During
normal interactive startup it is a no-op.  During batch runs via
`vcupp-install-packages', it sets `use-package-always-ensure' to t
so that every `use-package' form installs its package."
  (when (bound-and-true-p vcupp-install-packages-active-p)
    (setq use-package-always-ensure t)))

(defun vcupp-find-self-deps ()
  "Return names of packages in `package-alist' that depend on themselves."
  (let (result)
    (dolist (entry package-alist)
      (let ((name (car entry)))
        (dolist (pkg-desc (cdr entry))
          (when (assq name (package-desc-reqs pkg-desc))
            (cl-pushnew name result)))))
    result))

(defun vcupp-find-duplicate-packages ()
  "Return names of packages that have both a bare and versioned directory.
A bare directory (e.g. `foo/') indicates a VC package while a versioned
directory (e.g. `foo-1.2/') indicates an ELPA package.  Having both for the
same base name usually means a dependency was pulled from ELPA that a
`:compile-files' constraint should have prevented."
  (when (file-directory-p package-user-dir)
    (let (bare versioned result)
      (dolist (entry (directory-files package-user-dir nil "\\`[^.]"))
        (when (file-directory-p (expand-file-name entry package-user-dir))
          (if (string-match "\\`\\(.+?\\)-[0-9]" entry)
              (push (match-string 1 entry) versioned)
            (push entry bare))))
      (dolist (name bare)
        (when (member name versioned)
          (push name result)))
      (sort result #'string<))))

(defun vcupp-unload-function ()
  "Remove all advice installed by vcupp.
Called automatically by `unload-feature'."
  (advice-remove 'use-package-normalize--vc-arg #'vcupp--normalize-vc-arg)
  (advice-remove 'use-package-handler/:vc #'vcupp--vc-handler-always-runtime)
  (advice-remove 'package-vc--unpack #'vcupp--save-spec-early)
  (advice-remove 'package-vc--unpack-1 #'vcupp--selected-file-deps)
  (advice-remove 'project-remember-projects-under #'vcupp--skip-elpa)
  (advice-remove 'package-strip-rcs-id #'vcupp--handle-pre-release)
  (advice-remove 'package--compile #'vcupp--byte-compile-targets)
  (advice-remove 'package--native-compile-async #'vcupp--native-compile-targets)
  (advice-remove 'package--save-selected-packages
                 #'vcupp--sanitize-selected-packages)
  nil)

(provide 'vcupp)
;;; vcupp.el ends here
