;;; test/test.el --- ERT tests for vcupp -*- lexical-binding: t; -*-

(eval-and-compile
  (let ((test-dir (file-name-directory
                   (or load-file-name buffer-file-name))))
    (add-to-list 'load-path test-dir)
    (add-to-list 'load-path (expand-file-name ".." test-dir))))

(require 'cl-lib)
(require 'ert)
(require 'vcupp)
(require 'vcupp-batch)
(require 'vcupp-native-comp)
(require 'vcupp-install-packages)
(require 'comp-run)

(defvar my-test-setup-marker nil)
(defvar my-test-load-order nil)
(defvar my-test-post-load-called nil)

(defvar my-test-run-live-tests nil
  "When non-nil, include live tests that inspect the real elpa directory.")

(defun my-test--display-warning-fail (type message &optional _level _buffer-name)
  "Fail the current test for warning TYPE with MESSAGE."
  (ert-fail (format "Unexpected warning (%s): %s" type message)))

(advice-add 'display-warning :override #'my-test--display-warning-fail)

(defvar my-test-test-dir (file-name-directory
                          (or load-file-name buffer-file-name)))
(defvar my-test-project-dir
  (expand-file-name ".." my-test-test-dir))

(defun my-test-any-regexp-matches-p (regexps path)
  "Return non-nil when any regexp in REGEXPS matches PATH."
  (cl-some (lambda (regexp)
             (string-match-p regexp path))
           regexps))

(defun my-test--with-tmp-dir (fn)
  "Call FN with a temporary directory, cleaned up afterward."
  (let ((tmp (make-temp-file "vcupp-test-" t)))
    (unwind-protect
        (funcall fn tmp)
      (delete-directory tmp t))))

(defmacro my-test-with-tmp-dir (var &rest body)
  "Bind VAR to a temporary directory, evaluate BODY, then clean up."
  (declare (indent 1))
  `(my-test--with-tmp-dir (lambda (,var) ,@body)))

(defun my-test-write-el-file (dir name content)
  "Write an .el file NAME in DIR with CONTENT.  Return the path."
  (let ((path (expand-file-name name dir)))
    (with-temp-file path
      (insert content))
    path))

(defun my-test-write-stale-elc (el-file)
  "Byte-compile EL-FILE, then update the .el so the .elc is stale."
  (let ((elc-file (concat el-file "c")))
    (byte-compile-file el-file)
    (sleep-for 1.1)
    (with-temp-file el-file
      (insert-file-contents el-file))
    elc-file))

;; ---------------------------------------------------------------------------
;; vcupp.el -- :compile-files normalization
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-normalize-vc-arg/compile-files-single ()
  "Single :compile-files value is wrapped in a list."
  (let ((use-package-vc-prefer-newest t))
    (let ((result (vcupp--normalize-vc-arg
                   #'use-package-normalize--vc-arg
                   '(my-pkg :url "https://example.com" :compile-files "foo.el"))))
      (should (equal (plist-get (nth 1 result) :compile-files) '("foo.el"))))))

(ert-deftest vcupp-normalize-vc-arg/compile-files-list ()
  ":compile-files list passes through."
  (let ((use-package-vc-prefer-newest t))
    (let ((result (vcupp--normalize-vc-arg
                   #'use-package-normalize--vc-arg
                   '(my-pkg :url "https://example.com"
                            :compile-files ("a.el" "b.el")))))
      (should (equal (plist-get (nth 1 result) :compile-files)
                     '("a.el" "b.el"))))))

(ert-deftest vcupp-normalize-vc-arg/compile-files-quoted-list ()
  "Quoted :compile-files list is unquoted."
  (let ((use-package-vc-prefer-newest t))
    (let ((result (vcupp--normalize-vc-arg
                   #'use-package-normalize--vc-arg
                   '(my-pkg :url "https://example.com"
                            :compile-files '("a.el" "b.el")))))
      (should (equal (plist-get (nth 1 result) :compile-files)
                     '("a.el" "b.el"))))))

(ert-deftest vcupp-normalize-vc-arg/no-compile-files-delegates ()
  "Without :compile-files, delegates to the original normalizer."
  (let ((use-package-vc-prefer-newest t))
    (let ((result (vcupp--normalize-vc-arg
                   #'use-package-normalize--vc-arg
                   '(my-pkg :url "https://example.com"))))
      (should (equal (plist-get (nth 1 result) :url) "https://example.com"))
      (should-not (plist-member (nth 1 result) :compile-files)))))

(ert-deftest vcupp-normalize-vc-arg/rev-newest ()
  ":rev :newest normalizes to nil (track HEAD)."
  (let ((use-package-vc-prefer-newest nil))
    (let ((result (vcupp--normalize-vc-arg
                   #'use-package-normalize--vc-arg
                   '(my-pkg :url "https://example.com"
                            :compile-files "foo.el"
                            :rev :newest))))
      (should (null (nth 2 result))))))

(ert-deftest vcupp-normalize-vc-arg/rev-last-release ()
  ":rev :last-release passes through."
  (let ((use-package-vc-prefer-newest t))
    (let ((result (vcupp--normalize-vc-arg
                   #'use-package-normalize--vc-arg
                   '(my-pkg :url "https://example.com"
                            :compile-files "foo.el"
                            :rev :last-release))))
      (should (eq (nth 2 result) :last-release)))))

;; ---------------------------------------------------------------------------
;; vcupp.el -- file expansion and compile targets
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-expand-el-file-specs/glob ()
  "Glob patterns expand to matching .el files."
  (my-test-with-tmp-dir tmp
    (my-test-write-el-file tmp "foo.el" "(provide 'foo)")
    (my-test-write-el-file tmp "bar.el" "(provide 'bar)")
    (my-test-write-el-file tmp "foo-autoloads.el" "")
    (let ((files (vcupp--expand-el-file-specs tmp '("*.el"))))
      (should (= (length files) 2))
      (should (cl-every (lambda (f) (string-match-p "/\\(foo\\|bar\\)\\.el\\'" f))
                        files)))))

(ert-deftest vcupp-expand-el-file-specs/filters-generated ()
  "Generated files (-autoloads.el, -pkg.el) are excluded."
  (my-test-with-tmp-dir tmp
    (my-test-write-el-file tmp "foo.el" "(provide 'foo)")
    (my-test-write-el-file tmp "foo-autoloads.el" "")
    (my-test-write-el-file tmp "foo-pkg.el" "")
    (let ((files (vcupp--expand-el-file-specs tmp '("*.el"))))
      (should (= (length files) 1))
      (should (string-match-p "/foo\\.el\\'" (car files))))))

(ert-deftest vcupp-generated-el-file-p ()
  "Detects autoloads and pkg files."
  (should (vcupp--generated-el-file-p "foo-autoloads.el"))
  (should (vcupp--generated-el-file-p "bar-pkg.el"))
  (should-not (vcupp--generated-el-file-p "foo.el"))
  (should-not (vcupp--generated-el-file-p "foo-utils.el")))

(ert-deftest vcupp-package-compile/uses-elpaignore-for-dev-files ()
  "Package compilation ignores vcupp's dev-only Elisp files."
  (let* ((pkg-desc (package-desc-create
                    :name 'vcupp
                    :version '(0 1 0)
                    :summary ""
                    :dir my-test-project-dir))
         (script-file (expand-file-name "scripts/byte-compile-local.el"
                                        my-test-project-dir))
         (test-file (expand-file-name "test/test.el" my-test-project-dir))
         (main-file (expand-file-name "vcupp.el" my-test-project-dir))
         captured-ignores)
    (cl-letf (((symbol-function 'byte-recompile-directory)
               (lambda (_dir _depth _force)
                 (setq captured-ignores byte-compile-ignore-files))))
      (package--compile pkg-desc))
    (should captured-ignores)
    (should (my-test-any-regexp-matches-p captured-ignores script-file))
    (should (my-test-any-regexp-matches-p captured-ignores test-file))
    (should-not (my-test-any-regexp-matches-p captured-ignores main-file))))

;; ---------------------------------------------------------------------------
;; vcupp.el -- default test ignores
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-default-test-ignores/finds-test-dir ()
  "Returns a pattern when test/ exists."
  (my-test-with-tmp-dir tmp
    (make-directory (expand-file-name "test" tmp))
    (let ((ignores (vcupp--default-test-ignores tmp)))
      (should (= (length ignores) 1))
      (should (string-match-p
               (car ignores)
               (expand-file-name "test/foo.el" tmp)))
      (should-not (string-match-p
                   (car ignores)
                   (expand-file-name "foo.el" tmp))))))

(ert-deftest vcupp-default-test-ignores/finds-tests-dir ()
  "Returns a pattern when tests/ exists."
  (my-test-with-tmp-dir tmp
    (make-directory (expand-file-name "tests" tmp))
    (let ((ignores (vcupp--default-test-ignores tmp)))
      (should (= (length ignores) 1))
      (should (string-match-p
               (car ignores)
               (expand-file-name "tests/bar.el" tmp))))))

(ert-deftest vcupp-default-test-ignores/no-test-dir ()
  "Returns nil when no test directories exist."
  (my-test-with-tmp-dir tmp
    (should (null (vcupp--default-test-ignores tmp)))))

(ert-deftest vcupp-default-test-ignores/nil-dir ()
  "Returns nil when given nil."
  (should (null (vcupp--default-test-ignores nil))))

;; ---------------------------------------------------------------------------
;; vcupp.el -- VC package byte-compile with test exclusions
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-byte-compile-targets/vc-pkg-excludes-test-dir ()
  "VC packages without compile targets exclude test directories."
  (my-test-with-tmp-dir tmp
    (let ((test-dir (expand-file-name "test" tmp)))
      (make-directory test-dir)
      (my-test-write-el-file tmp "foo.el"
                             (concat my-test-el-header "(provide 'foo)\n"))
      (my-test-write-el-file test-dir "foo-test.el"
                             (concat my-test-el-header "(require 'foo)\n"))
      (let* ((pkg-desc (package-desc-create
                        :name 'foo
                        :version '(1 0)
                        :kind 'vc
                        :dir tmp))
             (package-vc-selected-packages nil)
             captured-ignores)
        (cl-letf (((symbol-function 'byte-recompile-directory)
                   (lambda (_dir _depth _force)
                     (setq captured-ignores byte-compile-ignore-files))))
          (package--compile pkg-desc))
        (should captured-ignores)
        (should (my-test-any-regexp-matches-p
                 captured-ignores
                 (expand-file-name "test/foo-test.el" tmp)))
        (should-not (my-test-any-regexp-matches-p
                     captured-ignores
                     (expand-file-name "foo.el" tmp)))))))

(ert-deftest vcupp-byte-compile-targets/non-vc-delegates ()
  "Non-VC packages without compile targets delegate to orig-fn."
  (my-test-with-tmp-dir tmp
    (my-test-write-el-file tmp "foo.el"
                           (concat my-test-el-header "(provide 'foo)\n"))
    (let* ((pkg-desc (package-desc-create
                      :name 'foo
                      :version '(1 0)
                      :summary ""
                      :dir tmp))
           orig-called)
      (cl-letf (((symbol-function 'byte-recompile-directory)
                 (lambda (_dir _depth _force) nil)))
        (vcupp--byte-compile-targets
         (lambda (_pd) (setq orig-called t))
         pkg-desc))
      (should orig-called))))

;; ---------------------------------------------------------------------------
;; vcupp.el -- pre-release version handling
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-handle-pre-release/dev-suffix ()
  "-DEV suffix is normalized to snapshot."
  (let ((result (vcupp--handle-pre-release #'package-strip-rcs-id "0.3.3-DEV")))
    (should (equal (version-to-list result) '(0 3 3 -4)))))

(ert-deftest vcupp-handle-pre-release/snapshot-suffix ()
  "-SNAPSHOT suffix is normalized to snapshot."
  (let ((result (vcupp--handle-pre-release #'package-strip-rcs-id "1.22.0-SNAPSHOT")))
    (should (equal (version-to-list result) '(1 22 0 -4)))))

(ert-deftest vcupp-handle-pre-release/rc-suffix ()
  "-rc suffix is recognized natively by `version-to-list'."
  (let ((result (vcupp--handle-pre-release #'package-strip-rcs-id "2.0.0-rc1")))
    (should (equal (version-to-list result) '(2 0 0 -1 1)))))

(ert-deftest vcupp-handle-pre-release/normal-version ()
  "Normal version strings pass through unchanged."
  (let ((result (vcupp--handle-pre-release #'package-strip-rcs-id "1.2.3")))
    (should (equal result "1.2.3"))))

(ert-deftest vcupp-handle-pre-release/nil-input ()
  "nil input returns nil."
  (should (null (vcupp--handle-pre-release #'package-strip-rcs-id nil))))

;; ---------------------------------------------------------------------------
;; vcupp.el -- vcupp-suppress-native-comp-jit
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-suppress-native-comp-jit/sets-variables ()
  "Sets native-comp variables when not in batch native-comp."
  (let ((vcupp-native-comp-active-p nil)
        (native-comp-async-report-warnings-errors t)
        (native-comp-jit-compilation t))
    (vcupp-suppress-native-comp-jit)
    (should (eq native-comp-async-report-warnings-errors 'silent))
    (should (null native-comp-jit-compilation))))

(ert-deftest vcupp-suppress-native-comp-jit/noop-when-active ()
  "No-op when vcupp-native-comp-active-p is set."
  (let ((vcupp-native-comp-active-p t)
        (native-comp-async-report-warnings-errors t)
        (native-comp-jit-compilation t))
    (vcupp-suppress-native-comp-jit)
    (should (eq native-comp-async-report-warnings-errors t))
    (should (eq native-comp-jit-compilation t))))

;; ---------------------------------------------------------------------------
;; vcupp-batch.el -- effective-args
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-batch-effective-args/defaults ()
  "Default values are used when no args are provided."
  (let ((vcupp-batch-args nil)
        (vcupp-batch-root (expand-file-name user-emacs-directory))
        (vcupp-batch-load-files nil)
        (vcupp-batch-compile-files nil)
        (vcupp-batch-setup-forms nil)
        (vcupp-batch-preload-features nil)
        (vcupp-batch-delete-elc-globs nil)
        (vcupp-batch-post-load-forms nil)
        (vcupp-batch-post-install-forms nil)
        (vcupp-batch-refresh-contents t)
        (vcupp-batch-package-native-compile t))
    (let ((result (vcupp-batch-effective-args)))
      (should (equal (plist-get result :load-files) '("early-init.el" "init.el")))
      (should (equal (plist-get result :compile-files) '("early-init.el" "init.el")))
      (should (null (plist-get result :setup-forms)))
      (should (null (plist-get result :delete-elc-globs)))
      (should (eq (plist-get result :refresh-contents) t)))))

(ert-deftest vcupp-batch-effective-args/plist-overrides ()
  "Plist values override variable defaults."
  (let ((vcupp-batch-args nil)
        (vcupp-batch-root "/default/root/")
        (vcupp-batch-load-files nil)
        (vcupp-batch-compile-files nil)
        (vcupp-batch-setup-forms nil)
        (vcupp-batch-preload-features nil)
        (vcupp-batch-delete-elc-globs nil)
        (vcupp-batch-post-load-forms nil)
        (vcupp-batch-post-install-forms nil)
        (vcupp-batch-refresh-contents t)
        (vcupp-batch-package-native-compile t))
    (let ((result (vcupp-batch-effective-args
                   '(:root "/my/config/"
                     :load-files ("a.el" "b.el")
                     :compile-files ("a.el")
                     :refresh-contents nil))))
      (should (equal (plist-get result :root) (expand-file-name "/my/config/")))
      (should (equal (plist-get result :load-files) '("a.el" "b.el")))
      (should (equal (plist-get result :compile-files) '("a.el")))
      (should (null (plist-get result :refresh-contents))))))

(ert-deftest vcupp-batch-effective-args/compile-files-defaults-to-load-files ()
  "compile-files falls back to load-files when not specified."
  (let ((vcupp-batch-args nil)
        (vcupp-batch-root "/root/")
        (vcupp-batch-load-files nil)
        (vcupp-batch-compile-files nil)
        (vcupp-batch-setup-forms nil)
        (vcupp-batch-preload-features nil)
        (vcupp-batch-delete-elc-globs nil)
        (vcupp-batch-post-load-forms nil)
        (vcupp-batch-post-install-forms nil)
        (vcupp-batch-refresh-contents t)
        (vcupp-batch-package-native-compile t))
    (let ((result (vcupp-batch-effective-args
                   '(:load-files ("x.el" "y.el")))))
      (should (equal (plist-get result :compile-files) '("x.el" "y.el"))))))

;; ---------------------------------------------------------------------------
;; vcupp-batch.el -- expand-file
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-batch-expand-file/relative ()
  "Relative paths expand against vcupp-batch-root."
  (let ((vcupp-batch-root "/my/config/"))
    (should (equal (vcupp-batch-expand-file "init.el")
                   "/my/config/init.el"))))

(ert-deftest vcupp-batch-expand-file/absolute ()
  "Absolute paths pass through."
  (let ((vcupp-batch-root "/my/config/"))
    (should (equal (vcupp-batch-expand-file "/other/file.el")
                   "/other/file.el"))))

;; ---------------------------------------------------------------------------
;; vcupp-batch.el -- run-setup
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-batch-run-setup/sets-load-prefer-newer ()
  "run-setup always sets load-prefer-newer to t."
  (my-test-with-tmp-dir tmp
    (let ((vcupp-batch-root tmp)
          (vcupp-batch-preload-features nil)
          (vcupp-batch-setup-forms nil)
          (vcupp-batch-delete-elc-globs nil)
          (load-prefer-newer nil))
      (vcupp-batch-run-setup)
      (should (eq load-prefer-newer t)))))

(ert-deftest vcupp-batch-run-setup/evaluates-setup-forms ()
  "Setup forms are evaluated."
  (my-test-with-tmp-dir tmp
    (let ((vcupp-batch-root tmp)
          (vcupp-batch-preload-features nil)
          (vcupp-batch-setup-forms '((setq my-test-setup-marker 42)))
          (vcupp-batch-delete-elc-globs nil))
      (setq my-test-setup-marker nil)
      (vcupp-batch-run-setup)
      (should (= my-test-setup-marker 42)))))

(ert-deftest vcupp-batch-run-setup/deletes-elc-globs ()
  "Matching .elc files are deleted."
  (my-test-with-tmp-dir tmp
    (let ((init-dir (expand-file-name "init" tmp)))
      (make-directory init-dir)
      (my-test-write-el-file init-dir "foo.elc" "")
      (my-test-write-el-file init-dir "bar.elc" "")
      (my-test-write-el-file init-dir "baz.el" "(provide 'baz)")
      (let ((vcupp-batch-root tmp)
            (vcupp-batch-preload-features nil)
            (vcupp-batch-setup-forms nil)
            (vcupp-batch-delete-elc-globs '("init/*.elc")))
        (vcupp-batch-run-setup)
        (should-not (file-exists-p (expand-file-name "init/foo.elc" tmp)))
        (should-not (file-exists-p (expand-file-name "init/bar.elc" tmp)))
        (should (file-exists-p (expand-file-name "init/baz.el" tmp)))))))

;; ---------------------------------------------------------------------------
;; vcupp-batch.el -- load-config
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-batch-load-config/loads-files ()
  "Config files are loaded in order."
  (my-test-with-tmp-dir tmp
    (my-test-write-el-file tmp "a.el"
                           "(push 'a my-test-load-order)\n(provide 'a)")
    (my-test-write-el-file tmp "b.el"
                           "(push 'b my-test-load-order)\n(provide 'b)")
    (setq my-test-load-order nil)
    (let ((vcupp-batch-root tmp)
          (vcupp-batch-load-files '("a.el" "b.el"))
          (vcupp-batch-post-load-forms nil))
      (vcupp-batch-load-config)
      (should (equal my-test-load-order '(b a))))))

(ert-deftest vcupp-batch-load-config/evaluates-post-load-forms ()
  "Post-load forms are evaluated after loading."
  (my-test-with-tmp-dir tmp
    (my-test-write-el-file tmp "a.el" "(provide 'a)")
    (setq my-test-post-load-called nil)
    (let ((vcupp-batch-root tmp)
          (vcupp-batch-load-files '("a.el"))
          (vcupp-batch-post-load-forms
           '((setq my-test-post-load-called t))))
      (vcupp-batch-load-config)
      (should my-test-post-load-called))))

(ert-deftest vcupp-batch-load-config/calls-post-load-functions ()
  "Post-load function entries are called after loading."
  (my-test-with-tmp-dir tmp
    (my-test-write-el-file tmp "a.el" "(provide 'a)")
    (setq my-test-post-load-called nil)
    (let ((vcupp-batch-root tmp)
          (vcupp-batch-load-files '("a.el"))
          (vcupp-batch-post-load-forms
           (list (lambda () (setq my-test-post-load-called t)))))
      (vcupp-batch-load-config)
      (should my-test-post-load-called))))

;; ---------------------------------------------------------------------------
;; vcupp-batch.el -- post-install runner
;; ---------------------------------------------------------------------------

(defvar my-test-post-install-marker nil)

(ert-deftest vcupp-batch-run-post-install/evaluates-forms ()
  "Post-install forms are evaluated."
  (let ((my-test-post-install-marker nil)
        (vcupp-batch-post-install-forms
         '((setq my-test-post-install-marker 'done))))
    (vcupp-batch-run-post-install)
    (should (eq my-test-post-install-marker 'done))))

(ert-deftest vcupp-batch-run-post-install/calls-functions ()
  "Post-install function entries are called."
  (let ((my-test-post-install-marker nil)
        (vcupp-batch-post-install-forms
         (list (lambda () (setq my-test-post-install-marker 'done)))))
    (vcupp-batch-run-post-install)
    (should (eq my-test-post-install-marker 'done))))

;; ---------------------------------------------------------------------------
;; vcupp-batch.el -- with-effective-args macro
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-batch-with-effective-args/binds-variables ()
  "Macro binds batch variables from plist."
  (let ((vcupp-batch-args nil)
        (vcupp-batch-root "/default/")
        (vcupp-batch-load-files nil)
        (vcupp-batch-compile-files nil)
        (vcupp-batch-setup-forms nil)
        (vcupp-batch-preload-features nil)
        (vcupp-batch-delete-elc-globs nil)
        (vcupp-batch-post-load-forms nil)
        (vcupp-batch-post-install-forms nil)
        (vcupp-batch-refresh-contents t)
        (vcupp-batch-package-native-compile t))
    (vcupp-batch-with-effective-args '(:root "/test/" :load-files ("x.el"))
      (should (equal vcupp-batch-root (expand-file-name "/test/")))
      (should (equal vcupp-batch-load-files '("x.el"))))))

;; ---------------------------------------------------------------------------
;; vcupp-batch.el -- load-prefer-newer with stale .elc
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-batch-run-setup/load-prefer-newer-bypasses-stale-elc ()
  "With load-prefer-newer, loading prefers .el over stale .elc."
  (my-test-with-tmp-dir tmp
    (let* ((el-file (my-test-write-el-file
                     tmp "my-stale-lib.el"
                     ";;; my-stale-lib.el --- -*- lexical-binding: t -*-\n(defvar my-stale-lib-value \"original\")\n(provide 'my-stale-lib)\n"))
           (elc-file (concat el-file "c")))
      (byte-compile-file el-file)
      (should (file-exists-p elc-file))
      (sleep-for 1.1)
      (with-temp-file el-file
        (insert ";;; my-stale-lib.el --- -*- lexical-binding: t -*-\n(defvar my-stale-lib-value \"modified\")\n(provide 'my-stale-lib)\n"))
      (let ((vcupp-batch-root tmp)
            (vcupp-batch-preload-features nil)
            (vcupp-batch-setup-forms nil)
            (vcupp-batch-delete-elc-globs nil)
            (load-prefer-newer nil))
        (vcupp-batch-run-setup)
        (add-to-list 'load-path tmp)
        (setq features (delq 'my-stale-lib features))
        (require 'my-stale-lib)
        (should (equal (symbol-value 'my-stale-lib-value) "modified"))))))

;; ---------------------------------------------------------------------------
;; vcupp-native-comp.el -- active-p lifecycle
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-native-comp-active-p/initially-nil ()
  "vcupp-native-comp-active-p starts as nil."
  (should (null vcupp-native-comp-active-p)))

;; ---------------------------------------------------------------------------
;; vcupp-native-comp.el -- compile-angel state management
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-native-comp-enable-compile-angel/nil-input ()
  "Returns nil when compile-angel is not available."
  (should (null (vcupp-native-comp--enable-compile-angel nil))))

(ert-deftest vcupp-native-comp-disable-compile-angel/nil-state ()
  "No-op when state is nil."
  (vcupp-native-comp--disable-compile-angel nil))

(ert-deftest vcupp-native-comp-disable-compile-angel/empty-state ()
  "No-op when state has :enabled nil."
  (vcupp-native-comp--disable-compile-angel '(:enabled nil)))

;; ---------------------------------------------------------------------------
;; vcupp-native-comp.el -- use-compile-angel-p
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-native-comp-use-compile-angel-p/default ()
  "Defaults to vcupp-native-comp-use-compile-angel."
  (let ((vcupp-native-comp-use-compile-angel t))
    (should (vcupp-native-comp--use-compile-angel-p '())))
  (let ((vcupp-native-comp-use-compile-angel nil))
    (should-not (vcupp-native-comp--use-compile-angel-p '()))))

(ert-deftest vcupp-native-comp-use-compile-angel-p/plist-override ()
  "Plist :use-compile-angel overrides the default."
  (let ((vcupp-native-comp-use-compile-angel t))
    (should-not (vcupp-native-comp--use-compile-angel-p
                 '(:use-compile-angel nil))))
  (let ((vcupp-native-comp-use-compile-angel nil))
    (should (vcupp-native-comp--use-compile-angel-p
             '(:use-compile-angel t)))))

;; ---------------------------------------------------------------------------
;; vcupp.el -- package activation helper
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-activate-package/activates-installed-package ()
  "Activates an installed package when present in `package-alist'."
  (let* ((pkg-desc (package-desc-create
                    :name 'foo
                    :version '(1 0)
                    :kind 'vc
                    :dir "/tmp/foo"))
         (package-alist (list (list 'foo pkg-desc)))
         (activated nil))
    (cl-letf (((symbol-function 'package-activate-1)
               (lambda (pkg &rest _args)
                 (setq activated pkg))))
      (vcupp-activate-package 'foo))
    (should (eq activated pkg-desc))))

(ert-deftest vcupp-activate-package/noop-when-missing ()
  "Does nothing when the package is not present in `package-alist'."
  (let ((package-alist nil)
        (activated nil))
    (cl-letf (((symbol-function 'package-activate-1)
               (lambda (&rest _args)
                 (setq activated t))))
      (vcupp-activate-package 'foo))
    (should-not activated)))

;; ---------------------------------------------------------------------------
;; vcupp.el -- preload-package
;; ---------------------------------------------------------------------------

(defun my-test-write-autoloads (dir pkg-name &optional extra-content)
  "Write a minimal autoloads file for PKG-NAME in DIR.
Includes an `add-to-list' for load-path and optional EXTRA-CONTENT."
  (my-test-write-el-file
   dir (format "%s-autoloads.el" pkg-name)
   (concat
    ";;; " pkg-name "-autoloads.el --- autoloads -*- lexical-binding: t -*-\n"
    "(add-to-list 'load-path\n"
    "  (or (and load-file-name\n"
    "           (directory-file-name (file-name-directory load-file-name)))\n"
    "      (car load-path)))\n"
    (or extra-content "")
    "(provide '" pkg-name "-autoloads)\n")))

(ert-deftest vcupp-preload-package/loads-vc-autoloads ()
  "Loads autoloads from a bare VC-style directory."
  (my-test-with-tmp-dir tmp
    (let* ((pkg-dir (expand-file-name "mypkg" tmp)))
      (make-directory pkg-dir)
      (my-test-write-autoloads pkg-dir "mypkg")
      (let ((package-user-dir tmp)
            (load-path load-path))
        (vcupp-preload-package 'mypkg)
        (should (member pkg-dir load-path))))))

(ert-deftest vcupp-preload-package/loads-versioned-autoloads ()
  "Loads autoloads from a versioned ELPA-style directory."
  (my-test-with-tmp-dir tmp
    (let* ((pkg-dir (expand-file-name "mypkg-1.2" tmp)))
      (make-directory pkg-dir)
      (my-test-write-autoloads pkg-dir "mypkg")
      (let ((package-user-dir tmp)
            (load-path load-path))
        (vcupp-preload-package 'mypkg)
        (should (member pkg-dir load-path))))))

(ert-deftest vcupp-preload-package/prefers-vc-over-versioned ()
  "Prefers bare VC-style directory when both exist."
  (my-test-with-tmp-dir tmp
    (let* ((vc-dir (expand-file-name "mypkg" tmp))
           (elpa-dir (expand-file-name "mypkg-1.2" tmp)))
      (make-directory vc-dir)
      (make-directory elpa-dir)
      (my-test-write-autoloads vc-dir "mypkg")
      (my-test-write-autoloads elpa-dir "mypkg")
      (let ((package-user-dir tmp)
            (load-path load-path))
        (vcupp-preload-package 'mypkg)
        (should (member vc-dir load-path))
        (should-not (member elpa-dir load-path))))))

(ert-deftest vcupp-preload-package/noop-when-not-installed ()
  "Returns nil without error when the package is not installed."
  (my-test-with-tmp-dir tmp
    (let ((package-user-dir tmp)
          (load-path load-path))
      (should-not (vcupp-preload-package 'nonexistent))
      (should-not (cl-find-if (lambda (p) (string-prefix-p tmp p))
                              load-path)))))

(ert-deftest vcupp-preload-package/noop-when-autoloads-missing ()
  "Returns nil without error when directory exists but autoloads are missing."
  (my-test-with-tmp-dir tmp
    (let* ((pkg-dir (expand-file-name "mypkg" tmp)))
      (make-directory pkg-dir)
      (let ((package-user-dir tmp)
            (load-path load-path))
        (should-not (vcupp-preload-package 'mypkg))
        (should-not (member pkg-dir load-path))))))

;; ---------------------------------------------------------------------------
;; vcupp-install-packages.el -- active-p lifecycle
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-install-packages-active-p/initially-nil ()
  "vcupp-install-packages-active-p starts as nil."
  (should (null vcupp-install-packages-active-p)))

;; ---------------------------------------------------------------------------
;; vcupp.el -- vcupp-ensure-packages-on-install
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-ensure-packages-on-install/sets-ensure-when-active ()
  "Sets use-package-always-ensure when install sentinel is active."
  (let ((vcupp-install-packages-active-p t)
        (use-package-always-ensure nil))
    (vcupp-ensure-packages-on-install)
    (should (eq use-package-always-ensure t))))

(ert-deftest vcupp-ensure-packages-on-install/noop-when-inactive ()
  "No-op when vcupp-install-packages-active-p is nil."
  (let ((vcupp-install-packages-active-p nil)
        (use-package-always-ensure nil))
    (vcupp-ensure-packages-on-install)
    (should (null use-package-always-ensure))))

;; ---------------------------------------------------------------------------
;; vcupp-install-packages.el -- VC spec recording
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-install-packages-record-vc-spec ()
  "Recording a VC spec stores it in the alist."
  (let ((vcupp-install-packages--desired-vc-specs nil))
    (vcupp-install-packages--record-vc-spec
     'my-pkg nil '(my-pkg (:url "https://example.com") nil) nil nil)
    (should (equal (alist-get 'my-pkg vcupp-install-packages--desired-vc-specs)
                   '(my-pkg (:url "https://example.com") nil)))))

(ert-deftest vcupp-install-packages-record-vc-spec/preserves-order ()
  "Recording VC specs preserves declaration order and updates duplicates."
  (let ((vcupp-install-packages--desired-vc-specs nil))
    (vcupp-install-packages--record-vc-spec
     'dep nil '(dep (:url "https://example.com/dep") nil) nil nil)
    (vcupp-install-packages--record-vc-spec
     'app nil '(app (:url "https://example.com/app") nil) nil nil)
    (vcupp-install-packages--record-vc-spec
     'dep nil '(dep (:url "https://example.com/dep2") nil) nil nil)
    (should (equal vcupp-install-packages--desired-vc-specs
                   '((dep . (dep (:url "https://example.com/dep2") nil))
                     (app . (app (:url "https://example.com/app") nil)))))))

;; ---------------------------------------------------------------------------
;; vcupp-install-packages.el -- VC spec syncing
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-install-packages-sync-vc-specs/populates-selected-packages ()
  "Syncing copies recorded specs into `package-vc-selected-packages'."
  (let ((vcupp-install-packages--desired-vc-specs
         '((my-pkg . (my-pkg (:url "https://example.com"
                              :compile-files ("my-pkg.el"))
                             nil))))
        (package-vc-selected-packages nil))
    (vcupp-install-packages--sync-vc-specs)
    (should (equal (alist-get 'my-pkg package-vc-selected-packages
                              nil nil #'string=)
                   '(:url "https://example.com"
                     :compile-files ("my-pkg.el"))))))

(ert-deftest vcupp-install-packages-sync-vc-specs/multiple-packages ()
  "Syncing handles multiple recorded specs."
  (let ((vcupp-install-packages--desired-vc-specs
         '((pkg-a . (pkg-a (:url "https://a.com") nil))
           (pkg-b . (pkg-b (:url "https://b.com"
                             :compile-files ("b.el"))
                           nil))))
        (package-vc-selected-packages nil))
    (vcupp-install-packages--sync-vc-specs)
    (should (equal (alist-get 'pkg-a package-vc-selected-packages
                              nil nil #'string=)
                   '(:url "https://a.com")))
    (should (equal (alist-get 'pkg-b package-vc-selected-packages
                              nil nil #'string=)
                   '(:url "https://b.com"
                     :compile-files ("b.el"))))))

;; ---------------------------------------------------------------------------
;; vcupp-install-packages.el -- VC package conversion
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-install-packages-install-desired-vc/replaces-elpa-package ()
  "Desired VC packages replace existing non-VC packages."
  (let* ((pkg-desc (package-desc-create
                    :name 'foo
                    :version '(1 0)
                    :dir "/tmp/foo-1.0"))
         (package-alist (list (list 'foo pkg-desc)))
         (vcupp-install-packages--desired-vc-specs
          '((foo . (foo (:url "https://example.com/foo") nil))))
         deleted installed)
    (cl-letf (((symbol-function 'package-vc-p) (lambda (_) nil))
              ((symbol-function 'package-delete)
               (lambda (desc force nosave)
                 (setq deleted (list desc force nosave))))
              ((symbol-function 'package-vc-install)
               (lambda (pkg-spec rev)
                 (setq installed (list pkg-spec rev)))))
      (vcupp-install-packages--install-desired-vc-packages))
    (should (equal deleted (list pkg-desc t t)))
    (should (equal installed
                   '((foo :url "https://example.com/foo") nil)))))

(ert-deftest vcupp-install-packages-install-desired-vc/keeps-vc-package ()
  "Desired VC packages are left alone when already installed from VC."
  (let* ((pkg-desc (package-desc-create
                    :name 'foo
                    :version '(1 0)
                    :kind 'vc
                    :dir "/tmp/foo"))
         (package-alist (list (list 'foo pkg-desc)))
         (vcupp-install-packages--desired-vc-specs
          '((foo . (foo (:url "https://example.com/foo") nil))))
         called)
    (cl-letf (((symbol-function 'package-vc-p) (lambda (_) t))
              ((symbol-function 'package-delete)
               (lambda (&rest _) (setq called t)))
              ((symbol-function 'package-vc-install)
               (lambda (&rest _) (setq called t))))
      (vcupp-install-packages--install-desired-vc-packages))
    (should-not called)))

(ert-deftest vcupp-install-packages-install-desired-vc/installs-over-built-in ()
  "Desired VC packages install when only a built-in package is present."
  (let ((package-alist nil)
        (vcupp-install-packages--desired-vc-specs
         '((foo . (foo (:url "https://example.com/foo") nil))))
        installed)
    (cl-letf (((symbol-function 'package-installed-p) (lambda (_) t))
              ((symbol-function 'package-vc-install)
               (lambda (pkg-spec rev)
                 (setq installed (list pkg-spec rev)))))
      (vcupp-install-packages--install-desired-vc-packages))
    (should (equal installed
                   '((foo :url "https://example.com/foo") nil)))))

(ert-deftest vcupp-install-packages-install-recorded-vc-deps/converts-before-install ()
  "Runtime VC installs first convert already-recorded non-VC packages."
  (let* ((dep-desc (package-desc-create
                    :name 'dep
                    :version '(1 0)
                    :dir "/tmp/dep-1.0"))
         (package-alist (list (list 'dep dep-desc)))
         (package-vc-selected-packages nil)
         (vcupp-install-packages-active-p t)
         (vcupp-install-packages--desired-vc-specs
          '((dep . (dep (:url "https://example.com/dep") nil))
            (app . (app (:url "https://example.com/app") nil))))
         events)
    (cl-letf (((symbol-function 'package-vc-p) (lambda (_) nil))
              ((symbol-function 'package-installed-p) (lambda (_) nil))
              ((symbol-function 'package-delete)
               (lambda (desc _force _nosave)
                 (push (list 'delete (package-desc-name desc)) events)))
              ((symbol-function 'package-vc-install)
               (lambda (pkg-spec _rev)
                 (push (list 'install-vc (car pkg-spec)) events))))
      (vcupp-install-packages--install-recorded-vc-deps
       (lambda (_arg &optional _local-path)
         (push '(use-package-install app) events))
       '(app (:url "https://example.com/app") nil)))
    (should (equal (nreverse events)
                   '((delete dep)
                     (install-vc dep)
                     (use-package-install app))))
    (should (equal (alist-get 'dep package-vc-selected-packages nil nil
                              #'string=)
                   '(:url "https://example.com/dep")))))

;; ---------------------------------------------------------------------------
;; vcupp-install-packages.el -- stale .elc cleanup
;; ---------------------------------------------------------------------------

(defconst my-test-el-header
  ";;; foo.el --- test -*- lexical-binding: t -*-\n"
  "Standard header for test .el files to avoid byte-compile warnings.")

(ert-deftest vcupp-install-packages-clean-stale-elc/removes-stale ()
  "Stale .elc files are deleted when .el is newer."
  (my-test-with-tmp-dir tmp
    (let* ((el-file (my-test-write-el-file
                     tmp "foo.el"
                     (concat my-test-el-header "(provide 'foo)\n")))
           (elc-file (concat el-file "c"))
           (warning-minimum-level :error))
      (byte-compile-file el-file)
      (should (file-exists-p elc-file))
      (sleep-for 1.1)
      (with-temp-file el-file
        (insert my-test-el-header "(provide 'foo)\n"))
      (should (file-newer-than-file-p el-file elc-file))
      (let* ((pkg-name 'foo)
             (pkg-desc (package-desc-create
                        :name pkg-name
                        :version '(1 0)
                        :kind 'vc
                        :dir tmp))
             (package-alist (list (list pkg-name pkg-desc))))
        (cl-letf (((symbol-function 'package-vc-p) (lambda (_) t)))
          (vcupp-install-packages--clean-stale-vc-elc-files))
        (should-not (file-exists-p elc-file))
        (should (file-exists-p el-file))))))

(ert-deftest vcupp-install-packages-clean-stale-elc/keeps-fresh ()
  "Fresh .elc files are kept."
  (my-test-with-tmp-dir tmp
    (let* ((el-file (my-test-write-el-file
                     tmp "foo.el"
                     (concat my-test-el-header "(provide 'foo)\n")))
           (elc-file (concat el-file "c"))
           (warning-minimum-level :error))
      (byte-compile-file el-file)
      (should (file-exists-p elc-file))
      (should-not (file-newer-than-file-p el-file elc-file))
      (let* ((pkg-name 'foo)
             (pkg-desc (package-desc-create
                        :name pkg-name
                        :version '(1 0)
                        :kind 'vc
                        :dir tmp))
             (package-alist (list (list pkg-name pkg-desc))))
        (cl-letf (((symbol-function 'package-vc-p) (lambda (_) t)))
          (vcupp-install-packages--clean-stale-vc-elc-files))
        (should (file-exists-p elc-file))))))

;; ---------------------------------------------------------------------------
;; vcupp-batch.el -- plist-value helper
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-batch-plist-value/present ()
  "Returns plist value when key is present."
  (should (equal (vcupp-batch--plist-value '(:foo 42) :foo 99) 42)))

(ert-deftest vcupp-batch-plist-value/present-nil ()
  "Returns nil when key is present with nil value (not fallback)."
  (should (null (vcupp-batch--plist-value '(:foo nil) :foo 99))))

(ert-deftest vcupp-batch-plist-value/absent ()
  "Returns fallback when key is absent."
  (should (equal (vcupp-batch--plist-value '(:bar 1) :foo 99) 99)))

;; ---------------------------------------------------------------------------
;; vcupp.el -- self-dependency detection
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-find-self-deps/detects-self-dep ()
  "Detects packages that depend on themselves."
  (let* ((pkg-desc (package-desc-create
                    :name 'foo
                    :version '(1 0)
                    :reqs '((emacs (29 1)) (foo (1 0)))
                    :kind 'vc
                    :dir "/tmp/foo"))
         (package-alist (list (list 'foo pkg-desc))))
    (should (equal (vcupp-find-self-deps) '(foo)))))

(ert-deftest vcupp-find-self-deps/no-self-dep ()
  "Returns nil when no self-deps exist."
  (let* ((pkg-desc (package-desc-create
                    :name 'foo
                    :version '(1 0)
                    :reqs '((emacs (29 1)))
                    :kind 'vc
                    :dir "/tmp/foo"))
         (package-alist (list (list 'foo pkg-desc))))
    (should (null (vcupp-find-self-deps)))))

;; ---------------------------------------------------------------------------
;; vcupp.el -- duplicate package detection
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-find-duplicate-packages/detects-bare-and-versioned ()
  "Detects packages with both bare and versioned directories."
  (my-test-with-tmp-dir tmp
    (make-directory (expand-file-name "foo" tmp))
    (make-directory (expand-file-name "foo-1.2" tmp))
    (make-directory (expand-file-name "bar" tmp))
    (let ((package-user-dir tmp))
      (should (equal (vcupp-find-duplicate-packages) '("foo"))))))

(ert-deftest vcupp-find-duplicate-packages/no-duplicates ()
  "Returns nil when no duplicates exist."
  (my-test-with-tmp-dir tmp
    (make-directory (expand-file-name "foo" tmp))
    (make-directory (expand-file-name "bar-1.2" tmp))
    (let ((package-user-dir tmp))
      (should (null (vcupp-find-duplicate-packages))))))

;; ---------------------------------------------------------------------------
;; vcupp.el -- selected-file-deps filtering
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-selected-file-deps/uses-main-file-only ()
  "Dep scanning uses only :main-file, not :compile-files."
  (my-test-with-tmp-dir tmp
    (my-test-write-el-file tmp "main.el"
      ";;; main.el --- test -*- lexical-binding: t -*-\n;; Package-Requires: ((emacs \"29.1\"))\n(provide 'main)\n")
    (my-test-write-el-file tmp "ext.el"
      ";;; ext.el --- test -*- lexical-binding: t -*-\n;; Package-Requires: ((main \"1.0\") (emacs \"29.1\"))\n(provide 'ext)\n")
    (let* ((pkg-desc (package-desc-create
                      :name 'main
                      :version '(1 0)
                      :kind 'vc
                      :dir tmp))
           (package-vc-selected-packages
            `((main . (:url "https://example.com"
                       :main-file "main.el"
                       :compile-files ("ext.el")))))
           captured-files)
      (vcupp--selected-file-deps
       (lambda (_pkg-desc _pkg-dir)
         (setq captured-files (directory-files tmp t "\\.el\\'")))
       pkg-desc tmp)
      (should (= (length captured-files) 1))
      (should (string-match-p "/main\\.el\\'" (car captured-files))))))

(ert-deftest vcupp-dep-scan-files/main-file-only ()
  "Returns only :main-file even when :compile-files is also set."
  (my-test-with-tmp-dir tmp
    (my-test-write-el-file tmp "main.el" "(provide 'main)")
    (my-test-write-el-file tmp "ext.el" "(provide 'ext)")
    (let* ((pkg-desc (package-desc-create
                      :name 'main :version '(1 0) :kind 'vc :dir tmp))
           (package-vc-selected-packages
            `((main . (:url "https://example.com"
                       :main-file "main.el"
                       :compile-files ("ext.el"))))))
      (let ((files (vcupp--dep-scan-files pkg-desc)))
        (should (= (length files) 1))
        (should (string-match-p "/main\\.el\\'" (car files)))))))

(ert-deftest vcupp-dep-scan-files/falls-back-to-compile-files ()
  "Without :main-file, falls back to :compile-files."
  (my-test-with-tmp-dir tmp
    (my-test-write-el-file tmp "a.el" "(provide 'a)")
    (my-test-write-el-file tmp "b.el" "(provide 'b)")
    (let* ((pkg-desc (package-desc-create
                      :name 'my-pkg :version '(1 0) :kind 'vc :dir tmp))
           (package-vc-selected-packages
            `((my-pkg . (:url "https://example.com"
                         :compile-files ("a.el" "b.el"))))))
      (let ((files (vcupp--dep-scan-files pkg-desc)))
        (should (= (length files) 2))))))

(ert-deftest vcupp-selected-file-deps/no-spec-scans-all ()
  "Without a spec, directory-files is not filtered."
  (my-test-with-tmp-dir tmp
    (my-test-write-el-file tmp "main.el"
      ";;; main.el --- test -*- lexical-binding: t -*-\n(provide 'main)\n")
    (my-test-write-el-file tmp "ext.el"
      ";;; ext.el --- test -*- lexical-binding: t -*-\n(provide 'ext)\n")
    (let* ((pkg-desc (package-desc-create
                      :name 'main
                      :version '(1 0)
                      :kind 'vc
                      :dir tmp))
           (package-vc-selected-packages nil)
           captured-files)
      (vcupp--selected-file-deps
       (lambda (_pkg-desc _pkg-dir)
         (setq captured-files (directory-files tmp t "\\.el\\'")))
       pkg-desc tmp)
      (should (>= (length captured-files) 2)))))

(ert-deftest vcupp-sanitize-selected-packages/strips-pos ()
  "Symbols-with-pos are interned before `package-selected-packages' is saved."
  (let* ((symbols-with-pos-enabled nil)
         (pos-sym (position-symbol 'xterm-color 10))
         (package-selected-packages (list pos-sym 'magit-section))
         saved)
    (should (symbol-with-pos-p pos-sym))
    (should-not (symbolp pos-sym))
    (vcupp--sanitize-selected-packages
     (lambda (&optional value)
       (when value
         (setq package-selected-packages value))
       (setq saved package-selected-packages)))
    (should (equal saved '(xterm-color magit-section)))
    (should-not (cl-some #'symbol-with-pos-p saved))))

(ert-deftest vcupp-selected-file-deps/emacs-31-one-arg ()
  "Emacs 31 `package-vc--unpack-1' is called with PKG-DESC only."
  (my-test-with-tmp-dir tmp
    (my-test-write-el-file tmp "main.el"
      ";;; main.el --- test -*- lexical-binding: t -*-\n;; Package-Requires: ((emacs \"29.1\"))\n(provide 'main)\n")
    (let* ((pkg-desc (package-desc-create
                      :name 'main
                      :version '(1 0)
                      :kind 'vc
                      :dir tmp))
           (package-vc-selected-packages
            `((main . (:url "https://example.com"
                       :main-file "main.el"))))
           called-with)
      (vcupp--selected-file-deps
       (lambda (desc &rest extra)
         (setq called-with (cons desc extra)))
       pkg-desc)
      (should (equal called-with (list pkg-desc))))))

;; ---------------------------------------------------------------------------
;; vcupp.el -- compile-files keyword registration
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-compile-files-keyword-registered ()
  ":compile-files is registered in use-package-vc-valid-keywords."
  (should (memq :compile-files use-package-vc-valid-keywords)))

;; ---------------------------------------------------------------------------
;; vcupp.el -- :vc handler override
;; ---------------------------------------------------------------------------

(ert-deftest vcupp-vc-handler/always-includes-install-in-body ()
  "The :vc handler override includes `use-package-vc-install' in the body.
The built-in handler omits the install call from byte-compiled output,
which means packages missing at compile time stay missing at runtime."
  (let* ((body (vcupp--vc-handler-always-runtime
                'my-test-pkg :vc
                '(my-test-pkg (:url "https://example.com") nil)
                '() '())))
    (should (cl-some (lambda (form)
                       (and (listp form)
                            (eq (car form) 'use-package-vc-install)))
                     body))))

(ert-deftest vcupp-vc-handler/calls-install-at-compile-time ()
  "The :vc handler override preserves compile-time install behavior."
  (let ((installed nil))
    (cl-letf (((symbol-function 'use-package-vc-install)
               (lambda (&rest _) (setq installed t)))
              (byte-compile-current-file t))
      (vcupp--vc-handler-always-runtime
       'my-test-pkg :vc
       '(my-test-pkg (:url "https://example.com") nil)
       '() '()))
    (should installed)))

(ert-deftest vcupp-vc-handler/skips-compile-time-install-at-runtime ()
  "The :vc handler override does not call install at runtime."
  (let ((installed nil))
    (cl-letf (((symbol-function 'use-package-vc-install)
               (lambda (&rest _) (setq installed t))))
      (vcupp--vc-handler-always-runtime
       'my-test-pkg :vc
       '(my-test-pkg (:url "https://example.com") nil)
       '() '()))
    (should-not installed)))

;; ---------------------------------------------------------------------------
;; Live tests -- use fixture packages in a temporary elpa directory
;; ---------------------------------------------------------------------------

(defvar my-test-fixtures-dir
  (expand-file-name "fixtures" my-test-test-dir))

(defun my-test-install-fixture (fixture-name elpa-dir deps &optional kind)
  "Copy fixture FIXTURE-NAME into ELPA-DIR and generate package metadata.
DEPS is the dependency list for the -pkg.el file, e.g.
\\='((emacs \"29.1\")).  KIND defaults to vc."
  (let* ((src (expand-file-name fixture-name my-test-fixtures-dir))
         (dst (expand-file-name fixture-name elpa-dir))
         (kind (or kind 'vc)))
    (copy-directory src dst nil nil t)
    (with-temp-file (expand-file-name
                     (concat fixture-name "-pkg.el") dst)
      (insert (format "%s%s\n"
                      ";;; Generated -*- no-byte-compile: t -*-\n"
                      (pp-to-string
                       `(define-package ,fixture-name "1.0" "Test fixture"
                          ',deps :kind ,kind)))))
    (with-temp-file (expand-file-name
                     (concat fixture-name "-autoloads.el") dst)
      (insert (format ";;; %s-autoloads.el --- autoloads -*- lexical-binding: t -*-\n(provide '%s-autoloads)\n;;; %s-autoloads.el ends here\n"
                      fixture-name fixture-name fixture-name)))
    dst))

(defun my-test-install-versioned-fixture (name version elpa-dir deps)
  "Create a minimal versioned ELPA package NAME-VERSION in ELPA-DIR.
DEPS is the dependency list."
  (let ((dir (expand-file-name (format "%s-%s" name version) elpa-dir)))
    (make-directory dir t)
    (with-temp-file (expand-file-name (concat name "-pkg.el") dir)
      (insert (format "%s%s\n"
                      ";;; Generated -*- no-byte-compile: t -*-\n"
                      (pp-to-string
                       `(define-package ,name ,version "Test fixture"
                          ',deps)))))
    (with-temp-file (expand-file-name (concat name ".el") dir)
      (insert (format ";;; %s.el --- Stub -*- lexical-binding: t -*-\n(provide '%s)\n;;; %s.el ends here\n"
                      name name name)))
    dir))

(defmacro my-test-with-fixture-elpa (elpa-var &rest body)
  "Set up a temporary elpa dir bound to ELPA-VAR, evaluate BODY, then clean up.
Saves and restores package state."
  (declare (indent 1))
  `(my-test-with-tmp-dir ,elpa-var
     (let ((package-user-dir ,elpa-var)
           (package-alist nil)
           (package-activated-list nil)
           (package--initialized nil)
           (package-vc-selected-packages nil))
       ,@body)))

(ert-deftest vcupp-live/healthy-elpa-no-self-deps ()
  :tags '(:live)
  "Fixture elpa with correct deps has no self-dependencies."
  (skip-unless my-test-run-live-tests)
  (my-test-with-fixture-elpa elpa
    (my-test-install-fixture "multi-file-pkg" elpa '((emacs "29.1")))
    (my-test-install-fixture "simple-pkg" elpa '((emacs "29.1")))
    (package-initialize)
    (should-not (vcupp-find-self-deps))))

(ert-deftest vcupp-live/detects-self-dep-from-extension ()
  :tags '(:live)
  "Fixture elpa with extension deps in -pkg.el triggers self-dep detection."
  (skip-unless my-test-run-live-tests)
  (my-test-with-fixture-elpa elpa
    (my-test-install-fixture "multi-file-pkg" elpa
                             '((emacs "29.1") (multi-file-pkg "1.0")))
    (package-initialize)
    (should (memq 'multi-file-pkg (vcupp-find-self-deps)))))

(ert-deftest vcupp-live/detects-duplicate-packages ()
  :tags '(:live)
  "Bare VC dir + versioned ELPA dir for same package is flagged."
  (skip-unless my-test-run-live-tests)
  (my-test-with-fixture-elpa elpa
    (my-test-install-fixture "multi-file-pkg" elpa '((emacs "29.1")))
    (my-test-install-versioned-fixture "multi-file-pkg" "1.0" elpa
                                       '((emacs "29.1")))
    (should (equal (vcupp-find-duplicate-packages)
                   '("multi-file-pkg")))))

(ert-deftest vcupp-live/no-false-positive-duplicates ()
  :tags '(:live)
  "A versioned ELPA package without a bare counterpart is not flagged."
  (skip-unless my-test-run-live-tests)
  (my-test-with-fixture-elpa elpa
    (my-test-install-fixture "simple-pkg" elpa '((emacs "29.1")))
    (my-test-install-versioned-fixture "dep-pkg" "2.0" elpa
                                       '((emacs "29.1")))
    (should-not (vcupp-find-duplicate-packages))))

(ert-deftest vcupp-live/clean-package-initialize ()
  :tags '(:live)
  "Package activation produces no max-lisp-eval-depth errors."
  (skip-unless my-test-run-live-tests)
  (my-test-with-fixture-elpa elpa
    (my-test-install-fixture "multi-file-pkg" elpa '((emacs "29.1")))
    (my-test-install-fixture "simple-pkg" elpa '((emacs "29.1")))
    (let ((nesting-errors 0))
      (advice-add 'message :before
                  (lambda (fmt &rest _args)
                    (when (and (stringp fmt)
                               (string-match-p "max-lisp-eval-depth" fmt))
                      (setq nesting-errors (1+ nesting-errors))))
                  '((name . my-test-nesting-check)))
      (unwind-protect
          (package-initialize)
        (advice-remove 'message 'my-test-nesting-check))
      (should (= nesting-errors 0)))))

(ert-deftest vcupp-live/test-dir-excluded-from-byte-compile ()
  :tags '(:live)
  "VC package byte-compilation excludes test/ directories."
  (skip-unless my-test-run-live-tests)
  (my-test-with-fixture-elpa elpa
    (my-test-install-fixture "multi-file-pkg" elpa '((emacs "29.1")))
    (package-initialize)
    (let* ((pkg-desc (cadr (assq 'multi-file-pkg package-alist)))
           (pkg-dir (package-desc-dir pkg-desc))
           (test-file (expand-file-name "test/multi-file-pkg-test.el" pkg-dir))
           (main-file (expand-file-name "multi-file-pkg.el" pkg-dir))
           captured-ignores)
      (cl-letf (((symbol-function 'byte-recompile-directory)
                 (lambda (_dir _depth _force)
                   (setq captured-ignores byte-compile-ignore-files))))
        (package--compile pkg-desc))
      (should captured-ignores)
      (should (my-test-any-regexp-matches-p captured-ignores test-file))
      (should-not (my-test-any-regexp-matches-p captured-ignores main-file)))))

;;; test/test.el ends here
