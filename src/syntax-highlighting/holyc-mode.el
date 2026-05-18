;;; holyc-mode.el --- Major mode for HolyC programming language -*- lexical-binding: t; -*-

;; Author: HolyC-Lang Contributors
;; URL: https://github.com/Jamesbarford/holyc-lang
;; Version: 1.0
;; Keywords: languages, holyc
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; Major mode for editing HolyC source files (.HC, .HH).
;; Derived from cc-mode (C mode) for indentation and basic syntax support.
;; Adds HolyC-specific keywords, types, and constants.

;;; Code:

(require 'cc-mode)

(defvar holyc-mode-syntax-table
  (let ((table (make-syntax-table c-mode-syntax-table)))
    table)
  "Syntax table for `holyc-mode'.")

(defvar holyc-types
  '("U0" "Bool" "I8" "U8" "I16" "U16" "I32" "U32" "I64" "U64" "F64"
    "auto" "atomic" "reg" "noreg")
  "HolyC type keywords.")

(defvar holyc-keywords
  '("sizeof"
    "try" "catch" "throw"
    "class" "union" "public" "private"
    "extern" "_extern" "inline" "static" "volatile"
    "asm")
  "HolyC-specific keywords not covered by cc-mode.")

(defvar holyc-constants
  '("NULL" "TRUE" "FALSE"
    "EXIT_OK" "EXIT_FAIL"
    "I64_MIN" "I64_MAX" "U64_MAX" "U8_MAX" "I8_MAX" "I8_MIN"
    "STDOUT" "STDERR" "STDIN" "__BUFSIZ__"
    "DT_REG" "DT_DIR")
  "HolyC constants.")

(defvar holyc-preprocessor-keywords
  '("define" "ifdef" "ifndef" "endif" "elif" "elifdef" "undef" "defined")
  "HolyC preprocessor keywords (without # prefix).")

(defvar holyc-font-lock-keywords
  (let ((types-regexp (regexp-opt holyc-types 'words))
        (keywords-regexp (regexp-opt holyc-keywords 'words))
        (constants-regexp (regexp-opt holyc-constants 'words))
        (preproc-regexp (regexp-opt holyc-preprocessor-keywords 'words)))
    `((,types-regexp . font-lock-type-face)
      (,keywords-regexp . font-lock-keyword-face)
      (,constants-regexp . font-lock-constant-face)
      (,preproc-regexp . font-lock-preprocessor-face)))
  "Font-lock keywords for `holyc-mode'.")

;;;###autoload
(define-derived-mode holyc-mode c-mode "HolyC"
  "Major mode for editing HolyC source code.
Derived from C mode for indentation and basic syntax support.
Adds HolyC-specific keywords, types, and constants."
  :syntax-table holyc-mode-syntax-table
  (font-lock-add-keywords nil holyc-font-lock-keywords)
  (setq-local comment-start "// ")
  (setq-local comment-end ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.HC\\'" . holyc-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.HH\\'" . holyc-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hc\\'" . holyc-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hh\\'" . holyc-mode))

(provide 'holyc-mode)

;;; holyc-mode.el ends here
