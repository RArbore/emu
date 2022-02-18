;;; emu-mode.el --- Major mode for editing emu source code.  -*- lexical-binding: t; -*-

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major mode for editing emu source code.  It provides
;; proper identation and syntax highlighting.

;;; Code:

(eval-when-compile
  (require 'rx))

(defconst emu--font-lock-defaults
  (let ((keywords '("func" "struct" "undefined" "if" "else" "while" "for" "switch" "case" "return" "break" "continue" "pure" "const" "inline" "comptime" "restrict"))
        (types '("void" "bool" "u8" "u16" "u32" "u64" "i8" "i16" "i32" "i64" "f32" "f64" )))
    `(((,(rx-to-string `(: (or ,@keywords))) 0 font-lock-keyword-face)
       ("\\([[:word:]]+\\)\s*(" 1 font-lock-function-name-face)
       (,(rx-to-string `(: (or ,@types))) 0 font-lock-type-face)))))

(defvar emu-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\( "()" st)

    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?- "w" st)

    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?' "'" st)

    ;; '==' as punctuation
    (modify-syntax-entry ?= ".")
    st))

(defun emu-indent-line ()
  "Indent current line."
  (let (indent
        boi-p
        move-eol-p
        (point (point)))
    (save-excursion
      (back-to-indentation)
      (setq indent (car (syntax-ppss))
            boi-p (= point (point)))
      (when (and (eq (char-after) ?\n)
                 (not boi-p))
        (setq indent 0))
      (when boi-p
        (setq move-eol-p t))
      (when (or (eq (char-after) ?\))
                (eq (char-after) ?\}))
        (setq indent (1- indent)))
      (delete-region (line-beginning-position)
                     (point))
      (indent-to (* tab-width indent)))
    (when move-eol-p
      (move-end-of-line nil))))

;;;###autoload
(define-derived-mode emu-mode prog-mode "emu"
  "Major mode for emu source files."
  :abbrev-table emu-mode-abbrev-table
  (setq font-lock-defaults emu--font-lock-defaults)
  (setq-local comment-start "//")
  (setq-local comment-start-skip "//+[\t ]*")
  (setq-local indent-line-function #'emu-indent-line)
  (setq-local indent-tabs-mode t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.emu" . emu-mode))

(provide 'emu-mode)
;;; emu-mode.el ends here
