;; This is a derived-major-mode for editing GABC files.
;; For information on Gregorio, please see:
;; http://home.gna.org/gregorio/

;; Keyboard bindings. Change them here

(defvar gregorio-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") 'gregorio-to-tex)
    (define-key map (kbd "C-c u") 'gregorio-transpose-region-up)
    (define-key map (kbd "C-c d") 'gregorio-transpose-region-down)
    map)
  "Keymap used for gregorio-mode.")

;; Here are the faces for syntax coloring. Change them here.

(setq
 comment-face      'font-lock-comment-face
 keyword-face      'font-lock-constant-face
 title-fields-face 'font-lock-builtin-face
 notes-face        'font-lock-keyword-face
 modifiers-face    'font-lock-preprocessor-face
 html-face         'nobreak-space
 text-face         'bold
 accented-face     'bold
)

;; the keywords that are in the beginning of a gabc file. [meta-data]

(defvar gregorio-keywords
      '("name"
	"gabc-copyright"
	"score-copyright"
	"office-part"
	"occasion"
	"meter"
	"commentary"
	"arranger"
	"gabc-version"
	"author"
	"date"
	"manuscript"
	"manuscript-reference"
	"manuscript-storage-place"
	"book"
	"transcriber"
	"transcription-date"
	"gregoriotex-font"
	"mode"
	"initial-style"
	"centering-scheme"
	"user-notes"
	"annotation"
	"style"
	)
  "List of possible attribues for the header in gabc files.")

;; you can add to the list if necessary, just include the quotes for each item.

;;;; The rest of the file shouldn't need to be modified, but feel free to do so!

;; The various types of syntax coloring. Change the faces at the beginning of
;; the file.

(defvar gregorio-comments-regexp "^%.?+"
  "Regexp for comments which start with % on the beginning of the line.")

(defvar gregorio-keywords-regexp (regexp-opt gregorio-keywords)
  "Regexp for header attributes.")

(defvar gregorio-title-fields-regexp ":.+;"
  "Regexp for the values of keywords.")

(defvar gregorio-notes-regexp "([^)]+)"
  "Regexp for the notes contained within ()'s.")

(defvar gregorio-text-regexp "\\ca\\|æ\\|œ"
  "Regexp for sepcial text so it can be read easier.")

(defvar gregorio-modifiers-regexp "{[^}]+}"
  "Regexp control characters.")

(defvar gregorio-text-accented-regexp (regexp-opt '("á" "é" "í" "ó" "ú"))
  "Regexp for accented vowels.")

(defvar gregorio-html-text-regexp "<[^>]+>"
  "Regexp for html tags.")

;; set the faces.

(defvar gregorio-font-lock-keywords
      `((,gregorio-comments-regexp . comment-face)
	(,gregorio-keywords-regexp . keyword-face)
	(,gregorio-title-fields-regexp . title-fields-face)
	(,gregorio-notes-regexp . notes-face)
	(,gregorio-modifiers-regexp . modifiers-face)
	(,gregorio-html-text-regexp . html-face)
	(,gregorio-text-regexp . text-face)
	(,gregorio-text-accented-regexp . accented-face))
  "Expressions to highligh in gregorio mode.")

(defvar gregorio-mode-hook nil
  "Function(s) to call after starting up gregorio-mode.")

(defun gregorio-to-tex ()
  "convert buffer to tex, output to another buffer"
  (interactive)
  (setf new-tex (concat (file-name-base) ".tex"))
  (shell-command-on-region
   (point-min) (point-max)
   "gregorio -sS"
   new-tex
   nil 't)
   (switch-to-buffer-other-window new-tex)
   (tex-mode)) ;; new buffer is in tex-mode for syntax coloring and ready to save!

(defun gregorio-transpose-region-up (start end arg)
  "Transpose region upwards by one diatonic step.

With a numerical prefix argument, transpose by N diatonic steps.
i.e. C-u 2 \\[gregorio-transpose-region-up] Will transpose the region upwards by two steps.

N.B. This function does not check to see if the resulting score
will be out of range for gregorio. i.e. a tone higher than 'm'."
  (interactive "r\np")
  (unless arg (setq arg '(1)))
  (save-restriction
    (narrow-to-region start end)
    (goto-char 1)
    (let ((case-fold-search nil))
      (while (search-forward-regexp "(\\([^)]+\\))" nil t)
	(replace-match
	 (concat "("
		 (replace-regexp-in-string "[a-mA-M]"
		    (lambda (char) (char-to-string
				    (+ (string-to-char char) arg)))
		    (match-string 1) t)
		 ")")
	 t)))))

(defun gregorio-transpose-region-down (start end arg)
  "Transpose region downwards by one diatonic step.

With a numerical prefix argument, transpose by N diatonic steps.
i.e. C-u 2 \\[gregorio-transpose-region-down] Will transpose the region downwards by two steps.

N.B. This function does not check to see if the resulting score
will be out of range for gregorio. i.e. a tone lower than 'a'."
  (interactive "r\np")
  (unless arg (setq arg '(1)))
  (save-restriction
    (narrow-to-region start end)
    (goto-char 1)
    (let ((case-fold-search nil))
      (while (search-forward-regexp "(\\([^)]+\\))" nil t)
	(replace-match
	 (concat "("
		 (replace-regexp-in-string "[a-mA-M]"
		    (lambda (char) (char-to-string
				    (- (string-to-char char) arg)))
		    (match-string 1) t)
		 ")")
	 t)))))

;; define the derived mode. Note we use tex-mode as basis.

(define-derived-mode gregorio-mode tex-mode
  "gregorio"
  "Major Mode for editing .gabc files.

Commands:
\\{gregorio-mode-keymap}"

  (set (make-local-variable 'font-lock-defaults)
       '(gregorio-font-lock-keywords))
  (use-local-map gregorio-mode-keymap)
  (run-hooks 'gregorio-mode-hook))

;; hooks for opening .gabc files, so this mode loads automatically.

(or (assoc "\\.gabc$" auto-mode-alist)
    (add-to-list 'auto-mode-alist '("\\.gabc\\'" . gregorio-mode)))

;; let's wrap it all up.

(provide 'gregorio-mode)
