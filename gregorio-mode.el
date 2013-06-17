;; This is a derived-major-mode for editing GABC files.
;; For information on Gregorio, please see:
;; http://home.gna.org/gregorio/

;; Keyboard bindings. Change them here

(global-set-key [f1] 'gregorio-to-tex) ;; to-tex in another buffer.
(global-set-key (kbd "C-c u") 'transpose-region-up) ;;
(global-set-key (kbd "C-c d") 'transpose-region-down)

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

(setq gregorio-keywords
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
	))

;; you can add to the list if necessary, just include the quotes for each item.

;;;; The rest of the file shouldn't need to be modified, but feel free to do so!


;; The various types of syntax coloring. Change the faces at the beginning of the file.

;; comments start with % on the beginning of the line.
(setq gregorio-comments-regexp "^%.?+")
;; keywords list as regexp
(setq gregorio-keywords-regexp
      (regexp-opt gregorio-keywords))
;; fields in other face, just because we can 
(setq gregorio-title-fields-regexp ":.+;")
;; the notes are always in parens, treat them like keywords
(setq gregorio-notes-regexp "([^)]+)")
;; the text in a special face so we can read it easier
(setq gregorio-text-regexp "\\ca\\|æ\\|œ") 
;; sometimes control characters
(setq gregorio-modifiers-regexp "{[^}]+}")
;; accented characters. Can also put vowels in different face if wanted.
(setq gregorio-text-accented-regexp
      (regexp-opt '("á" "é" "í" "ó" "ú")))
;; sometimes people insert html, just simple here
(setq gregorio-html-text-regexp "<[^>]+>")

;; set the faces. 

(setq gregorio-font-lock-keywords
      `((,gregorio-comments-regexp . comment-face)
	(,gregorio-keywords-regexp . keyword-face)
	(,gregorio-title-fields-regexp . title-fields-face)
	(,gregorio-notes-regexp . notes-face)
	(,gregorio-modifiers-regexp . modifiers-face)
	(,gregorio-html-text-regexp . html-face)
	(,gregorio-text-regexp . text-face)
	(,gregorio-text-accented-regexp . accented-face)
        ))

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

;; doing it this way so more pairs can be added 
;; there must be a better way of doing this.
;; TO DO : change this ugly beast.

(defvar trans-up-pairs
  [["a" "b"]
   ["b" "c"]
   ["c" "d"]
   ["d" "e"]
   ["e" "f"]
   ["f" "g"]
   ["g" "h"]
   ["h" "i"]
   ["i" "j"]
   ["j" "k"]
   ["k" "l"]
   ["l" "%"]])

(defvar trans-down-pairs
  [["a" "%"]
   ["b" "a"]
   ["c" "b"]
   ["d" "c"]
   ["e" "d"]
   ["f" "e"]
   ["g" "f"]
   ["h" "g"]
   ["i" "h"]
   ["j" "i"]
   ["k" "j"]
   ["l" "k"]
   ["%" "l"]])

;; Ugly worker function for transposing.
(defun replace-pairs-in-string (str pairs)
  "Replace STR by find/replace PAIRS sequence."
  ;; TO DO : this work-around is not elegant, find better solution
(save-excursion
  (let (my-var (myStr str) (tempMapPoints '()))
    ;; insert unicode points into the first of pair 
    (setq my-var 0)
    (while (< my-var (length pairs))
      (setq tempMapPoints (cons (format "⚎ด%x" my-var) tempMapPoints ))
      (setq my-var (1+ my-var))
      )
    ;; replace it
    (setq my-var 0)
    (while (< my-var (length pairs))
      (setq myStr (replace-regexp-in-string
                   (regexp-quote (elt (elt pairs my-var) 0))
                   (elt tempMapPoints my-var)
                   myStr t t))
      (setq my-var (1+ my-var))
      )
    ;; return it back
    (setq my-var 0)
    (while (< my-var (length pairs))
      (setq myStr (replace-regexp-in-string
                   (elt tempMapPoints my-var)
                   (elt (elt pairs my-var) 1)
                   myStr t t))
      (setq my-var (1+ my-var))
      )
    ;; final value
    myStr))) ;; O my this is ugly code. Please change asap.
  
(defun transpose-region-up (start end)
  "Transpose region upwards"
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char 1)
    (let ((case-fold-search nil))
      (while (search-forward-regexp "(\\([^)]+\\))" nil t)
	(replace-match (concat "("
			(replace-pairs-in-string (match-string 1) trans-up-pairs)
			")")
		       t nil)))))

(defun transpose-region-down (start end)
  "Transpose region upwards"
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char 1)
    (let ((case-fold-search nil))
      (while (search-forward-regexp "(\\([^)]+\\))" nil t)
	(replace-match (concat "("
			(replace-pairs-in-string (match-string 1) trans-down-pairs)
			")")
		       t nil)))))

;; define the derived mode. Note we use tex-mode as basis.

(define-derived-mode gregorio-mode tex-mode
  "gregorio"
  "Major Mode for editing .gabc files"
  
  (setq font-lock-defaults '((gregorio-font-lock-keywords)))
)

;; hooks for opening .gabc files, so this mode loads automatically.

(or (assoc "\\.gabc$" auto-mode-alist)
    (setq auto-mode-alist (cons '("\\.gabc$" . gregorio-mode) auto-mode-alist)))

;; let's wrap it all up.

(provide 'gregorio-mode)
