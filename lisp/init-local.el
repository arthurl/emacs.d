;;; Mode customisations

;; Markdown
(setq markdown-command "pandoc -f markdown+tex_math_single_backslash+smart -t html -s --mathjax --highlight-style=pygments")

;; Eglot
(with-eval-after-load 'eglot
  (setq eglot-confirm-server-initiated-edits nil))



;;; Global customisations

;; Disable lax space matching
(setq search-whitespace-regexp nil)

;; Font
;; see format: https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html
(set-face-font 'default "Iosevka-11")  ;; 59 x 118
;;(set-face-font 'default "Fira Mono-10")  ;; 67 x 102
;;(set-face-font 'default "Source Code Pro-10")  ;; 64 x 102
(set-face-font 'variable-pitch "Fira Sans Light-12")
(face-spec-set 'fixed-pitch '((t . (:inherit default))) 'face-defface-spec)



;;; Writing and language
;; Spell check
(setq ispell-dictionary "en_GB")



;;; Org-mode
;; Parent cannot be marked as DONE if child nodes are not all DONE.
(setq org-enforce-todo-dependencies t)

;; Set up custom agenda view
(setq org-agenda-custom-commands
      '(("c" "(C)ore / Export view"
         ((tags-todo
           ;; Show NEXT items, but not if they are scheduled for the future, or
           ;; if they are due today (because those will already appear in the
           ;; list of deadlines).
           "-SCHEDULED>\"<now>\"-DEADLINE<=\"<today>\"/!NEXT"
           ((org-agenda-overriding-header "Next actions:")
            (org-agenda-sorting-strategy
             '(category-keep priority-down tag-up effort-up))
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if 'regexp ":STYLE:[[:space:]]+habit"))))
          (agenda
           ""
           ((org-agenda-span 7)
            (org-agenda-time-grid nil)
            (org-deadline-warning-days 0)
            (org-agenda-sorting-strategy
             '(habit-down todo-state-up category-keep priority-down tag-up effort-up))
            (org-agenda-entry-types '(:deadline :scheduled))
            (org-agenda-skip-function
             '(or
               ;; 'done = '("DONE" "DEFERRED" "CANCELLED")
               (org-agenda-skip-entry-if 'todo 'done)
               (and
                ;; Skip if NEXT but not a habit or with a deadline.
                (org-agenda-skip-entry-if 'todo '("NEXT"))
                (org-agenda-skip-entry-if 'notregexp ":STYLE:[[:space:]]+habit")
                                        ; regex /s- does not work ..?
                (org-agenda-skip-entry-if 'notdeadline)))))))
         ((org-agenda-dim-blocked-tasks 'invisible)
          (org-agenda-prefix-format "- ")
          (org-agenda-remove-tags '"prefix")
          (org-agenda-with-colors t))
         ("agenda.html"))))

(with-eval-after-load 'org (require 'org-habit))
;; Org habit graph position
(setq org-habit-graph-column 55
      org-habit-following-days 3)


(provide 'init-local)
