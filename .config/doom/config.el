(setq initial-major-mode 'org-mode)

(map! :leader
      (:prefix-map ("t" . "toggle")
       :desc "org-indent-mode"        "C-i"   #'org-indent-mode))

(map! :map my-evil-org-mode-map
      :m "]n"  #'org-next-visible-heading
      :m "[n"  #'org-previous-visible-heading
      :m "]j"  #'org-journal-next-entry
      :m "[j"  #'org-journal-previous-entry
      )

(map! :after doc-view
      :map doc-view-mode-map
      :nmv "C-d" #'doc-view-scroll-down-or-previous-page
      :nmv "C-u" #'doc-view-scroll-up-or-next-page)
(after! doc-view
  (setq doc-view-continuous t))

(map! :after snake
      :map snake-mode-map
      :nmv "n" #'snake-start-game
      :nmv "j" #'snake-move-down
      :nmv "k" #'snake-move-up
      :nmv "h" #'snake-move-left
      :nmv "l" #'snake-move-right)

(setq org-directory "~/org")

(after! org
  (setq org-ellipsis "↴"))

(defun display-ansi-colors ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(add-hook 'org-babel-after-execute-hook #'display-ansi-colors)

(setq org-src-block-faces '(("emacs-lisp" (:background "#482652"))
                            ("sh" (:background "#223814"))
                            ("tmux" (:background "#324725"))
                            ("python" (:background "#611d1d"))
                            ("jupyter-python" (:background "#371703"))
                            ("jupyter-R" (:background "#281b69"))
                            ;; ("jupyter-python" (:background "#FFFFFF"))
                            ))

(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)

(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(after! org
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
           "NEXT(e)"  ; The next task after the current TODO is done
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)")))
  (setq org-log-done t))

(setq org-agenda-files
      (append
       (directory-files-recursively "~/org/projects/" ".org$")
       (directory-files-recursively "~/org/journal/" "[0-9]+$")
       (directory-files-recursively "~/org/00_roam2/" ".org$")
       '("~org/index.org"
         "~org/recurring-events.org")))

(after! org-journal
  (setq org-journal-dir "~/org/journal/")
  (setq org-journal-file-type `monthly)
  (setq org-journal-date-format "%A, %d %B %Y"))

(map!
 (:map calendar-mode-map
   :n "o" #'org-journal-display-entry
   :n "p" #'org-journal-previous-entry
   :n "n" #'org-journal-next-entry
   :n "O" #'org-journal-new-date-entry))

(after! org
  (setq org-duration-format 'h:mm))

(setq org-roam-directory (file-truename "~/org/00_roam2"))

(org-roam-db-autosync-mode)

(setq org-roam-mode-sections
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            #'org-roam-unlinked-references-section
            ))

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

(use-package! org-transclusion
  :after org
  :init
  (map!
   :map org-transclusion-map
   :leader
   (:prefix ("n" . "notes")
            (:prefix ("i" . "transclusion")
                     :desc "add transcluded text at point" :nvme "a" #'org-transclusion-add
                     :desc "add all active transclusions in buffer" :nvme "A" #'org-transclusion-add-all
                     :desc "make transclusion from link at point" :nvme "l" #'org-transclusion-make-from-link
                     :desc "remove transcluded text at point" :nvme "r" #'org-transclusion-remove
                     :desc "remove all transcluded text in buffer" :nvme "R" #'org-transclusion-remove-all
                     :desc "open source of transclusion at point" :nvme "o" #'org-transclusion-open-source
                     :desc "move to source of transclusion at point" :nvme "O" #'org-transclusion-move-to-source
                     :desc "Org Transclusion Mode" :nvme "t" #'org-transclusion-mode
                     :desc "activate transclusion setup in buffer" :nvme "C-a" #'org-transclusion-activate
                     :desc "deactivate transclusion setup in buffer" :nvme "C-d" #'org-transclusion-deactivate
                     :desc "demote transcluded subtree at point" :nvme "C-h" #'org-transclusion-demote-subtree
                     :desc "promote transcluded subtree at point" :nvme "C-l" #'org-transclusion-promote-subtree
                     :desc "exit live-sync edit at point" :nvme "C-e" #'org-transclusion-live-sync-exit
                     :desc "start live-sync edit at point" :nvme "C-s" #'org-transclusion-live-sync-start
                     :desc "paste to live-sync edit " :nvme "C-p" #'org-transclusion-live-sync-paste
                     )
            )
   )
  )

(defun my/org-ctrl-c-ctrl-c ()
  (interactive)
  (setq inhibit-read-only t)
  (org-ctrl-c-ctrl-c)
  (setq inhibit-read-only nil))

(map!
 :after org
 :map org-mode-map
 "C-c C-c" #'my/org-ctrl-c-ctrl-c
 )

(use-package! ox-twbs
  :after ox)

(after! deft
  (setq deft-default-extension "org")
  (setq deft-extensions '("org" "md" "txt"))
  (setq deft-use-filename-as-title t)
  (setq deft-directory "~/org/00_zd")
  (setq deft-auto-save-interval 60)
  )

(defun zetteldeft-go-home ()
  "Move to a designated home note.
Set `zetteldeft-home-id' to an ID string of your home note."
  (interactive)
  (if (stringp zetteldeft-home-id)
      (zetteldeft-find-file
        (zetteldeft--id-to-full-path zetteldeft-home-id))
    (message "No home set. Provide a string to zetteldeft-home-id.")))

(after! zetteldeft
  (setq zetteldeft-home-id "2021-11-30-2245")
  )

(setq zetteldeft-home-id "2021-11-30-2245")

;(defun zd-dir-big ()
;  (interactive)
;  (let ((gc-cons-threshold most-positive-fixnum))
;    (deft)
;    (setq deft-directory "~/zd-big/")
;    (deft-refresh)))

(map! :map zetteldeft-map
      :leader
      (:prefix ("d" . "zettel")
                :desc "deft"            :nvme "d" #'deft
                :desc "new search"      :nvme "D" #'zetteldeft-deft-new-search
                :desc "refresh"         :nvme "R" #'deft-refresh
                :desc "search at point" :nvme "s" #'zetteldeft-search-at-point
                :desc "search current id" :nvme "c" #'zetteldeft-search-current-id
                :desc "follow link"     :nvme "f" #'zetteldeft-follow-link
                :desc "avy file other window" :nvme "F" #'zetteldeft-avy-file-search-ace-window
                :desc "browse"          :nvme "." #'zetteldeft-browse
                :desc "go home"         :nvme "h" #'zetteldeft-go-home
                :desc "avy link search" :nvme "l" #'zetteldeft-avy-link-search
                :desc "insert list of links" :nvme "L" #'zetteldeft-insert-list-links-block
                :desc "avy tag search"  :nvme "t" #'zetteldeft-avy-tag-search
                :desc "tag list"        :nvme "T" #'zetteldeft-tag-buffer
                :desc "insert tag"      :nvme "#" #'zetteldeft-tag-insert
                :desc "remove tag"      :nvme "$" #'zetteldeft-tag-remove
                :desc "search tag"      :nvme "/" #'zetteldeft-search-tag
                :desc "insert id"       :nvme "i" #'zetteldeft-find-file-id-insert
                :desc "insert id full search" :nvme "C-i" #'zetteldeft-full-search-id-insert
                :desc "insert full title" :nvme "I" #'zetteldeft-find-file-full-title-insert
                :desc "insert title full search" :nvme "C-I" #'zetteldeft-full-search-full-title-insert
                :desc "find file"       :nvme "o" #'zetteldeft-find-file
                :desc "new file"        :nvme "n" #'zetteldeft-new-file
                :desc "new file & link" :nvme "N" #'zetteldeft-new-file-and-link
                :desc "new file & backlink" :nvme "B" #'zetteldeft-new-file-and-backlink
                :desc "add backlink"    :nvme "b" #'zetteldeft-backlink-add
                :desc "rename"          :nvme "r" #'zetteldeft-file-rename
                :desc "count words"     :nvme "x" #'zetteldeft-count-words
                )
      )

(add-hook 'org-mode-hook #'org-zotxt-mode)
(add-hook 'org-mode-hook #'org-zotxt-mode)

(map! :map zotxt-map
      :leader
      (:prefix ("z" . "zotero")
                :desc "org insert item"         :nvme "i" #'org-zotxt-insert-reference-link
                :desc "org update link here"    :nvme "u" #'org-zotxt-update-reference-link-at-point
                :desc "org update all links"    :nvme "U" #'org-zotxt-update-all-reference-links
                :desc "org open attachment"     :nvme "a" #'org-zotxt-open-attachment
                :desc "insert citekey"          :nvme "k" #'zotxt-citekey-insert
                :desc "select citekey in Zotero" :nvme "s" #'zotxt-citekey-select-item-at-point
                )
      )

(defun zotero-open (zotero-link)
  (start-process "zotero_open" nil "open" (concat "zotero:" zotero-link)))

(after! ol
  (org-link-set-parameters "zotero" :follow #'zotero-open))

(add-hook 'org-mode-hook 'rainbow-mode)

(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'gfm-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

(with-eval-after-load 'org
;; Allow multiple line Org emphasis markup.
;; http://emacs.stackexchange.com/a/13828/115
(setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
;; Below is needed to apply the modified `org-emphasis-regexp-components'
;; settings from above.
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components))

(setq doom-modeline-enable-word-count t)
