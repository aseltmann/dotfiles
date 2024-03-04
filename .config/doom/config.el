(setq initial-major-mode 'org-mode)

(pdf-tools-install)

(global-set-key (kbd "M-o") 'other-window)

(defun hrs/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'hrs/kill-current-buffer)

(map! :leader
      (:prefix-map ("t" . "toggle")
       :desc "org-indent-mode"        "C-i"   #'org-indent-mode))

(after! which-key
  (setq which-key-use-C-h-commands t))

(setq org-directory "~/org")

(setq org-ellipsis "↴")



(after! (:and jupyter org)
  (setq jupyter-org-mime-types '(:text/org :image/svg+xml :image/jpeg
                                 :image/png :text/html :text/markdown
                                 :text/latex :text/plain))
  ;; Set some default header arguments for example.
  (setq org-babel-default-header-args:sh    '((:results . "verbatim"))
        org-babel-default-header-args:jupyter-python '((:kernel . "python3")
                                                       (:session . "/jpy:localhost#8888:a37e524a-8134-4d8f-b24a-367acaf1bdd3")
                                                       (:pandoc . "t")
                                                       (:async . "yes")
                                                       )))

(defun display-ansi-colors ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(add-hook 'org-babel-after-execute-hook #'display-ansi-colors)

(use-package ob-tmux
  ;; Install package automatically (optional)
  :custom
  (org-babel-default-header-args:tmux
   '((:results . "silent")	;
     (:session . "default")	; The default tmux session to send code to
     (:socket  . nil)))		; The default tmux socket to communicate with
  ;; The tmux sessions are prefixed with the following string.
  ;; You can customize this if you like.
  (org-babel-tmux-session-prefix "ob-")
  ;; The terminal that will be used.
  ;; You can also customize the options passed to the terminal.
  ;; The default terminal is "gnome-terminal" with options "--".
  (org-babel-tmux-terminal "xterm")
  (org-babel-tmux-terminal-opts '("-T" "ob-tmux" "-e"))
  ;; Finally, if your tmux is not in your $PATH for whatever reason, you
  ;; may set the path to the tmux binary as follows:
  (org-babel-tmux-location "/usr/local/bin/tmux"))

(defun ob-tmux--insert-result ()
  (interactive)
  (let ((info (org-babel-get-src-block-info 'light)))
    (when (and info (string-equal "tmux" (nth 0 info)))
      (let* ((params (nth 2 info))
             (org-session (cdr (assq :session params)))
             (socket (cdr (assq :socket params)))
             (socket (when socket (expand-file-name socket)))
             (ob-session (ob-tmux--from-org-session org-session socket)))
        (org-babel-insert-result
             (ob-tmux--execute-string ob-session
                                      "capture-pane"
                                      "-p" ;; print to stdout

                                      "-t" (ob-tmux--session ob-session))
             '("replace"))))))

(defun ob-tmux--edit-result ()
  (interactive)
  (pcase (org-babel-get-src-block-info 'light)
    (`(,_ ,_ ,arguments ,_ ,_ ,start ,_)
     (save-excursion
       ;; Go to the results, if there aren't any then run the block.
       (goto-char start)
       (goto-char (or (org-babel-where-is-src-block-result)
                      (progn (org-babel-execute-src-block)
                             (org-babel-where-is-src-block-result))))
       (end-of-line)
       (skip-chars-forward " \r\t\n")
       (org-edit-special)
       (delete-trailing-whitespace)
       (end-of-buffer)
       t))
    (_ nil)))

(defun ob-tmux--open-src-block-result (orig-fun &rest args)
  (let ((info (org-babel-get-src-block-info 'light)))
    (if (and info (string-equal "tmux" (nth 0 info)))
        (progn
          (ob-tmux--insert-result)
          (ob-tmux--edit-result))
      (apply orig-fun args))))

(advice-add 'org-babel-open-src-block-result
            :around #'ob-tmux--open-src-block-result)

(setq org-src-block-faces '(("emacs-lisp" (:background "#482652"))
                            ("sh" (:background "#223814"))
                            ("tmux" (:background "#324725"))
                            ("python" (:background "#142b3e"))
                            ("jupyter-python" (:background "#371703"))
                            ))

(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)

(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(use-package! ox-twbs
  :after ox)

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
       '("~org/index.org"
         "~org/recurring-events.org")))

(after! org-journal
  (setq org-journal-dir "~/org/journal/")
  (setq org-journal-file-type `monthly)
  (setq org-journal-date-format "%A, %d %B %Y"))

(setq org-duration-format 'h:mm)

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

(org-link-set-parameters "zotero" :follow #'zotero-open)

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
