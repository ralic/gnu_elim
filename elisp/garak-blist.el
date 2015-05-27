;; -*- lexical-binding: t -*-

(require 'garak)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; patterns
(defvar garak-blist-root-node-text  "Gª")
(defvar garak-blist-root-node-re
  (concat "^\\(?:" garak-blist-root-node-text "\\|+\\)\\s-*\\(.+\\)"))
(defvar garak-blist-account-node-re "^[├└|`][──]a \\[\\S-+\\]\\(.*?\\) \\(.*\\)")
(defvar garak-blist-contact-node-re "^[├└|`][──]b \\[\\S-+\\]\\(.*?\\) \\(.*\\)")

(defvar garak-blist-font-lock-keywords
  '(("^\\(Gª\\)\\s-*\\(.+\\)"
     (1 (garak-blist-icon-props))
     (2 font-lock-constant-face ))
    ("^\\(\\+\\) \\(\\[\\(\\S-+?\\)\\]\\(.*?\\) \\)\\(.*\\)"
     (1 (garak-blist-icon-props))
     (2 (garak-blist-node-proto-props ))
     (5 (garak-blist-node-label-props )))
    ("^[├└|`][-─]\\([ab]\\) \\(\\[\\(\\S-+?\\)\\]\\(.*?\\)\\) \\(.*\\)"
     (1 (garak-blist-node-status-props))
     (2 (garak-blist-node-proto-props ))
     (5 (garak-blist-node-label-props )))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; metadata helpers
(defun garak-blist-name-or-uid (x)
  (let ((uid (string-to-number x)))
    (if (equal 0 uid) x uid)))

(defun garak-blist-node-name (node)
  (or (elim-avalue "bnode-alias"   node)
      (elim-avalue "contact-alias" node)
      (elim-avalue "server-alias"  node)
      (elim-avalue "bnode-name"    node)
      (elim-avalue "account-name"  node)))

(defun garak-blist-node-label-props ()
  (let ((type (aref (match-string 1) 0))
        (suid (match-string 3))
        what node)
    (setq what (garak-blist-name-or-uid suid)
          node (cond ((eq type ?a) (elim-account-status garak-elim-process what))
                     ((eq type ?b) (elim-buddy-data garak-elim-process what))))
    (list 'face    font-lock-variable-name-face
          'display (garak-blist-node-name node)) ))

(defun garak-blist-node-proto-props ()
  (let (protocol icon)
    (setq protocol (match-string 2)
          protocol (save-match-data (cadr (split-string protocol "[][]"))))
    (if (equal "none" protocol)
        (setq icon "")
      (setq icon (elim-avalue (concat ":prpl-" protocol) garak-icons))
      (or icon (setq icon (elim-avalue (concat ":" protocol) garak-icons))))
    (list 'face font-lock-type-face 'display (or icon "○"))))

(defun garak-blist-node-status (type what)
  (let (node-data conn-data)
    (cond ((eq type ?a)
           (setq node-data (elim-account-status garak-elim-process what)
                 conn-data (elim-account-connection garak-elim-process what))
           (let ((status (elim-avalue "status-type" node-data))
                 (conn   (elim-avalue "state"       conn-data))
                 (online (elim-avalue "connected"   node-data)))
             (cond ((eq   conn :connecting   ) ":connecting"   )
                   ((eq   conn :disconnected ) ":off"          )
                   ((null conn               ) ":off"          )
                   ((eq status :offline      ) ":off"          )
                   ((eq status :available    ) ":on"           )
                   ((eq status :unavailable  ) ":unavailable"  )
                   ((eq status :invisible    ) ":invisible"    )
                   ((eq status :away         ) ":away"         )
                   ((eq status :extended-away) ":extended-away")
                   ((eq status :mobile       ) ":away"         )
                   ((eq status :tune         ) ":away"         )
                   ((eq conn   :connected    ) ":on"           )
                   (online                     ":on"           )
                   (t                          ":off"          ))))
          ((eq type ?b)
           (setq node-data (elim-buddy-data garak-elim-process what))
           (let ((type    (elim-avalue "bnode-type" node-data))
                 (allowed (elim-avalue "allowed"    node-data))
                 (online  (elim-avalue "connected"  node-data)))
             (cond ((eq type :chat-node   ) ":chat" )
                   ((eq type :group-node  ) ":group")
                   ((eq type :contact-node)
                    (setq online
                          (elim-avalue "contact-online-buddies" node-data))
                    (if (and online (< 0 online)) ":person" ":off"))
                   ((eq type :buddy-node  )
                    (if (not allowed)
                        ":blocked"
                      (symbol-name (elim-avalue "status-type" node-data))) )
                   (t ":offline")) ))
          (t ":off")) ))

(defun garak-blist-node-status-props ()
  (let ((type (aref (match-string 1) 0))
        (suid (match-string 4))
        icon-name what)
    (setq what      (garak-blist-name-or-uid suid)
          icon-name (garak-blist-node-status type what))
    ;;(message "icon: %S ← (%S . %S)" icon-name type what)
    (list 'face font-lock-constant-face
          'display (elim-avalue icon-name garak-icons)) ))

(defun garak-blist-icon-props ()
  (let ((group (match-string 1)) icon)
    (setq icon
          (cdr (cond ((equal group garak-blist-root-node-text)
                      (assoc ":garak" garak-icons))
                     (t
                      (assoc ":group" garak-icons)))))
    (list 'face font-lock-builtin-face 'display icon)))

(defun garak-blist-account-text (adata)
  (let ((uid (car adata)) (val (cdr adata)) proto)
    (setq proto
          (replace-regexp-in-string "^:?prpl-" "" (elim-avalue :proto val)))
    (format "a [%s]%s %s"
            proto uid (elim-avalue :name val)) ))

(defun garak-blist-buddy-text (bnode &optional prefix)
  (let (uid cname type auid name proto adata)
    (setq uid   (elim-avalue "bnode-uid"     bnode)
          cname (elim-avalue "contact-alias" bnode)
          type  (elim-avalue "bnode-type"    bnode)
          auid  (elim-avalue "account-uid"   bnode)
          proto (elim-avalue "im-protocol"   bnode)
          name  (if (< 0 (length cname)) cname (garak-buddy-node-label bnode)))
    (setq proto (if proto (replace-regexp-in-string "^:?prpl-" "" proto)))
    (if (and (not prefix) (eq :group-node type)) (setq prefix ?+))
    (if (and (not proto) auid)
        (setq adata (elim-account-data garak-elim-process auid)
              proto (elim-avalue :proto adata)))
    (format "%c [%s]%s %s" (or prefix ?b) (or proto "none") uid name) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keymap functions
(defun garak-blist-maybe-pop-menu (&optional here keys)
  "Handles events in the buddy list buffer, popping up a menu
if the keypress or mouse-click is on an account or contact."
  (interactive "@")
  (setq keys (this-command-keys)
        here (if (and (vectorp keys)
                      (setq keys (aref keys 0))
                      (eventp keys))
                 (posn-point (event-start keys))
               (point)))
  (save-excursion
    (goto-char here)
    (goto-char (point-at-bol))
    ;;(message "invoking garak-blist-maybe-pop-menu at %S in response to %S"
    ;;         (point) keys)
    (cond ((looking-at garak-blist-root-node-re)
           (garak-blist-open-close-group))
          ;; the ccount menu
          ((looking-at garak-blist-account-node-re)
           (garak-blist-pop-account-menu (match-string 1) keys))
          ;; the buddy menu
          ((looking-at garak-blist-contact-node-re)
           (garak-blist-pop-contact-menu (match-string 1) keys))) ))

(defun garak-blist-pop-account-menu (acct &optional event)
  ""
  (let (menu data name op proc ccb menu-cb handler ev)
    (setq acct    (garak-blist-name-or-uid acct)
          proc     garak-elim-process
          ccb     'garak-account-options-ui-cb
          data    (elim-account-status garak-elim-process acct)
          name    (garak-blist-node-name data)
          handler 'garak-account-list-node-command
          ev      (and (eventp event) event)
          menu    (list name
                        (cons name
                              '(("Log In"         :login )
                                ("Log Out"        :logout)
                                ("--single-line")
                                ("Remove"         :remove)
                                ("--single-line")
                                ("Configure"      :config)
                                ("Extended Menu"  :menu  )))))
    (setq op (car (if (display-popup-menus-p)
                      (x-popup-menu t menu)
                    (tmm-prompt menu))))
    (cond ((eq op :login ) (elim-connect         proc  acct))
          ((eq op :logout) (elim-disconnect      proc  acct))
          ((eq op :config) (elim-account-options proc  acct ccb))
          ((eq op :remove) (garak-maybe-remove-account acct))
          ((eq op :menu  )
           (setq menu-cb
                 (lambda (proc name id attr args)
                   (garak-account-menu-response-handler proc name id
                                                        attr args ev)))
           (elim-account-menu proc acct menu-cb) )
          (t (elim-debug "UI Account Operation `%S' not implemented" op))) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keymaps
(defvar garak-blist-font-lock-defaults
  '(garak-blist-font-lock-keywords t nil))

(defvar garak-blist-mode-map
  (let ((x (make-sparse-keymap)))
    (define-key x (kbd "<down-mouse-1>") 'garak-blist-maybe-pop-menu)
    (define-key x (kbd "RET")            'garak-blist-maybe-pop-menu)
    (define-key x (kbd "<enter>")        'garak-blist-maybe-pop-menu)
    x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buddy entry handlers
(defun garak-blist-insert-buddy-toplevel (bnode &optional kids spred)
  (let (last)
    (insert (garak-blist-buddy-text bnode ?+) "\n")
    (or kids (setq kids
                   (elim-buddy-children garak-elim-process
                                        (elim-avalue "bnode-uid" bnode))))
    (or spred (setq spred garak-buddy-list-sort-type))
    (if spred (setq kids (sort kids spred)))
    (setq kids
          (mapcar
           (lambda (N)
             (garak-buddy-list-show garak-elim-process (cdr N))) kids)
          kids (delq nil kids))
    (while kids
      (setq last (not (cdr kids)))
      (insert (if last "└─" "├─")
              (garak-blist-buddy-text (car kids))
              (if last "\n\n" "\n"))
      (setq kids (cdr kids))) ))

(defun garak-blist-insert-buddy-list ()
  (mapc (lambda (b)
          (or (assoc "bnode-parent" b)
              (garak-blist-insert-buddy-toplevel b)))
        (elim-buddy-list garak-elim-process)))

(defun garak-blist-insert-buddy-in-group-at (point data)
  (goto-char point)
  (forward-line 1)
  (if (eobp) (insert "\n"))
  (insert (if (looking-at "\\+\\|\\s-*$") "└─" "├─")
          (garak-blist-buddy-text data) "\n"))

(defun garak-blist-update-buddy-at (point data)
  (goto-char point)
  (or (looking-at "+") (forward-char 2))
  (delete-region (point) (line-end-position))
  (insert (garak-blist-buddy-text data)))

(defun garak-blist-delete-buddy-at (point)
  (let (last)
    (goto-char point)
    (setq last (looking-at "└─"))
    (delete-region (point-at-bol) (min (1+ (point-at-eol)) (point-max)))
    (when last
      (forward-line -1)
      (when (looking-at "[^└]─")
        (delete-char 2)
        (insert   "└─")))))

(defun garak-blist-add-container (bnode)
  (let (uid type prefix where)
    (setq type   (elim-avalue "bnode-type" bnode)
          prefix (cond
                  ((eq :group-node type) ?+)
                  ((eq :contact-node type)
                   (if (> (elim-avalue "contact-online-buddies" bnode) 1) ?b))))
    (when prefix
      (setq uid   (elim-avalue "bnode-uid"  bnode)
            where (garak-blist-find-node uid type))
      (when (not where)
        (goto-char (1+ (point-min)))
        (when (re-search-forward "^\\s-*$" nil t)
          (setq where (1+ (goto-char (match-beginning 0))))
          (insert "\n" (garak-blist-buddy-text bnode ?+) "\n"))))
    where))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; account entry handlers
(defun garak-blist-insert-account-list (&optional account-list)
  (let (last)
    (goto-char (point-min))
    (if (not (search-forward-regexp "^Gª .*\n" nil t))
        (insert "Gª Account\n"))
    (or account-list (setq account-list (elim-account-alist garak-elim-process)))
    (while account-list
      (setq last (not (cdr account-list)))
      (insert (if last "└─" "├─")
              (garak-blist-account-text (car account-list))
              (if last "\n\n" "\n"))
      (setq account-list (cdr account-list))) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movement and navigation
(defun garak-blist-find-node (uid type)
  "Given UID (for account, buddy etc) and TYPE (:account, :buddy-node,
:contact-node or :group-node) find the position of the start of the
line on which that UID's widget lies.\n
Returns that point, if found, or nil."
  (let (found target what suid)
    (setq what (cond ((eq :buddy-node       type) "^..b")
                     ((eq :chat-node        type) "^..b")
                     ((eq :contact-node     type) "^..b")
                     ((eq :group-node       type) "^\\+")
                     ((eq :account          type) "^..a")))
    (when (and uid what)
      (setq suid   (format "%s" uid)
            target (concat what " \\[\\S-+\\]" suid "\\s-"))
      (save-excursion
        (goto-char (point-min))
        (when (search-forward-regexp target nil t)
          (setq found (line-beginning-position)))))
    found))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the mode itself
(define-derived-mode garak-blist-mode text-mode "Contacts"
  "Mode for displaying and managing Garak (IM) contacts"
  :group 'garak-blist
  (let ((font-lock-defaults garak-blist-font-lock-defaults))
    (set (make-local-variable 'font-lock-extra-managed-props) '(display))
    (set (make-local-variable 'garak-elim-process) nil)
    ;; FIXME: this is just a hack while we're in development:
    (setq garak-elim-process (car (process-list)))
    (font-lock-set-defaults)))

(provide 'garak-blist)
