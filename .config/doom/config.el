;; .doom.d/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Takoda J Horton")
(setq user-mail-address "tjhorton04@gmail.com")

;; Each path is relative to `+mu4e-mu4e-mail-path', which is ~/.mail by default
(set-email-account! "tjhorton04@gmail.com"
  '((mu4e-sent-folder       . "/tjhorton04@gmail.com/Sent Mail")
    (mu4e-drafts-folder     . "/tjhorton04@gmail.com/Drafts")
    (mu4e-trash-folder      . "/tjhorton04@gmail.com/Trash")
    (mu4e-refile-folder     . "/tjhorton04@gmail.com/All Mail")
    (smtpmail-smtp-user     . "tjhorton04@gmail.com")
    (mu4e-compose-signature . "---\nTakoda Horton"))
  t)

(defvar takoda/leader-key "SPC z ")
(defvar takoda/local-leader-key doom-localleader-key)

(map! :prefix takoda/leader-key
      :n "Z" #'nil
      :n "X" #'nil
      :n "Q" #'nil)

(setq doom-theme 'doom-dracula)

(setq doom-font (font-spec :family "Source Code Pro" :size 15))

(display-time-mode 1)

(setq display-line-numbers-type 'relative)

(use-package prettier-js
  :hook (js2-mode . prettier-js-mode)
  :config
  (setq prettier-js-args '(
                           "--trailing-comma" "all"
                           "--bracket-spacing" "true")))

(use-package emmet-mode
  :hook (mhtml-mode . emmet-mode) ;; when you load a html file emmet will load with it
  :config
  (map! :i "C-h" #'emmet-expand-line)) ;; this will make emmet do its thing.

(use-package doom-snippets
  :load-path "~/.config/doom/snippets"
  :after yasnippet)

(add-hook! org-mode 'hl-todo-mode)

(setq org-src-ask-before-returning-to-edit-buffer 'nil)

(map! :map emacs-lisp-mode-map
      :prefix takoda/local-leader-key
      :n "'" #'org-edit-src-exit
      :n "=" #'org-edit-src-abort)

(map! :prefix ((concat takoda/leader-key "p ") . "password")
 :n "c" #'password-store-copy ;; Copy password to clipboard
 :n "e" #'password-store-edit ;; Edit Password in emacs
 :n "i" #'password-store-insert ;; create password for existing account.
 :n "g" #'password-store-generate ;; Generates random encrypted password!
 :n "R" #'password-store-remove ;; NOTE This will delete your password USE WITH CAUTION!
 :n "C" #'password-store-clear) ;; Clear the copied password from the kill-ring(clipboard).
(map! :prefix ((concat doom-leader-alt-key " z p ") . "password")
 :ie "c" #'password-store-copy ;; Copy password to clipboard
 :ie "e" #'password-store-edit ;; Edit Password in emacs
 :ie "i" #'password-store-insert ;; create password for existing account.
 :ie "g" #'password-store-generate ;; Generates random encrypted password!
 :ie "R" #'password-store-remove ;; NOTE This will delete your password USE WITH CAUTION!
 :ie "C" #'password-store-clear) ;; Clear the copied password from the kill-ring(clipboard).

(map! "s-RET" #'ansi-term)

(defvar my-term-shell "/bin/zsh")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(map! :prefix takoda/leader-key
 :n "TAB" #'eshell)

(setq avy-all-windows-alt 't)

(symon-mode)
(map! :prefix takoda/leader-key
 :n "s" #'symon-mode)
