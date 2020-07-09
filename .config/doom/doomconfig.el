(setq user-full-name "Takoda J Horton")
(setq user-mail-address "tjhorton04@gmail.com")

(setq doom-theme 'doom-material)

(setq doom-font (font-spec :family "Source Code Pro" :size 15))

(display-time-mode 1)

(setq display-line-numbers-type 'relative)

(use-package symon
  :init (symon-mode)
  :config
  (map! :n "Z s" #'symon-mode))

(use-package prettier-js
  :hook (js2-mode . prettier-js-mode)
  :config
  (setq prettier-js-args '(
                           "--trailing-comma" "all"
                           "--bracket-spacing" "true")))

(use-package emmet-mode
  :hook (mhtml-mode . emmet-mode) ;; when you load a html file emmet will load with it
  :config
  (map! :ie "C-h" #'emmet-expand-line)) ;; this will make emmet do its thing.

(use-package doom-snippets
  :load-path "~/.config/doom/snippets"
  :after yasnippet)

(add-hook! org-mode 'hl-todo-mode)

(setq org-src-ask-before-returning-to-edit-buffer 'nil)

(map! :map emacs-lisp-mode-map
      :prefix "Z "
      :n "'" #'org-edit-src-exit
      :n "=" #'org-edit-src-abort)

(map! :prefix ("SPC z" . "password")
 :n "c" #'password-store-copy ;; Copy password to clipboard
 :n "e" #'password-store-edit ;; Edit Password in emacs
 :n "i" #'password-store-insert ;; create password for existing account.
 :n "g" #'password-store-generate ;; Generates random encrypted password!
 :n "R" #'password-store-remove ;; NOTE This will delete your password USE WITH CAUTION!
 :n "C" #'password-store-clear) ;; Clear the copied password from the kill-ring(clipboard).

(map! :prefix ((concat doom-leader-alt-key " z ") . "password")
 :ie "c" #'password-store-copy ;; Copy password to clipboard
 :ie "e" #'password-store-edit ;; Edit Password in emacs
 :ie "i" #'password-store-insert ;; create password for existing account.
 :ie "g" #'password-store-generate ;; Generates random encrypted password!
 :ie "R" #'password-store-remove ;; NOTE This will delete your password USE WITH CAUTION!
 :ie "C" #'password-store-clear) ;; Clear the copied password from the kill-ring(clipboard).

(map! "s-M-x" #'ansi-term)

(defvar my-term-shell "/bin/zsh")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(setq avy-all-windows-alt 't)

;; Each path is relative to `+mu4e-mu4e-mail-path', which is ~/.mail by default
(set-email-account! "tjhorton04@gmail.com"
  '((mu4e-sent-folder       . "/Sent")
    (mu4e-drafts-folder     . "/Drafts")
    (mu4e-trash-folder      . "/Trash")
    (mu4e-refile-folder     . "/All Mail")
    (smtpmail-smtp-user     . "tjhorton04@gmail.com")
    (mu4e-compose-signature . "---\nTakoda Horton"))
  t)

;; I have my "default" parameters from Gmail
(setq mu4e-sent-folder "/home/takoda/.mail/gmail/Sent"
      ;; mu4e-sent-messages-behavior 'delete ;; Unsure how this should be configured
      mu4e-drafts-folder "/home/takoda/.mail/gmail/Drafts"
      user-mail-address "tjhorton04@gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s")
(map! :prefix "SPC / "
      :nv "d" #'engine/search-duckduckgo)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elfeed-feeds
   (quote
    ("https://www.archlinux.org/feeds/packages/x86_64/"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(map! :n "SPC o c" #'calendar)
(map! :ie "M-SPC o c" #'calendar)

;; Load EXWM.
(require 'exwm)

;; Set the initial number of workspaces (they can also be created later).
(setq exwm-workspace-number 10)

;; All buffers created in EXWM mode are named "*EXWM*". You may want to
;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
;; are run when a new X window class name or title is available.  Here's
;; some advice on this topic:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + For applications with multiple windows (e.g. GIMP), the class names of
;    all windows are probably the same.  Using window titles for them makes
;;   more sense.
;; In the following example, we use class names for all windows except for
;; Java applications and GIMP.
(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

    ;; this is a way to declare truly global/always working keybindings
    ;; this is a nifty way to go back from char mode to line mode without using the mouse
    (exwm-input-set-key (kbd "s-r") #'exwm-reset)
    (exwm-input-set-key (kbd "s-k") #'exwm-workspace-delete)
    (exwm-input-set-key (kbd "s-w") #'exwm-workspace-swap)

    ;; the next loop will bind s-<number> to switch to the corresponding workspace
    (dotimes (i 10)
      (exwm-input-set-key (kbd (format "s-%d" i))
                          `(lambda ()
                             (interactive)
                             (exwm-workspace-switch-create ,i))))

    ;; the simplest launcher, I keep it in only if dmenu eventually stopped working or something
    (exwm-input-set-key (kbd "s-&")
                        (lambda (command)
                          (interactive (list (read-shell-command "$ ")))
                          (start-process-shell-command command nil command)))

;; To add a key binding only available in line-mode, simply define it in
;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)


;; You can hide the minibuffer and echo area when they're not used, by
;; uncommenting the following line.
;(setq exwm-workspace-minibuffer-position 'bottom)

;; Do not forget to enable EXWM. It will start by itself when things are
;; ready.  You can put it _anywhere_ in your configuration.
(exwm-enable)

(defun exwm/run-program (name)
  (interactive)
  (start-process name nil name))

(defun takoda/launch-browser ()
  (interactive)
  (exwm/run-program "firefox"))

(defun takoda/launch-games ()
  (interactive)
  (exwm/run-program "lutris"))

(defun takoda/launch-pa-control ()
  (interactive)
  (exwm/run-program "pavucontrol"))

(defun takoda/scr-lock ()
  (interactive)
  (exwm/run-program "slock"))

(defun takoda/launch-tor-browser ()
  (interactive)
  (exwm/run-program "tor-browser"))

(global-set-key (kbd "M-s-b") 'takoda/launch-browser)
(global-set-key (kbd "M-s-g") 'takoda/launch-games)
(map! "M-s-p" #'takoda/launch-pa-control)
(global-set-key (kbd "s-C-x") 'takoda/scr-lock)
(map! "M-s-t" #'takoda/launch-tor-browser)

(require 'exwm-systemtray)
(exwm-systemtray-enable)

(push ?\M-\  exwm-input-prefix-keys)
(evil-set-initial-state 'exwm-mode 'emacs)
(setq persp-init-frame-behaviour nil)

(map! :prefix doom-localleader-alt-key
      :e "f" #'exwm-layout-set-fullscreen
      :e "RET" #'exwm-workspace-move-window
      :e "t RET" #'exwm-layout-toggle-mode-line
      :e "t f" #'exwm-floating-toggle-floating)

;; The following example demonstrates how to use simulation keys to mimic
;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
;; and DEST is what EXWM actually sends to application.  Note that both SRC
;; and DEST should be key sequences (vector or string).
(setq exwm-input-simulation-keys
      '(
        ;; movement
        ([?\C-h] . [left])
        ([?\M-b] . [C-left])
        ([?\C-l] . [right])
        ([?\M-f] . [C-right])
        ([?\C-k] . [up])
        ([?\C-j] . [down])
        ([?\C-0] . [home])
        ([?\C-$] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ;; cut/paste.
        ([?\M-d] . [?\C-x])
        ([?\M-y] . [?\C-c])
        ([?\M-p] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])))

(exwm/run-program "gis-weather")
;;(exwm/run-program "another-program")

(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(0 "HDMI-A-0"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output HDMI-A-0 --mode 1360x768 --pos 0x0 --rotate normal ")))
(exwm-randr-enable)
