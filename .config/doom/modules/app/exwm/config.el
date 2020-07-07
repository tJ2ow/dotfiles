;;; app/exwm/config.el -*- lexical-binding: t; -*-
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

(push ?\M-\  exwm-input-prefix-keys)
(evil-set-initial-state 'exwm-mode 'emacs)
(setq persp-init-frame-behaviour nil)

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
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ;; cut/paste.
        ([?\M-d] . [?\C-x])
        ([?\M-y] . [?\C-c])
        ([?\M-p] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])))

;; You can hide the minibuffer and echo area when they're not used, by
;; uncommenting the following line.
;(setq exwm-workspace-minibuffer-position 'bottom)

;; Do not forget to enable EXWM. It will start by itself when things are
;; ready.  You can put it _anywhere_ in your configuration.
(exwm-enable)

;; System tray
(require 'exwm-systemtray)
(exwm-systemtray-enable)

;; Programs
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

;; Keys
(global-set-key (kbd "M-s-b") 'takoda/launch-browser)
(global-set-key (kbd "M-s-g") 'takoda/launch-games)
(map! "M-s-p" #'takoda/launch-pa-control)
(global-set-key (kbd "s-C-x") 'takoda/scr-lock)

;; xrandr for proper resolutions
(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(0 "HDMI-A-0"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output HDMI-A-0 --mode 1360x768 --pos 0x0 --rotate normal ")))
(exwm-randr-enable)

;; autorun programs
(exwm/run-program "gis-weather")
