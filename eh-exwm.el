;;; eh-exwm.el --- Tumashu's basic emacs configuation

;; Copyright (c) 2011 2012, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/tumashu.github.com
;; Version: 0.0.3

;; This file is not part of GNU Emacs.

;;; Commentary:

;; * Description
;; Tumashu's exwm configure

;; * Configure
;; Create ~/.xinitrc file or ~/.xsession file which content likes
;; the below example:

;; #+BEGIN_EXAMPLE
;; # You may need to comment out the next line to disable access control
;; xhost +

;; # Set themes, etc
;; # gnome-settings-daemon &

;; # Set fallback cursor
;; # xsetroot -cursor_name left_ptr

;; # Kill beep
;; xset b off

;; # Tell emacs load eh-exwm.el file
;; export eh_enable_exwm="yes"

;; # Emacs X input method (exim) setting
;; export XMODIFIERS=@im=exim
;; export GTK_IM_MODULE=xim
;; export QT_IM_MODULE=xim
;; export CLUTTER_IM_MODULE=xim

;; # Launch exwm
;; exec dbus-launch --exit-with-session emacs

;; #+END_EXAMPLE

;; * Run exwm
;; Run startx command or login with display-manager

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(use-package exwm
  :if (string= (getenv "eh_enable_exwm") "yes")
  :ensure nil
  :demand t
  :config

  ;; Disable menu-bar, tool-bar and scroll-bar to increase the usable space
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; Shrink fringes to 1 pixel
  (fringe-mode 1)

  ;; Disable dialog boxes since they are unusable in EXWM
  (setq use-dialog-box nil)

  ;; eh-exwm own variables
  (defvar eh-exwm/mode-line-buttons nil)
  (defvar eh-exwm/apps-mode-line-active-p nil)

  ;; All buffers created in EXWM mode are named "*EXWM*". You may want to change
  ;; when a new window class name or title is available.
  ;; it in `exwm-update-class-hook' and `exwm-update-title-hook', which are run
  (add-hook 'exwm-update-class-hook
            #'(lambda ()
                (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                            (string= "gimp" exwm-instance-name))
                  (exwm-workspace-rename-buffer (concat "Exwm:" exwm-class-name)))))

  (add-hook 'exwm-update-title-hook
            #'(lambda ()
                (when (or (not exwm-instance-name)
                          (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                  (exwm-workspace-rename-buffer (concat "Exwm:" exwm-title)))))

  (defun eh-exwm/create-mode-line-button (string mouse-1-action mouse-2-action)
    "Create clickable button's code which is used by mode-line-format."
    `(:eval (propertize
             ,string
             'face 'mode-line-buffer-id
             'mouse-face 'mode-line-highlight
             'local-map
             (let ((map (make-sparse-keymap)))
               (define-key map [mode-line mouse-1]
                 #'(lambda (event)
                     (interactive "e")
                     (with-selected-window (posn-window (event-start event))
                       ,mouse-1-action)))
               (define-key map [mode-line mouse-3]
                 #'(lambda (event)
                     (interactive "e")
                     (with-selected-window (posn-window (event-start event))
                       ,mouse-2-action)))
               map))))

  (defun eh-exwm/find-x-window-buffer (regexp)
    "Find a buffer of x-window which class, install or title is matched `regexp'."
    (let* ((buffers (buffer-list))
           (buffers-list (list nil nil nil)))

      (dolist (buffer buffers)
        (let ((wininfo `((0 . ,(buffer-local-value 'exwm-title buffer))
                         (1 . ,(buffer-local-value 'exwm-instance-name buffer))
                         (2 . ,(buffer-local-value 'exwm-class-name buffer)))))
          (dolist (x wininfo)
            (when (eh-exwm/string-match-p regexp (cdr x))
              (setf (nth (car x) buffers-list)
                    (append (list buffer) (nth (car x) buffers-list)))))))

      (caar (delq nil
                  (sort buffers-list
                        #'(lambda (a b)
                            (< (length a) (length b))))))))

  (defun eh-exwm/jump-or-exec (regexp cmd &optional shortcut-name current-window)
    "Jump to a window which class, instance or title matched `regexp',
if matched window can't be found, run shell command `cmd'."
    (when (and (not current-window)
               (featurep 'switch-window))
      (switch-window--then
       "Move to window: "
       #'(lambda () (other-window 1))
       nil nil 1))

    (let ((buffer (eh-exwm/find-x-window-buffer regexp)))
      (if buffer
          (exwm-workspace-switch-to-buffer buffer)
        (start-process-shell-command cmd nil cmd)))

    (let ((buffer (eh-exwm/find-x-window-buffer regexp)))
      (push (eh-exwm/create-mode-line-button
             (format "[%s] " (or shortcut-name regexp))
             `(eh-exwm/jump-or-exec ,regexp ,cmd ,shortcut-name t)
             `(kill-buffer ,buffer))
            eh-exwm/mode-line-buttons)))

  (defun eh-exwm/string-match-p (regexp string)
    (and (stringp regexp)
         (stringp string)
         (string-match-p regexp string)))

  (defun eh-exwm/apps-mode-line-enable ()
    (let ((buttons (cl-delete-duplicates
                    eh-exwm/mode-line-buttons
                    :test #'(lambda (x y)
                              (equal (nth 1 (cadr x))
                                     (nth 1 (cadr y)))))))
      (setq mode-line-format
            `(,(eh-exwm/create-mode-line-button
                "[E]" '(eh-exwm/emacs-mode-line-enable) '(eh-exwm/emacs-mode-line-enable))
              ,(eh-exwm/create-mode-line-button
                "[+]" '(delete-other-windows) '(delete-other-windows))
              ,(eh-exwm/create-mode-line-button
                "[D]" '(delete-window) '(delete-window))
              " -"
              ,(eh-exwm/create-mode-line-button
                "[X]" '(kill-buffer) '(kill-buffer))
              "- "
              ,@buttons
              "- "
              ,(eh-exwm/create-mode-line-button
                "[F]" '(exwm-floating-toggle-floating) '(exwm-floating-toggle-floating))
              ,(eh-exwm/create-mode-line-button
                "[_]" '(exwm-floating-hide) '(exwm-floating-hide))
              ,(eh-exwm/create-mode-line-button
                "[-]" '(split-window-below) '(split-window-below))
              ,(eh-exwm/create-mode-line-button
                "[|]" '(split-window-right) '(split-window-right))
              " -:"
              mode-line-mule-info
              "-"))
      (setq eh-exwm/mode-line-active-p t)
      (force-mode-line-update)))

  (setq-default mode-line-format
                `(,(eh-exwm/create-mode-line-button
                    "[E]" '(eh-exwm/apps-mode-line-enable) '(eh-exwm/apps-mode-line-enable))
                  ,(default-value 'mode-line-format)))

  (defun eh-exwm/emacs-mode-line-enable ()
    (setq mode-line-format (default-value 'mode-line-format))
    (setq eh-exwm/apps-mode-line-active-p nil)
    (force-mode-line-update))

  (defun eh-exwm/update-mode-line ()
    (interactive)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (if (eq major-mode 'exwm-mode)
            (eh-exwm/apps-mode-line-enable)
          (eh-exwm/emacs-mode-line-enable)))))

  (add-hook 'exwm-manage-finish-hook #'eh-exwm/update-mode-line)

  (defun eh-exwm/run-shell-command (cmd)
    (start-process-shell-command cmd nil cmd))

  (defun eh-exwm/run-shell-command-interactively (cmd)
    (interactive
     (list (read-shell-command "Run shell command: ")))
    (start-process-shell-command cmd nil cmd))

  (defun exwm-generate-debian-menu-commands ()
    (let ((file "/var/lib/emacs/exwm/exwm-menu.el")
          debian-menu-alist)
      (when (file-exists-p file)
        (load file)
        (setq debian-menu-alist
              (when (boundp 'exwm-debian-menu-alist)
                exwm-debian-menu-alist))
        (dolist (debian-menu debian-menu-alist)
          (let* ((debian-menu-command (nth 1 debian-menu))
                 (debian-menu-name (nth 2 debian-menu))
                 (exwm-command-name
                  (concat "EXWM/"
                          (replace-regexp-in-string
                           "^-\\|-$" ""
                           (replace-regexp-in-string
                            "-+" "-"
                            (replace-regexp-in-string
                             "[^a-zA-Z0-9]" "-"
                             (replace-regexp-in-string
                              "/Debian\\|/Applications" ""
                              debian-menu-name)))))))
            (eval `(defun ,(intern exwm-command-name) ()
                     (interactive)
                     (start-process-shell-command ,exwm-command-name nil ,debian-menu-command))))))))

  (defun eh-exwm/suspend-computer ()
    (interactive)
    (eh-exwm/run-shell-command "systemctl suspend"))

  (defun eh-exwm/hibernate-computer ()
    (eh-exwm/run-shell-command "systemctl hibernate"))

  (defun eh-exwm/restart-computer ()
    (interactive)
    (eh-exwm/run-shell-command "systemctl reboot"))

  (defun eh-exwm/shutdown-commputer ()
    (interactive)
    (eh-exwm/run-shell-command "systemctl poweroff"))

  (defun eh-exwm/firefox ()
    (interactive)
    (eh-exwm/jump-or-exec "Iceweasel" "iceweasel" "Firefox"))

  (defun eh-exwm/file-manager ()
    (interactive)
    (eh-exwm/jump-or-exec "Nautilus" "nautilus --no-desktop"))

  (defun eh-exwm/crossover ()
    (interactive)
    (eh-exwm/jump-or-exec "Crossover" "/opt/cxoffice/bin/crossover"))

  (defun eh-exwm/launch-crossover-app (app bottle &optional shortcut-name)
    (eh-exwm/jump-or-exec
     app
     (format "/opt/cxoffice/bin/wine --bottle %s --cx-app '%s'" bottle app)
     shortcut-name))

  (defun eh-exwm/qq ()
    (interactive)
    (eh-exwm/launch-crossover-app "TM.exe" "腾讯_TM_2013" "TM"))

  (defun eh-exwm/word ()
    (interactive)
    (eh-exwm/launch-crossover-app "WINWORD.EXE" "Microsoft_Office_2007" "Words"))

  (defun eh-exwm/excel ()
    (interactive)
    (eh-exwm/launch-crossover-app "EXCEL.EXE" "Microsoft_Office_2007" "Excel"))

  (defun eh-exwm/ppt ()
    (interactive)
    (eh-exwm/launch-crossover-app "POWERPNT.EXE" "Microsoft_Office_2007" "PPT"))

  (defun eh-exwm/winxp ()
    (interactive)
    (eh-exwm/jump-or-exec "VirtualBox" "VBoxManage startvm winxp"))

  (defun eh-exwm/mplayer ()
    (interactive)
    (eh-exwm/jump-or-exec "Smplayer" "smplayer"))

  (defun eh-exwm/htop ()
    (interactive)
    (eh-exwm/jump-or-exec "htop" "xfce4-terminal -T htop -e htop" "Top"))

  (defun eh-exwm/x-terminal-emulator ()
    (interactive)
    (eh-exwm/jump-or-exec "default-terminal" "xfce4-terminal -T default-terminal" "Terminal"))

  (defun eh-exwm/launch-new-terminal ()
    (interactive)
    (eh-exwm/run-shell-command "xfce4-terminal"))

  (defun eh-exwm/power-manager-settings ()
    (interactive)
    (eh-exwm/run-shell-command "xfce4-power-manager-settings"))

  (defun eh-exwm/power-manager ()
    (interactive)
    (eh-exwm/run-shell-command "xfce4-power-manager"))

  (defun eh-exwm/network-manager-applet ()
    (interactive)
    (eh-exwm/run-shell-command "nm-applet"))

  (defun eh-exwm/volit ()
    (interactive)
    (eh-exwm/run-shell-command "volti"))

  (defun eh-exwm/xscreensaver ()
    (interactive)
    (eh-exwm/run-shell-command "xscreensaver -no-splash &"))

  (defun eh-exwm/lock-screen ()
    (interactive)
    (eh-exwm/run-shell-command "exec xscreensaver-command -lock"))

  (defun eh-exwm/switch-to-1-workspace ()
    (interactive)
    (exwm-workspace-switch 0))

  (defun eh-exwm/switch-to-2-workspace ()
    (interactive)
    (exwm-workspace-switch 1))

  (defun eh-exwm/switch-to-3-workspace ()
    (interactive)
    (exwm-workspace-switch 2))

  (defun eh-exwm/switch-to-4-workspace ()
    (interactive)
    (exwm-workspace-switch 3))

  ;; Don't Delete the below two lines
  (global-unset-key (kbd "C-t"))
  (push ?\C-t exwm-input-prefix-keys)

  (exwm-input-set-key (kbd "C-t R")  nil)
  (exwm-input-set-key (kbd "C-t q")  nil)
  (exwm-input-set-key (kbd "C-t m")  nil)
  (exwm-input-set-key (kbd "C-t v")  'eh-exwm/file-manager)
  (exwm-input-set-key (kbd "C-t c")  'eh-exwm/x-terminal-emulator)
  (exwm-input-set-key (kbd "C-t ff") 'eh-exwm/firefox)
  (exwm-input-set-key (kbd "C-t fq") 'eh-exwm/qq)
  (exwm-input-set-key (kbd "C-t fj") 'eh-exwm/jabref)
  (exwm-input-set-key (kbd "C-t fc") 'eh-exwm/cajviewer)
  (exwm-input-set-key (kbd "C-t fp") 'eh-exwm/pdfreader)
  (exwm-input-set-key (kbd "C-t fw") 'eh-exwm/winxp)
  (exwm-input-set-key (kbd "C-t w")  'exwm-workspace-switch)
  (exwm-input-set-key (kbd "C-t x")  'eh-exwm/x-terminal-emulator)
  (exwm-input-set-key (kbd "C-t c")  'eh-exwm/run-shell-command-interactively)

  (exwm-input-set-key (kbd "C-t 1")  'eh-exwm/switch-to-1-workspace)
  (exwm-input-set-key (kbd "C-t 2")  'eh-exwm/switch-to-2-workspace)
  (exwm-input-set-key (kbd "C-t 3")  'eh-exwm/switch-to-3-workspace)
  (exwm-input-set-key (kbd "C-t 4")  'eh-exwm/switch-to-4-workspace)

  ;; We always need a way to go back to line-mode from char-mode
  (exwm-input-set-key (kbd "C-t t") 'exwm-reset)
  (exwm-input-set-key (kbd "C-t C-t") 'exwm-reset)
  (exwm-input-set-key (kbd "s-r") 'exwm-reset)

  ;; The following example demonstrates how to set a key binding only available
  ;; in line mode. It's simply done by first push the prefix key to
  ;; `exwm-input-prefix-keys' and then add the key sequence to `exwm-mode-map'.
  ;; The example shorten 'C-c q' to 'C-q'.
  (push ?\C-q exwm-input-prefix-keys)
  (push ?\C-\\ exwm-input-prefix-keys)
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; The following example demonstrates how to use simulation keys to mimic the
  ;; behavior of Emacs. The argument to `exwm-input-set-simulation-keys' is a
  ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press and
  ;; DEST is what EXWM actually sends to application. Note that SRC must be a key
  ;; sequence (of type vector or string), while DEST can also be a single key.
  (exwm-input-set-simulation-keys
   '(([?\C-b] . left)
     ([?\C-f] . right)
     ([?\C-p] . up)
     ([?\C-n] . down)
     ([?\C-a] . home)
     ([?\C-e] . end)
     ([?\M-v] . prior)
     ([?\C-v] . next)))

  ;; Resize window
  (defun eh-exwm/xor (b1 b2)
    (or (and b1 b2)
        (and (not b1) (not b2))))

  (defun eh-exwm/move-border-left-or-right (arg dir)
    "General function covering eh-exwm/move-border-left and eh-exwm/move-border-right.
If DIR is t, then move left, otherwise move right."
    (interactive)
    (when (null arg)
      (setq arg 3))
    (let ((left-edge (nth 0 (window-edges))))
      (if (eh-exwm/xor (= left-edge 0) dir)
          (shrink-window arg t)
        (enlarge-window arg t))))

  (defun eh-exwm/move-border-up-or-down (arg dir)
    "General function covering eh-exwm/move-border-up and eh-exwm/move-border-down.
If DIR is t, then move up, otherwise move down."
    (interactive)
    (when (null arg)
      (setq arg 3))
    (let ((top-edge (nth 1 (window-edges))))
      (if (eh-exwm/xor (= top-edge 0) dir)
          (shrink-window arg nil)
        (enlarge-window arg nil))))

  (defun eh-exwm/move-border-left (arg)
    (interactive "P")
    (eh-exwm/move-border-left-or-right arg t))

  (defun eh-exwm/move-border-right (arg)
    (interactive "P")
    (eh-exwm/move-border-left-or-right arg nil))

  (defun eh-exwm/move-border-up (arg)
    (interactive "P")
    (eh-exwm/move-border-up-or-down arg t))

  (defun eh-exwm/move-border-down (arg)
    (interactive "P")
    (eh-exwm/move-border-up-or-down arg nil))

  (defvar eh-exwm/iresize-mode-map
    (let ((m (make-sparse-keymap)))
      (define-key m (kbd "C-p") 'eh-exwm/move-border-up)
      (define-key m (kbd "C-n") 'eh-exwm/move-border-down)
      (define-key m (kbd "C-f") 'eh-exwm/move-border-right)
      (define-key m (kbd "C-b") 'eh-exwm/move-border-left)
      (define-key m (kbd "C-c C-c") 'eh-exwm/iresize-mode)
      m))

  (define-minor-mode eh-exwm/iresize-mode
    :initial-value nil
    :lighter " Exwm-iresize"
    :keymap eh-exwm/iresize-mode-map
    :group 'eh-exwm/iresize)

  (exwm-input-set-key (kbd "C-S-<up>") 'eh-exwm/move-border-up)
  (exwm-input-set-key (kbd "C-S-<down>") 'eh-exwm/move-border-down)
  (exwm-input-set-key (kbd "C-S-<left>") 'eh-exwm/move-border-left)
  (exwm-input-set-key (kbd "C-S-<right>") 'eh-exwm/move-border-right)

  ;; Generate debian menu
  (exwm-generate-debian-menu-commands)

  ;; Don't delete it
  (exwm-enable)

  (use-package windmove
    :ensure nil
    :config
    (exwm-input-set-key (kbd "C-<up>") 'windmove-up)
    (exwm-input-set-key (kbd "C-<down>") 'windmove-down)
    (exwm-input-set-key (kbd "C-<left>") 'windmove-left)
    (exwm-input-set-key (kbd "C-<right>") 'windmove-right))

  (use-package start-menu
    :ensure nil
    :config
    (start-menu-enable)
    (exwm-input-set-key (kbd "C-t ,")  'start-menu-popup))

  (use-package exwm-systemtray
    :ensure nil
    :config
    (setq exwm-systemtray-height 16)
    (exwm-systemtray-enable)
    (add-hook 'exwm-init-hook 'eh-exwm/network-manager-applet t)
    (add-hook 'exwm-init-hook 'eh-exwm/volit t)
    (add-hook 'exwm-init-hook 'eh-exwm/power-manager t))

  (use-package exim
    :ensure nil
    :config (add-hook 'exwm-init-hook 'exim-start)))

(provide 'eh-exwm)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-exwm.el ends here
