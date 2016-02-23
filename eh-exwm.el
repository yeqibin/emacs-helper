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
;; #+END_EXAMPLE

;; # Launch exwm
;; exec dbus-launch --exit-with-session emacs

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

  ;; All buffers created in EXWM mode are named "*EXWM*". You may want to change
  ;; it in `exwm-update-class-hook' and `exwm-update-title-hook', which are run
  ;; when a new window class name or title is available.
  (add-hook 'exwm-update-class-hook #'eh-exwm/rename-buffer)
  (add-hook 'exwm-update-title-hook #'eh-exwm/rename-buffer)

  (defun eh-exwm/rename-buffer ()
    (exwm-workspace-rename-buffer
     (format "EXWM@%s@%s@%s"
             (or exwm-title "")
             (or exwm-instance-name "")
             (or exwm-class-name ""))))

  (defun eh-exwm/jump-or-exec (regexp cmd)
    "Jump to a window which buffer's name matched `regexp',
if matched window can't be found, run shell command `cmd',
this command must work with `eh-exwm/rename-buffer'."
    (let* ((buffers (buffer-list))
           buffers-matched-title
           buffers-matched-instance
           buffers-matched-class
           buffers-alist buffer buffer-window)

      (dolist (buffer buffers)
        (let* ((buffer-name (buffer-name buffer))
               (list (split-string buffer-name "@")))
          (when (string-match-p regexp buffer-name)
            (when (string-match-p regexp (nth 1 list))
              (push buffer buffers-matched-title))
            (when (string-match-p regexp (nth 2 list))
              (push buffer buffers-matched-instance))
            (when (string-match-p regexp (nth 3 list))
              (push buffer buffers-matched-class)))))

      (dolist (x (list buffers-matched-class
                       buffers-matched-instance
                       buffers-matched-title))
        (when x
          (push (list (length x) x) buffers-alist)))

      (setq buffer
            (car (car (cdr (car (sort buffers-alist
                                      #'(lambda (a b)
                                          (< (car a) (car b)))))))))
      (setq buffer-window
            (when buffer
              (get-buffer-window buffer)))

      (if buffer
          (if buffer-window
              (select-window buffer-window)
            (switch-to-buffer buffer))
        (start-process-shell-command cmd nil cmd))))

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
    (eh-exwm/jump-or-exec "Iceweasel" "iceweasel"))

  (defun eh-exwm/file-manager ()
    (interactive)
    (eh-exwm/jump-or-exec "Nautilus" "nautilus --no-desktop"))

  (defun eh-exwm/crossover ()
    (interactive)
    (eh-exwm/jump-or-exec "Crossover" "/opt/cxoffice/bin/crossover"))

  (defun eh-exwm/launch-crossover-app (app bottle)
    (eh-exwm/jump-or-exec
     app
     (format "/opt/cxoffice/bin/wine --bottle %s --cx-app '%s'" bottle app)))

  (defun eh-exwm/qq ()
    (interactive)
    (eh-exwm/launch-crossover-app "TM.exe" "腾讯_TM_2013"))

  (defun eh-exwm/word ()
    (interactive)
    (eh-exwm/launch-crossover-app "WINWORD.EXE" "Microsoft_Office_2007"))

  (defun eh-exwm/excel ()
    (interactive)
    (eh-exwm/launch-crossover-app "EXCEL.EXE" "Microsoft_Office_2007"))

  (defun eh-exwm/ppt ()
    (interactive)
    (eh-exwm/launch-crossover-app "POWERPNT.EXE" "Microsoft_Office_2007"))

  (defun eh-exwm/winxp ()
    (interactive)
    (eh-exwm/jump-or-exec "VirtualBox" "VBoxManage startvm winxp"))

  (defun eh-exwm/mplayer ()
    (interactive)
    (eh-exwm/jump-or-exec "Smplayer" "smplayer"))

  (defun eh-exwm/htop ()
    (interactive)
    (eh-exwm/jump-or-exec "htop" "x-terminal-emulator -T htop -t htop -e htop"))

  (defun eh-exwm/x-terminal-emulator ()
    (interactive)
    (eh-exwm/jump-or-exec "default-terminal" "x-terminal-emulator -T default-terminal -t default-terminal"))

  (defun eh-exwm/launch-new-terminal ()
    (interactive)
    (eh-exwm/run-shell-command "x-terminal-emulator"))

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

  ;; Generate debian menu
  (exwm-generate-debian-menu-commands)

  ;; Don't delete it
  (exwm-enable)

  (use-package start-menu
    :ensure nil
    :config
    (start-menu-enable)
    (setq exwm-systemtray-height 16)
    (exwm-input-set-key (kbd "C-t ,")  'start-menu-popup)
    (add-hook 'exwm-init-hook 'eh-exwm/network-manager-applet t)
    (add-hook 'exwm-init-hook 'eh-exwm/volit t)
    (add-hook 'exwm-init-hook 'eh-exwm/power-manager t))

  (use-package exwm-systemtray
    :ensure nil
    :config (exwm-systemtray-enable))

  (use-package exim
    :ensure nil
    :config (add-hook 'exwm-init-hook 'exim-start)))

(provide 'eh-exwm)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-exwm.el ends here
