;;; eh-org-capture.el --- Tumashu's emacs configuation

;; Copyright (c) 2011-2013, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/emacs-helper
;; Version: 0.0.1
;; Package-Requires: ((eh-basic "0.0.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  这个文件是tumashu个人专用的emacs配置文件，emacs中文用户可以参考。
;;  使用方式：
;;  1. 安装 [[https://github.com/nobuoka/AppLauncher][Firefox AppLauncher 扩展]]
;;  2. 配置 AppLauncher
;;     1. Name: org-capture
;;     2. Path: /home/feng/emacs/bin/emacsclient
;;     3. Args: --socket-name=sawfish-emacs-daemon
;;              org-protocol://capture://f/&eurl;/&etitle;/&etext;
;;     其中, "f" 代表 org-capture 使用 key 为 "f" 的模板

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

;; 自定义变量
(use-package org-capture
  :ensure nil
  :config
  (setq eh-org-todo-file "~/org/i-todo.org")
  (setq eh-org-note-file "~/org/i-notes.org")
  (setq eh-org-contacts-file "~/org/i-contacts.org")
  (setq eh-org-account-file "~/org/i-account.org")
  (setq eh-org-journal-file "~/org/i-journal.org")
  (setq eh-org-schedule-file "~/org/i-schedule.org")

  ;; capture模板
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline eh-org-todo-file "Tasks")
           "* TODO %? \n %i \n %a")

          ("l" "Link" entry (file+olp eh-org-note-file "Web Links")
           "* %a\n %?\n %i")

          ("a" "account" table-line (file+headline eh-org-account-file "Account")
           "|%?||||||%u|")

          ("j" "Journal" entry (file+datetree eh-org-journal-file)
           "* %?\n %U\n %i\n  %a")

          ("s" "Schedule" entry (file+headline eh-org-schedule-file "Schedule")
           "* %?\n %T\n  %a")

          ("n" "Notes" entry  (file+headline eh-org-note-file "Notes")
           "** %?
   :PROPERTIES:
   :DATE: %u
   :END:
"
           :empty-lines 1)
          ("f" "firefox-org-capture" entry  (file+headline eh-org-note-file "Notes-from-web")
           "** %a
%i
%?
"
           :empty-lines 1)

          ("d" "Simple-Notes" entry  (file+headline eh-org-note-file "Simple-Notes")
           "** %?"
           :empty-lines 1)
          ("c" "Contacts" entry (file eh-org-contacts-file)
           "* %?
:PROPERTIES:
:ALIAS:
:PHONE:
:EMAIL:
:NOTE:
:END:")))

  (setq eh-org-capture-frame-name "tumashu-org-capture")

  (defun eh-org-capture-delete-frame (&rest args)
    "Close capture frame"
    (if (equal eh-org-capture-frame-name (frame-parameter nil 'name))
        (delete-frame)))

  (defun eh-org-capture-delete-other-windows (&rest args)
    "Delete the extra window if we're in a capture frame"
    (if (equal eh-org-capture-frame-name (frame-parameter nil 'name))
        (delete-other-windows)))

  (defun eh-org-capture (orig-fun &optional goto keys)
    "Create a new frame and run org-capture."
    (interactive)
    (let ((frame-window-system
           (cond ((eq system-type 'darwin) 'ns)
                 ((eq system-type 'gnu/linux) 'x)
                 ((eq system-type 'windows-nt) 'w32)))
          (after-make-frame-functions
           #'(lambda (frame)
               (progn
                 (select-frame frame)
                 (setq word-wrap nil)
                 (setq truncate-lines nil)
                 (funcall orig-fun goto keys)
                 (setq header-line-format
                       (list "Capture buffer. "
                             (propertize (substitute-command-keys "Finish \\[org-capture-finalize], ")
                                         'mouse-face 'mode-line-highlight
                                         'keymap
                                         (let ((map (make-sparse-keymap)))
                                           (define-key map [header-line mouse-1] 'org-capture-finalize)
                                           map))
                             (propertize (substitute-command-keys "abort \\[org-capture-kill]. ")
                                         'mouse-face 'mode-line-highlight
                                         'keymap
                                         (let ((map (make-sparse-keymap)))
                                           (define-key map [header-line mouse-1] 'org-capture-kill)
                                           map))))))))
      (make-frame `((name . ,eh-org-capture-frame-name)
                    (window-system . ,frame-window-system)
                    (width . 100)
                    (height . 20)
                    (tool-bar-lines . 0)
                    (menu-bar-lines . 1)))))

  (advice-add 'org-capture :around #'eh-org-capture)
  (advice-add 'org-capture-finalize :after #'eh-org-capture-delete-frame)
  (advice-add 'org-switch-to-buffer-other-window :after #'eh-org-capture-delete-other-windows)
  )

(provide 'eh-org-capture)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-org-capture.el ends here
