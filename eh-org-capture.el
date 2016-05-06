;;; eh-org-capture.el --- Tumashu's emacs configuation

;; Copyright (c) 2011-2013, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/emacs-helper
;; Version: 0.0.1
;; Package-Requires: ((eh-basic "0.0.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  这个文件是tumashu个人专用的emacs配置文件，emacs中文用户可以参考。

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

  (use-package org-capture-pop-frame)

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
:END:"))))

(provide 'eh-org-capture)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-org-capture.el ends here
