;;; eh-misc.el --- Tumashu's emacs configuation

;; Copyright (c) 2011 2012, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/tumashu.github.com
;; Version: 0.0.1

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

;; eh-website
(use-package org-webpage
  :ensure nil
  :config
  ;; my website's owp config
  (use-package eh-website
    :ensure nil)
  ;; org-webpage's owp config
  (use-package owp-devtools
    :ensure nil)
  (use-package chinese-pyim
    :ensure nil
    :config
    ;; chinese-pyim owp config
    (use-package chinese-pyim-devtools
      :ensure nil)))

;; easy-lentic
(use-package easy-lentic
  :ensure nil
  :config
  (easy-lentic-mode-setup))

;; bbdb
(use-package eh-ebib
  :ensure nil
  :if (not (eq system-type 'windows-nt))
  :bind (("C-c b" . eh-ebib)))

;; sdcv
(use-package chinese-yasdcv
  :ensure nil
  :if (not (eq system-type 'windows-nt))
  :bind (("C-c d" . yasdcv-translate-at-point)))

;; emms
(use-package eh-emms
  :ensure nil)

;; sql
(use-package sql-mssql
  :ensure nil)

;; xah-fly-keys
(use-package xah-fly-keys
  :ensure nil
  :disabled t)

;; elisp setting
(use-package lisp-mode
  :ensure nil
  :config
  (use-package aggressive-indent
    :config
    (defun eh-elisp-setup ()
      ;; 跟踪行尾空格
      (setq show-trailing-whitespace t)
      ;; 高亮TAB
      (setq highlight-tabs t)
      ;; 自动缩进
      (aggressive-indent-mode))
    (add-hook 'emacs-lisp-mode-hook
              #'eh-elisp-setup)))

;; org-journal
(use-package org-journal
  :config
  (use-package org
    :ensure nil)
  (use-package org-agenda
    :ensure nil)
  (setq org-journal-dir "E:/doc/journal/")
  (setq org-journal-file-format "%Y%m%d.org")
  (setq org-agenda-files
        (if (file-directory-p org-journal-dir)
            (append (directory-files org-journal-dir t ".org$")
                    org-agenda-files)
          org-agenda-files))
  :bind
  (("C-c C-j" . org-journal-new-entry)))

;; ESS
(use-package ess
  :config
  (setq ess-eval-visibly-p nil)
  (setq ess-ask-for-ess-directory nil))

;; aggressive-indent
(use-package aggressive-indent)

;; autopair
(use-package autopair
  :config
  (autopair-global-mode 1))

;; visual-regexp
(use-package visual-regexp
  :config
  (use-package visual-regexp-steroids)
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark)))

;; multi-term
(use-package multi-term
  :config
  (setq multi-term-program "/bin/bash")
  (setq multi-term-buffer-name "term")
  (setq term-scroll-show-maximum-output nil)
  (setq term-scroll-to-bottom-on-output nil)
  (setq multi-term-dedicated-select-after-open-p t)
  (setq term-bind-key-alist
        (append '(("C-c C-x" . eh-term-send-ctrl-x)
                  ("C-c C-h" . eh-term-send-ctrl-h))
                term-bind-key-alist))

  (remove-hook 'term-mode-hook 'eh-term-setup)
  (remove-hook 'term-mode-hook 'multi-term-keystroke-setup)
  (remove-hook 'kill-buffer-hook 'multi-term-kill-buffer-hook)

  (add-hook 'term-mode-hook 'eh-term-setup)
  (add-hook 'term-mode-hook 'multi-term-keystroke-setup)
  (add-hook 'kill-buffer-hook 'multi-term-kill-buffer-hook)

  (defun eh-term-setup ()
    (setq truncate-lines t)
    (setq term-buffer-maximum-size 0)
    (setq show-trailing-whitespace nil)
    (multi-term-handle-close))

  (defun eh-term-send-ctrl-x ()
    "Send C-x in term mode."
    (interactive)
    (term-send-raw-string "\C-x"))

  (defun eh-term-send-ctrl-z ()
    "Send C-z in term mode."
    (interactive)
    (term-send-raw-string "\C-z"))

  (defun eh-term-send-ctrl-h ()
    "Send C-h in term mode."
    (interactive)
    (term-send-raw-string "\C-h")))

;; magit
(use-package magit
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind (("C-c g" . magit-status)))

;; wdired and dired-ranger
(use-package dired
  :ensure nil
  :config
  (use-package wdired)
  (use-package dired-ranger))

;; multiple-cursors
(use-package multiple-cursors
  :bind (("C-x C-x m" . mc/edit-lines)
         ("C-c >" . mc/mark-next-like-this)
         ("C-c <" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; ace-jump
(use-package ace-jump-mode
  :init
  (autoload 'ace-jump-mode "ace-jump-mode" nil t)
  (bind-key "C-j" 'ace-jump-mode))

(use-package ace-window
  :disabled t
  :bind (("C-x o" . ace-window))
  :config
  (setq aw-keys '(?a ?b ?c ?d ?e ?f ?g ?h ?i))
  (ace-window-display-mode))

;; switch window
(use-package switch-window
  :bind (("C-x o" . switch-window)
         ("C-x 1" . switch-window-then-maximize)
         ("C-x 2" . switch-window-then-split-below)
         ("C-x 3" . switch-window-then-split-right)
         ("C-x 0" . switch-window-then-delete))
  :config
  (setq switch-window-increase 8)
  (setq switch-window-shortcut-style 'qwerty))

;; expand-region
(use-package expand-region
  :bind (("C-c =" . er/expand-region)))

;; wrap-region
(use-package wrap-region
  :ensure t
  :config
  (wrap-region-add-wrappers
   '(("*" "*" nil org-mode)
     ("~" "~" nil org-mode)
     ("/" "/" nil org-mode)
     ("=" "=" "+" org-mode)
     ("_" "_" nil org-mode)
     ("$" "$" nil (org-mode latex-mode))
     ("\n#+BEGIN_SRC emacs-lisp\n" "\n#+END_SRC\n" "#" org-mode)
     ("\n;; #+BEGIN_SRC emacs-lisp\n" "\n;; #+END_SRC\n" "#" emacs-lisp-mode)))
  (add-hook 'org-mode-hook 'wrap-region-mode)
  (add-hook 'emacs-lisp-mode-hook 'wrap-region-mode)
  (add-hook 'latex-mode-hook 'wrap-region-mode))

;; browse-kill-ring
(use-package browse-kill-ring
  :bind (("C-c y" . eh-browse-kill-ring))
  :config
  (setq browse-kill-ring-highlight-current-entry t)
  (setq browse-kill-ring-separator
        (concat "\n" (make-string 70 ?=) "\n"))
  (add-hook 'browse-kill-ring-hook
            'eh-browse-kill-ring-settings)
  (defun eh-browse-kill-ring-settings ()
    (interactive)
    (setq browse-kill-ring-show-preview nil)
    (define-key browse-kill-ring-mode-map (kbd "C-c C-k") 'browse-kill-ring-quit)
    (define-key browse-kill-ring-mode-map (kbd "C-k") 'browse-kill-ring-quit)
    (define-key browse-kill-ring-mode-map (kbd "k") 'browse-kill-ring-quit)
    (define-key browse-kill-ring-mode-map (kbd "C-/") 'browse-kill-ring-quit)
    (define-key browse-kill-ring-mode-map (kbd "C-n") 'browse-kill-ring-forward)
    (define-key browse-kill-ring-mode-map (kbd "C-p") 'browse-kill-ring-previous)
    (define-key browse-kill-ring-mode-map (kbd "C-c C-c") 'browse-kill-ring-insert-and-quit)
    (define-key browse-kill-ring-mode-map (kbd "y") 'browse-kill-ring-insert-and-quit))

  (defun eh-browse-kill-ring ()
    (interactive)
    (let ((clipboard-output
           (if (string= system-type "windows-nt")
               (w32-get-clipboard-data)
             (x-get-clipboard))))
      (when (and clipboard-output
                 (not (string= (car kill-ring) clipboard-output)))
        (kill-new clipboard-output))
      (if (car kill-ring)
          (browse-kill-ring)
        (message "kill ring is empty")))))

;; General project support
(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (use-package wgrep
    :config
    (projectile-global-mode 1)
    (setq projectile-enable-caching nil)
    :bind
    (("C-x F" . projectile-find-file)
     ("C-S-s" . projectile-grep))))

;; ebib-handy
(use-package ebib-handy
  :config (ebib-handy-enable))

;; undo tree
(use-package undo-tree
  :bind (("C-c /" . undo-tree-visualize))
  :config
  (global-undo-tree-mode)
  (add-hook 'undo-tree-visualizer-mode-hook
            'eh-undo-tree-visualizer-settings)
  (defun eh-undo-tree-visualizer-settings ()
    (interactive)
    (define-key undo-tree-visualizer-mode-map (kbd "C-c C-k") 'undo-tree-visualizer-quit)
    (define-key undo-tree-visualizer-mode-map (kbd "C-k") 'undo-tree-visualizer-quit)
    (define-key undo-tree-visualizer-mode-map (kbd "k") 'undo-tree-visualizer-quit)
    (define-key undo-tree-visualizer-mode-map (kbd "C-g") 'undo-tree-visualizer-abort)))

;; calfw
(use-package holidays
  :ensure nil
  :config
  (defvar eh-calendar-holidays nil)
  (setq eh-calendar-holidays
        '(;;公历节日
          (holiday-fixed 1 1 "元旦")
          (holiday-fixed 2 14 "情人节")
          (holiday-fixed 3 8 "妇女节")
          (holiday-fixed 3 14 "白色情人节")
          (holiday-fixed 4 1 "愚人节")
          (holiday-fixed 5 1 "劳动节")
          (holiday-fixed 5 4 "青年节")
          (holiday-float 5 0 2 "母亲节")
          (holiday-fixed 6 1 "儿童节")
          (holiday-float 6 0 3 "父亲节")
          (holiday-fixed 9 10 "教师节")
          (holiday-fixed 10 1 "国庆节")
          (holiday-fixed 12 25 "圣诞节")
          ;; 农历节日
          (holiday-lunar 1 1 "春节" 0)
          (holiday-lunar 1 2 "春节" 0)
          (holiday-lunar 1 3 "春节" 0)
          (holiday-lunar 1 15 "元宵节" 0)
          (holiday-solar-term "清明" "清明节")
          (holiday-solar-term "小寒" "小寒" )
          (holiday-solar-term "大寒" "大寒" )
          (holiday-solar-term "立春" "立春" )
          (holiday-solar-term "雨水" "雨水" )
          (holiday-solar-term "惊蛰" "惊蛰" )
          (holiday-solar-term "春分" "春分" )
          (holiday-solar-term "谷雨" "谷雨" )
          (holiday-solar-term "立夏" "立夏" )
          (holiday-solar-term "小满" "小满" )
          (holiday-solar-term "芒种" "芒种" )
          (holiday-solar-term "夏至" "夏至" )
          (holiday-solar-term "小暑" "小暑" )
          (holiday-solar-term "大暑" "大暑" )
          (holiday-solar-term "立秋" "立秋" )
          (holiday-solar-term "处暑" "处暑" )
          (holiday-solar-term "白露" "白露" )
          (holiday-solar-term "秋分" "秋分" )
          (holiday-solar-term "寒露" "寒露" )
          (holiday-solar-term "霜降" "霜降" )
          (holiday-solar-term "立冬" "立冬" )
          (holiday-solar-term "小雪" "小雪" )
          (holiday-solar-term "大雪" "大雪" )
          (holiday-solar-term "冬至" "冬至" )
          (holiday-lunar 5 5 "端午节" 0)
          (holiday-lunar 8 15 "中秋节" 0)
          (holiday-lunar 7 7 "七夕情人节" 0)
          (holiday-lunar 12 8 "腊八节" 0)
          (holiday-lunar 9 9 "重阳节" 0)
          (holiday-lunar 12 22 "冬至" 0)))

  (setq calendar-holidays eh-calendar-holidays))

(use-package calendar
  :ensure nil
  :config
  (setq calendar-month-name-array
        ["一月" "二月" "三月" "四月" "五月" "六月"
         "七月" "八月" "九月" "十月" "十一月" "十二月"])
  (setq calendar-day-name-array
        ["星期日" "星期一" "星期二" "星期三" "星期四" "星期五" "星期六"])

  ;; 一周第一天，0表示星期天, 1表示星期一
  (setq calendar-week-start-day 0))

(use-package calfw
  :config

  (use-package calfw-cal :ensure calfw)
  (use-package calfw-ical :ensure calfw)
  (use-package calfw-org :ensure calfw)
  (use-package cal-china-x)

  (use-package org-capture
    :ensure org
    :config
    ;; 为calfw设置一个capture模板并添加到org-capture-templates
    (setq cfw:org-capture-template
          '("calfw2org" "calfw2org" entry (file+headline eh-org-schedule-file "Schedule")
            "* %?\n %(cfw:org-capture-day)\n %a"))
    (setq org-capture-templates
          (append org-capture-templates (list cfw:org-capture-template))))

  ;; 日历表格边框设置
  (setq cfw:fchar-junction ?+
        cfw:fchar-vertical-line ?|
        cfw:fchar-horizontal-line ?-
        cfw:fchar-left-junction ?+
        cfw:fchar-right-junction ?+
        cfw:fchar-top-junction ?+
        cfw:fchar-top-left-corner ?+
        cfw:fchar-top-right-corner ?+ )

  (defun eh-cfw-render-toolbar (width current-view prev-cmd next-cmd)
    "Translate words: 'Month', 'Week', 'Day' and 'Two day' to Chinese"
    (let* ((prev (cfw:render-button " < " prev-cmd))
           (today (cfw:render-button "今天" 'cfw:navi-goto-today-command))
           (next (cfw:render-button " > " next-cmd))
           (month (cfw:render-button
                   "显示一月" 'cfw:change-view-month
                   (eq current-view 'month)))
           (tweek (cfw:render-button
                   "显示两周" 'cfw:change-view-two-weeks
                   (eq current-view 'two-weeks)))
           (week (cfw:render-button
                  "显示一周" 'cfw:change-view-week
                  (eq current-view 'week)))
           (day (cfw:render-button
                 "显示一天" 'cfw:change-view-day
                 (eq current-view 'day)))
           (sp  " ")
           (toolbar-text
            (cfw:render-add-right
             width (concat sp prev sp next sp today sp)
             (concat day sp week sp tweek sp month sp))))
      (cfw:render-default-content-face toolbar-text 'cfw:face-toolbar)))

  (advice-add 'cfw:render-toolbar :override #'eh-cfw-render-toolbar)

  (defun eh-calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :view 'month
     :contents-sources
     (list
      ;; orgmode source
      (cfw:org-create-source "Green")))))

;;;autoload (require 'eh-misc)
(provide 'eh-misc)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-misc.el ends here
