;;; eh-org.el --- Tumashu's org-mode configuation

;; Copyright (c) 2012, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/tumashu.github.com
;; Version: 0.0.2
;; Package-Requires: ((org "7.8.00"))
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
(use-package org
  :ensure nil
  :config

  (use-package ox
    :ensure nil
    :config
    ;; Export language
    (setq org-export-default-language "zh-CN"))

  (use-package ox-html
    :ensure nil
    :config
    ;; html
    (setq org-html-coding-system 'utf-8)
    (setq org-html-head-include-default-style t)
    (setq org-html-head-include-scripts t)

    ;; Chinese hack
    (defun eh-org-clean-space (text backend info)
      "在export为HTML时，删除中文之间不必要的空格"
      (when (org-export-derived-backend-p backend 'html)
        (let ((regexp "[[:multibyte:]]")
              (string text))
          ;; org默认将一个换行符转换为空格，但中文不需要这个空格，删除。
          (setq string
                (replace-regexp-in-string
                 (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp)
                 "\\1\\2" string))
          ;; 删除粗体之前的空格
          (setq string
                (replace-regexp-in-string
                 (format "\\(%s\\) +\\(<\\)" regexp)
                 "\\1\\2" string))
          ;; 删除粗体之后的空格
          (setq string
                (replace-regexp-in-string
                 (format "\\(>\\) +\\(%s\\)" regexp)
                 "\\1\\2" string))
          string)))

    (add-to-list 'org-export-filter-paragraph-functions
                 'eh-org-clean-space))

  (use-package ox-latex
    :ensure nil
    :config
    ;; 不要在latex输出文件中插入\maketitle
    (setq org-latex-title-command "")
    (setq org-latex-date-format "%Y-%m-%d")
    (setq org-export-with-LaTeX-fragments 'imagemagick)
    (setq org-latex-create-formula-image-program 'imagemagick))

  (use-package ox-latex-chinese
    :ensure nil
    :config (oxlc/toggle-ox-latex-chinese t))

  (use-package ox-ascii
    :ensure nil)

  (use-package ox-beamer
    :ensure nil)

  (use-package ox-md
    :ensure nil)

  (use-package ox-deck
    :ensure nil)

  (use-package ox-rss
    :ensure nil)

  (use-package ox-s5
    :ensure nil)

  ;; org-plus-contrib
  (use-package ox-extra
    :ensure nil
    :config
    ;; 如果一个标题包含TAG: “ignore” ,导出latex时直接忽略这个标题，
    ;; 但对它的内容没有影响。
    (ox-extras-activate '(latex-header-blocks ignore-headlines)))

  (use-package org-mime
    :ensure nil)

  (use-package org-bookmark
    :ensure nil)

  (use-package org-protocol
    :ensure nil)

  (use-package org-screenshot
    :ensure nil)

  (use-package ox-bibtex-chinese
    :ensure nil)

  (use-package ebib-handy
    :ensure nil)

  (use-package ob-R
    :ensure nil)

  (use-package ob-plantuml
    :ensure nil
    :config
    (setq org-plantuml-jar-path "~/bin/plantuml.jar"))

  (use-package org-agenda
    :ensure nil
    :config
    (setq org-agenda-files
          (append (file-expand-wildcards "~/org/*.org")))

    (setq org-agenda-custom-commands
          '(("l" "agenda:"
             ((agenda  "" ((org-agenda-overriding-header "Two-Days")
                           (org-agenda-span 2)))
              (tags-todo "生活|IT|购物")))))

    (setq org-agenda-remove-tags t))

  (use-package ob-core
    :ensure  nil
    :config
    (setq org-confirm-babel-evaluate nil))

  (use-package org-capture
    :ensure nil
    :config
    (use-package eh-org-capture
      :ensure nil))

  ;; 自定义变量
  (setq eh-org-mathtoweb-file "~/bin/mathtoweb.jar")

  (setq org-latex-to-mathml-convert-command
        "java -jar %j -unicode -force -df %o %I"
        org-latex-to-mathml-jar-file
        eh-org-mathtoweb-file)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

  (add-to-list 'auto-mode-alist '("\.\(org\|org_archive\)$" . org-mode))
  (setq org-insert-heading-respect-content nil)
  (setq org-log-done t)
  (setq org-startup-indented nil)
  (setq org-edit-src-content-indentation 0)
  (setq org-export-backends
        '(ascii beamer html latex md deck rss s5))

  ;; truncate line depend context
  (defun eh-org-truncate-lines (&optional arg)
    (interactive "P")
    (cond
     ((or (and (boundp 'org-clock-overlays) org-clock-overlays)
          org-occur-highlights)
      (and (boundp 'org-clock-overlays) (org-clock-remove-overlays))
      (org-remove-occur-highlights)
      (org-remove-latex-fragment-image-overlays)
      (message "Temporary highlights/overlays removed from current buffer"))
     (t
      (let* ((context (org-element-context)) (type (org-element-type context)))
        (case type
          ((table table-cell table-row item plain-list)
           (toggle-truncate-lines 1))
          (t (toggle-truncate-lines -1)))))))

  (defun eh-org-ctrl-c-ctrl-c (&optional arg)
    (interactive)
    (eh-org-truncate-lines arg)
    (org-ctrl-c-ctrl-c arg))

  (org-defkey org-mode-map "\C-c\C-c" 'eh-org-ctrl-c-ctrl-c)

  ;; org默认使用"_下标"来定义一个下标，使用"^上标"定义一个上标，
  ;; 但这种方式在中文环境中与下划线冲突。
  ;; 这里强制使用"_{下标}"来定义一个下标。"^{上标}"来定义一个上标。
  (setq org-export-with-sub-superscripts '{})
  (setq org-use-sub-superscripts '{})

  ;; org-bable设置
  (setq org-src-fontify-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     ;; (R . t)
     (org . t)
     (ditaa . nil)
     (dot . nil)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (mscgen . t)
     (latex . t)
     (ocaml . nil)
     (perl . t)
     (python . nil)
     (ruby . nil)
     (screen . nil)
     (shell . t)
     (sql . nil)
     (sqlite . nil)))

  ;; org-babel hook
  (add-hook 'org-babel-after-execute-hook 'eh-org-babel-after-execute-function)

  (defun eh-org-babel-after-execute-function ()
    (when (not org-export-current-backend)
      (org-display-inline-images)
      (eh-org-babel-align-tables)))

  (defun eh-org-babel-align-tables (&optional info)
    "Align all tables in the result of the current source"
    (interactive)
    (let ((location (org-babel-where-is-src-block-result nil info)))
      (when location
        (save-excursion
          (goto-char location)
          (when (looking-at (concat org-babel-result-regexp ".*$"))
            (while (< (point) (progn (forward-line 1) (org-babel-result-end)))
              (when (org-at-table-p)
                (toggle-truncate-lines 1)
                (org-table-align)
                (goto-char (org-table-end)))
              (forward-line)))))))

  ;; 开启cdlatex
  (use-package cdlatex
    :ensure nil
    :config
    (add-hook 'org-mode-hook 'turn-on-org-cdlatex))

  ;; 开启自动断行
  (add-hook 'org-mode-hook #'(lambda ()
                               (setq visual-line-fringe-indicators '(nil nil))
                               (visual-line-mode)
                               (if visual-line-mode
                                   (setq word-wrap nil))))

  ;; export filter
  ;; (defun eh-convert-punctuation (text backend info)
  ;;   "将半角标点符号全部替换为全角标点符号"
  ;;   (when (memq backend '(odt html))
  ;;     (replace-regexp-in-string
  ;;      ";" "；"
  ;;      (replace-regexp-in-string
  ;;       ":" "："
  ;;       (replace-regexp-in-string
  ;;        "·" "。"
  ;;        (replace-regexp-in-string
  ;;	","  "，"
  ;;	(replace-regexp-in-string
  ;;   "\\."  "。"
  ;;   (replace-regexp-in-string "\n" "" text))))))))

  ;; (add-to-list 'org-export-filter-plain-text-functions
  ;;              'eh-convert-punctuation)

  ;; Use Cairo graphics device by default,which can get better graphics quality.
  ;; you shoule add below lines to you ~/.Rprofile
  ;;    require("Cairo")
  ;;    CairoFonts(regular="SimSun:style=Regular",
  ;;             bold="SimHei:style=Regular",
  ;;             italic="KaiTi_GB2312:style=Regular",
  ;;             symbol="Symbol")
  ;;
  ;; (setq org-babel-R-graphics-devices
  ;;   '((:bmp "bmp" "filename")
  ;;     (:jpg "jpeg" "filename")
  ;;     (:jpeg "jpeg" "filename")
  ;;     (:tikz "tikz" "file")
  ;;     (:tiff "tiff" "filename")
  ;;     (:png "CairoPNG" "filename")
  ;;     (:svg "CairoSVG" "file")
  ;;     (:pdf "CairoPDF" "file")
  ;;     (:ps "CairoPS" "file")
  ;;     (:postscript "postscript" "file")))

  ;; Add new easy templates
  (setq org-structure-template-alist
        (append '(("r" "#+BEGIN_SRC R\n?\n#+END_SRC")
                  ("e" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC")
                  ("ex" "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE")
                  ("rh" "#+PROPERTY: header-args:R  :session *R* :tangle yes :colnames yes :rownames no :width 700 :height 500 :exports both")
                  ("rv" "#+BEGIN_SRC R :results value\n?\n#+END_SRC")
                  ("ro" "#+BEGIN_SRC R :results output verbatim\n?\n#+END_SRC")
                  ("rg" "#+BEGIN_SRC R :results graphics :file ?\n\n#+END_SRC")
                  ("rs" "#+BEGIN_SRC R :results output silent\n?\n#+END_SRC")
                  ("rd" "#+BEGIN_SRC R :colnames no :results value drawer\n`%c%` <- function(a,b){c(a,b)}\n?\n#+END_SRC"))
                org-structure-template-alist))

  (defun eh-org-fill-paragraph ()
    "Fill org paragraph"
    (interactive)
    (let ((fill-column 10000000))
      (org-fill-paragraph))))

;;;###autoload(require 'eh-org)
(provide 'eh-org)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-org.el ends here
