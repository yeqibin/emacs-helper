;;; eh-gnus-common.el --- Tumashu's gnus configuation file

;; Copyright (c) 2008-2009, Andy Stewart
;;               2011-2012, Feng Shu

;; Author: Andy Stewartf <lazycat.manatee@gmail.com>
;;         Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/tumashu.github.com
;; Version: 0.0.6
;; Keywords: gnus

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  这个文件是tumashu个人专用的gnus配置文件，中文gnus用户可以参考。

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

;; require
(use-package gnus
  :ensure nil
  :config

  (use-package mm-encode
    :ensure nil)
  (use-package mm-decode
    :ensure nil)
  (use-package rfc2047
    :ensure nil)
  (use-package nnir
    :ensure nil)
  (use-package gnus-demon
    :ensure nil)
  (use-package eww
    :ensure nil)

  ;; 邮件分类设置
  (setq nnmail-treat-duplicates 'delete
        nnmail-split-fancy-match-partial-words t
        nnmail-mail-splitting-decodes t
        nnmail-mail-splitting-charset 'utf-8)

  ;; 存储设置
  (setq gnus-startup-file "~/Gnus/.newsrc")                  ;初始文件
  (setq gnus-init-file "~/Gnus/.gnus")                       ;.gnus位置
  (setq gnus-default-directory nil)                          ;默认目录
  (setq gnus-home-directory "~/")                            ;主目录
  (setq gnus-dribble-directory "~/Gnus/")                    ;恢复目录
  (setq gnus-directory "~/Gnus/News/")                       ;新闻组的存储目录
  (setq gnus-article-save-directory "~/Gnus/News/")          ;文章保存目录
  (setq gnus-kill-files-directory "~/Gnus/News/trash/")      ;文件删除目录
  (setq gnus-agent-directory "~/Gnus/News/agent/")           ;代理目录
  (setq gnus-cache-directory "~/Gnus/News/cache/")           ;缓存目录
  (setq gnus-cache-active-file "~/Gnus/News/cache/active")   ;缓存激活文件
  (setq message-directory "~/Gnus/Mail/")                    ;邮件的存储目录
  (setq message-auto-save-directory "~/Gnus/Mail/drafts")    ;自动保存的目录
  (setq mail-source-directory "~/Gnus/Mail/incoming")        ;邮件的源目录
  (setq nnmail-message-id-cache-file "~/Gnus/.nnmail-cache") ;nnmail的消息ID缓存
  (setq nnml-newsgroups-file "~/Gnus/Mail/newsgroup")        ;邮件新闻组解释文件
  (setq nntp-marks-directory "~/Gnus/News/marks")            ;nntp组存储目录
  (setq mml-default-directory "~/")                          ;附件的存储位置

  ;; 设置桌面提醒功能
  (setq gnus-notifications-minimum-level 1)
  (setq gnus-notifications-use-google-contacts nil)
  (setq gnus-notifications-use-gravatar nil)

  ;; 默认禁用nnfolder
  (setq gnus-message-archive-group nil)

  ;; 设置message-mode发信的方式，这里默认使用/usr/sbin/sendmail.
  ;; 在 `gnus-posting-styles' 中设置 "X-Message-SMTP-Method" 邮件头可以实现
  ;; 更为复杂的邮件发送方式。
  (setq message-send-mail-function 'message-smtpmail-send-it)

  ;; 设置gnus默认编码: 如果常与国外联系，可以设置为utf-8
  ;; 如果只在本国使用，可以设置为本地编码，比如: gbk
  (setq gnus-default-charset 'gbk)

  ;; 根据method来确定编码
  (setq gnus-group-name-charset-method-alist
        '(((nntp "news.newsfan.net") . gbk)
          ((nntp "news.cn99.com") . gbk)))

  ;; 根据组名称来确定组名称解析使用的编码
  (setq gnus-group-name-charset-group-alist
        '((".*" . gbk)))

  ;; 确定组默认使用的编码。
  (setq gnus-group-charset-alist
        '((".*" . gbk)))

  ;; 如果还有乱码，手动调整
  (setq gnus-summary-show-article-charset-alist
        '((1 . gbk)
          (2 . utf-8)
          (3 . big5)
          (4 . utf-7)))

  ;; 邮件MIME类型设置不正确时，gnus的处理方式。
  (setq gnus-newsgroup-ignored-charsets
        '(unknown-8bit x-unknown x-gbk))

  ;; 设置邮件附件文件名的编码方式以及邮件subject的编码方式
  (defalias 'mail-header-encode-parameter 'rfc2047-encode-parameter)
  (add-to-list 'rfc2047-charset-encoding-alist '(gbk . B))
  (add-to-list 'rfc2047-charset-encoding-alist '(gb18030 . B))

  ;; 常规设置
  (setq gnus-agent t)                                 ; 开启agent
  (setq read-mail-command 'gnus)                      ; 使用gnus阅读邮件
  (setq mail-user-agent 'gnus-user-agent)             ; 使用gnus发送邮件
  (setq gnus-inhibit-startup-message t)               ; 关闭启动时的画面
  (setq gnus-novice-user nil)                         ; 关闭新手设置, 不进行确认
  (setq gnus-expert-user t)                           ; 不询问用户
  (setq gnus-show-threads t)                          ; 显示邮件threads
  (setq gnus-interactive-exit t)                      ; 退出时进行交互式询问
  (setq gnus-use-dribble-file t)                      ; 创建恢复文件
  (setq gnus-always-read-dribble-file t)              ; 读取恢复文件
  (setq gnus-asynchronous t)                          ; 异步操作
  (setq gnus-large-newsgroup 2000)                    ; 设置大容量的新闻组默认显示的大小
  (setq gnus-large-ephemeral-newsgroup nil)           ; 设置临时新闻组默认显示的大小
  (setq gnus-read-active-file 'some)
  (setq gnus-nov-is-evil nil)
  (setq gnus-summary-ignore-duplicates t)             ; 忽略具有相同ID的消息
  (setq gnus-treat-fill-long-lines t)                 ; 自动折行
  (setq message-confirm-send t)                       ; 发邮件前需要确认（防止误发）
  (setq message-kill-buffer-on-exit t)                ; 发送邮件后删除buffer
  (setq message-from-style 'angles)                   ; `From' 头的显示风格
  (setq message-syntax-checks '((sender . disabled))) ; 语法检查
  (setq nnmail-expiry-wait 7)                         ; 邮件自动删除的期限 (单位: 天)
  (setq nnmairix-allowfast-default t)                 ; 加快进入搜索结果的组
  (setq gnus-use-correct-string-widths t)             ; 使用正确的字符宽度
  (setq gc-cons-threshold 3500000)                    ; 加快gnus的速度
  (setq gnus-use-cross-reference t)                   ; 开启交叉索引
  (setq gnus-summary-display-while-building 50)       ; 生成summary时,每50封显示一下

  ;; 进入summer模式时，禁止自动选择第一个article,
  ;; 这样设置主要是因为有些article下载速度极慢，
  ;; 会降低响应速度
  (setq gnus-auto-select-first nil)
  (setq gnus-auto-select-next nil)

  ;; 设置gnus启动时,组级别大于3的不自动更新。
  ;; 当你添加了许多速度慢的组时，比如rss,imap等，启动速度会相当慢。这时你
  ;; 可以把它们的组级别设置为大于3的值，这样启动时就不自动更新了。
  ;; 当你需要更新这些组的时候，使用 "4-g" "5-g" 等快捷键
  (setq gnus-activate-level 3)

  ;; 双窗口布局(垂直)
  ;; (gnus-add-configuration
  ;;  '(article
  ;;    (horizontal 1.0
  ;;         (summary 0.50 point)
  ;;         (article 1.0))))

  ;; 双窗口布局(水平)
  (gnus-add-configuration
   '(article
     (vertical 1.0
               (summary 0.25 point)
               (article 1.0))))

  ;; 三窗口布局
  ;; (gnus-add-configuration
  ;;  '(article
  ;;    (horizontal 1.0
  ;;         (vertical 25
  ;;           (group 1.0))
  ;;         (vertical 1.0
  ;;           (summary 0.25 point)
  ;;           (article 1.0)))))
  ;; (gnus-add-configuration
  ;;  '(summary
  ;;    (horizontal 1.0
  ;;         (vertical 25
  ;;           (group 1.0))
  ;;         (vertical 1.0
  ;;           (summary 1.0 point)))))

  ;; 设置图片显示方式
  (setq mm-inline-large-images t)
  (add-to-list 'mm-attachment-override-types "image/*")

  ;; 设置summary缓冲区的显示格式
  (setq gnus-extra-headers
        '(To From))
  (setq nnmail-extra-headers gnus-extra-headers)
  (setq gnus-summary-gather-subject-limit 'fuzzy)
  (setq gnus-summary-make-false-root 'adopt)
  (setq gnus-summary-line-format
        (concat
         "%U%R |"
         "%ua"
         "%2{%ub%}"
         "%uc"
         "%B"
         "%I"
         "%2{%ud%}"
         "%ue"
         "%3{%uf%}"
         "\n"))

  (defun eh-gnus-find-invisible-foreground ()
    (let ((candidates
           (remove
            "unspecified-bg"
            (nconc
             (list (face-background 'default))
             (mapcar
              (lambda (alist)
                (when (boundp alist)
                  (cdr (assoc 'background-color (symbol-value alist)))))
              '(default-frame-alist initial-frame-alist
                 window-system-default-frame-alist))
             (list (face-foreground 'eh-gnus-face-3))))))
      (car (remove nil candidates))))

  (defface eh-gnus-face-2
    '((t :inherit default))
    "Face used by `gnus-face-2'")

  (defface eh-gnus-face-3
    '((t :inherit default))
    "Face used by `gnus-face-3'")

  (setq gnus-face-2 'eh-gnus-face-2)
  (setq gnus-face-3 'eh-gnus-face-3)

  ;; 显示箭头设置
  (defun gnus-user-format-function-a (header)
    (let ((date (mail-header-date header)))
      (if (zerop gnus-tmp-level)
          "-> " "")))

  ;; 显示时间设置
  (defun gnus-user-format-function-b (header)
    (let ((date (mail-header-date header)))
      (if (zerop gnus-tmp-level)
          "" (concat "     " (concat (gnus-user-date date) "  ")))))

  ;; 显示主题设置
  (defun gnus-user-format-function-c (header)
    (let ((date (mail-header-date header))
          (subject (mail-header-subject header)))
      (if (zerop gnus-tmp-level)
          (concat subject
                  " ("
                  (gnus-user-date date)")") "")))

  ;; 提取From名字
  (defun eh-mail-header-from-name (from)
    (cond
     ((string-match "<[^>]+> *$" from)
      (let ((beg (match-beginning 0)))
        (or (and (string-match "^\".+\"" from)
                 (substring from 1 (1- (match-end 0))))
            (substring from 0 beg))))
     ((string-match "(.+)" from)
      (substring from
                 (1+ (match-beginning 0)) (1- (match-end 0))))
     (t from)))

  ;; 显示发件人设置
  (defun gnus-user-format-function-d (header)
    (let ((from (mail-header-from header)))
      (if (zerop gnus-tmp-level)
          "" (eh-mail-header-from-name from))))

  ;; 显示箭头设置
  (defun gnus-user-format-function-e (header)
    (if (zerop gnus-tmp-level)
        "" "----> "))

  ;; 显示隐藏Subject, 用于搜索
  (defun gnus-user-format-function-f (header)
    (let ((date (mail-header-date header))
          (subject (mail-header-subject header)))
      (if (zerop gnus-tmp-level)
          ""
        subject)))

  (setq gnus-user-date-format-alist
        '(((gnus-seconds-today) . "%H:%M")
          ((+ (* 24 3600)    (gnus-seconds-today)) . "YD   ")
          ((- (gnus-seconds-month) (* 72 3600)) . "%dth ")
          ((- (gnus-seconds-month) (* 48 3600)) . "%drd ")
          ((- (gnus-seconds-month) (* 24 3600)) . "%dnd ")
          ((gnus-seconds-month) . "%dst ")
          ((gnus-seconds-year)  . "%m-%d")
          (t . "%Y ")))

  ;; 设置threads的样式
  (setq gnus-thread-indent-level 0)
  (setq gnus-summary-same-subject "")
  (setq gnus-sum-thread-tree-indent "    ")
  (setq gnus-sum-thread-tree-single-indent "")
  (setq gnus-sum-thread-tree-root "")
  (setq gnus-sum-thread-tree-false-root "")
  (setq gnus-sum-thread-tree-vertical "|")
  (setq gnus-sum-thread-tree-leaf-with-other "|----")
  (setq gnus-sum-thread-tree-single-leaf " `----")

  ;; 高亮当前行
  (defface eh-gnus-hl-line
    '((t :inherit hl-line))
    "Face used for highlight gnus line")

  (defun eh-gnus-hl-line ()
    (hl-line-mode 1)
    (set (make-local-variable 'hl-line-face) 'eh-gnus-hl-line)
    (setq cursor-type nil))

  (add-hook 'gnus-summary-mode-hook 'eh-gnus-hl-line)
  (add-hook 'gnus-group-mode-hook 'eh-gnus-hl-line)

  ;; 将邮件的发出时间转换为本地时间
  (add-hook 'gnus-article-prepare-hook 'gnus-article-date-local)

  ;; 跟踪组的时间轴
  (add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

  ;; 将新闻组分组
  ;; (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

  (defun eh-gnus-summary-setup ()
    (interactive)
    ;; summary buffer行距设置
    (setq line-spacing 3)

    ;; summary线程中日期和发邮人名字使用的face。
    (set-face-foreground 'eh-gnus-face-2 "orange")

    ;; 设置一个face,用来隐藏不需要显示的文字
    (set-face-foreground 'eh-gnus-face-3
                         (eh-gnus-find-invisible-foreground))

    ;; summary buffer不显示右fringe
    (set-fringe-style  '(nil . 0))

    ;; 重新定义键盘绑定
    (local-set-key (kbd "SPC")
                   (lambda ()
                     (interactive)
                     (gnus-summary-next-page)
                     (move-beginning-of-line 1)))
    (local-set-key (kbd "C-p")
                   (lambda ()
                     (interactive)
                     (delete-other-windows)
                     (previous-line 1)))
    (local-set-key (kbd "C-n")
                   (lambda ()
                     (interactive)
                     (delete-other-windows)
                     (next-line 1)))
    (local-set-key (kbd "<up>")
                   (lambda ()
                     (interactive)
                     (delete-other-windows)
                     (previous-line 1)))
    (local-set-key (kbd "<down>")
                   (lambda ()
                     (interactive)
                     (delete-other-windows)
                     (next-line 1)))
    (local-set-key (kbd "<return>")
                   (lambda ()
                     (interactive)
                     (eh-gnus-view-article-with-eww)
                     (move-beginning-of-line 1)))
    (local-set-key (kbd "C-<return>")
                   (lambda ()
                     (interactive)
                     (eh-gnus-view-article-with-eww t)
                     (move-beginning-of-line 1)))
    (local-set-key (kbd "<f1>") 'gnus-uu-mark-all)
    (local-set-key (kbd "<f2>") 'gnus-uu-unmark-thread)
    (local-set-key (kbd "<f3>") 'gnus-uu-mark-thread))

  (add-hook 'gnus-summary-mode-hook 'eh-gnus-summary-setup)

  (defun eh-gnus-parse-netrc-file (file)
    "Parse netrc-file `file'."
    (when (file-exists-p file)
      (with-temp-buffer
        (let ((tokens '("machine" "login" "account" "port"
                        "user-full-name" "user-mail-address"))
              alist elem result pair)
          (insert-file-contents file)
          (goto-char (point-min))
          ;; Go through the file, line by line.
          (while (not (eobp))
            (narrow-to-region (point) (point-at-eol))
            ;; For each line, get the tokens and values.
            (while (not (eobp))
              (skip-chars-forward "\t ")
              ;; Skip lines that begin with a "#".
              (if (eq (char-after) ?#)
                  (goto-char (point-max))
                (unless (eobp)
                  (setq elem
                        (if (= (following-char) ?\")
                            (read (current-buffer))
                          (buffer-substring
                           (point) (progn (skip-chars-forward "^\t ")
                                          (point)))))
                  (cond
                   ((member elem tokens)
                    ;; Tokens that don't have a following value are ignored,
                    ;; except "default".
                    (when (and pair (or (cdr pair)
                                        (equal (car pair) "default")))
                      (push pair alist))
                    (setq pair (list elem)))
                   (t
                    ;; Values that haven't got a preceding token are ignored.
                    (when pair
                      (setcdr pair elem)
                      (push pair alist)
                      (setq pair nil)))))))
            (when alist
              (push (nreverse alist) result))
            (setq alist nil
                  pair nil)
            (widen)
            (forward-line 1))
          (nreverse result)))))

  (defun eh-gnus-select-mail-account ()
    (interactive)
    (let* ((netrc-info
            (eh-gnus-parse-netrc-file
             (expand-file-name "~/.authinfo.gpg")))
           (account-used
            (completing-read "Which account will be used? "
                             `("# None"
                               ,@(delq nil
                                       (mapcar
                                        #'(lambda (x)
                                            (cdr (assoc "user-mail-address" x)))
                                        netrc-info))
                               "# Sendmail")))
           (account-info
            (when account-used
              (car (delq nil
                         (mapcar
                          #'(lambda (x)
                              (cond
                               ((and (not (string= account-used "# None"))
                                     (member `("user-mail-address" . ,account-used) x))
                                x)
                               ((string= account-used "# Sendmail")
                                'sendmail)))
                          netrc-info))))))

      (when account-info
        (message-replace-header
         "X-Message-SMTP-Method"
         (if (eq account-info 'sendmail)
             "sendmail"
           (format "smtp %s %s %s"
                   (cdr (assoc "machine" account-info))
                   (cdr (assoc "port" account-info))
                   (cdr (assoc "login" account-info)))) nil t)
        (message-replace-header
         "From"
         (if (eq account-info 'sendmail)
             "localhost"
           (format "\"%s\" <%s>"
                   (cdr (assoc "user-full-name" account-info))
                   (cdr (assoc "user-mail-address" account-info)))) nil t)
        ;; Sort headers
        (let ((message-header-format-alist
               `((To)
                 (Subject)
                 (Cc)
                 (From)
                 (Newsgroups)
                 (In-Reply-To)
                 (Fcc)
                 (Bcc)
                 (Date)
                 (Organization)
                 (Distribution)
                 (Lines)
                 (Expires)
                 (Message-ID)
                 (References . message-shorten-references)
                 (User-Agent))))
          (message-sort-headers)
          ;; Move cursor to "To: " header
          (message-goto-to)))))

  (add-hook 'message-setup-hook 'eh-gnus-select-mail-account)

  ;; visual
  (setq gnus-treat-emphasize t
        gnus-treat-buttonize t
        gnus-treat-buttonize-head 'head
        gnus-treat-unsplit-urls 'last
        gnus-treat-leading-whitespace 'head
        gnus-treat-highlight-citation t
        gnus-treat-highlight-signature t
        gnus-treat-date-lapsed 'head
        gnus-treat-strip-trailing-blank-lines t
        gnus-treat-strip-cr t
        gnus-treat-overstrike nil
        gnus-treat-display-x-face t
        gnus-treat-display-face t
        gnus-treat-display-smileys nil
        gnus-treat-x-pgp-sig 'head)

  ;; 设置邮件报头显示的信息
  (setq gnus-visible-headers
        (mapconcat 'regexp-quote
                   '("From:" "Newsgroups:" "Subject:" "Date:"
                     "Organization:" "To:" "Cc:" "Followup-To" "Gnus-Warnings:"
                     "X-Sent:" "X-URL:" "User-Agent:" "X-Newsreader:"
                     "X-Mailer:" "Reply-To:" "X-Spam:" "X-Spam-Status:" "X-Now-Playing"
                     "X-Attachments" "X-Diagnostic" "X-RSS-URL")
                   "\\|"))

  ;; 设置邮件日期显示格式,使用两行日期，一行具体日期时间，
  ;; 另一行显示article, 距现在多长时间
  (setq gnus-article-date-headers '(user-defined))
  (setq gnus-article-time-format
        (lambda (time)
          (concat "X-Sent:   "
                  (format-time-string "%Y年%m月%d日 星期%u %R" time)
                  "\n"
                  "X-Lasped: "
                  (article-lapsed-string time))))

  ;; 用 Supercite 显示多种多样的引文形式
  (setq sc-attrib-selection-list nil
        sc-auto-fill-region-p nil
        sc-blank-lines-after-headers 1
        sc-citation-delimiter-regexp "[>]+\\|\\(: \\)+"
        sc-cite-blank-lines-p nil
        sc-confirm-always-p nil
        sc-electric-references-p nil
        sc-fixup-whitespace-p t
        sc-nested-citation-p nil
        sc-preferred-header-style 4
        sc-use-only-preference-p nil)

  ;; 使用threads显示文章标题。
  (setq gnus-use-trees t)

  ;; 用最小窗口显示
  (setq gnus-tree-minimize-window nil)

  ;; 构建threads时抓取旧文章标题,
  ;; 注意： 网速不快时不要使用这个选项。
  (setq gnus-fetch-old-headers 'some)

  ;; 生成水平树
  (setq gnus-generate-tree-function
        'gnus-generate-horizontal-tree)

  ;; 聚集threads的方式
  (setq gnus-summary-thread-gathering-function
        'gnus-gather-threads-by-subject)

  ;; Thread root排序
  (setq gnus-thread-sort-functions
        '(gnus-thread-sort-by-most-recent-number
          gnus-thread-sort-by-most-recent-date))

  ;; Subthread排序
  (setq gnus-subthread-sort-functions
        '(gnus-thread-sort-by-number
          gnus-thread-sort-by-date))

  ;; 自动跳到第一个没有阅读的组
  (add-hook 'gnus-switch-on-after-hook
            'gnus-group-first-unread-group)
  (add-hook 'gnus-summary-exit-hook
            'gnus-group-first-unread-group)

  ;; 每隔10分钟刷新一下
  (add-hook 'gnus-startup-hook
            '(lambda ()
               (progn (setq gnus-use-demon t)
                      (gnus-demon-add-handler
                       'gnus-demon-scan-news 10 nil))))
  (use-package org
    :ensure nil
    :config
    ;; 在message-mode中使用org-mode
    (add-hook 'message-mode-hook 'turn-on-orgstruct)
    (add-hook 'message-mode-hook 'turn-on-orgstruct++))

  ;; 启用桌面提醒功能
  (use-package gnus-notifications
    :ensure nil
    :config
    (add-hook 'gnus-after-getting-new-news-hook
              'gnus-notifications)))

(provide 'eh-gnus-common)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-gnus-common.el ends here
