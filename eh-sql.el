;;; eh-sql.el --- Tumashu's sql configure

;; Copyright (c) 2011 2012, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/tumashu.github.com
;; Version: 0.0.3
;; Package-Requires: ((starter-kit "2.0.2"))

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
(require 'sql)

(defcustom eh-sql-mssql-program "sqsh"
  "Use sqsh to connect MS sql.

sqsh 是 isql 的增强版，可以连接 sybase 服务器或者 MS sql 服务器，
其登录命令是：

   sqsh -s SERV -U user -P passwd -D db -L bcp_colsep=','

登录成功后，可以运行相关的sql语句，例如：

   1> select * from dbo.tzd_brly
   1> go

Starts `sql-interactive-mode' after doing some setup."
  :type 'file
  :group 'SQL)

(defcustom eh-sql-mssql-options '("-w" "80" "-m" "pretty")
  "List of additional options for `eh-sql-mssql-program'."
  :type '(repeat string)
  :version "20.8"
  :group 'SQL)

(defcustom eh-sql-mssql-login-params '(server user password database)
  "List of login parameters needed to connect to MS sql by sqsh."
  :type 'sql-login-params
  :version "24.1"
  :group 'SQL)

(defalias 'eh-sql-comint-mssql 'sql-comint-sybase
  "由于 sqsh 和 isql 兼容，所以 sql.el 内置的 `sql-sybase' 就可以使用 sqsh，
但出于直观，我还是自定义了一个新的后端：mssql，与内置的ms后端做区别。")

(push '(mssql
        :name "mssql"
        :font-lock sql-mode-ms-font-lock-keywords
        :sqli-program eh-sql-mssql-program
        :sqli-options eh-sql-mssql-options
        :sqli-login eh-sql-mssql-login-params
        :sqli-comint-func eh-sql-comint-mssql
        ;; sqsh 的 prompt 类似 "1>" 或者 "2>"， 所以需要重新定义 `:prompt-regexp'
        ;; 值得注意的是： regexp 中最后的 *空格* 绝对不能少，否则 prompt 就会消失，
        ;; 这个问题困扰我好长时间，最后慢慢调试代码后才发现问题，
        ;; 具体细节可以看：`sql-interactive-remove-continuation-prompt' 这个函数的定义。
        :prompt-regexp "^[0-9]+> "
        :prompt-length 5
        :syntax-alist ((?@ . "_"))
        :terminator ("^go" . "go"))
      sql-product-alist)

(defun eh-sql-mssql (&optional buffer)
  (interactive "P")
  (sql-product-interactive 'mssql buffer))

;; sqsh 的输出有问题，默认输出的列特别的宽，看起来很痛苦，pretty style 的显示
;; 稍微好一点，但它添加了太多的分割，感觉很眼花，这里添加一个过滤功能，将不必要的
;; 分割去掉。
(defun eh-sql-pretty-output (orig-fun oline)
  (let ((output (funcall orig-fun oline)))
    (if (and (boundp 'eh-sql-mssql-program)
             (stringp eh-sql-mssql-program)
             (string-match-p "sqsh$" eh-sql-mssql-program))
        (eh-sql-clean-sqsh-pretty-output output)
      output)))

(defun eh-sql-clean-sqsh-pretty-output (output)
  (with-temp-buffer
    (insert output)
    ;; 删除 pretty 表格的水平分割线
    (goto-char (point-min))
    (while (re-search-forward "^+[-+]+\n|" nil t)
      (replace-match "|" nil t))
    ;; pretty 表格的表头中，使用 "-” 而不是 "=” 做为分割符
    (goto-char (point-min))
    (while (re-search-forward "=" nil t)
      (replace-match "-" nil t))
    ;; sqsh 输出的表格，如果存在中文字符，表格就无法对齐，
    ;; 这里通过添加空格来对齐表格。
    (goto-char (point-min))
    (while (re-search-forward "|[^|]+" nil t)
      (let* ((str (match-string 0))
             (char-list (string-to-list str))
             (count 0))
        (dolist (x char-list)
          (when (string-match-p "\\cc" (char-to-string x))
            (setq count (+ 1 count))))
        (replace-match
         (concat str (make-string count ?\ )))))
    (buffer-string)))

(advice-add 'sql-interactive-remove-continuation-prompt'
            :around #'eh-sql-pretty-output)

;; ** mssql object name completion
;; 1. Output all column-names to a buffer
;; 2. Output all table-names to a buffer
;; 2. Use company-abbrev backend to complete
(defun eh-sql-cache-dabbrev ()
  (interactive)
  (when (null sql-buffer)
    (error "No SQLi buffer available"))
  (let ((str (replace-regexp-in-string
              "[ *]" "" sql-buffer)))
    ;; Create a buffer which contain all column-names.
    (sql-redirect
     sql-buffer
     '("select distinct COLUMN_NAME from information_schema.columns" "go")
     (concat "eh-sql-column-list-" str))

    ;; Create a buffer which contain all table-names.
    (sql-redirect
     sql-buffer
     '("select distinct TABLE_NAME from information_schema.columns" "go")
     (concat "eh-sql-table-list-" str))

    ;; Create a buffer which contain table-names which include schema.
    (sql-redirect
     sql-buffer
     '("select distinct TABLE_SCHEMA + '.' + TABLE_NAME from information_schema.columns" "go")
     (concat "eh-sql-schema-table-list-" str))))

(add-hook 'sql-mode-hook
          #'(lambda ()
              (setq truncate-lines t)
              (sql-highlight-ms-keywords)
              (setq tab-width 2)))

(add-hook 'sql-interactive-mode-hook
          #'(lambda ()
              (setq truncate-lines t)))

(defun eh-sql-connect ()
  "Connect selected database in `sql-connection-alist' with `sql-connect'."
  (interactive)
  (setq sql-product 'mssql)
  (let ((connect-name
         (completing-read "Which database do you want to connect: "
                          (mapcar #'(lambda (x)
                                      (symbol-name (car x)))
                                  sql-connection-alist))))
    (sql-connect (intern connect-name))
    (message "Cache all table-names and column-names...")
    (sit-for 3)
    (eh-sql-cache-dabbrev)
    (message "Table-names and column-names cache finished!")))

(provide 'eh-sql)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-sql.el ends here
