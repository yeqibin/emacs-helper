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

;; sqsh 是 isql 的增强版：
;; sqsh -s SERV -U user -P passwd -D db -L bcp_colsep=','
;; 1> select * from dbo.tzd_brly
;; 1> go
(setq sql-sybase-program "sqsh")
(setq sql-sybase-options '("-w" "80" "-m" "pretty"))

;; sqsh 的 prompt 类似 "1>" 或者 "2>"， 所以需要重新定义 `:prompt-regexp'
;; 值得注意的是： regexp 中最后的 *空格* 绝对不能少，否则 prompt 就会消失，
;; 这个问题困扰我好长时间，最后慢慢调试代码后才发现问题，
;; 具体细节可以看：`sql-interactive-remove-continuation-prompt' 这个函数的定义。
(sql-set-product-feature 'sybase :prompt-regexp "^[0-9]+> ")

;; sqsh 的输出有问题，默认输出的列特别的宽，看起来很痛苦，pretty style 的显示
;; 稍微好一点，但它添加了太多的分割，感觉很眼花，这里添加一个过滤功能，将不必要的
;; 分割去掉。
(defun eh-sql-pretty-output (orig-fun oline)
  (eh-sql-clean-sqsh-pretty-output
   (funcall orig-fun oline)))

(defun eh-sql-clean-sqsh-pretty-output (output)
  (replace-regexp-in-string
   "=" "-"
   (replace-regexp-in-string
    "^+[-+]+\n|" "|" output)))

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
  ;; Create buffer `eh-sql-all-columns' which contain
  ;; all column-names.
  (sql-redirect sql-buffer
                '("select distinct name from sys.columns" "go")
                "eh-sql-all-columns")
  ;; Create buffer `eh-sql-all-columns' which contain
  ;; all table-names.
  (sql-redirect sql-buffer
                '("select distinct name from sys.objects" "go")
                "eh-sql-all-tables"))

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
  (setq sql-product 'sybase)
  (let ((connect-name
         (completing-read "Which database do you want to connect: "
                          (mapcar #'(lambda (x)
                                      (symbol-name (car x)))
                                  sql-connection-alist))))
    (sql-connect (intern connect-name))))

(provide 'eh-sql)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-sql.el ends here
