;;; eh-lentic.el --- Tumashu's basic emacs configuation

;; Copyright (c) 2011 2015, Feng Shu

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

(use-package lentic
  :ensure nil
  :config

  (use-package lentic-org
    :ensure nil
    :bind (("C-c j" . eh-lentic-switch-window))
    :config

    (defclass lentic-org2el-configuration
      (lentic-unmatched-chunk-configuration
       lentic-uncommented-chunk-configuration)
      ())

    (defmethod lentic-clone
      ((conf lentic-org2el-configuration)
       &optional start stop length-before
       start-converted stop-converted)
      (let ((clone-return (call-next-method conf)))
        ;; replace all ';; ;;; '  to ';;; '.
        (m-buffer-replace-match
         (m-buffer-match
          (lentic-that conf)
          ";; ;;; ")
         ";;; ")
        clone-return))

    (defmethod lentic-invert
      ((conf lentic-org2el-configuration))
      (lentic-m-oset
       (lentic-el2org-init)
       :that-buffer
       (lentic-this conf)))

;;;###autoload
    (defun lentic-org2el-init ()
      (lentic-org-oset
       (lentic-org2el-configuration
        "lb-org2el"
        :lentic-file
        (concat
         (file-name-sans-extension
          (buffer-file-name))
         ".el"))))

    (add-to-list 'lentic-init-functions
                 'lentic-org2el-init)

    (defclass lentic-el2org-configuration
      (lentic-unmatched-chunk-configuration
       lentic-commented-chunk-configuration)
      ())

    (defmethod lentic-create
      ((conf lentic-el2org-configuration))
      (let ((buf (call-next-method conf)))
        (with-current-buffer buf
          (show-all)
          ;; After run `show-all', the point move to
          ;; the end of buffer, reupdate the point.
          (lentic-update-point conf))
        buf))

    (defmethod lentic-invert
      ((conf lentic-el2org-configuration))
      (lentic-m-oset
       (lentic-org2el-init)
       :delete-on-exit t
       :that-buffer (lentic-this conf)))

;;;###autoload
    (defun lentic-el2org-init ()
      (lentic-org-oset
       (lentic-el2org-configuration
        "lb-el2org"
        ;; we don't really need a file and could cope without, but org mode assumes
        ;; that the buffer is file name bound when it exports. As it happens, this
        ;; also means that file saving is possible which in turn saves the el file
        :lentic-file
        (concat
         (file-name-sans-extension
          (buffer-file-name))
         ".org"))))

    (add-to-list 'lentic-init-functions
                 'lentic-el2org-init)

    (defun eh-lentic-switch-window ()
      (interactive)
      (when (derived-mode-p 'emacs-lisp-mode)
        ;; Set buffer-local variable `lentic-init'
        (setq lentic-init '(lentic-el2org-init)))
      (lentic-mode-create-from-init)
      (lentic-mode-split-window-below)
      (let ((window
             (get-buffer-window
              (lentic-mode-find-next-visible-lentic-buffer
               (current-buffer)))))
        (when (window-live-p window)
          (select-window window))))

    ))

;;;###autoload(require 'eh-lentic)
(provide 'eh-lentic)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-lentic.el ends here
