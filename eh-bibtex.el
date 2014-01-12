;;; eh-bibtex.el --- Tumashu's emacs configuation

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
(require 'bibtex)
(require 'reftex)
(require 'ebib)
(require 'eh-hanzi2pinyin)

;; bibtex autokey setting
(setq bibtex-autokey-year-length 4)
(setq bibtex-autokey-titleword-length 0)
(setq bibtex-autokey-titleword-separator "")
(setq bibtex-autokey-name-year-separator "")
(setq bibtex-autokey-year-title-separator "")
(setq bibtex-autokey-before-presentation-function '(lambda (x) (downcase (eh-hanzi2pinyin-simple x))))

;; ebib window setting
(setq ebib-layout 'full)
(setq ebib-window-vertical-split nil)
(setq ebib-width 80)
(setq ebib-index-window-size 10)
(setq ebib-uniquify-keys nil)
(setq eh-ebib-entry-buffer-only-show-abstact t)
(add-hook 'ebib-entry-mode-hook (lambda () (visual-line-mode t)))

(defun eh-bibtex-add-language-field-to-all-entries ()
  (interactive)
  (bibtex-map-entries
   (lambda (key begin end)
     (let ((case-fold-search t))
       (save-excursion
	 (goto-char begin)
	 (let ((language-field (bibtex-search-forward-field "language" t)))
	   (when language-field
	     (goto-char (car (cdr language-field)))
	     (bibtex-kill-field)))
	 (when (string-match-p "\\cc+" (bibtex-autokey-get-field "title"))
	   (bibtex-make-field '("language" nil "Chinese" nil) t)))))))

(defun eh-bibtex-add-alias-field-to-all-entries ()
  (interactive)
  (bibtex-map-entries
   (lambda (key begin end)
     (let ((case-fold-search t))
       (save-excursion
	 (goto-char begin)
	 (let ((alias-field (bibtex-search-forward-field "alias" t))
	       (title (bibtex-autokey-get-field "title"))
	       (author (bibtex-autokey-get-field "author")))
	   (when alias-field
	     (goto-char (car (cdr alias-field)))
	     (bibtex-kill-field))
	   (bibtex-make-field (list "alias" nil (eh-hanzi2pinyin-simple (concat author ", " title) t) nil) t)))))))

(defun eh-bibtex-add-autokeys-to-all-entries ()
  (interactive)
  (bibtex-map-entries
   (lambda (key begin end)
     (let ((case-fold-search t)
	   (entry-type (bibtex-type-in-head)))
       ;; set key
       (save-excursion
	 ;; First delete the old key so that a customized algorithm
	 ;; for generating the new key does not get confused by the
	 ;; old key.
	 (re-search-forward (if (bibtex-string= entry-type "string")
				bibtex-string-maybe-empty-head
			      bibtex-entry-maybe-empty-head))
	 (if (match-beginning bibtex-key-in-head)
	     (delete-region (match-beginning bibtex-key-in-head)
			    (match-end bibtex-key-in-head)))
	 (setq auto-key (bibtex-generate-autokey))
	 ;; Sometimes `bibtex-generate-autokey' returns an empty string
	 (if (string= "" auto-key)
	     (setq auto-key "!NEED_EDIT"))
	 (insert auto-key))))))

(defun eh-ebib-select-and-popup-entry ()
  (interactive)
  (let ((eh-ebib-entry-buffer-only-show-abstact nil))
    (ebib-select-and-popup-entry)))

(defadvice ebib-fill-entry-buffer (around eh-ebib-fill-entry-buffer
					  (&optional match-str) activate)
  (if eh-ebib-entry-buffer-only-show-abstact
      (progn
	(with-current-buffer (cdr (assoc 'entry ebib-buffer-alist))
	    (with-ebib-buffer-writable
	      (erase-buffer)
	      (let* ((entry (ebib-db-get-entry (ebib-cur-entry-key) ebib-cur-db))
		     (abstract (or (cdr (assoc 'abstract entry)) "")))
		(insert (or (replace-regexp-in-string
			     "\n\\{2,\\}" "\n\n"
			     (replace-regexp-in-string
			      "[}{]" ""
			      (replace-regexp-in-string
			       "	" ""
			       (replace-regexp-in-string "[:space:]+" " " abstract)) "")))))
	      (goto-char (point-min)))))
    ad-do-it))

;; ebib index buffer display
(defun ebib-display-entry (entry-key)
  "Display ENTRY-KEY in the index buffer at POINT."
  (with-current-buffer (cdr (assoc 'index ebib-buffer-alist))
    (with-ebib-buffer-writable
      (insert (format "%-20s %-15s %s\n"
                      entry-key
		      (car (split-string
			    (or (ebib-db-get-field-value 'author entry-key ebib-cur-db 'noerror 'unbraced)
				"  ") "[ \t\n]+and[ \t\n]+\\|," ))
		      (let ((title (ebib-db-get-field-value 'title entry-key ebib-cur-db 'noerror 'unbraced)))
			(if (> (length title) 60)
			    (concat (substring title 0 60) "...")
			  title))
		      "")))))

(defun eh-ebib-view-file ()
  (interactive)
  (ebib-execute-when
    ((entries)
     (let* ((name-string (car (split-string
				(or (car (ebib-db-get-field-value 'author (ebib-cur-entry-key) ebib-cur-db 'noerror 'unbraced 'xref))
				    "  ") "[ \t\n]+and[ \t\n]+\\|," )))
	   (files-list  (delete-if
			 (lambda (s)
			   (let ((case-fold-search t)
				 (string (replace-regexp-in-string " +" "" s)))
			     (not (or (string-match name-string string)
				      (if (featurep 'eh-hanzi2pinyin)
					  (string-match name-string (eh-hanzi2pinyin string)))))))
			 (eh-directory-files-recursively
			  (file-name-directory (ebib-db-get-filename ebib-cur-db)) t))))
       (cond
	((> (length files-list) 1)
	 (start-process "" nil "xdg-open" (ido-completing-read "Open file:" files-list)))
	((= (length files-list) 1)
	 (message "Opening file: %s" (car files-list))
	 (start-process "" nil "xdg-open" (car files-list)))
	((< (length files-list) 1)
	 (message "Can't find the corresponding file")))))))

(defun eh-reftex-get-bibfile-list ()
  "Return list of bibfiles for current document.
When using the chapterbib or bibunits package you should either
use the same database files everywhere, or separate parts using
different databases into different files (included into the mater file).
Then this function will return the applicable database files."

  ;; Ensure access to scanning info
  (reftex-access-scan-info)
  (or
   ;; Try inside this file (and its includes)
   (cdr (reftex-last-assoc-before-elt
         'bib (list 'eof (buffer-file-name))
         (member (list 'bof (buffer-file-name))
                 (symbol-value reftex-docstruct-symbol))))
   ;; Try after the beginning of this file
   (cdr (assq 'bib (member (list 'bof (buffer-file-name))
                           (symbol-value reftex-docstruct-symbol))))
   ;; Anywhere in the entire document
   (cdr (assq 'bib (symbol-value reftex-docstruct-symbol)))))

(defun eh-ebib ()
  "Open ebib then search the marked string"
  (interactive)
  (load-library "bibtex")
  (let* ((path (if (buffer-file-name)
		   (car (eh-reftex-get-bibfile-list))))
	 (word (or (current-word nil t) ""))
	 (length (length word))
	 (key (if mark-active
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(if (and (string-match-p "\\cc+" word) (> length 3))
		    (buffer-substring-no-properties (- (point) 2) (point))
		  word))))
    (setq eh-ebib-push-buffer (current-buffer))
    (ebib (or path
	   (ido-read-file-name
               "Open bibtex file: "
               (car ebib-file-search-dirs))))
    (when key
      (eh-isearch-string key)
      (ebib-select-and-popup-entry))))

(defun eh-ebib-push-bibtex-key ()
  "Pushes the current entry to a LaTeX buffer.
The user is prompted for the buffer to push the entry into."
  (interactive)
  (let ((called-with-prefix (ebib-called-with-prefix)))
    (ebib-execute-when
      ((entries)
       (let ((citation-string
	      ;; 获取当前的entry-key
	      (if (ebib-db-marked-entries-p ebib-cur-db)
		  (mapconcat #'(lambda (key)
				 key)
			     (ebib-db-list-marked-entries ebib-cur-db)
			     ", ")
		(ebib-cur-entry-key))))
	 ;; 将citation-string插入到eh-ebib-push-buffer变量所
	 ;; 对应的buffer, (调用eh-ebib命令时,会设置eh-ebib-push-buffer变量)
	 (when citation-string
	   (with-current-buffer eh-ebib-push-buffer
	     (let* ((current-point (point))
		    (point1
		     (progn
		       (goto-char current-point)
		       (search-forward "{" nil t)))
		    (point1 (if point1 point1 (+ 1 (point-max))))
		    (point2
		     (progn
		       (goto-char current-point)
		       (search-forward "}" nil t)))
		    (point3
		     (progn
		       (goto-char current-point)
		       (search-backward "{" nil t)))
		    (point4
		     (progn
		       (goto-char current-point)
		       (search-backward "}" nil t)))
		    (point4 (if point4 point4 -1)))
	       (goto-char current-point)
	       (if (and point2 point3 (> point1 point2) (> point3 point4))
		   (let ((old-cite (buffer-substring-no-properties (+ point3 1) (- point2 1))))
		     (insert
		      (if (> (length (replace-regexp-in-string " " "" old-cite)) 0)
			  (concat ", " citation-string)
			citation-string)))
		 (insert (format "\\cite{%s}" citation-string)))))
	   (message "Pushed entries to buffer %s" eh-ebib-push-buffer)
	   (setq eh-ebib-push-buffer nil)
	   ;; 隐藏ebib窗口
	   (ebib-leave-ebib-windows))))
      ((default)
       (beep)))))

(defun eh-ebib-auto-generate-fields-to-all-entries ()
  "1. Add autokey field to all entries.
   2. Add language field to all entries.
   3. Add alias field to all entries. "
  (interactive)
  (let ((current-bib-file (ebib-db-get-filename ebib-cur-db)))
    (ebib-execute-when
      ((entries)
       (when (yes-or-no-p "Auto generate fields (key language and alias) to all entries? ")
	 (ebib-save-current-database)
	 (with-current-buffer (find-file-noselect current-bib-file)
	   (goto-char (point-min))
	   (message "Auto generate fields (key language and alias) to all entries in %s" current-bib-file)
	   (eh-bibtex-add-language-field-to-all-entries)
	   (eh-bibtex-add-autokeys-to-all-entries)
	   (eh-bibtex-add-alias-field-to-all-entries)
	   (save-buffer)
	   (kill-buffer))
	 ;; reload the current database
	 (ebib-reload-database ebib-cur-db)
	 (ebib-set-modified nil)
	 (ebib-redisplay)
	 (message "Database reloaded")))
      ((default)
       (beep)))))

(defun eh-convert-cite-key-to-pinyin ()
  "Convert bibtex key to pinyin"
  (interactive)
  (if (featurep 'eh-hanzi2pinyin)
      (progn
	(goto-char (point-min))
	(while (re-search-forward "\\\\cite{\\([^{}]+\\)}" nil t)
	  (let ((string (match-string 1)))
	    (replace-match (concat "\\\\cite{" (downcase (eh-hanzi2pinyin-simple string)) "}") t))))
    (message "Can't find eh-hanzi2pinyin")))

;; ebib mode keybinding
(ebib-key index "\C-xb" (lambda ()
			  (interactive)
			  (ebib-leave-ebib-windows)
			  (ibuffer)))
(ebib-key index "\C-xk" ebib-leave-ebib-windows)
(ebib-key index "\C-xq" ebib-quit)
(ebib-key index "q" ebib-quit)
(ebib-key index "f" eh-ebib-view-file)
(ebib-key index "\C-c\C-c" eh-ebib-push-bibtex-key)
(ebib-key index [(control k)] eh-ebib-auto-generate-fields-to-all-entries)
(ebib-key index [(return)] eh-ebib-select-and-popup-entry)
;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-bibtex.el ends here
