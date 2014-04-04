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
;; 在emacs中使用ibus
(require 'ibus)
(add-hook 'after-init-hook 'ibus-mode-on)
;; Change cursor color depending on IBus status
(setq ibus-cursor-color "red")
;; daemon模式下使用ibus
(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
                  (or ibus-mode (ibus-mode-on))))))

;; recentf
(require 'recentf)
(require 'recentf-ext)
(recentf-mode 1)
(setq recentf-max-saved-items 99)
(setq recentf-max-menu-items 99)
(setq recentf-exclude '("COMMIT" "autoloads" "archive-contents" "eld" "newsrc"))
(setq recentf-menu-filter 'eh-recentf-buffer-filter)
(setq recentf-show-file-shortcuts-flag nil)

(defun eh-recentf-buffer-filter (l)
  (let (filtered-names filtered-list  full name counters sufx (index 0))
    (dolist (elt l (nreverse filtered-list))
      (setq index (1+ index)
	    element (recentf-menu-element-value elt)
	    full (directory-file-name element)
	    directory  (file-name-directory full)
	    name (if (file-directory-p element)
		     (concat (file-name-nondirectory full) "/")
		   (file-name-nondirectory full))
            recentf-string (format "[%2s]:  %-30s (%s)" index name (abbreviate-file-name directory)))
      (push (recentf-make-menu-element recentf-string full) filtered-list))))

(global-set-key (kbd "C-x f") 'recentf-open-files)

;; wdired
(require 'wdired)

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-x C-x m") 'mc/edit-lines)
(global-set-key (kbd "C-c >") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c <") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(add-hook 'multiple-cursors-mode-enabled-hook
          (lambda()
	    (require 'phi-search)
            (local-set-key (kbd "C-s") 'phi-search)
            (local-set-key (kbd "C-r") 'phi-search)))

;; ace-jump
(require 'ace-jump-mode)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-j") 'ace-jump-mode)

;; switch window
(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)
(setq  switch-window-shortcut-style 'qwerty)

;; expand-region
(require 'expand-region)
(define-key global-map (kbd "C-c =") 'er/expand-region)

;; browse-kill-ring
(require' browse-kill-ring)
(setq browse-kill-ring-highlight-current-entry t)
(setq browse-kill-ring-separator (concat "\n" (make-string 70 ?=) "\n"))
(add-hook 'browse-kill-ring-hook 'eh-browse-kill-ring-settings)

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
  (let ((clipboard-output (x-get-clipboard)))
    (unless 
	(string= (car kill-ring) clipboard-output)
      (kill-new clipboard-output))
    (if (car kill-ring)
	(browse-kill-ring)
      (message "kill ring is empty"))))

(global-set-key (kbd "C-c y") 'eh-browse-kill-ring)

;; General project support
(require 'projectile)
(require 'wgrep)
(projectile-global-mode)
(setq projectile-enable-caching nil)
(global-set-key (kbd "C-x F") 'projectile-find-file)
(global-set-key (kbd "C-S-s") 'projectile-grep)

;; undo tree
(require 'undo-tree)
(global-undo-tree-mode)
(global-set-key (kbd "C-c /") 'undo-tree-visualize)
(add-hook 'undo-tree-visualizer-mode-hook 'eh-undo-tree-visualizer-settings)
(defun eh-undo-tree-visualizer-settings ()
  (interactive)
  (define-key undo-tree-visualizer-mode-map (kbd "C-c C-k") 'undo-tree-visualizer-quit)
  (define-key undo-tree-visualizer-mode-map (kbd "C-k") 'undo-tree-visualizer-quit)
  (define-key undo-tree-visualizer-mode-map (kbd "k") 'undo-tree-visualizer-quit)
  (define-key undo-tree-visualizer-mode-map (kbd "C-g") 'undo-tree-visualizer-abort))

;; 简化mode-line的显示(use setq-default to set it for /all/ modes)
(setq eh-mode-line-coding-format
      '(:eval
        (let* ((code (upcase (symbol-name buffer-file-coding-system)))
               (eol-type (coding-system-eol-type buffer-file-coding-system))
               (eol-string (if (eq 0 eol-type) "UNIX"
			     (if (eq 1 eol-type) "DOS"
			       (if (eq 2 eol-type) "MAC"
				 "???")))))
	  (if (string-match-p eol-string code)
	      code
	    (concat code " " eol)))))

(setq-default mode-line-format
	      (list
	       '(:eval (if buffer-read-only
			   (propertize "只读  "
				       'face 'font-lock-type-face
				       'help-echo "Buffer is read-only")
			 (if (buffer-modified-p)
			     (propertize "更改  "
					 'face 'font-lock-warning-face
					 'help-echo "Buffer has been modified")
			   (propertize (format-time-string "%H:%M ")
				       'help-echo
				       (concat (format-time-string "%c; ")
					       (emacs-uptime "Uptime:%hh"))))))
	       ;; buffer name
	       '(:eval (propertize "%b " 'face 'font-lock-keyword-face
				   'help-echo (buffer-file-name)))
	       ;; line and column
	       (propertize "(%02l,%02c) " 'face 'font-lock-type-face)
	       ;; position and size
	       (propertize "%p/%I " 'face 'font-lock-constant-face)
	       ;; current major mode
	       '(:eval (propertize "[%m] " 'face 'font-lock-string-face
				   'help-echo (symbol-name buffer-file-coding-system)))
	       ;; current minor-mode, emms song info or sdcv translation
	       '(:eval (if (= 0 (length eh-sdcv-mode-line-string))
			   (if emms-player-playing-p
			       (list emms-mode-line-string " " emms-playing-time-string)
			     (list "( " eh-mode-line-coding-format  minor-mode-alist " )"))
			 eh-sdcv-mode-line-string))
	       ;; show: -------
	       " %-"))

;; 使用sdcv查字典
(setq eh-sdcv-mode-line-string "")
(setq eh-sdcv-previous-word "")

;; 汉译英时使用scws中文分词系统
(setq eh-scws-program "/usr/local/scws/bin/scws")
(setq eh-scws-program-args "-c utf-8 -N -A -I -d /usr/local/scws/etc/dict.utf8.xdb -i ")

(defun eh-current-word ()
  (let ((word (or (current-word t t) ""))
	(current-char (string (preceding-char))))
    (if (not (string-match-p "\\cc" word))
	word
      (car (remove-if-not
	    '(lambda (x) (string-match-p current-char x))
	    (split-string
	     (replace-regexp-in-string
	      "\\Cc" " "
	      (shell-command-to-string
	       (concat eh-scws-program " " eh-scws-program-args " " word)))))))))

(defun eh-sdcv-mode-line ()
  (interactive)
  (let ((word (or (if mark-active
		      (buffer-substring-no-properties (region-beginning) (region-end))
		    (eh-current-word)) "")))
    (unless (string= word eh-sdcv-previous-word)
      (setq eh-sdcv-previous-word word)
      (let ((translate (eh-sdcv-get-translate word)))
	(setq eh-sdcv-mode-line-string
	      (unless (= 0 (length translate))
		(format "[%s: %s]" word
			(mapconcat 'identity translate ",")))))
      (force-mode-line-update))))

(defun eh-sdcv-get-translate (word)
  (let* ((translate
	  (if (string-match-p "\\cc" word)
	      (shell-command-to-string (concat "sdcv --utf8-output --utf8-input -n -u XDICT汉英辞典 " word))
	    (shell-command-to-string (concat "sdcv -n -u XDICT英汉辞典 " word))))
	 (string-regexp (concat "-->" word)))
    (when (string-match-p string-regexp translate)
      (remove-duplicates
       (mapcar '(lambda (x) (replace-regexp-in-string "^ +\\| +$" "" x))
	       (split-string (eh-wash-sdcv-output translate) "[,;]"))
       :test (lambda (x y) (or (= 0 (length y)) (equal x y)))
       :from-end t))))

(defun eh-wash-sdcv-output (str)
  (replace-regexp-in-string
   "^ +\\| +$" ""
   (replace-regexp-in-string
    ";+" "; "
    (replace-regexp-in-string
     " +" " "
     (replace-regexp-in-string
      "^;+\\|^ +" ""
      (replace-regexp-in-string
       "[\nˊ，。；：！？“]+\\|[a-zA-Z]+\\." ";"
       (replace-regexp-in-string
	".*\n*-->.+\\|\\[.+\\]" ""
	str)))))))

(defun eh-insert-translate ()
  (interactive)
  (let* ((word (or (if mark-active
		       (buffer-substring-no-properties (region-beginning) (region-end))
		     (eh-current-word)) ""))
	 (translate (eh-sdcv-get-translate word)))
    (if translate
	(let ((string (popup-menu* translate)))
	  (if (and (not mark-active) (string-match-p "\\cc" word))
	      (let ((length (length word)))
		(backward-char length)
		(re-search-forward word nil t)
		(replace-match (concat string " ")))
	    (progn  (eh-mark-word)
		    (delete-region (region-beginning) (region-end))
		    (insert string))))
      (message "Can't translate the word: %s" word))))

(defun eh-mark-word ()
  "Mark the entire word around or in front of point."
  (interactive)
  (let ((word-regexp "\\sw"))
    (when (or (looking-at word-regexp)
              (looking-back word-regexp (line-beginning-position)))
      (skip-syntax-forward "w")
      (set-mark (point))
      (skip-syntax-backward "w"))))

;; 每0.5秒运行一次eh-translate-with-sdcv
(run-with-timer 0 0.5 'eh-sdcv-mode-line)
(global-set-key (kbd "C-c d") 'eh-insert-translate)


;;;autoload(require 'eh-misc)
(provide 'eh-misc)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-misc.el ends here











