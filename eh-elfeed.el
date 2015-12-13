;;; eh-elfeed.el --- Tumashu's emacs configuation

;; Copyright (c) 2011 2012, Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/emacs-helper
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

(use-package elfeed
  :config
  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
          "http://www.terminally-incoherent.com/blog/feed/"
          ("https://linuxtoy.org/feed" linux linuxtoy)
          ("http://planet.emacsen.org/atom.xml" emacs planet-emacs)
          ("http://repo.or.cz/w/org-mode.git/rss" org)
          ("http://repo.or.cz/w/emacs.git/rss" emacs emacs.git)
          ("http://solidot.org.feedsportal.com/c/33236/f/556826/index.rss" linux solidot)
          ("http://www.phoronix.com/rss.php" linux phoronix)
          ("http://ergoemacs.org/emacs/blog.xml" emacs ergoemacs)
          ("http://emacsredux.com/atom.xml" emacs emacsredux)
          ("http://www.emacswiki.org/emacs/full-diff.rss?action=rss;days=7;all=0;showedit=0;full=1;diff=1" emacs emacswiki)
          ("http://planet.debian.org/rss20.xml" debian linux)
          ("http://planet.gnome.org/atom.xml" gnome)
          ("http://lwn.net/headlines/rss" linux lwn)
          ("http://news.baidu.com/n?cmd=1&class=civilnews&tn=rss" baidu-news civil)
          ("http://news.baidu.com/n?cmd=1&class=internet&tn=rss" baidu-news internet)
          ("http://news.baidu.com/n?cmd=1&class=technnews&tn=rss" baidu-news tech)
          ("http://news.baidu.com/n?cmd=1&class=finannews&tn=rss" baidu-news finance)
          ("http://news.baidu.com/ns?word=title%3A%C9%BD%CE%F7%BF%BC%CA%D4&tn=newsrss&sr=0&cl=2&rn=20&ct=0"
           baidu-news shanxi kaoshi)
          ("http://news.baidu.com/ns?word=%CE%C0%C9%FA%D5%FE%B2%DF&tn=newsrss&sr=0&cl=2&rn=20&ct=0" baidu-news zhengce)
          ("http://www.emacsist.com/rss" emacs emacsist))))

(provide 'eh-elfeed)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-elfeed.el ends here
