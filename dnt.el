;;; dnt.el --- Do Not Track           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; URL: https://github.com/ieure/dnt-el
;; Version: 0.0.1
;; Package-Requires: ((emacs "25") (s "1.12.0"))
;; Keywords: tools, www

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'subr-x)
(require 's)
(require 'url-parse)

(defun dnt--clean-podtrac-play (urlobj)
  "Strip Podtrac tracking garbage out of podcast URLs."
  ;; ex "https://play.podtrac.com/npr-344098539/npr.mc.tritondigital.com/WAITWAIT_PODCAST/media/anon.npr-podcasts/podcast/npr/waitwait/2019/03/20190302_waitwait_wwdtmpodcast190302-133e5545-b77b-4fdd-ac62-df172a41bb31.mp3?orgId=1&d=2993&p=344098539&story=699615041&t=podcast&e=699615041&ft=pod&f=344098539"
  (concat "https://"
          (caddr (s-split-up-to "/" (car (url-path-and-query urlobj)) 2))))

(defun dnt--clean-podtrac-www (urlobj)
  "Strip Podtrac tracking garbage out of podcast URLs."
  ;; ex "http://www.podtrac.com/pts/redirect.mp3/archive.org/download/rr12019/SciFi557.mp3"
  (concat "https://"
          (cadr (s-split-up-to "redirect.mp3/"
                               (car (url-path-and-query urlobj)) 1))))

(defun dnt--clean-google-analytics (urlobj)
  (let* ((path-and-query (url-path-and-query urlobj))
         (path (car path-and-query))
         (query (cdr path-and-query)))

    (if-let ((new-query (cl-remove-if
                         (lambda (elt)
                           (s-starts-with? "utm_" (car elt)))
                         (url-parse-query-string query))))
        (setq path (concat path "?" (url-build-query-string new-query))))
    (setf (url-filename urlobj) path)
    (url-recreate-url urlobj)))


;; https://www.amazon.com/AmazonBasics-Type-C-USB-Male-Cable/dp/B01GGKYQ02/ref=sr_1_1?s=amazonbasics&srs=10112675011&ie=UTF8&qid=1489067885&sr=8-1&keywords=usb-c
(defun dnt--clean-amazon (urlobj)
  (setf (url-filename urlobj) (car (s-split-up-to "ref=" (car (url-path-and-query urlobj)) 1)))
  (url-recreate-url urlobj))

(defun dnt--extract-url-from-query (urlobj param)
  (cadr (assoc param (url-parse-query-string (cdr (url-path-and-query urlobj))))))

(defun dnt--clean (url)
  "Strip tracking garbage out of URLs."
  (let* ((urlobj (url-generic-parse-url url)))
    (cond
     ((string= "play.podtrac.com" (url-host urlobj))
      (dnt--clean-podtrac-play urlobj))

     ((string= "www.podtrac.com" (url-host urlobj))
      (dnt--clean-podtrac-www urlobj))

     ((s-contains? "utm_" url)
      (dnt--clean-google-analytics urlobj))

     ((s-contains? "amazon" (url-host urlobj))
      (dnt--clean-amazon urlobj))

     ((string= "l.facebook.com" (url-host urlobj))
      (dnt--extract-url-from-query urlobj "u"))

     ((string= "out.reddit.com" (url-host urlobj))
      (dnt--extract-url-from-query urlobj "url"))

     ((s-contains? "steamcommunity.com/linkfilter" url)
      (dnt--extract-url-from-query urlobj "url"))

     (t url))))

;;;###autoload
(defun dnt (url)
  "Strip tracking garbage out of URLs."
  (let ((new (dnt--clean url)))
    (if (string= url new)
        url
      (dnt new))))

;;;###autoload
(defun dnt-emms ()
  "Strip tracking from any URL added to EMMS."
  (when (fboundp #'emms-add-url)
    (add-function :filter-args (symbol-function 'emms-add-url)
                  (lambda (url-arg)
                    (list (dnt (car url-arg)))))))

;;;###autoload
(defun dnt-browse-url ()
  "Strip tracking from any URL opened."
  (add-function :filter-args (symbol-function 'browse-url)
                (lambda (args)
                  (cons (dnt (car args))
                        (cdr args)))))

(provide 'dnt)

;;; dnt.el ends here
