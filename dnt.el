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
  "Return URLOBJ with Podtrac trackers removed."
  (concat "https://"
          (caddr (s-split-up-to "/" (car (url-path-and-query urlobj)) 2))))

(defun dnt--clean-podtrac-www (urlobj)
  "Return a URLOBJ with Podtrac trackers removed."
  (concat "https://"
          (cadr (s-split-up-to "redirect.mp3/"
                               (car (url-path-and-query urlobj)) 1))))

(defun dnt--clean-chartable (urlobj)
  "Strip Chartable tracking from URLs."
  (concat "https://" (cadddr (s-split-up-to "/" (car (url-path-and-query urlobj)) 3))))

(defun dnt--clean-google-analytics (urlobj)
  "Return a URLOBJ with Google Analytics tracking removed."
  (let* ((path-and-query (url-path-and-query urlobj))
         (path (car path-and-query)))
    (if-let ((new-query (thread-first (lambda (kv) (s-starts-with? "utm_" (car kv)))
                          (cl-remove-if (url-parse-query-string (cdr path-and-query))))))
        (setq path (concat path "?" (url-build-query-string new-query))))
    (setf (url-filename urlobj) path)
    (url-recreate-url urlobj)))

(defun dnt--clean-amazon (urlobj)
  "Return a URLOBJ with Amazon tracking removed."
  (setf (url-filename urlobj) (car (s-split-up-to "ref=" (car (url-path-and-query urlobj)) 1)))
  (url-recreate-url urlobj))

(defun dnt--clean-nyt (urlobj)
  "Return a URLOBJ with New York Times tracking removed."
  (setf (url-filename urlobj) (car (url-path-and-query urlobj)))
  (url-recreate-url urlobj))

(defun dnt--extract-url-from-query (urlobj param)
  "Return a URLOBJ from the PARAM query of a different URL."
  (cadr (assoc param (url-parse-query-string (cdr (url-path-and-query urlobj))))))

(defun dnt--clean (url)
  "Return a URL with one layer of tracking services removed."
  (let* ((urlobj (url-generic-parse-url url)))
    (cond
     ((string= "play.podtrac.com" (url-host urlobj))
      (dnt--clean-podtrac-play urlobj))

     ((or (string= "www.podtrac.com" (url-host urlobj))
          (string= "dts.podtrac.com" (url-host urlobj)))
      (dnt--clean-podtrac-www urlobj))

     ((or (string= "chtbl.com" (url-host urlobj)))
      (dnt--clean-chartable urlobj))

     ((s-contains? "utm_" url)
      (dnt--clean-google-analytics urlobj))

     ((s-contains? "smid=" url)
      (dnt--clean-nyt urlobj))

     ((s-contains? "amazon" (url-host urlobj))
      (dnt--clean-amazon urlobj))

     ((string= "l.facebook.com" (url-host urlobj))
      (dnt--extract-url-from-query urlobj "u"))

     ((string= "out.reddit.com" (url-host urlobj))
      (dnt--extract-url-from-query urlobj "url"))

     ((string= "r.tapatalk.com" (url-host urlobj))
      (dnt--extract-url-from-query urlobj "url"))

     ((s-contains? "steamcommunity.com/linkfilter" url)
      (dnt--extract-url-from-query urlobj "url"))

     (t url))))

;;;###autoload
(defun dnt (url)
  "Return a URL with tracking services removed."
  (let ((new (dnt--clean url)))
    (if (string= url new)
        url
      (dnt new))))

;;;###autoload
(defun dnt-emms ()
  "Enable tracker removal from URLs added to EMMS."
  (when (fboundp #'emms-add-url)
    (add-function :filter-args (symbol-function 'emms-add-url)
                  (lambda (url-arg)
                    (list (dnt (car url-arg)))))))

;;;###autoload
(defun dnt-browse-url ()
  "Enable tracker removal from browsed URLs."
  (add-function :filter-args (symbol-function 'browse-url)
                (lambda (args)
                  (cons (dnt (car args))
                        (cdr args)))))

(ert-deftest dnt--test-clean-tapatalk ()
  (should (string= "https://forums.arcade-museum.com/showthread.php?t=446645" (dnt "https://r.tapatalk.com/shareLink?share_fid=19164&share_tid=446645&url=https%3A%2F%2Fforums%2Earcade-museum%2Ecom%2Fshowthread%2Ephp%3Ft%3D446645&share_type=t"))))

(ert-deftest dnt--test-clean-amazon ()
  (should (string= "https://www.amazon.com/AmazonBasics-Type-C-USB-Male-Cable/dp/B01GGKYQ02/" (dnt "https://www.amazon.com/AmazonBasics-Type-C-USB-Male-Cable/dp/B01GGKYQ02/ref=sr_1_1?s=amazonbasics&srs=10112675011&ie=UTF8&qid=1489067885&sr=8-1&keywords=usb-c"))))

(ert-deftest dnt--test-clean-podtrac ()
  (should (string= "https://archive.org/download/rr12019/SciFi557.mp3" (dnt "http://www.podtrac.com/pts/redirect.mp3/archive.org/download/rr12019/SciFi557.mp3")))
  (should (string= "https://npr.mc.tritondigital.com/WAITWAIT_PODCAST/media/anon.npr-podcasts/podcast/npr/waitwait/2019/03/20190302_waitwait_wwdtmpodcast190302-133e5545-b77b-4fdd-ac62-df172a41bb31.mp3" (dnt "https://play.podtrac.com/npr-344098539/npr.mc.tritondigital.com/WAITWAIT_PODCAST/media/anon.npr-podcasts/podcast/npr/waitwait/2019/03/20190302_waitwait_wwdtmpodcast190302-133e5545-b77b-4fdd-ac62-df172a41bb31.mp3?orgId=1&d=2993&p=344098539&story=699615041&t=podcast&e=699615041&ft=pod&f=344098539"))))

(ert-deftest dnt--test-google-analytics ()
  (should (string= "http://meyerweb.com/eric/thoughts/2017/03/07/welcome-to-the-grid/" (dnt "http://meyerweb.com/eric/thoughts/2017/03/07/welcome-to-the-grid/?utm_source=frontendfocus&utm_medium=email"))))

(ert-deftest dnt--test-chartable ()
  (should (string= "https://rss.art19.com/episodes/47d9a8cd-4a25-408d-9205-3a13d36e4546.mp3" (dnt "https://dts.podtrac.com/redirect.mp3/chtbl.com/track/9EE2G/rss.art19.com/episodes/47d9a8cd-4a25-408d-9205-3a13d36e4546.mp3"))))

(provide 'dnt)

;;; dnt.el ends here
