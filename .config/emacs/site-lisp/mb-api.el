;;; mb-api.el --- Small client for MusicBrainz       -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author:  Oliwier Czerwiński <oliwier.czerwi@proton.me>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; So far, only artist search is implemented, and it's limited.

;;; Code:

(require 'url)
(require 'json)

(defun mb-api-search (artist)
  "Searches for ARTIST, and returns raw lisp data."
  (with-current-buffer
      (url-retrieve-synchronously (format "https://musicbrainz.org/ws/2/artist?query=%s&fmt=json" artist))
    (goto-char url-http-end-of-headers)
    (json-read)
    )
  )

(defun mb-api-search-tidy (artist)
  "Searches for ARTIST and returns a vector."
  (cdr (assoc 'artists (cdddr (mb-api-search artist))))
  )

(defun mb-api-exact (artist)
  "Searches for ARTIST, and returns an alist of names, disambiguations and IDs.
If there is no disambiguation, it puts (disambiguation . \"\").
NOTE that non latin characters will not be displayed correctly."
  (mapcar (lambda (x)
            (append (list
                     (assoc 'name x)
                     (if (assoc 'disambiguation x)
                         (assoc 'disambiguation x)
                       '(disambiguation . ""))
                     (assoc 'id x))))
          (append (mb-api-search-tidy artist) nil))
  )

(defun mb-api-select (data)
  "Prompt the user to select a name from the list DATA and return the corresponding ID."
  (let* ((name-list (mapcar (lambda (item)
                              (concat
                               (cdr (assoc 'name item))
                               ;; if there is disambiguation, add it
                               (unless (string= (cdr (assoc 'disambiguation item)) "")
                                 (format " (%s%s" (cdr (assoc 'disambiguation item)) ")"))
                               ))
                            data))
         (selected-name (completing-read "Select a name: " name-list)))
    (cdr (assoc 'id (cl-find-if
                     (lambda (item)
                       (string=
                        (concat (cdr (assoc 'name item))
                                (unless (string= (cdr (assoc 'disambiguation item)) "")
                                  (format " (%s%s" (cdr (assoc 'disambiguation item)) ")")))
                        selected-name))
                     data)))))

(defun mb-api-search-artist (artist)
  (interactive "sArtist: ")
  (browse-url (concat "https://musicbrainz.org/mbid/" (mb-api-select (mb-api-exact artist))))
)

(provide 'mb-api)
;;; mb-api.el ends here
