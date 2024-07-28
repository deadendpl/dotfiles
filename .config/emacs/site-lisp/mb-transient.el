;;; mb-transient.el --- Transient menu for MusicBrainz searches  -*- lexical-binding: t; -*-

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

;; A Transient menu for invoking MusicBrainz web searches.
;; FIXME `mb-make-frame' doesn't work if there is no Emacs frame running


;;; Code:

(require 'transient)

(defvar mb-search-method "indexed"
  "A search method.")

(defvar mb-type "release"
  "A search type.")

(defconst mb-type-list
  '(annotation
    area
    artist
    cdstub
    doc
    editor
    event
    instrument
    label
    place
    recording
    release
    release_group
    series
    tag
    work)
  "List of all available search types.")

(defvar mb-query nil
  "A search query.")

(defconst mb-advanced-syntaxes-list
  '(added
    address
    aid
    alias
    area
    areaaccent
    arid
    artist
    artistaccent
    artistname
    asin
    barcode
    begin
    beginarea
    bio
    catno
    code
    comment
    country
    creditname
    date
    description
    discid
    discids
    discidsmedium
    dur
    editor
    eid
    end
    endarea
    ended
    entity
    event
    eventaccent
    firstreleasedate
    format
    gender
    id
    iid
    instrument
    instrumentaccent
    ipi
    isni
    iso
    iso1
    iso2
    iso3
    isrc
    iswc
    label
    labelaccent
    laid
    lang
    lat
    long
    mediums
    name
    number
    packaging
    pid
    place
    placeaccent
    position
    primary_alias
    primarytype
    qdur
    quality
    recording
    recording_count
    recordingaccent
    reid
    release
    release_count
    releaseaccent
    releasegroup
    releasegroupaccent
    releases
    rgid
    rid
    script
    secondarytype
    series
    seriesaccent
    sid
    sortname
    status
    tag
    text
    tid
    title
    tnum
    tracks
    tracksmedium
    tracksrelease
    type
    video
    wid
    work
    workaccent)
  "List of all advanced syntax keywords.")

(defvar mb-optimal-width 54
  "A width optimal for invoking `mb-transient' in a floating frame.")

(defvar mb-optimal-height 27
  "A height optimal for invoking `mb-transient' in a floating frame.")

(defvar mb-buffer-name "*mb-transient*"
  "Buffer name used in `mb-make-buffer'.")

(defvar mb-frame-name "MusicBrainz Emacs Search"
  "Frame name used in `mb-make-frame'.")

(defun mb-set-search-method (val)
  "Sets search method to VAL."
  (interactive "s")
  (setq mb-search-method val))

(defun mb-set-search-method-indexed ()
  "Sets search method to indexed."
  (interactive)
  (mb-set-search-method "indexed"))
(defun mb-set-search-method-advanced ()
  "Sets search method to advanced."
  (interactive)
  (mb-set-search-method "advanced"))
(defun mb-set-search-method-direct ()
  "Sets search method to direct."
  (interactive)
  (mb-set-search-method "direct"))

(defun mb-set-type (val)
  "Sets search type to VAL."
  (interactive "s")
  (setq mb-type val))

(defun mb-set-type-artist ()
  "Sets search type to artist."
  (interactive)
  (mb-set-type "artist"))
(defun mb-set-type-doc ()
  "Sets search type to documentation."
  (interactive)
  (mb-set-type "doc"))
(defun mb-set-type-recording ()
  "Sets search type to recording."
  (interactive)
  (mb-set-type "recording"))
(defun mb-set-type-release ()
  "Sets search type to release."
  (interactive)
  (mb-set-type "release"))
(defun mb-set-type-release-group ()
  "Sets search type to release-group."
  (interactive)
  (mb-set-type "release_group"))
(defun mb-set-type-full ()
  "Chooses one of entries in `mb-type-list'."
  (interactive)
  (setq mb-type (completing-read (format-prompt "Type" nil)
                                 mb-type-list nil t)))

(defun mb-set-query ()
  "Sets search query to what user writes."
  (interactive)
  (let ((query (read-string (format-prompt "Query" nil))))
  (setq mb-query query)))

(defun mb-open ()
  "Combines `mb-query', `mb-type', and `mb-search-method' into a
search URL that gets opened with `browse-url'.

If it's run in a frame whose name matches `mb-frame-name', the
frame will be deleted. That way, when used in `mb-transient', the
frame will automatically close."
  (interactive)
  (browse-url (concat "https://musicbrainz.org" "/search?query=" mb-query "&type=" mb-type "&method=" mb-search-method))
  ;; frame deletion if it's in frame made by `mb-make-frame'
  (if (string-equal (cdr (assoc 'name (frame-parameters))) mb-frame-name)
      (delete-frame)))

(defun mb-advanced-query-set ()
  "Returns a string valid for doing advanced searches for a search URL."
  (interactive)
  (let ((syntax (completing-read (format-prompt "Advanced syntax" nil)
                                 mb-advanced-syntaxes-list nil nil)))
    (when syntax
      (concat syntax ":"
              (read-string (format-prompt syntax nil)))
      )))

(defun mb-advanced-method-setup ()
  "Sets search query to output of `mb-advanced-query-set'."
  (interactive)
  (mb-set-search-method-advanced)
  (setq mb-query (mb-advanced-query-set)))

(transient-define-prefix mb-transient ()
  "Search in MusicBrainz"
  ["Search method"
   ("si" "Indexed" mb-set-search-method-indexed :transient t)
   ("sa" "Indexed with Advanced Query Syntax (Fills Query)" mb-advanced-method-setup :transient t)
   ("sd" "Direct Database Search" mb-set-search-method-direct :transient t)]
  ["Type"
   ("ta" "Artist" mb-set-type-artist :transient t)
   ("td" "Documentation" mb-set-type-doc :transient t)
   ("tr" "Recording" mb-set-type-recording :transient t)
   ("tR" "Release" mb-set-type-release :transient t)
   ("tg" "Release Group" mb-set-type-release-group :transient t)
   ("tt" "All types" mb-set-type-full :transient t)]
  ["Query"
   ("<SPC>" "Enter query" mb-set-query :transient t)]
  ["Open"
   ("e" "Open" mb-open)])

;;; Making it as an external frame that will make it easy to invoke out of Emacs

(defun mb-make-buffer ()
  "Generates a buffer with name of `mb-buffer-name' with a placeholder text."
  (unless (get-buffer mb-buffer-name)
    (get-buffer-create mb-buffer-name)
    (set-buffer mb-buffer-name)
    (insert "Welcome to Transient MusicBrainz porcelain!")))

(defun mb-make-frame ()
  "Makes a frame with a placeholder buffer, and switches to that buffer."
  (unless (get-buffer mb-buffer-name)
    (mb-make-buffer))
  (let ((mb-frame (make-frame
                   `((name . ,mb-frame-name)
                     (width . ,mb-optimal-width)
                     ;; (height . ,mb-optimal-height)
                     ))))
    (with-selected-frame mb-frame (switch-to-buffer mb-buffer-name)))
  )

(defun mb-transient-frame ()
  "Wrapper for creating a frame with selected placeholder buffer,
and displaying `mb-transient'."
  (interactive)
  (mb-make-frame)
  (select-frame-by-name mb-frame-name)
  (mb-transient)
  )

(provide 'mb-transient)

;;; mb-transient.el ends here
