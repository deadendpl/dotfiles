;;; modus-ewal-theme.el --- Modus theme that uses pywal colors powered by ewal  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author:  <oliwier@archlinux>
;; Keywords: faces theme
;; Package-Requires: (modus-themes)

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

;; A theme built with modus that uses pywal colors and tries to look
;; good out of the box.

;;; Code:

(require 'ewal)
(require 'modus-themes)

(defvar modus-ewal-base-palette
  (mapcar (lambda (element)
            (list (car element) (cdr element)))
          (let* ((colors (ewal-load-colors))
                 (colors (assq-delete-all 'comment colors))
                 (colors (assq-delete-all 'cursor colors))
                 (colors (assq-delete-all 'white colors))
                 (colors (assq-delete-all 'black colors)))
            (setcar (assoc 'background colors) 'bg-main)
            (setcar (assoc 'foreground colors) 'fg-main)
            colors))
  "The base pywal palette edited to work with modus.")

(defvar modus-ewal-modus-palette
  (modus-themes-generate-palette modus-ewal-base-palette)
  "Palette generated based on `modus-ewal-base-palette'.")

(defvar modus-ewal-custom-faces
  '(
    ;; region by default is wonky
    `(region ((,c :background
                  ,(modus-themes-generate-color-blend
                    bg-main
                    (if (modus-themes-color-dark-p bg-main)
                        "#ffffff"
                      "#000000")
                    0.9))))
    `(meow-beacon-fake-selection
      ((,c :background
           ,(modus-themes-generate-color-blend
             bg-main
             (if (modus-themes-color-dark-p bg-main)
                 "#ffffff"
               "#000000")
             0.7)))))
  "A list of custom faces configuration.")

(defvar modus-ewal-load-theme-after-regeneration-p t
  "Whether to load the theme after regenerating it.")

(defun modus-ewal-reload-base-palette ()
  "Regenerate the base palette."
  (setq modus-ewal-base-palette
        (mapcar (lambda (element)
                  (list (car element) (cdr element)))
                (let* ((colors (ewal-load-colors))
                       (colors (assq-delete-all 'comment colors))
                       (colors (assq-delete-all 'cursor colors))
                       (colors (assq-delete-all 'white colors))
                       (colors (assq-delete-all 'black colors)))
                  (setcar (assoc 'background colors) 'bg-main)
                  (setcar (assoc 'foreground colors) 'fg-main)
                  colors))))

(defun modus-ewal-reload-modus-palette ()
  "Regenerate the base palette."
  (setq modus-ewal-modus-palette
        (modus-themes-generate-palette modus-ewal-base-palette)))

(defun modus-ewal-generate-theme ()
  "Generate the theme."
  (modus-themes-theme
   'modus-ewal
   'modus-themes
   "A pywal theme."
   (if (modus-themes-color-dark-p
        (car (alist-get 'bg-main modus-ewal-base-palette)))
       'dark
     'light)
   'modus-ewal-modus-palette
   nil
   nil
   'modus-ewal-custom-faces))

(defun modus-ewal-regenerate-theme ()
  "Regenerate the theme.
If `modus-ewal-load-theme-after-regeneration-p' is non-nil, reload
the theme as well."
  (modus-ewal-reload-base-palette)
  (modus-ewal-reload-modus-palette)
  (modus-ewal-generate-theme)
  (when modus-ewal-load-theme-after-regeneration-p
    (load-theme 'modus-ewal t)))

(modus-ewal-generate-theme)

(provide 'modus-ewal-theme)
;;; modus-ewal-theme.el ends here
