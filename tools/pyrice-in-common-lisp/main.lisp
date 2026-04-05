(defpackage :pyrice
  (:use :cl))
(in-package :pyrice)

(ql:quickload :clingon)
(ql:quickload :uiop)

(defparameter *wal-directory*
  (or (probe-file
       (merge-pathnames "wal/"
                        (uiop:ensure-directory-pathname
                         (uiop:getenv "XDG_CACHE_HOME"))))
      (probe-file (uiop:ensure-directory-pathname
                   (uiop:native-namestring "~/.cache/wal/"))))
  "The directory where pywal processed files are.")

(defparameter *config-directory*
  (or (probe-file
       (merge-pathnames "pyrice/"
                        (uiop:ensure-directory-pathname
                         (uiop:getenv "XDG_CONFIG_HOME"))))
      (probe-file (uiop:ensure-directory-pathname
                   (uiop:native-namestring "~/.config/pyrice/")))))

(defparameter *wallpaper-path* nil
  "The wallpaper that will be used.")

(defparameter *light-theme-p* nil
  "Whether a light theme will be generated.")

(defparameter *old-wallpaper-p* nil
  "Whether the previously used wallpaper will be reused.")

(defun is-file-an-image-p (file)
  "Return t if FILE is an image."
  (let* ((output (nth-value
                  0
                  (uiop:run-program
                   (format nil "file --mime-type \"~A\"" file)
                   :output '(:string :stripped t))))
         (mime-string (subseq output (+ 2 (position
                                           #\: output :from-end t))))
         (file-general-type (subseq mime-string 0
                                    (position #\/ mime-string))))
    (equal file-general-type "image")))

(defun get-random-image-from-dir (dir)
  "Get a random image from DIR."
  (let ((wallpaper-list)
        (*random-state* (make-random-state t)))
    (uiop/filesystem:collect-sub*directories
     dir t t
     (lambda (dir)
       (mapcar (lambda (item)
                 (push item wallpaper-list))
               (uiop:directory-files dir))))
    (loop :with file := (uiop:native-namestring
                         (nth (random (1- (length wallpaper-list)))
                              wallpaper-list))
          :until (is-file-an-image-p file)
          :do (setf file (uiop:native-namestring
                          (nth (random (1- (length wallpaper-list)))
                               wallpaper-list)))
          :finally (return file))))

(defun run-program-with-error-command (args error-format-string)
  "Runs a program and returns the command if it errors.
ARGS is a list of arguments."
  (handler-case (uiop:run-program args)
    (uiop/run-program:subprocess-error (c)
      (format t error-format-string
              (uiop/run-program:subprocess-error-command c)))))

(defun pywal-run (wallpaper backend &key (light nil))
  "Run pywal and wpgtk commands using BACKEND on WALLPAPER.
Light theme is generated if LIGHT is non-nil (nil is default)."
  (let* ((wpg-args `("wpg" "--noreload" "--backend" ,backend
                           "-ns" ,wallpaper))
         (wal-args `("wal" "--cols16" "--backend" ,backend
                           "-nei" ,wallpaper))
         (wpg-args (if light (append wpg-args '("-L")) wpg-args))
         (wal-args (if light (append wal-args '("-l")) wal-args)))
    (if (and
         (eql 0 (nth-value
                 2 (run-program-with-error-command
                    wpg-args
                    "Wpgtk returned an error. The command run was ~a~%")))
         (eql 0 (nth-value
                 2 (run-program-with-error-command
                    wal-args
                    "Pywal returned an error. The command run was ~a~%"))))
        (format t "Generated pywal theme.~%"))))


(defun run-before-hook ()
  "Run a before-hook.lisp file in the config directory."
  (let ((before-hook-file (ignore-errors (merge-pathnames
                                          "before-hook.lisp" *config-directory*))))
    (load before-hook-file)))

(defun run-after-hook ()
  "Run a after-hook.lisp file in the config directory."
  (let ((after-hook-file (ignore-errors (merge-pathnames
                                         "after-hook.lisp" *config-directory*))))
    (load after-hook-file)))

(defun cli-options ()
  "Returns a list of options for our main command"
  (list
   (clingon:make-option
    :string
    :description "Backend to use"
    :long-name "backend"
    :initial-value "haishoku"
    :key :backend)
   (clingon:make-option
    :flag
    :description "Disable time based theme checking"
    :long-name "time"
    :short-name #\t
    :key :time)
   (clingon:make-option
    :flag
    :description "Use light theme"
    :long-name "light"
    :short-name #\l
    :key :light)
   (clingon:make-option
    :flag
    :description "Use the current/old wallpaper"
    :long-name "old"
    :key :old)))

(defun cli-handler (cmd)
  "The handler function of our top-level command"
  (let* ((free-args (clingon:command-arguments cmd))
         (light (clingon:getopt cmd :light))
         (old (clingon:getopt cmd :old))
         (time (clingon:getopt cmd :time))
         (backend (clingon:getopt cmd :backend))
         (wallpaper (car free-args)))
    (when (ignore-errors (probe-file (merge-pathnames
                                      "before-hook.lisp" *config-directory*)))
      (format t "Before hook file located. Running the before hook...~%")
      (run-before-hook)
      (format t "Finished running before hook.~%"))
    (unless light
      (unless time
        (let ((current-hour (nth-value 2 (get-decoded-time))))
          (if (and (>= current-hour 6)
                   (< current-hour 17))
              (setf light t)))))
    (setf *light-theme-p* light)
    (setf *old-wallpaper-p* old)
    (let ((wallpaper-path
            (cond ((uiop:directory-exists-p wallpaper)
                   (get-random-image-from-dir wallpaper))
                  ((ignore-errors (probe-file wallpaper))
                   (uiop:native-namestring
                    (merge-pathnames (uiop:native-namestring wallpaper)
                                     (uiop:getcwd))))
                  (old
                   (uiop:read-file-string
                    (merge-pathnames "wal" *wal-directory*)))
                  (t (get-random-image-from-dir "~/Pictures/bg/")))))
      (setf *wallpaper-path* wallpaper-path)
      (format t "Using image at path: ~a~%" wallpaper-path)
      (pywal-run wallpaper-path backend :light light))
    (when (ignore-errors (probe-file (merge-pathnames
                                      "after-hook.lisp" *config-directory*)))
      (format t "After hook file located. Running the after hook...~%")
      (run-after-hook)
      (format t "Finished running after hook.~%"))
    (uiop:run-program "wal --preview" :output t)))

(defun cli-command ()
  "A command to the app."
  (clingon:make-command
   :name "pyrice"
   :description "Personal pywal and wpgtk wrapper"
   :version "testing"
   :authors '("Oliwier Czerwiński <oliwier.czerwi@proton.me>")
   :license "GPL 3.0"
   :options (cli-options)
   :handler #'cli-handler))

(defun main ()
  "The main entrypoint of our CLI program"
  (clingon:run (cli-command)))
