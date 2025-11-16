(defpackage :pyrice
  (:use :cl))
(in-package :pyrice)

(ql:quickload :clingon)
(ql:quickload :uiop)

(defun get-random-file-from-dir (dir)
  "Get a random file from DIR."
  (let ((wallpaper-list)
        (*random-state* (make-random-state t)))
    (uiop/filesystem:collect-sub*directories
     dir
     (constantly t)
     (constantly t)
     (lambda (dir)
       (mapcar (lambda (item)
                 (push item wallpaper-list))
               (uiop:directory-files dir))))
    (uiop:native-namestring
     (nth (random (1- (length wallpaper-list))) wallpaper-list))))

(defun pywal-run (wallpaper backend &key (light nil))
  "Run pywal and wpgtk commands using BACKEND on WALLPAPER.
Light theme is generated if LIGHT-P is non-nil."
  (let ((wpg-args `("wpg" "--noreload" "--backend" ,backend
                          "-ns" ,wallpaper))
        (wal-args `("wal" "--cols16" "--backend" ,backend
                          "-nei" ,wallpaper))
        (gsettings-icons-args
          `("gsettings" "set" "org.gnome.desktop.interface" "icon-theme"
                        ,(if light
                             "Papirus-Light"
                             "Papirus-Dark")))
        (gsettings-theme-args
          `("gsettings" "set" "org.gnome.desktop.interface" "color-scheme"
                        ,(if light
                             "prefer-light"
                             "prefer-dark"))))
    (when light
      (setf wpg-args (append wpg-args '("-L")))
      (setf wal-args (append wal-args '("-l"))))
    (uiop:run-program wpg-args)
    (uiop:run-program wal-args)
    (format t "Generated pywal theme.~%")
    (uiop:run-program gsettings-icons-args)
    (uiop:run-program gsettings-theme-args)
    (format t "Set GTK preffered color scheme and icons.~%")))

(defun read-colors-file ()
  (with-open-file (stream (merge-pathnames
                           "wal/colors"
                           (uiop:ensure-directory-pathname
                            (uiop:getenv "XDG_CACHE_HOME")))
                          :direction :input)
    (loop for line = (read-line stream nil nil)
          while line collect line)))

(defun gradience-wrap ()
  "Apply new pywal theme with gradience to libadwaita."
  (let ((gradience-dir (uiop:ensure-directory-pathname
                        (merge-pathnames
                         "presets/user"
                         (uiop:ensure-directory-pathname
                          (uiop:getenv "XDG_CONFIG_HOME"))))))
    (ensure-directories-exist gradience-dir)
    (uiop:copy-file
     (merge-pathnames "wal/pywal.json"
                      (uiop:ensure-directory-pathname
                       (uiop:getenv "XDG_CACHE_HOME")))
     (merge-pathnames "pywal.json" gradience-dir))
    (uiop:launch-program '("gradience-cli" "apply" "-n"
                           "pywal" "--gtk" "both"))
    (format t "Applying Gradience theme in background.~%")))

(defun gtk2-icon-setup (light)
  "Change GTK 2 configuration to use Papirus light or dark icons
depending on LIGHT's value"
  (let ((config-file (uiop:native-namestring
                      (merge-pathnames
                       "gtk-2.0/gtkrc"
                       (uiop:ensure-directory-pathname
                        (uiop:getenv "XDG_CONFIG_HOME"))))))
    (when (probe-file config-file)
      (uiop:run-program `("sed" "-i"
                                ,(if light
                                     "s/Papirus-Dark/Papirus-Light/g"
                                     "s/Papirus-Light/Papirus-Dark/g")
                                ,config-file))))
  (format t "Set up GTK 2 icons.~%"))

(defun find-and-kill-process (name &key (signal "-TERM"))
  "Find processes whose command line contains NAME and send them SIGNAL."
  (let* ((pids (uiop:run-program
                `("pgrep" ,name)
                :output :string
                :ignore-error-status t))
         (pid-list (mapcar (lambda (item)
                             (string-trim '(#\Newline #\Space) item))
                           (split-sequence:split-sequence
                            #\Newline pids))))
    (dolist (pid pid-list)
      (unless (equal pid "")
        (uiop:run-program `("kill" ,signal ,pid))))))

(defun program-success-p (program)
  "Return t if PROGRAM succeeded or nil."
  (let ((returned-number (nth-value 2 (uiop:run-program
                                       program :ignore-error-status t))))
    (if (= returned-number 0)
        t
        nil)))

(defun waybar-setup ()
  "Start waybar or reload its CSS."
  (if (program-success-p "pgrep waybar")
      (progn
        (uiop:run-program
         `("sed" "-i" "1i\\\\" ,(uiop:native-namestring
                                 (merge-pathnames
                                  "waybar/style.css"
                                  (uiop:ensure-directory-pathname
                                   (uiop:getenv "XDG_CONFIG_HOME"))))))
        (uiop:run-program
         "sed -i 1d $XDG_CONFIG_HOME/waybar/style.css"
         :force-shell t))
      (uiop:launch-program "waybar"))
  (format t "Set up waybar.~%"))

(defun qutebrowser-setup ()
  "Reload qutebrowser if it's running."
  (when (program-success-p "pgrep qutebrowser")
    (uiop:launch-program "qutebrowser :config-source")
    (format t "Setting up qutebrowser in background.~%")))

(defun sway-setup ()
  "Change colors in sway"
  (when (program-success-p "pgrep sway")
    (let* ((colors (read-colors-file))
           (background (car colors))
           (foreground (car (last colors))))
      (dolist (item `(("client.focused" ,background ,(nth 11 colors)
                                        ,foreground)
                      ("client.focused_inactive" ,background ,background
                                                 ,foreground)
                      ("client.unfocused" ,background ,background
                                          ,foreground)
                      ("client.urgent" ,background ,background
                                       ,foreground)
                      ("client.placeholder" ,background ,background
                                            ,foreground)))
        (uiop:run-program `("swaymsg" ,@item))))
    (format t "Changed sway colors.~%")))

(defun swaybg-setup (wallpaper &optional keep)
  "Set the wallpaper using swaybg.
If no swaybg process is running, start it.
If one is running and KEEP is NIL, kill it and start a new one.
If one is running and KEEP is non-NIL, do nothing."
  (if (program-success-p "pgrep swaybg")
      ;; swaybg is running
      (unless keep
        (find-and-kill-process "swaybg")
        (uiop:launch-program `("swaybg" "-m" "fit" "-i" ,wallpaper)))
      ;; swaybg not running
      (uiop:launch-program `("swaybg" "-m" "fit" "-i" ,wallpaper)))
  (format t "Set up wallpaper.~%"))

(defun swaync-setup ()
  "Reload swaync colors."
  (when (program-success-p "pgrep swaync")
    (uiop:run-program "swaync-client -rs")
    (format t "Reloaded swaync colors.~%")))

(defun emacs-setup (light)
  "Handles Emacs theme.
It changes 2 variables depending on value of LIGHT."
  (when (program-success-p "pgrep emacs")
    (let ((args `("emacsclient"
                  "-e"
                  ,(concatenate 'string
                                "(setq ewal-doom-one-brighter-comments "
                                (if light
                                    "t "
                                    "nil ")
                                "ewal-doom-one-comment-bg nil "
                                "ewal-dark-palette-p "
                                (if light
                                    "nil)"
                                    "t)")))))
      (uiop:run-program args :ignore-error-status t))
    (uiop:run-program "emacsclient -e \"(load-theme 'ewal-doom-one t)\""
                      :ignore-error-status t)
    (format t "Reloaded Emacs theme.~%")))

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
    (unless light
      (unless time
        (let ((current-hour (nth-value 2 (get-decoded-time))))
          (if (and (>= current-hour 8)
                   (< current-hour 16))
              (setf light t)))))
    (let ((wallpaper-path
            (cond ((uiop:directory-exists-p wallpaper)
                   (get-random-file-from-dir wallpaper))
                  ((ignore-errors (probe-file wallpaper))
                   (uiop:native-namestring
                    (merge-pathnames (uiop:native-namestring wallpaper)
                                     (uiop:getcwd))))
                  (old
                   (uiop:read-file-string
                    (merge-pathnames
                     "wal/wal"
                     (uiop:ensure-directory-pathname
                      (uiop:getenv "XDG_CACHE_HOME")))))
                  (t (get-random-file-from-dir "~/Pictures/bg/")))))
      (format t "Using image at path: ~a~%" wallpaper-path)
      (pywal-run wallpaper-path backend :light light)
      (swaybg-setup wallpaper-path old))
    (sway-setup)
    (waybar-setup)
    (gradience-wrap)
    (gtk2-icon-setup light)
    (emacs-setup light)
    (qutebrowser-setup)
    (swaync-setup)
    (uiop:run-program "wal --preview" :output t)
    (uiop:launch-program "notify-send \"New rice applied\"")))

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
