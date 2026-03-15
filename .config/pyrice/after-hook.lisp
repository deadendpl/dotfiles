(in-package :pyrice)

(defun gsettings-run (light)
  "Run gsettings shell commands.
The commands will be different based on LIGHT's value."
  (let ((gsettings-icons-args
          `("gsettings" "set" "org.gnome.desktop.interface"
                        "icon-theme" ,(if light
                                          "Papirus-Light"
                                          "Papirus-Dark")))
        (gsettings-theme-args
          `("gsettings" "set" "org.gnome.desktop.interface"
                        "color-scheme" ,(if light
                                            "prefer-light"
                                            "prefer-dark"))))
    (uiop:run-program gsettings-icons-args)
    (uiop:run-program gsettings-theme-args)
    (format t "Set GTK preffered color scheme and icons.~%")))

(defun read-colors-file ()
  (with-open-file (stream (merge-pathnames "colors" *wal-directory*)
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
     (merge-pathnames "pywal.json" *wal-directory*)
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
                `("pgrep" "-x" ,name)
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
    (= returned-number 0)))

(defun command-running-p (command)
  "Return t if COMMAND is running.
It's running pgrep with exact name matching."
  (program-success-p (format nil "pgrep -x ~A" command)))

(defun waybar-setup ()
  "Start waybar or reload its CSS."
  (if (command-running-p "waybar")
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
  (when (command-running-p "qutebrowser")
    (uiop:launch-program "qutebrowser :config-source")
    (format t "Setting up qutebrowser in background.~%")))

(defun sway-setup ()
  "Change colors in sway"
  (when (command-running-p "sway")
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
  (if (command-running-p "swaybg")
      ;; swaybg is running
      (unless keep
        (find-and-kill-process "swaybg")
        (uiop:launch-program `("swaybg" "-m" "fit" "-i" ,wallpaper)))
      ;; swaybg not running
      (uiop:launch-program `("swaybg" "-m" "fit" "-i" ,wallpaper)))
  (format t "Set up wallpaper.~%"))

(defun swaync-setup ()
  "Reload swaync colors."
  (when (command-running-p "swaync")
    (uiop:run-program "swaync-client -rs")
    (format t "Reloaded swaync colors.~%")))

(defun emacs-ewal-setup (light)
  "Handles Emacs theme based on ewal.
It changes 2 variables depending on value of LIGHT."
  (when (command-running-p "emacs")
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

(defun emacs-modus-setup ()
  "Handles Emacs theme based on modus."
  (when (command-running-p "emacs")
    (uiop:run-program
     "emacsclient -e \"(modus-ewal-theme-regenerate-theme)\"")
    (format t "Reloaded Emacs theme.~%")))

(gsettings-run *light-theme-p*)
(swaybg-setup *wallpaper-path* *old-wallpaper-p*)
(sway-setup)
(waybar-setup)
(gradience-wrap)
(gtk2-icon-setup *light-theme-p*)
;; (emacs-setup *light-theme-p*)
(emacs-modus-setup)
(qutebrowser-setup)
(swaync-setup)
(uiop:launch-program "notify-send \"New rice applied\"")
