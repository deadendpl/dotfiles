(in-package :pyrice)

(defun gtk2-config-file-setup ()
  "Create GTK2 configuration file."
  (unless (probe-file
           (merge-pathnames "gtk-2.0/"
                            (uiop:ensure-directory-pathname
                             (uiop:getenv "XDG_CONFIG_HOME"))))
    (ensure-directories-exist
     (merge-pathnames "gtk-2.0/"
                      (uiop:ensure-directory-pathname
                       (uiop:getenv "XDG_CONFIG_HOME")))))
  (unless (probe-file (merge-pathnames
                       "gtk-2.0/gtkrc"
                       (uiop:ensure-directory-pathname
                        (uiop:getenv "XDG_CONFIG_HOME"))))
    (with-open-file (stream (merge-pathnames
                             "gtk-2.0/gtkrc"
                             (uiop:ensure-directory-pathname
                              (uiop:getenv "XDG_CONFIG_HOME")))
                            :direction :output
                            :if-exists nil
                            :if-does-not-exist :create)
      (format stream "gtk-icon-theme-name = \"Papirus-Dark\"~%")
      (format stream "gtk-theme-name = \"FlatColor\""))))

(gtk2-config-file-setup)
