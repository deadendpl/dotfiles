(setq frame-inhibit-implied-resize t ;; that makes startup slightly faster
      gc-cons-threshold most-positive-fixnum)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
