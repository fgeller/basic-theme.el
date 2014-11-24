(deftheme minimal "Minimalistic theme based on base16 colors.")

(defvar base16-colors-alist
  '(("black" . "#181818")
    ("gray1" . "#282828")
    ("gray2" . "#383838")
    ("gray3" . "#585858")
    ("gray4" . "#b8b8b8")
    ("gray5" . "#d8d8d8")
    ("gray6" . "#e8e8e8")
    ("white" . "#f8f8f8")
    ("red" . "#ab4642")
    ("orange" . "#dc9656")
    ("yellow" . "#f7ca88")
    ("green" . "#a1b56c")
    ("turquoise" . "#86c1b9")
    ("blue" . "#7cafc2")
    ("purple" . "#ba8baf")
    ("brown" . "#a16046")))

(defmacro base16/with-color-variables (&rest body)
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   base16-colors-alist))
     ,@body))

;;; Theme Faces
(base16/with-color-variables
  (custom-theme-set-faces
   'minimal
   `(default ((t (:foreground ,black :background ,white))))
   `(fringe ((t (:foreground ,gray3 :background ,gray6))))
   `(hi-yellow ((t (:foreground ,red :background ,white))))
   `(isearch ((t (:foreground nil :background ,gray5))))
   `(isearch-fail ((t (:foreground nil :background ,orange))))
   `(lazy-highlight ((t (:foreground nil :background ,gray5))))
   `(linum  ((t (:foreground ,gray3 :background ,gray6))))
   `(minibuffer-prompt ((t (:foreground ,red :background nil))))
   `(mode-line ((t (:foreground ,gray3 :background ,gray6 :box nil))))
   `(mode-line-buffer-id ((t (:weight normal))))
   `(mode-line-inactive ((t (:foreground ,gray3 :background ,gray6))))
   `(region ((t (:foreground nil :background ,yellow))))
   `(trailing-whitespace ((t (:foreground nil :background ,red))))
   `(button ((t (:foreground ,blue :underline nil))))

   `(helm-candidate-number ((t (:foreground ,gray3 :background ,gray6))))
   `(helm-header ((t (:foreground ,gray3 :background ,gray6 :box nil))))
   `(helm-selection ((t (:foreground nil :background ,yellow :underline nil))))
   `(helm-source-header ((t (:foreground ,gray3 :background ,blue :underline nil))))

   `(magit-item-highlight ((t (:foreground nil :background nil))))
   `(magit-section-title ((t (:foreground nil :background nil))))
   `(magit-branch ((t (:foreground nil :background nil))))
   `(magit-log-sha1 ((t (:foreground ,gray4 :background nil))))
   `(magit-log-author ((t (:foreground ,black :background nil))))
   `(magit-log-head-label-head ((t (:foreground ,gray4 :background nil))))
   `(magit-log-head-label-local ((t (:foreground ,gray4 :background nil))))
   `(magit-log-head-label-default ((t (:foreground ,gray4 :background nil))))
   `(magit-log-head-label-remote ((t (:foreground ,gray4 :background nil))))
   `(magit-log-head-label-tags ((t (:foreground ,gray4 :background nil))))
   `(magit-key-mode-button-face ((t (:foreground ,gray4 :background nil))))
   `(magit-key-mode-header-face ((t (:foreground ,gray4 :background nil))))
   `(magit-key-mode-switch-face ((t (:foreground ,blue :background nil))))
   ))

(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'minimal)
