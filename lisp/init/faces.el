(defconst my-linux-font '(:family "Iosevka Nerd Font" :height 105))

(use-package faces
  :ensure nil
  :custom-face
  (default ((((type x pgtk)) ,my-linux-font)))
  (fixed-pitch ((((type x pgtk)) ,my-linux-font)))
  (tab-line-tab-current ((t (:inherit unspecified)))))
