(use-package reformatter
  :ensure t
  :init
  ;; (reformatter-define nixfmt
  ;;   :program "nixfmt"
  ;;   :lighter " Fmt")
  ;; (reformatter-define yq-fmt
  ;;   :program "yamlfmt"
  ;;   :lighter " fmt"
  ;;   :args '("-in"))
  (reformatter-define tofu-fmt
    :program "tofu"
    :args '("fmt" "-")
    :lighter " TofuFmt")
  :config
  (define-advice reformatter--do-region (:around (fun &rest args)
                                                 quit-not-delete)
    (cl-letf (((symbol-function #'delete-windows-on)
               #'quit-windows-on))
      (apply fun args))))
