(use-package face-remap
  :ensure nil
  :commands text-scale-mode turn-off-text-scale
  :config
  (defun turn-off-text-scale ()
    (interactive)
    (text-scale-mode -1))
  (defun my-variable-pitch-mode-hook ()
    (setq-local cursor-type
                (if (bound-and-true-p buffer-face-mode)
                    'bar
                  t)))
  :hook (buffer-face-mode . my-variable-pitch-mode-hook))
