(use-package python
  :ensure nil
  :custom
  (python-flymake-command '("ruff"
                            "check"
                            "--quiet"
                            "--output-format=concise"
                            "--stdin-filename=stdin"))
  (python-indent-guess-indent-offset-verbose nil)
  (python-shell-dedicated 'project))
