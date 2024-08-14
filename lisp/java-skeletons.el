;; -*- lexical-binding: t; -*-
(require 'treesit)

(defun inside-java-ts-class ()
  (treesit-parent-until
   (treesit-node-at (point)) (lambda (node)
                               (string= (treesit-node-type node)
                                        "class_declaration"))))

(defun insert-c-block ()
  (let (inside
        end
        (beg (point)))
    (insert "{\n")
    (setq inside (point))
    (insert "\n}")
    (setq end (point))
    (goto-char inside)
    (save-excursion
      (indent-region beg end))
    (indent-for-tab-command)))

(defun java-skeleton-class ()
  (interactive)
  (let* (inside
         end
         (beg (point))
         (inner (inside-java-ts-class))
         (name (if inner
                   (read-string "Inner class name: ")
                 (file-name-base buffer-file-name)))
         (privacy (completing-read "Class visibility: "
                                   (if (not inner) '("public"  "/* package */")
                                     '("public"
                                       "private"
                                       "public static"
                                       "private static"))))
         (type-type (completing-read
                     "Class type: "
                     '("class" "enum" "interface" "record" "abstract class"))))
    (insert privacy " " type-type " " name " ")
    (insert-c-block)))

(defun java-skeleton-method ()
  (interactive)
  (let ((privacy (completing-read "Method visibility: "
                                  '("public"
                                    "public static"
                                    "private"
                                    "private static"
                                    "protected")))
        (name (read-string "Method name: ")))
    (insert privacy " " )))

(defun java-skeleton-javadoc ()
  (interactive)
  (let (inside
        end
        (beg (pos-bol)))
    (insert "/**\n* ")
    (setq inside (point))
    (insert "\n*/")
    (setq end (point))
    (goto-char inside)
    (save-excursion
      (indent-region beg end))))

(defun java-javadoc-newline ()
  (interactive)
  (insert "\n* ")
  (indent-for-tab-command))

(defun lina-java-close-curly-bracket ()
  (interactive)
  (let (inside)
    (newline)
    (setq inside (point))
    (newline)
    (self-insert-command 1 ?})
    (goto-char inside)
    (indent-for-tab-command)
    (prog-fill-reindent-defun)))

(provide 'java-skeletons)
