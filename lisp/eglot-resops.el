;;; eglot-resops.el --- add resource operation support to eglot  -*- lexical-binding: t; -*-

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'eglot)
(require 'map)
(require 'seq)

(defconst eglot-resops--lsp-interface-alist-appendage
  '((CreateFile (:kind :uri) (:options))
    (CreateFileOptions nil (:overwrite :ignoreIfExists))
    (DeleteFile (:kind :uri) (:options))
    (DeleteFileOptions nil (:recursive :ignoreIfNotExists))
    (RenameFile (:kind :oldUri :newUri) (:options))
    (RenameFileOptions nil (:overwrite :ignoreIfExists)))
  "Definitions of resource operation interfaces.
See https://microsoft.github.io/language-server-protocol/\
specifications/lsp/3.17/specification/#resourceChanges.")

(defun eglot-resops--append-lsp-interface-alist ()
  "Append resource operation interfaces to `eglot--lsp-interface-alist'."
  (unless (seq-every-p
           (lambda (interface)
             (assq interface eglot--lsp-interface-alist))
           (mapcar #'car eglot-resops--lsp-interface-alist-appendage))
    (nconc eglot--lsp-interface-alist
           eglot-resops--lsp-interface-alist-appendage)))

(defun eglot-resops--override-client-capabilities ()
  "Override `eglot-client-capabilities' to advertise resource operation \
support.
See https://microsoft.github.io/language-server-protocol/specifications\
/lsp/3.17/specification/#workspaceEditClientCapabilities."
  (cl-defmethod eglot-client-capabilities :around (server)
    (map-merge 'plist
               (cl-call-next-method server)
               '(:workspace
                 (:workspaceEdit
                  (:documentChanges
                   t
                   :resourceOperations
                   ["create"  "delete" "rename"]))))))

(defun eglot-resops--uri-to-relative-path (uri &optional root)
  (file-relative-name (eglot-uri-to-path uri)
                      (file-name-directory
                       (or root (project-root (eglot--current-project))))))

(defun eglot-resops--format-resource-operation (op)
  (let ((root (project-root (eglot--current-project))))
    (apply
     #'format
     (eglot--dcase op
       (((CreateFile) uri)
        `("create %s"
          ,(eglot-resops--uri-to-relative-path uri root)))
       (((DeleteFile) uri)
        `("DELETE %s!"
          ,(eglot-resops--uri-to-relative-path uri root)))
       (((RenameFile) oldUri newUri)
        `("RENAME %s to %s!"
          ,@(let ((old-path (eglot-resops--uri-to-relative-path oldUri root)))
              (list old-path
                    (eglot-resops--uri-to-relative-path
                     newUri
                     (eglot-uri-to-path oldUri))))))))))

(defun eglot-resops--eglot--apply-workspace-edit-around
    (eglot--apply-workspace-edit wedit &rest r)
  (let ((root (project-root (eglot--current-project)))
        (edits+ops (seq-group-by (lambda (edit)
                                   (if (seq-every-p
                                        (apply-partially #'plist-member edit)
                                        '(:textDocument :edits))
                                       'TextDocumentEdit
                                     'resource-operation))
                                 (plist-get wedit :documentChanges))))
    (print edits+ops)
    (plist-put wedit :documentChanges (cdr (assq 'TextDocumentEdit edits+ops)))
    (if-let ((resops (cdr (assq 'resource-operation edits+ops))))
        (progn
          (yes-or-no-p
           (format
            "[eglot-resops] Server wants to change the file system:
%s\nProceed? "
            (mapconcat #'eglot-resops--format-resource-operation resops "\n"))))
      (apply eglot--apply-workspace-edit wedit r))))

(defun eglot-resops--advise-eglot--apply-workspace-edit-around ()
  (advice-add #'eglot--apply-workspace-edit :around
              #'eglot-resops--eglot--apply-workspace-edit-around))

;;;###autoload
(defun eglot-resops-init ()
  "Set up resource operation support for Eglot."
  (eglot-resops--append-lsp-interface-alist)
  (eglot-resops--override-client-capabilities)
  (eglot-resops--advise-eglot--apply-workspace-edit-around))

(provide 'eglot-resops)

;;; eglot-resops.el ends here
