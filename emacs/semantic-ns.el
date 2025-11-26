;;; semantic-ns.el --- IDE support for Semantic Namespace Framework -*- lexical-binding: t -*-

;; Copyright (C) 2024
;; Author: SOODF Project
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (cider "1.0"))
;; Keywords: clojure, semantic, architecture

;;; Commentary:
;; 
;; Provides interactive commands for querying and navigating
;; semantic namespace registries via CIDER REPL.
;;
;; Commands:
;;   M-x semantic-ns-list-entities     - List all registered entities
;;   M-x semantic-ns-list-aspects      - List all semantic aspects
;;   M-x semantic-ns-find-by-aspect    - Find entities with aspect
;;   M-x semantic-ns-entity-info       - Show entity details
;;   M-x semantic-ns-data-flow         - Show data flow for function
;;   M-x semantic-ns-check-axioms      - Run axiom validation
;;   M-x semantic-ns-dependents        - Find what depends on entity
;;   M-x semantic-ns-dependencies      - Find entity's dependencies
;;   M-x semantic-ns-producers         - Find producers of data key
;;   M-x semantic-ns-consumers         - Find consumers of data key
;;   M-x semantic-ns-generate-docs     - Generate markdown docs

;;; Code:

(require 'cider)

(defgroup semantic-ns nil
  "Semantic Namespace IDE support."
  :group 'clojure
  :prefix "semantic-ns-")

(defvar semantic-ns-ide-ns "semantic-namespace.runtime.ide"
  "Clojure namespace containing IDE functions.")

(defface semantic-ns-header-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for headers in semantic-ns buffers.")

(defface semantic-ns-entity-face
  '((t :inherit font-lock-function-name-face))
  "Face for entity names.")

(defface semantic-ns-aspect-face
  '((t :inherit font-lock-type-face))
  "Face for aspect keywords.")

(defface semantic-ns-error-face
  '((t :inherit error))
  "Face for errors.")

(defface semantic-ns-warning-face
  '((t :inherit warning))
  "Face for warnings.")

(defface semantic-ns-success-face
  '((t :inherit success))
  "Face for success indicators.")

;;; Helpers

(defun semantic-ns--eval (form)
  "Evaluate FORM in CIDER and return parsed result."
  (let* ((code (format "(do (require '%s) %s)" semantic-ns-ide-ns form))
         (result (cider-nrepl-sync-request:eval code)))
    (when-let ((value (nrepl-dict-get result "value")))
      (car (read-from-string value)))))

(defun semantic-ns--buffer (name)
  "Get or create semantic-ns buffer with NAME."
  (let ((buf (get-buffer-create (format "*semantic-ns: %s*" name))))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (semantic-ns-mode))
    buf))

(defun semantic-ns--insert-header (text)
  "Insert TEXT as a header."
  (insert (propertize text 'face 'semantic-ns-header-face) "\n")
  (insert (make-string (length text) ?=) "\n\n"))

(defun semantic-ns--insert-entity (entity)
  "Insert ENTITY with proper face."
  (insert (propertize (format "%s" entity) 'face 'semantic-ns-entity-face)))

(defun semantic-ns--insert-aspect (aspect)
  "Insert ASPECT with proper face."
  (insert (propertize (format "%s" aspect) 'face 'semantic-ns-aspect-face)))

(defun semantic-ns--completing-read-entity (prompt)
  "Read entity with completion using PROMPT."
  (let ((entities (semantic-ns--eval "(mapv :dev-id (list-all-entities))")))
    (completing-read prompt (mapcar #'symbol-name entities))))

(defun semantic-ns--completing-read-aspect (prompt)
  "Read aspect with completion using PROMPT."
  (let ((aspects (semantic-ns--eval "(list-aspects)")))
    (completing-read prompt (mapcar #'symbol-name aspects))))

(defun semantic-ns--completing-read-data-key (prompt)
  "Read data key with completion using PROMPT."
  (let ((keys (semantic-ns--eval "(complete-data-key \"\")")))
    (completing-read prompt keys)))

;;; Major mode

(defvar semantic-ns-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'revert-buffer)
    (define-key map (kbd "RET") 'semantic-ns-entity-info-at-point)
    (define-key map (kbd "a") 'semantic-ns-find-by-aspect)
    (define-key map (kbd "e") 'semantic-ns-entity-info)
    (define-key map (kbd "d") 'semantic-ns-data-flow)
    (define-key map (kbd "c") 'semantic-ns-check-axioms)
    map)
  "Keymap for semantic-ns-mode.")

(define-derived-mode semantic-ns-mode special-mode "Semantic-NS"
  "Major mode for semantic namespace query results."
  (setq buffer-read-only t))

;;; Commands

;;;###autoload
(defun semantic-ns-list-entities ()
  "List all registered semantic entities."
  (interactive)
  (let* ((entities (semantic-ns--eval "(list-all-entities)"))
         (buf (semantic-ns--buffer "entities")))
    (with-current-buffer buf
      (semantic-ns--insert-header "Registered Entities")
      (let ((by-type (seq-group-by (lambda (e) (cdr (assq 'type e))) entities)))
        (dolist (type '(endpoint function component schema other))
          (when-let ((items (cdr (assq type by-type))))
            (insert (propertize (format "%ss (%d)\n" (capitalize (symbol-name type)) (length items))
                                'face 'semantic-ns-header-face))
            (dolist (item items)
              (insert "  ")
              (semantic-ns--insert-entity (cdr (assq 'dev-id item)))
              (insert "\n"))
            (insert "\n"))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-list-aspects ()
  "List all semantic aspects in the registry."
  (interactive)
  (let* ((aspects (semantic-ns--eval "(list-aspects)"))
         (buf (semantic-ns--buffer "aspects")))
    (with-current-buffer buf
      (semantic-ns--insert-header "Semantic Aspects")
      (dolist (aspect aspects)
        (insert "  ")
        (semantic-ns--insert-aspect aspect)
        (insert "\n"))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-find-by-aspect (aspect)
  "Find all entities with ASPECT."
  (interactive
   (list (semantic-ns--completing-read-aspect "Aspect: ")))
  (let* ((entities (semantic-ns--eval (format "(entities-with-aspect :%s)" aspect)))
         (buf (semantic-ns--buffer (format "aspect:%s" aspect))))
    (with-current-buffer buf
      (semantic-ns--insert-header (format "Entities with %s" aspect))
      (if entities
          (dolist (entity entities)
            (insert "  ")
            (semantic-ns--insert-entity entity)
            (insert "\n"))
        (insert "  (none found)\n"))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-entity-info (entity)
  "Show detailed info for ENTITY."
  (interactive
   (list (semantic-ns--completing-read-entity "Entity: ")))
  (let* ((info (semantic-ns--eval (format "(entity-info :%s)" entity)))
         (buf (semantic-ns--buffer (format "entity:%s" entity))))
    (with-current-buffer buf
      (semantic-ns--insert-header (format "Entity: %s" entity))
      (when info
        (insert (propertize "Aspects:\n" 'face 'semantic-ns-header-face))
        (dolist (aspect (cdr (assq 'aspects info)))
          (insert "  ")
          (semantic-ns--insert-aspect aspect)
          (insert "\n"))
        (insert "\n")
        
        (when-let ((ctx (cdr (assq 'context info))))
          (insert (propertize "Context (inputs):\n" 'face 'semantic-ns-header-face))
          (dolist (k ctx)
            (insert (format "  %s\n" k)))
          (insert "\n"))
        
        (when-let ((resp (cdr (assq 'response info))))
          (insert (propertize "Response (outputs):\n" 'face 'semantic-ns-header-face))
          (dolist (k resp)
            (insert (format "  %s\n" k)))
          (insert "\n"))
        
        (when-let ((deps (cdr (assq 'deps info))))
          (when (> (length deps) 0)
            (insert (propertize "Dependencies:\n" 'face 'semantic-ns-header-face))
            (dolist (d deps)
              (insert "  ")
              (semantic-ns--insert-entity d)
              (insert "\n"))
            (insert "\n"))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-data-flow (entity)
  "Show data flow for ENTITY."
  (interactive
   (list (semantic-ns--completing-read-entity "Function: ")))
  (let* ((flow (semantic-ns--eval (format "(data-flow :%s)" entity)))
         (buf (semantic-ns--buffer (format "data-flow:%s" entity))))
    (with-current-buffer buf
      (semantic-ns--insert-header (format "Data Flow: %s" entity))
      (if flow
          (dolist (item flow)
            (let ((needs (cdr (assq 'needs item)))
                  (produced-by (cdr (assq 'produced-by item)))
                  (satisfied (cdr (assq 'satisfied? item))))
              (insert (format "  %s <- " needs))
              (if (and produced-by (> (length produced-by) 0))
                  (progn
                    (semantic-ns--insert-entity (car produced-by))
                    (insert (if satisfied
                                (propertize " âœ“" 'face 'semantic-ns-success-face)
                              (propertize " ?" 'face 'semantic-ns-warning-face))))
                (insert (propertize "(endpoint input)" 'face 'font-lock-comment-face)))
              (insert "\n")))
        (insert "  (no data flow info)\n"))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-check-axioms ()
  "Run axiom validation and show results."
  (interactive)
  (let* ((result (semantic-ns--eval "(check-axioms)"))
         (buf (semantic-ns--buffer "axioms")))
    (with-current-buffer buf
      (semantic-ns--insert-header "Axiom Validation")
      (if (cdr (assq 'valid? result))
          (insert (propertize "âœ“ All axioms pass\n\n" 'face 'semantic-ns-success-face))
        (insert (propertize "âœ— Validation failed\n\n" 'face 'semantic-ns-error-face)))
      
      (when-let ((errors (cdr (assq 'errors result))))
        (when (> (length errors) 0)
          (insert (propertize "Errors:\n" 'face 'semantic-ns-error-face))
          (dolist (e errors)
            (insert (format "  â€¢ %s: %s\n"
                            (cdr (assq 'axiom e))
                            (cdr (assq 'message e)))))
          (insert "\n")))
      
      (when-let ((warnings (cdr (assq 'warnings result))))
        (when (> (length warnings) 0)
          (insert (propertize "Warnings:\n" 'face 'semantic-ns-warning-face))
          (dolist (w warnings)
            (insert (format "  â€¢ %s: %s\n"
                            (cdr (assq 'axiom w))
                            (cdr (assq 'message w)))))
          (insert "\n")))
      
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-dependents (entity)
  "Find what depends on ENTITY."
  (interactive
   (list (semantic-ns--completing-read-entity "Entity: ")))
  (let* ((deps (semantic-ns--eval (format "(dependents-of :%s)" entity)))
         (buf (semantic-ns--buffer (format "dependents:%s" entity))))
    (with-current-buffer buf
      (semantic-ns--insert-header (format "Dependents of %s" entity))
      (if (and deps (> (length deps) 0))
          (dolist (d deps)
            (insert "  ")
            (semantic-ns--insert-entity d)
            (insert "\n"))
        (insert "  (nothing depends on this)\n"))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-dependencies (entity)
  "Find ENTITY's dependencies."
  (interactive
   (list (semantic-ns--completing-read-entity "Entity: ")))
  (let* ((deps (semantic-ns--eval (format "(dependencies-of :%s)" entity)))
         (buf (semantic-ns--buffer (format "dependencies:%s" entity))))
    (with-current-buffer buf
      (semantic-ns--insert-header (format "Dependencies of %s" entity))
      (if (and deps (> (length deps) 0))
          (dolist (d deps)
            (insert "  ")
            (semantic-ns--insert-entity d)
            (insert "\n"))
        (insert "  (no dependencies)\n"))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-producers (data-key)
  "Find functions that produce DATA-KEY."
  (interactive
   (list (semantic-ns--completing-read-data-key "Data key: ")))
  (let* ((producers (semantic-ns--eval (format "(producers-of %s)" data-key)))
         (buf (semantic-ns--buffer (format "producers:%s" data-key))))
    (with-current-buffer buf
      (semantic-ns--insert-header (format "Producers of %s" data-key))
      (if (and producers (> (length producers) 0))
          (dolist (p producers)
            (insert "  ")
            (semantic-ns--insert-entity p)
            (insert "\n"))
        (insert "  (no producers - endpoint input?)\n"))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-consumers (data-key)
  "Find functions that consume DATA-KEY."
  (interactive
   (list (semantic-ns--completing-read-data-key "Data key: ")))
  (let* ((consumers (semantic-ns--eval (format "(consumers-of %s)" data-key)))
         (buf (semantic-ns--buffer (format "consumers:%s" data-key))))
    (with-current-buffer buf
      (semantic-ns--insert-header (format "Consumers of %s" data-key))
      (if (and consumers (> (length consumers) 0))
          (dolist (c consumers)
            (insert "  ")
            (semantic-ns--insert-entity c)
            (insert "\n"))
        (insert "  (no consumers)\n"))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-generate-docs ()
  "Generate and display markdown documentation."
  (interactive)
  (let* ((md (semantic-ns--eval "(generate-markdown)"))
         (buf (semantic-ns--buffer "docs")))
    (with-current-buffer buf
      (insert md)
      (goto-char (point-min))
      (when (fboundp 'markdown-mode)
        (markdown-mode))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-execution-order ()
  "Show topologically sorted execution order."
  (interactive)
  (let* ((order (semantic-ns--eval "(execution-order)"))
         (buf (semantic-ns--buffer "execution-order")))
    (with-current-buffer buf
      (semantic-ns--insert-header "Execution Order (by data flow)")
      (let ((n 1))
        (dolist (entity order)
          (insert (format "  %d. " n))
          (semantic-ns--insert-entity entity)
          (insert "\n")
          (setq n (1+ n))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun semantic-ns-system-summary ()
  "Show system overview."
  (interactive)
  (let* ((summary (semantic-ns--eval "(system-summary)"))
         (buf (semantic-ns--buffer "summary")))
    (with-current-buffer buf
      (semantic-ns--insert-header "System Summary")
      (insert (format "%s\n\n" (cdr (assq 'summary summary))))
      (insert (propertize "Domains:\n" 'face 'semantic-ns-header-face))
      (dolist (d (cdr (assq 'domains summary)))
        (insert (format "  %s\n" d)))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;; Hydra (optional)

(with-eval-after-load 'hydra
  (defhydra semantic-ns-hydra (:color blue :hint nil)
    "
Semantic Namespace
â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
_e_ entities    _a_ by aspect    _i_ entity info
_d_ data flow   _x_ exec order   _c_ check axioms
_p_ producers   _u_ consumers    _s_ summary
_D_ deps of     _R_ dependents   _g_ gen docs
_q_ quit
"
    ("e" semantic-ns-list-entities)
    ("a" semantic-ns-find-by-aspect)
    ("i" semantic-ns-entity-info)
    ("d" semantic-ns-data-flow)
    ("x" semantic-ns-execution-order)
    ("c" semantic-ns-check-axioms)
    ("p" semantic-ns-producers)
    ("u" semantic-ns-consumers)
    ("s" semantic-ns-system-summary)
    ("D" semantic-ns-dependencies)
    ("R" semantic-ns-dependents)
    ("g" semantic-ns-generate-docs)
    ("q" nil)))

(provide 'semantic-ns)
;;; semantic-ns.el ends here
