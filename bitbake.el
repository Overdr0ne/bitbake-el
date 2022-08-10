;;; bitbake.el --- Running bitbake from emacs

;; Author: Damien Merenne
;; URL: https://github.com/canatella/bitbake-el
;; Created: 2014-02-11
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24.1") (dash "2.6.0") (mmm-mode "0.5.4") (s "1.10.0"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package provides integration of the Yocto Bitbake tool with
;; emacs. Its main features are:
;;
;; - interacting with the bitbake script so that you can run bitbake
;;   seamlessly from emacs. If your editing a recipe, recompiling is
;;   just one M-x bitbake-recompile command away,
;; - deploying recipes output directly to your target device over ssh
;;   for direct testing (if your image supports read-write mode),
;; - generating wic images,
;; - a global minor mode providing menu and shortcuts,
;; - an mmm based mode to edit bitbake recipes.


;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'ansi-color)
(require 'comint)
(require 'mmm-mode)
(require 's)
(require 'dash)
(eval-when-compile (require 'cl-lib))

;;; User customizable variables
(defgroup bitbake nil
  "Run bitbake commands from emacs"
  :group 'processes)

(defcustom bitbake-poky-directory "/opt/poky"
  "The default yocto poky directory."
  :type '(directory)
  :group 'bitbake)

(defcustom bitbake-build-directory ""
  "The default yocto build directory."
  :type '(directory)
  :group 'bitbake)

(defcustom bitbake-server-host nil
  "The name or IP address to use as host address of the server process.  If set, the server accepts remote connections; otherwise it is local."
  :type '(choice
          (string :tag "Name ro IP address")
          (const :tag "Local" nil))
  :group 'bitbake)

;;;###autoload
(put 'bitbake-server-host 'risky-local-variable t)

(defcustom bitbake-server-port nil
  "The port number that the server process should listen on.  A nil value means to use a random port number."
  :group 'bitbake
  :type '(choice
          (string :tag "Port number")
          (const :tag "Random" nil))
  :version "24.1")
;;;###autoload
(put 'server-port 'risky-local-variable t)

(defcustom bitbake-deploy-ssh-host nil
  "The host where to deploy package over ssh."
  :group 'bitbake
  :type '(string)
  :version "24.1")

(defcustom bitbake-wic-definition-file nil
  "Path the wic definition file (wks file) to use for creating hdd image.

If a relative path is used, it will be relative to the poky directory."
  :group 'bitbake
  :type '(string)
  :version "24.1")

(defcustom bitbake-image-path nil
  "The path to the bitbake image."
  :group 'bitbake
  :type '(string)
  :version "24.1")

(defcustom bitbake-flash-device nil
  "The device file where to dd the hdd image.

If using a USB stick to boot the target platform, use a udev rule
to create a link to that disk and use the link name
here.  Calling bitbake-flash will copy the hdd image on the usb disk if present."
  :group 'bitbake
  :type '(string)
  :version "24.1")

;;; Local variables
(defvar bitbake-force nil "Global variable dictating whether or not to force tasks.")
(defvar bitbake-current-server-host nil "The actual host name or IP address of the bitbake server instance.")
(defvar bitbake-current-server-port nil "The actual port of the bitbake server instance.")
(defvar bitbake-current-poky-directory nil "The actual directory holding bitbake binaries.")
(defvar bitbake-current-build-directory nil "The actual build directory.")
(defvar bitbake-recipes-cache '() "Cache of bitbake recipes.")
(defvar bitbake-recipe-variables-cache '() "Cache of bitbake recipe variables.")
(defvar bitbake-recipe-tasks-cache '() "Cache of bitbake recipe variables.")
(defvar bitbake-recipe-history nil "History list of recipe names entered in the minibuffer.")
(defvar bitbake-image-history nil "History list of image names entered in the minibuffer.")
(defvar bitbake-task-history nil "History list of task names entered in the minibuffer.")
(defvar bitbake-task-queue nil "List of task left to execute.")
(defvar bitbake-current-task nil "The currently active task.")
(defvar bitbake-current-command nil "The running command.")
(defvar bitbake-last-disk-image nil "The last build disk image file.")
(defvar bitbake-buffer-prompt "/////---bitbake$ " "The prompt used in the bitbake buffer.")
(defvar bitbake-buffer-prompt-regexp (concat "^" (regexp-quote bitbake-buffer-prompt)) "A regexp matching the prompt")

(make-variable-buffer-local 'bitbake-last-disk-image)

;;; Minor mode functions

(defun bitbake-toggle-force ()
  "Toggle the `bitbake-force' variable."
  (interactive)
  (setq bitbake-force (not bitbake-force))
  (if bitbake-force
      (message "Bitbake: Force enabled")
    (message "Bitbake: Force disabled")))

(defun bitbake-read-poky-directory ()
  "Read the poky directory."
  (read-directory-name "Poky directory" bitbake-poky-directory bitbake-poky-directory))

(defun bitbake-read-build-directory ()
  "Read the build directory."
  (read-directory-name "Build directory" bitbake-build-directory bitbake-build-directory))

(defun bitbake-wait-for-prompt ()
  "Wait for BITBAKE-BUFFER-PROMPT to appear in current buffer."
  (let ((bol))
    (save-excursion
      (beginning-of-line)
      (setq bol (point)))
    (while (not (search-backward bitbake-buffer-prompt bol t))
      (sleep-for 0 300))
    (goto-char (process-mark (get-buffer-process (current-buffer))))))

(defun bitbake-buffer ()
  "Fetch or create the bitbake buffer."
  (let ((buffer-name "*bitbake*")
        (default-directory bitbake-build-directory))
    (or (get-buffer buffer-name)
        (progn
          (let ((buffer (shell buffer-name)))
            (with-current-buffer buffer
              (setq-local comint-move-point-for-output t)
              (setq-local comint-prompt-regexp bitbake-buffer-prompt-regexp)
              (process-send-string (current-buffer) (format "export PS1='%s' && cd %s\n" bitbake-buffer-prompt default-directory))
              (bitbake-wait-for-prompt)
              (comint-send-input nil t)
              (erase-buffer)
              (bitbake-wait-for-prompt)
              buffer))))))

(defun bitbake-capture-buffer ()
  "Fetch or create the bitbake capture buffer."
  (let ((buffer (get-buffer-create "*bitbake-temp*")))
    (with-current-buffer buffer
      (text-mode)
      buffer)))

(defun bitbake-add-path (path)
  "Add PATH to the current path."
  (let ((components (s-split ":" (or (getenv "PATH") "") t)))
    (unless (member path components)
      (setenv "PATH" (s-join ":" (-distinct (cons path components)))))))

(defun bitbake-remove-path (path)
  "Remove PATH from the current path."
  (let ((components (s-split ":" (or (getenv "PATH") "") t)))
    (when (member path components)
      (setenv "PATH" (s-join ":" (delete path components))))))

(defun bitbake-setup-environment (poky-directory build-directory)
  "Add POKY-DIRECTORY to PATH environment variable and set BBPATH to BUILD-DIRECTORY."
  (setenv "BBPATH" build-directory)
  (setenv "BUILDDIR" build-directory)
  (bitbake-add-path (format "%sscripts" poky-directory))
  (bitbake-add-path (format "%sbitbake/bin" poky-directory))
  (message "Bitbake: updated path to %s" (getenv "PATH")))

(defun bitbake-cleanup-environment (poky-directory)
  "Remove POKY-DIRECTORY from PATH environment variable and unset BBPATH."
  (setenv "BBPATH")
  (setenv "BUILDDIR")
  (bitbake-remove-path (format "%sscripts" poky-directory))
  (bitbake-remove-path (format "%sbitbake/bin" poky-directory)))

(defun bitbake-shell-command (command &optional capture-buffer)
  "Run shell COMMAND in bitbake buffer.

Capture output in CAPTURE-BUFFER."
  (let ((capture-buffer (if capture-buffer capture-buffer (bitbake-capture-buffer))))
    (with-current-buffer capture-buffer
      (setq-local comint-prompt-regexp bitbake-buffer-prompt-regexp)
      (erase-buffer))
    (with-current-buffer (bitbake-buffer)
      (setq-local comint-prompt-regexp bitbake-buffer-prompt-regexp)
      (setq bitbake-current-command command)
      (comint-redirect-send-command (format "echo '%s' && %s 2>&1" command command) capture-buffer t t))))

(defun bitbake-raw (argument-string &optional capture-buffer)
  "Call bitbake with raw ARGUMENT-STRING in CAPTURE-BUFFER."
  (interactive "sBitbake arguments: ")
  (bitbake-shell-command (string-join (list "bitbake" argument-string) " ") capture-buffer))

(defun bitbake-clear-caches ()
  "Clear all bitbake caches."
  (interactive)
  (setq bitbake-recipes-cache '()
        bitbake-recipe-variables-cache '()
        bitbake-recipe-tasks-cache '()))

;;;###autoload
(defun bitbake-start-server (poky-directory build-directory)
  "Start a bitbake server instance.

Start a bitbake server using POKY-DIRECTORY to find the bitbake
binary and BUILD-DIRECTORY as the build directory."
  (interactive (let ((ask (consp current-prefix-arg)))
                 (list (if (or ask (not bitbake-poky-directory))
                           (bitbake-read-poky-directory)
                         bitbake-poky-directory)
                       (if (or ask (not bitbake-build-directory))
                           (bitbake-read-build-directory)
                         bitbake-build-directory))))

  ;; stop server if running
  (when (getenv "BBSERVER")
    (message "Stopping other instance of bitbake server")
    (bitbake-stop-server))

  ;; cleanup caches
  (setq bitbake-recipes-cache '()
        bitbake-recipe-variables-cache '()
        bitbake-recipe-tasks-cache '())

  ;; prepare environment
  (setq bitbake-current-server-host (or bitbake-server-host "localhost")
        bitbake-current-server-port (or bitbake-server-port (+ 1024 (random (- 65535 1024))))
        bitbake-current-poky-directory (file-name-as-directory poky-directory)
        bitbake-current-build-directory (file-name-as-directory build-directory))
  (bitbake-setup-environment  bitbake-current-poky-directory bitbake-current-build-directory)

  ;; start the server
  (shell-command (format "cd /tmp && bitbake -B %s:%s --server-only"
                         bitbake-current-server-host bitbake-current-server-port))

  (setenv "BBSERVER" (format "%s:%s" bitbake-current-server-host bitbake-current-server-port)))

(defun bitbake-stop-server ()
  "Stop the bitbake server instance."
  (interactive)
  (unless (getenv "BBSERVER")
    (user-error "No bitbake server running"))
  (message "Bitbake: stopping server")
  (call-process-shell-command (format "bitbake --kill-server --remote-server=%s" (getenv "BBSERVER")))
  (delete-file (concat (getenv "BBPATH") "bitbake.lock"))
  (delete-file (concat (getenv "BBPATH") "bitbake.sock"))
  (bitbake-cleanup-environment bitbake-current-poky-directory)
  (message "Bitbake: server is down, cleaning up")
  (setenv "BBSERVER")
  (setq bitbake-recipes-cache nil
        bitbake-recipe-variables-cache nil
        bitbake-recipe-tasks-cache nil
        bitbake-current-server-host nil
        bitbake-current-server-port nil
        bitbake-current-poky-directory nil
        bitbake-current-build-directory nil
        bitbake-current-task nil
        bitbake-task-queue nil)
  (setq-local kill-buffer-query-functions '())
  (let* ((buffer (get-buffer "*bitbake*"))
         (process (get-buffer-process buffer)))
    (when process (kill-process process))
    (when buffer (kill-buffer buffer))))

(defun bitbake-s-command (command message)
  "Run COMMAND synchronously, sending output to bitbake-capture-buffer.

If COMMAND fails, raise user error with MESSAGE."
  (if (getenv "BBSERVER")
      (shell-command command (bitbake-capture-buffer) nil)
    (user-error "Server not started")))

(defun bitbake-fetch-recipes ()
  "Fetch the availables bitbake recipes for the POKY-DIRECTORY and the BUILD-DIRECTORY."
  (message "Bitbake: fetching recipes")
  (with-current-buffer (bitbake-capture-buffer)
    (bitbake-s-command "bitbake -s 2>&1" "Unable to fetch recipes")
    (let ((start (progn (re-search-forward "===========")
                        (1+ (line-end-position))))
          (end (progn (re-search-forward "Summary:")
                      (1- (line-beginning-position)))))
      (mapcar (lambda (entry)
                (when (cl-every #'identity (split-string entry))
                  (split-string entry)))
              (split-string (buffer-substring-no-properties start
                                                            end)
                            "\\\n")))))

(defun bitbake-recipes (&optional fetch)
  "Return the bitbake recipes list.

If FETCH is non-nil, invalidate cache and fetch the recipes list again."
  (when (or fetch (not bitbake-recipes-cache))
    (setq bitbake-recipes-cache (bitbake-fetch-recipes)))
  bitbake-recipes-cache)

(defun bitbake-recipe-info (recipe)
  "Return the recipe list associated to RECIPE."
  (or (assoc recipe (bitbake-recipes)) (user-error "Unknown recipe %s" recipe)))

(defun bitbake-recipe-names ()
  "Return the list of available recipe names."
  (mapcar (lambda (entry) (when (car entry) (car entry))) (bitbake-recipes)))

(defun bitbake-buffer-recipe (&optional buffer)
  "Return a recipe name for the current buffer or BUFFER if given."
  (setq buffer (or buffer (current-buffer)))
  (when (stringp (buffer-file-name buffer))
    (let ((bitbake-directory (locate-dominating-file (buffer-file-name buffer)
                                                     (lambda (dir)
                                                       (and (file-directory-p dir)
                                                            (directory-files dir t "\\.bb\\(append\\)?\\'"))))))
      (when bitbake-directory
        (let ((bitbake-file (car (directory-files bitbake-directory t "[^/_]+\\(_[^_]+\\)?\\.bb\\(append\\)?\\'"))))
          (with-temp-buffer
            (goto-char (point-min))
            (insert bitbake-file)
            (goto-char (point-min))
            (if (re-search-forward "\\([^/_]+\\)\\(_[^_]+\\)?\\.bb\\(append\\)?\\'" nil t)
                (match-string 1))))))))

(defun bitbake-read-recipe ()
  "Read a recipe name in the minibuffer, with completion."
  (let ((default (or (bitbake-buffer-recipe) (car (last bitbake-recipe-history)))))
    (completing-read "Recipe: " (bitbake-recipe-names) nil t nil 'bitbake-recipe-history)))

(defun bitbake-image-names ()
  "Return the list of available image names."
  (seq-filter (lambda (recipe)
                (when (stringp (first recipe))
                  (string-match-p ".*-image\\(-.*\\|$\\)" (first recipe))))
              (bitbake-recipes)))

(defun bitbake-read-image ()
  "Read a image name in the minibuffer, with completion."
  (completing-read "Image: " (bitbake-image-names) nil t nil 'bitbake-image-history))

(defun bitbake-parse-recipe-tasks (buffer)
  "Parse the list of recipe tasks in BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((tasks))
      (while (re-search-forward "^do_" nil t)
        (setq tasks (cons (buffer-substring (point)
                                            (progn (forward-sexp) (point)))
                          tasks)))
      tasks)))

(defun bitbake-fetch-recipe-tasks (recipe)
  "Fetch the list of bitbake tasks for RECIPE."
  (message "Bitbake: fetching recipe %s tasks" recipe)
  (with-current-buffer (bitbake-capture-buffer)
    (shell-command (format "bitbake %s -c listtasks" recipe) (bitbake-capture-buffer))
    (bitbake-parse-recipe-tasks (current-buffer))))

(defun bitbake-recipe-tasks (recipe &optional fetch)
  "Return the bitbake tasks for RECIPE.

If FETCH is non-nil, invalidate cache and fetch the tasks again."
  (when (or fetch (not (assoc recipe bitbake-recipe-tasks-cache)))
    (assq-delete-all recipe bitbake-recipe-tasks-cache)
    (setq bitbake-recipe-tasks-cache (cons (list recipe (bitbake-fetch-recipe-tasks recipe)) bitbake-recipe-tasks-cache)))
  (cadr (assoc recipe bitbake-recipe-tasks-cache)))

(defun bitbake-read-tasks (recipe)
  "Read a task name in the minibuffer, with completion for task RECIPE."
  (let ((tasks (bitbake-recipe-tasks recipe)))
    (completing-read "Task: " tasks nil t nil 'bitbake-task-history)))

(defun bitbake-parse-recipe-variables (buffer)
  "Parse bitbake variables BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "^[[:alnum:]_]+()[[:space:]]+{")
    (beginning-of-line)
    (let ((limit (point))
          (variables))
      (goto-char (point-min))
      (while (re-search-forward "^\\([[:alnum:]~+.${}/_-]+\\)=\"\\([^\"]*\\)" limit t)
        (let ((name (substring-no-properties (match-string 1)))
              (value (substring-no-properties (match-string 2))))
          (while (equal (string (char-before)) "\\")
            (re-search-forward "\\(\"[^\"]*\\)" limit t)
            (setq value (concat (substring value 0 -1) (substring-no-properties (match-string 1)))))
          (setq variables (cons (cons name value) variables))))
      variables)))

(defun bitbake-fetch-recipe-variables (recipe)
  "Fetch bitbake variables for RECIPE."
  (message "Bitbake: fetching recipe %s variables" recipe)
  (with-temp-buffer
    (shell-command (format "bitbake -e %s 2>&1" recipe) (current-buffer))
    (bitbake-parse-recipe-variables (current-buffer))))

(defun bitbake-recipe-variables (recipe &optional fetch)
  "Return the bitbake variables for RECIPE.

If FETCH is non-nil, invalidate cache and fetch the variables again."
  (when (or fetch (not (assoc recipe bitbake-recipe-variables-cache)))
    (assq-delete-all recipe bitbake-recipe-variables-cache)
    (setq bitbake-recipe-variables-cache (cons (list recipe (bitbake-fetch-recipe-variables recipe)) bitbake-recipe-variables-cache)))
  (cadr (assoc recipe bitbake-recipe-variables-cache)))

(defun bitbake-read-variable (recipe &optional fetch)
  "Read variable for RECIPE, FETCH variables if t."
  (completing-read (format "%s variable: " recipe)
                   (bitbake-recipe-variables recipe bitbake-force)))

(defun bitbake-recipe-variable (recipe variable &optional fetch)
  "Return the value of VARIABLE for RECIPE.

If FETCH is non-nil, invalidate cache and fetch the variables again."
  (cdr (assoc variable (bitbake-recipe-variables recipe fetch))))

(defun bitbake-read-recipe-variable (recipe variable)
  (interactive (list nil nil))
  (let* ((recipe (bitbake-read-recipe))
         (variable (bitbake-read-variable recipe)))
    (message (cdr (assoc variable (bitbake-recipe-variables recipe nil))))))

(defun bitbake-find-recipe-workdir (recipe)
  (interactive (list (bitbake-read-recipe)))
  (find-file (bitbake-recipe-variable recipe "WORKDIR")))

(defun bitbake-uuid ()
  "Generate a random UUID."
  (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 6))
          (random (expt 16 6))))

(defun bitbake-recipe-taint-task (recipe task)
  "Taint RECIPE TASK as a workarround for bitbake -f not working in server mode."
  (let ((taint-file-name (format "%s.do_%s.taint" (bitbake-recipe-variable recipe "STAMP") task)))
    (with-temp-file taint-file-name
      (insert (bitbake-uuid)))))

(defun bitbake-task-dequeue ()
  "Schedule the next task in queue."
  (message "Bitbake: queuing next task...")
  (with-current-buffer (bitbake-buffer)
    (let ((task (car bitbake-task-queue)))
      (when task
        (setq bitbake-task-queue (cdr bitbake-task-queue)
              bitbake-current-task task)
        (add-hook 'comint-redirect-hook 'bitbake-task-dequeue nil t)
        (message "Bitbake: running task")
        (run-at-time 0 nil task)))))

(defun bitbake-task-cleanup ()
  "Cleanup the previous task."
  (message "Bitbake: command \"%s\" finished" bitbake-current-command)
  (setq bitbake-current-command nil)
  (setq bitbake-current-task nil)
  (with-current-buffer (bitbake-capture-buffer)
    (goto-char (point-min))
    (while (re-search-forward "^[| ]*ERROR: \\([^(]+\\)\\( (log file is located at \\(.*\\))\\)?$" nil t)
      (message "Bitbake: error - %s" (match-string 1))
      (let ((log-file (match-string 3)))
        (if log-file
            (find-file log-file))))))

(defmacro bitbake-command-enqueue (varlist &rest body)
  "Put command with VARLIST and BODY in the queue."
  (declare (indent 1))
  `(bitbake-task-enqueue
    (lexical-let ,(mapcar (lambda (var)
                            (list var var))
                          varlist)
      (lambda ()
        (with-current-buffer (bitbake-buffer)
          (condition-case err
              (progn
                (progn ,@body)
                (bitbake-task-cleanup)
                (bitbake-task-trigger))
            (error (bitbake-reset-queue)
                   (message "Bitbake: error - %s." (error-message-string err)))))))))

(defun bitbake-task-trigger ()
  "Maybe run next process in queue if no other task is active."
  (if bitbake-task-queue
      (unless bitbake-current-task
        (message "Bitbake: triggering next task")
        (display-buffer (bitbake-buffer))
        (bitbake-task-dequeue))
    (message "Bitbake: task queue empty")
    (remove-hook 'comint-redirect-hook 'bitbake-task-dequeue t)))

(defun bitbake-task-enqueue (task)
  "Queue TASK for running in bitbake buffer."
  (message "Bitbake: queuing command")
  (setq bitbake-task-queue (append bitbake-task-queue (list task)))
  (bitbake-task-trigger))

(defun bitbake-reset-queue ()
  "Reset task queue."
  (interactive)
  (setq bitbake-task-queue nil
        bitbake-current-task nil))

;;;###autoload
(defun bitbake-task (task recipe &optional force)
  "Run bitbake TASK on RECIPE.

Force the task if FORCE is t."
  (interactive (let* ((recipe (bitbake-read-recipe))
                      (task (bitbake-read-tasks recipe))
                      (force prefix-arg))
                 (list task recipe force)))
  (let ((force (if force force bitbake-force)))
    (bitbake-command-enqueue (recipe task force)
      (when force
        (bitbake-recipe-taint-task recipe task))
      (bitbake-shell-command (format "bitbake %s %s -c %s" recipe (if force "-f" "") task)))))

;;;###autoload
(defun bitbake-recipe (recipe)
  "Run bitbake RECIPE."
  (interactive (list (bitbake-read-recipe)))
  (bitbake-command-enqueue (recipe)
    (bitbake-shell-command (format "bitbake %s " recipe))))

;;;###autoload
(defun bitbake-clean (recipe)
  "Run bitbake clean on RECIPE."
  (interactive (list (bitbake-read-recipe)))
  (bitbake-task "clean" recipe t))

;;;###autoload
(defun bitbake-cleanall (recipe)
  "Run bitbake clean on RECIPE."
  (interactive (list (bitbake-read-recipe)))
  (bitbake-task "cleanall" recipe t))

;;;###autoload
(defun bitbake-compile (recipe)
  "Run bitbake compile on RECIPE."
  (interactive (list (bitbake-read-recipe)))
  (bitbake-task "compile" recipe t))

;;;###autoload
(defun bitbake-devshell (recipe)
  "Run bitbake devshell on RECIPE."
  (interactive (list (bitbake-read-recipe)))
  (bitbake-task "devshell" recipe t))

;;;###autoload
(defun bitbake-install (recipe)
  "Run bitbake install on RECIPE."
  (interactive (list (bitbake-read-recipe)))
  (bitbake-task "install" recipe t))

;;;###autoload
(defun bitbake-fetch (recipe)
  "Run bitbake fetch on RECIPE."
  (interactive (list (bitbake-read-recipe)))
  (bitbake-task "fetch" recipe t)
  (bitbake-task "unpack" recipe t))

;;;###autoload
(defun bitbake-package (recipe)
  "Run bitbake package on RECIPE."
  (interactive (list (bitbake-read-recipe)))
  (bitbake-task "package" recipe t))

;;;###autoload
(defun bitbake-prepare (recipe)
  "Run bitbake prepare on RECIPE."
  (interactive (list (bitbake-read-recipe)))
  (bitbake-task "prepare" recipe t))

;;;###autoload
(defun bitbake-recompile (recipe)
  "Run bitbake clean compile and install on RECIPE."
  (interactive (list (bitbake-read-recipe)))
  (bitbake-task "cleanall" recipe)
  (bitbake-task "fetch" recipe)
  (bitbake-recipe recipe))

;;;###autoload
(defun bitbake-deploy (recipe)
  "Deploy artifacts of RECIPE to bitbake-deploy-ssh host."
  (interactive (list (bitbake-read-recipe)))
  (bitbake-command-enqueue (recipe)
    (let ((image (bitbake-recipe-variable recipe "D")))
      (message "Duma: deploying %s" recipe)
      (bitbake-shell-command (format "tar -C %s -cf - . | ssh %s tar -C / -xf -" image bitbake-deploy-ssh-host)))))

;;;###autoload
(defun bitbake-recompile-deploy (recipe)
  "Recompile RECIPE and deploy its artifacts."
  (interactive (list (bitbake-read-recipe)))
  (bitbake-recompile recipe)
  (bitbake-deploy recipe))

;;;###autoload
(defun bitbake-image (image)
  "Run bitbake IMAGE."
  (interactive (list (bitbake-read-image)))
  (bitbake-command-enqueue (image)
    (when bitbake-force
      (bitbake-recipe-taint-task image "rootfs"))
    (bitbake-shell-command (format "bitbake %s" image))))

;;;###autoload
(defun bitbake-cleanall-world ()
  "Get bitbake dependency graph for RECIPE."
  (interactive)
  (bitbake-shell-command (format "bitbake world -c cleanall --continue")))

;;;###autoload
(defun bitbake-dependency-graph (recipe)
  "Get bitbake dependency graph for RECIPE."
  (interactive (list (bitbake-read-recipe)))
  (bitbake-shell-command (format "bitbake -g %s -u taskexp" recipe)))

(defun bitbake-get-ev-buffer ()
  "Get/create a buffer for displaying the bitbake environement."
  (let ((buf (get-buffer-create "*bitbake-environment*")))
    (with-current-buffer buf
      (shell-script-mode)
      (display-buffer buf)
      buf)))
;;;###autoload
(defun bitbake-environment (recipe)
  "Run bitbake RECIPE."
  (interactive (list (bitbake-read-recipe)))
  (bitbake-command-enqueue (recipe)
    (bitbake-shell-command (format "bitbake -e %s" recipe)
                           (bitbake-get-ev-buffer))))

(defun bitbake-workdir (recipe)
  "Open RECIPE workdir."
  (interactive (list (bitbake-read-recipe)))
  (find-file (bitbake-recipe-variable recipe "WORKDIR")))

(defun bitbake-file (recipe)
  "Open RECIPE bitbake file."
  (interactive (list (bitbake-read-recipe)))
  (find-file (bitbake-recipe-variable recipe "FILE")))

(defun bitbake-rootfs (image)
  "Open IMAGE root fs."
  (interactive (list (bitbake-read-image)))
  (find-file (bitbake-recipe-variable image "IMAGE_ROOTFS")))

(defun wic-read-definition-file ()
  "Read path to a wic wks definition file."
  (if bitbake-wic-definition-file
      (if (file-name-absolute-p bitbake-wic-definition-file) bitbake-wic-definition-file
        (format "%s%s" bitbake-current-poky-directory bitbake-wic-definition-file))
    (read-file-name "Definition file: " bitbake-current-poky-directory nil t nil
                    (lambda (name)
                      (or (file-directory-p name)
                          (string-match "\\.wks\\'" name))))))

;;;###autoload
(defun bitbake-wic-create (wks image)
  "Run wic WKS -e IMAGE."
  (interactive (list (wic-read-definition-file)
                     (bitbake-read-image)))
  (let ((rootfs (bitbake-recipe-variable image "IMAGE_ROOTFS"))
        (kernel (bitbake-recipe-variable image "STAGING_KERNEL_DIR"))
        (hdddir (bitbake-recipe-variable image "HDDDIR"))
        (staging-data (bitbake-recipe-variable image "STAGING_DATADIR"))
        (native-sysroot (bitbake-recipe-variable image "STAGING_DIR_NATIVE"))
        (deploy (bitbake-recipe-variable image "DEPLOY_DIR_IMAGE"))
        (last-prompt (process-mark (get-buffer-process (bitbake-buffer)))))
    (bitbake-command-enqueue (wks rootfs staging-data kernel native-sysroot deploy)
      (bitbake-shell-command (format "wic create %s -r %s -b %s -k %s -n %s -o %s"
                                     wks rootfs staging-data kernel native-sysroot deploy)))
    (bitbake-command-enqueue ()
      (let (disk-image)
        (with-current-buffer (bitbake-capture-buffer)
          (goto-char (point-min))
          (unless (re-search-forward "The new image(s) can be found here:\n *\\(.*\\)" nil t)
            (error "Unable to execute wic command, see *bitbake* for details"))
          (setq disk-image (match-string 1)))
        (message "Disk image %s created" disk-image)
        (setq bitbake-last-disk-image disk-image)))))

;;;###autoload
(defun bitbake-hdd-image (wks image)
  "Create an hdd image using wic based on WKS definition file and bitbake IMAGE."
  (interactive (list (wic-read-definition-file)
                     (bitbake-read-image)))
  (bitbake-image image)
  (bitbake-wic-create wks image))

;;;###autoload
(defun bitbake-read-wic ()
  "Read a IMAGE in the minibuffer, with completion."
  (read-file-name "Image: "
                  (if bitbake-image-path
                      bitbake-image-path
                    (concat (first (file-expand-wildcards (concat (getenv "BBPATH") "tmp*/deploy/images*") t)) "/."))
                  nil
                  nil
                  nil
                  (lambda (filename) (or (and (string-match-p "wic" filename)
                                              (string-match-p "bz2" filename))
                                         (string-match-p "/" filename)))))

;;;###autoload
(defun bitbake-read-device ()
  "Read a DEVICE in the minibuffer, with completion."
  (read-file-name "Device: "
                  (if bitbake-flash-device
                      bitbake-flash-device
                    "/dev/.")))

;;;###autoload
(defun bitbake-flash (image device)
  "Flash IMAGE to DEVICE."
  (interactive (list (bitbake-read-wic)
                     (bitbake-read-device)))
  (with-temp-buffer
    (setq image (expand-file-name image))
    (cd "/sudo::/")
    (shell-command (concat "umount " device "*"))
    (async-shell-command (concat "bmaptool copy " image " " device))))

;;; Mode definition

;;;###autoload
(defvar bitbake-minor-mode-map nil "Keymap for bitbake-mode.")

(setq bitbake-minor-mode-map nil)
(when (not bitbake-minor-mode-map)
  (setq bitbake-minor-mode-map (make-sparse-keymap))
  (define-key bitbake-minor-mode-map (kbd "C-c C-/ s") 'bitbake-start-server)
  (define-key bitbake-minor-mode-map (kbd "C-c C-/ k") 'bitbake-stop-server)
  (define-key bitbake-minor-mode-map (kbd "C-c C-/ r") 'bitbake-recompile)
  (define-key bitbake-minor-mode-map (kbd "C-c C-/ d") 'bitbake-recompile-deploy)
  (define-key bitbake-minor-mode-map (kbd "C-c C-/ f") 'bitbake-flash-image)
  (define-key bitbake-minor-mode-map (kbd "C-c C-/ t") 'bitbake-task))

(easy-menu-define bitbake-menu bitbake-minor-mode-map
  "BitBake"
  '("BitBake"
    ["Start server"   bitbake-start-server]
    ["Stop server"    bitbake-stop-server ]
    ["Recipe"         bitbake-recipe]
    ["Task"           bitbake-task]
    ("Tasks"
     ["clean"         bitbake-clean]
     ["compile"       bitbake-compile]
     ["install"       bitbake-install]
     ["fetch"         bitbake-fetch]
     ["recompile"     bitbake-recompile]
     ["deploy"        bitbake-deploy]
     ["recompile, deploy" bitbake-recompile-deploy])
    ("Image"
     ["build"         bitbake-image]
     ["wic"           bitbake-wic-create]
     ["hdd"           bitbake-hdd-image]
     ["flash"         bitbake-flash-image])))
(defun bitbake-tmm-menu ()
  "Display tmm menu for bitbake."
  (interactive)
  (tmm-prompt bitbake-menu))

(define-minor-mode bitbake-minor-mode
  "Toggle Bitake mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state."
  :init-value nil
  :lighter nil
  :keymap bitbake-minor-mode-map
  :group 'bitbake
  :global t)

(defun bitbake-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.

For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((comment-start "#") (comment-end ""))
    (comment-dwim arg)))

(defvar bitbake-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    table)
  "Syntax table used in `bitbake-mode'.")

(defvar bitbake-font-lock-defaults
  `((
     ;; fakeroot python do_foo() {
     ("\\b\\(include\\|require\\|inherit\\|python\\|addtask\\|export\\|fakeroot\\|unset\\)\\b" . font-lock-keyword-face)
     ;; do_install_append() {
     ("^\\(fakeroot *\\)?\\(python *\\)?\\([a-zA-Z0-9\-_+.${}/~]+\\) *( *) *{" 3 font-lock-function-name-face)
     ;; do_deploy[depends] ??=
     ("^\\(export *\\)?\\([a-zA-Z0-9\-_+.${}/~]+\\(\\[[a-zA-Z0-9\-_+.${}/~]+\\]\\)?\\) *\\(=\\|\\?=\\|\\?\\?=\\|:=\\|+=\\|=+\\|.=\\|=.\\)" 2 font-lock-variable-name-face)
     )))

(defun bitbake-indent-line ()
  "Indent current line as bitbake code."
  (interactive)
  (beginning-of-line)
  (if (looking-back "\\\\\n" nil)
      (indent-line-to tab-width)
    (indent-line-to 0)))

;;;###autoload
(define-derived-mode bitbake-mode prog-mode
  "A mode for editing bitbake recipe files."
  :syntax-table bitbake-syntax-table
  (setq font-lock-defaults bitbake-font-lock-defaults)
  (setq mode-name "BitBake")
  (setq comment-start "# ")
  (set (make-local-variable 'indent-line-function) 'bitbake-indent-line)
  (define-key bitbake-mode-map [remap comment-dwim] 'bitbake-comment-dwim))

(mmm-add-classes
 '((bitbake-shell
    :submode shell-script-mode
    :delimiter-mode nil
    :case-fold-search nil
    :front "^\\(fakeroot *\\)?\\([a-zA-Z0-9\-_+.${}/~]+\\) *( *) *{"
    :back "^}")
   (bitbake-python
    :submode python-mode
    :delimiter-mode nil
    :case-fold-search nil
    :front "^\\(fakeroot *\\)?python *\\([a-zA-Z0-9\-_+.${}/~]+\\) *( *) *{"
    :back "^}")))

(mmm-add-mode-ext-class 'bitbake-mode "\\.bb\\(append\\|class\\)?\\'" 'bitbake-shell)
(mmm-add-mode-ext-class 'bitbake-mode "\\.bb\\(append\\|class\\)?\\'" 'bitbake-python)
(add-to-list 'auto-mode-alist
             '("\\.inc\\|\\.bb\\(append\\|class\\)?\\'" . bitbake-mode))

(provide 'bitbake)

;;; bitbake.el ends here
