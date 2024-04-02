;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
;;;
;;; This file is part of rde.
;;;
;;; rde is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; rde is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rde.  If not, see <http://www.gnu.org/licenses/>.

(define-module (rde features python)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages python)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:export (feature-python))

;; This is the current recommended way to go by
;; https://github.com/b3nj5m1n/xdg-ninja, where it's specified that this won't
;; work if python is invoked with the -i flag. But it does work with `run-python'
;; emacs command, which is the recommended way to use the python repl in RDE.
;; The official PR seems to be here : https://github.com/python/cpython/pull/26377
(define python-startup-file
  (plain-file "pythonrc"
              "\
import os
import atexit
import readline
from pathlib import Path

if readline.get_current_history_length() == 0:

    state_home = os.environ.get(\"XDG_STATE_HOME\")
    if state_home is None:
        state_home = Path.home() / \".local\" / \"state\"
    else:
        state_home = Path(state_home)

    history_path = state_home / \"python_history\"
    if history_path.is_dir():
        raise OSError(f\"'{history_path}' cannot be a directory\")

    history = str(history_path)

    try:
        readline.read_history_file(history)
    except OSError: # Non existent
        pass

    def write_history():
        try:
            readline.write_history_file(history)
        except OSError:
            pass

    atexit.register(write_history)"))

(define* (feature-python
          #:key
          (python python-wrapper)
          (emacs-python-black emacs-python-black)
          (black? #f))
  "Configure python for emacs. If black? is #t, configure the
emacs-python-black package, which provides useful functions for formatting
python files."
  (ensure-pred file-like? python)
  (ensure-pred file-like? emacs-python-black)
  (ensure-pred boolean? black?)

  (define f-name 'python)

  (define (get-home-services config)
    (list
     (simple-service
      'add-python-home-package
      home-profile-service-type
      (list python))
     (simple-service
      'python-xdg-base-dirs-specification
      home-environment-variables-service-type
      `(("IPYTHONDIR" . "$XDG_CONFIG_HOME/ipython")
        ("PYTHONSTARTUP" . ,python-startup-file)))
     (when (get-value 'emacs config)
       (rde-elisp-configuration-service
        f-name
        config
        `(,@(if black?
                '((eval-when-compile (require 'python-black))
                  (add-hook 'python-mode
                            'python-black-on-save-mode-enable-dwim))
                '())

          ,@(if (get-value 'emacs-org config)
                `((with-eval-after-load 'org
                    (add-to-list 'org-structure-template-alist
                                 '("py" . "src python")))
                  (with-eval-after-load 'ob-core
                    (require 'ob-python))
                  (with-eval-after-load 'ob-python
                    (setq org-babel-python-command
                          ,(file-append python "/bin/python"))))
                '()))
        #:elisp-packages
        (if black? (list emacs-python-black) '())))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
