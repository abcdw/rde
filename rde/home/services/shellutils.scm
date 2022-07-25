;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021 Andrew Tropin <andrew@trop.in>
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

(define-module (rde home services shellutils)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (rde home services shells)
  #:use-module (gnu packages shellutils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:export (home-zsh-plugin-manager-service-type
            home-zsh-autosuggestions-service-type
            home-zsh-direnv-service-type))

(define (add-zsh-plugins-load-command packages)
  (home-zsh-extension
   (zshrc
    (map
     (lambda (p)
       (let ((x (package-name p)))
         #~(string-append
            "source " #$p #$(format #f "/share/zsh/plugins/~a/~a.zsh" x x))))
     packages))))

(define home-zsh-plugin-manager-service-type
  (service-type (name 'home-zsh-plugin-manager)
                (extensions
                 (list (service-extension
                        home-zsh-service-type
                        add-zsh-plugins-load-command)
                       (service-extension
                        home-profile-service-type
                        identity)))
                (compose concatenate)
                (extend append)
                (default-value '())
                (description "\
Install plugins into the home profile and configure Zsh to load them.")))

(define home-zsh-autosuggestions-service-type
  (service-type
   (name 'home-zsh-autosuggestions)
   (extensions
    (list
     (service-extension home-zsh-plugin-manager-service-type list)
     (service-extension
      home-zsh-service-type
      (const
       (home-zsh-extension
        ;; We set variables in zshrc because we need them only in
        ;; interactive shell.
        (zshrc '("# Improve the behavior and perfomance of auto suggestions"
                 "ZSH_AUTOSUGGEST_MANUAL_REBIND=true"
                 "ZSH_AUTOSUGGEST_USE_ASYNC=true"
                 "ZSH_AUTOSUGGEST_STRATEGY=(history completion)")))))))
   (default-value zsh-autosuggestions)
   (description "Enable Fish-like fast and unobtrusive autosuggestions
for Zsh, and set reasonable default values for some plugin's variables
to improve perfomance.")))
