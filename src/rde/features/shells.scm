;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022, 2024 Andrew Tropin <andrew@trop.in>
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

(define-module (rde features shells)
  #:use-module (rde packages)
  #:use-module (rde features)
  #:use-module (rde predicates)
  #:use-module (gnu home services)
  #:use-module (rde home services shells)
  #:use-module (rde system services admin)
  #:use-module (gnu services)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages shellutils)
  #:use-module (guix gexp)
  #:use-module (rde gexp)

  #:export (feature-zsh
            feature-bash))

(define* (feature-zsh
          #:key
          (zsh zsh)
          (zsh-autosuggestions zsh-autosuggestions)
          (rde-defaults? #t)
          (zshrc '())
          (zprofile '())
          (zshenv '())
          (zlogout '())
          (default-shell? #t)
          (enable-zsh-autosuggestions? #t))
  "Configure Zsh."
  (ensure-pred file-like? zsh)
  (ensure-pred file-like? zsh-autosuggestions)
  (ensure-pred boolean? default-shell?)
  (ensure-pred boolean? enable-zsh-autosuggestions?)
  (ensure-pred boolean? rde-defaults?)

  (define (zsh-home-services config)
    "Returns home services related to Zsh."
    (list
     (when default-shell?
       (simple-service
        'zsh-set-default-shell-to-zsh
        home-environment-variables-service-type
        `(("SHELL" . ,(file-append zsh "/bin/zsh")))))

     (when enable-zsh-autosuggestions?
       (simple-service
        'zsh-autosuggestions-add-config
        home-zsh-service-type
        (home-zsh-extension
         (privileged? #t)
         ;; We set variables in zshrc because we need them only in
         ;; interactive shell.
         (zshrc `("# Improve the behavior and perfomance of auto suggestions"
                  "ZSH_AUTOSUGGEST_MANUAL_REBIND=true"
                  "ZSH_AUTOSUGGEST_USE_ASYNC=true"
                  "ZSH_AUTOSUGGEST_STRATEGY=(history completion)"
                  ,#~(format #f "source ~a/share/zsh/plugins/~a/~a.zsh"
                             #$zsh-autosuggestions
                             "zsh-autosuggestions"
                             "zsh-autosuggestions"))))))

     (when (get-value 'wayland config #f)
       (let* ((wl-clipboard (get-value
                             'wl-clipboard config
                             (@ (gnu packages xdisorg) wl-clipboard)))
              (wl-copy      (file-append wl-clipboard "/bin/wl-copy"))
              (wl-paste     (file-append wl-clipboard "/bin/wl-paste")))
         (simple-service
          'zsh-make-zle-use-system-clipboard
          home-zsh-service-type
          (home-zsh-extension
           (zshrc
            (list
             ""
             #~(format #f "\
rde-copy-region-as-kill () {
  zle copy-region-as-kill
  print -rn $CUTBUFFER | ~a
}
zle -N rde-copy-region-as-kill

rde-kill-region () {
  zle kill-region
  print -rn $CUTBUFFER | ~a
}
zle -N rde-kill-region

rde-yank () {
  CUTBUFFER=$(~a)
  zle yank
}
zle -N rde-yank

bindkey -e '\\ew' rde-copy-region-as-kill
bindkey -e '^W' rde-kill-region
bindkey -e '^Y' rde-yank

" #$wl-copy #$wl-copy #$wl-paste)))))))

     ;; https://github.com/purcell/envrc
     ;; home-zsh-direnv-service
     (service
      home-zsh-service-type
      (home-zsh-configuration
       (xdg-flavor? #t)
       (package zsh)
       (zshrc
        `(,@(if rde-defaults?
                `(,(slurp-file-like (local-file "./zsh/zshrc"))
                  ; FIXME: Doesn't belong here, doesn't rely on full paths
                  "alias state-sync='herd sync state \
&& pass git push origin master'")
                '())
          ,@zshrc))
       (zprofile zprofile)
       (zshenv zshenv)
       (zlogout zlogout)))))

  (define (get-system-services config)
    (list
     (simple-service
      'zsh-preserve-zdotdir-env
      sudoers-service-type
      (list "
# Load zsh config from custom location
Defaults:%wheel env_keep+=ZDOTDIR"))))

  (feature
   (name 'zsh)
   (values (make-feature-values zsh))
   (home-services-getter zsh-home-services)
   (system-services-getter get-system-services)))

(define* (feature-bash
          #:key
          (bash bash))
  "Configure Bash."
  (ensure-pred file-like? bash)

  (define (get-home-services config)
    "Returns home services related to Bash."
    (list
     (simple-service
      'bash-set-bash-histfile
      home-environment-variables-service-type
      `(("HISTFILE" . "$XDG_CACHE_HOME/.bash_history")))

     (service
      home-bash-service-type
      (home-bash-configuration
       (package bash)))))

  (feature
   (name 'bash)
   (values (make-feature-values bash))
   (home-services-getter get-home-services)))
