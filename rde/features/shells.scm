(define-module (rde features shells)
  #:use-module (rde packages)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (gnu home services)
  #:use-module (gnu home-services shells)
  #:use-module (gnu home-services shellutils)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu services)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages shellutils)
  #:use-module (guix gexp)

  #:export (feature-zsh
            feature-bash))

(define* (feature-zsh
	  #:key
	  (zsh zsh)
	  (default-shell? #t)
	  (enable-zsh-autosuggestions? #t))
  "Configure Zsh."
  (ensure-pred package? zsh)

  (define (zsh-home-services config)
    "Returns home services related to Zsh."
    (list
     (when default-shell?
       (simple-service
	'set-default-shell-to-zsh
	home-environment-variables-service-type
	`(("SHELL" . ,(file-append zsh "/bin/zsh")))))

     ;; zsh-autosuggestions is very cool plugin, but a little
     ;; distractive, I find it a little against Attention-friendly
     ;; principle
     (when enable-zsh-autosuggestions?
       (service home-zsh-autosuggestions-service-type
                zsh-autosuggestions-latest))

     (when (get-value 'wayland config)
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
	(list
	 (slurp-file-gexp (local-file "./zsh/zshrc"))
	 "alias state-sync='herd sync state && pass git push origin master'"))))))

  (feature
   (name 'zsh)
   (values (make-feature-values zsh))
   (home-services-getter zsh-home-services)))

(define* (feature-bash
	  #:key
	  (bash bash))
  "Configure Bash."
  (ensure-pred package? bash)

  (define (get-home-services config)
    "Returns home services related to Bash."
    (list
     (simple-service
      'set-bash-histfile
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
