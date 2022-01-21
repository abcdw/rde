(define-module (rde features terminals)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde features fontutils)
  #:use-module (gnu home services)
  #:use-module (gnu home-services base)
  #:use-module (gnu home-services terminals)
  #:use-module (gnu home-services shells)
  #:use-module (gnu home-services wm)
  #:use-module (gnu services)
  #:use-module (rde packages)
  #:use-module (gnu packages terminals)
  #:use-module (guix gexp)

  #:export (feature-alacritty
            feature-vterm))

(define (font-weight->style weight)
  "Transform kebab-cased symbols to capitalized strings without dashes."
  (string-capitalize (string-delete #\- (symbol->string weight))))

(define* (feature-alacritty
	  #:key
	  config-file
	  (package alacritty)
          (default-terminal? #t))
  "Configure Alacritty terminal."
  (ensure-pred maybe-file-like? config-file)
  (ensure-pred any-package? package)

  ;; TODO: Implement home service and rewrite to it to make this
  ;; feature extendable.
  (define (alacritty-home-services config)
    "Returns home services related to Alacritty."
    (define font-mono (get-value 'font-monospace config))
    (list
     (service
      home-alacritty-service-type
      (home-alacritty-configuration
       (package package)
       (config
        `((window . ((padding . ((x . 10)
                                 (y . 5)))))
          ,@(if font-mono
                `((font . ((normal . ((style . , (font-weight->style
                                                  (font-weight font-mono)))
                                      (family . ,(font-name font-mono))))
                           (size . ,(font-size font-mono)))))
              '())
          ,@(if config-file
                `((import . #(,config-file)))
                '())))))))

  (feature
   (name 'alacritty)
   (values `((alacritty . ,package)
             ,@(if default-terminal?
                 `((default-terminal . ,(file-append package "/bin/alacritty")))
                 '())))
   (home-services-getter alacritty-home-services)))


(define* (feature-vterm
	  #:key
	  (emacs-vterm emacs-vterm-latest))
  "Configure Alacritty terminal."
  (ensure-pred any-package? emacs-vterm)

  (define (get-home-services config)
    (require-value 'emacs config)
    (list
     (elisp-configuration-service
      'vterm
      `(,@(if (get-value 'emacs-consult config)
              `((eval-when-compile
                 (require 'cl-macs))

                (with-eval-after-load
                 'vterm
                 (defun vterm-consult-yank-pop-wrapper (orig-fun &rest args)
                   "Use `vterm-insert' instead of `insert-for-yank' if
`major-mode' is `vterm-mode'."
                   (interactive "p")
                   (if (equal major-mode 'vterm-mode)
                       (let ((inhibit-read-only t)
                             (yank-undo-function (lambda (_s _e) (vterm-undo))))
                         (cl-letf (((symbol-function 'insert-for-yank)
                                    'vterm-insert))
                                  (apply orig-fun args)))
                       (apply orig-fun args)))

                 (advice-add 'consult-yank-pop :around
                             'vterm-consult-yank-pop-wrapper)))
              '()))
      #:elisp-packages `(,emacs-vterm
                         ,@(if (get-value 'emacs-consult config)
                               (list (get-value 'emacs-consult config))
                               '())))

     (when (get-value 'sway config)
       (simple-service
        'emacs-vterm-add-sway-bind
        home-sway-service-type
        `((bindsym $mod+Shift+Return exec
		   ,(get-value 'emacs-client-create-frame config)
                   "-e \"(vterm t)\""))))

     (when (get-value 'zsh config)
       (simple-service
        'emacs-vterm-zsh-configuration
        home-zsh-service-type
        (home-zsh-extension
	 (zshrc
          (list #~(format #f "source ~a"
                          #$(local-file "./zsh/vterm" "setup-vterm")))))))))

  (feature
   (name 'vterm)
   (values `((vterm . #t)
             (emacs-vterm . ,emacs-vterm)))
   (home-services-getter get-home-services)))
