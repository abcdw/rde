;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     csv
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-help-tooltip t
                      auto-completion-complete-with-key-sequence "C-m"
                      auto-completion-enable-snippets-in-popup t)
     (ranger :variables ranger-show-preview t)
     (shell :variables
            ;; shell-enable-smart-eshell t
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-shell 'eshell)

     ;; language layers
     emacs-lisp
     (clojure :variables
              clojure-enable-fancify-symbols t
              cider-repl-history-size 1000
              cider-repl-history-file "~/.cider_history")
     ;; java
     haskell
     (c-c++ :variables c-c++-enable-clang-support t)
     python
     ;; ob-jupyter
     ipython-notebook
     lua

     ;; markup and syntax layers
     nixos
     html
     latex
     markdown
     yaml
     (org :variables org-enable-github-support t)
     nginx
     sql

     ;; vcs layers
     version-control
     git
     github

     ;; other layers
     pdf-tools
     restclient
     command-log
     gnus
     twitter
     slack
     fasd
     docker
     speed-reading
     wakatime
     ;; :variables wakatime-cli-path "/usr/local/lib/python2.7/dist-packages/wakatime/cli.py"

     semantic
     syntax-checking
     spell-checking


     clojure-additions
     ;; org-additions

     ;; helm
     ivy
     spacemacs-layouts
     ;; themes-megapack
     (colors :variables colors-enable-rainbow-identifiers nil))

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(keyfreq
     leuven-theme doom-themes all-the-icons
     try
     align-cljlet
     ;; emms
     ;; bbdb bbdb-vcard w3m
     transmission)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7)) ;; TODO: revisit
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode ;; TODO: revisit
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(doom-one
                         leuven
                         zenburn
                         sanityinc-tomorrow-day
                         spacemacs-dark
                         spacemacs-light)
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Iosevka"
                               ;; :size 28
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text t
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "m"
   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout t
   ;; If non-nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands.
   dotspacemacs-auto-generate-layout-names nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non-nil the paste micro-state is enabled. When enabled pressing `p'
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state t
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil ;; TODO: revisit
   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title nil
   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide nil
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil ;; TODO: revisit
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'current
   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   dotspacemacs-frame-title-format "%I@%S"
   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil
   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t
   ))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (setq evil-normal-state-cursor '("#51afef" box))

  ;; private.el contains some tokens and other credentials
  ;; https://github.com/yuya373/emacs-slack/blob/master/README.md
  (load-file "~/.spacemacs.d/private.el")
  (setq alert-default-style 'libnotify)

  ;; (spaceline-compile)

  (defun save-all-and-normal-state ()
    (interactive)
    (save-some-buffers t)
    ;; (evil-normal-state)
    )
  (add-hook 'focus-out-hook 'save-all-and-normal-state)

  ;; (spaceline-all-the-icons-theme)
  (setq powerline-default-separator nil)

  ;; https://github.com/syl20bnr/spacemacs/blob/develop/doc/DOCUMENTATION.org#binding-keys
  (define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward)

  ;; Make C-g do esacpe with first attempt
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode)
        ;; evil-escape-key-sequence "jk"
        evil-escape-delay 0.25)
  (cl-pushnew #'minibufferp evil-escape-inhibit-functions :test #'eq)
  (add-hook 'after-init-hook #'evil-escape-mode)
  (define-key evil-insert-state-map (kbd "C-g") #'evil-escape)
  (define-key evil-lisp-state-map (kbd "C-g") #'evil-escape)
  (define-key evil-operator-state-map (kbd "C-g") #'evil-escape)
  (define-key evil-replace-state-map (kbd "C-g") #'evil-escape)
  (define-key evil-visual-state-map (kbd "C-g") #'evil-escape)

  (define-key evil-insert-state-map (kbd "C-h") 'backward-delete-char)
  (define-key evil-lisp-state-map (kbd "SPC") 'spacemacs-cmds)
  ;; Fix C-k in company mode and make C-g works better
  (add-hook
   'company-completion-started-hook
   (lambda (&rest ignore)
     (when evil-mode
       (when (evil-insert-state-p)
         (define-key evil-insert-state-map (kbd "C-g") #'evil-escape)
         (define-key evil-insert-state-map (kbd "C-k") nil)))))

  ;; TODO: revisit this hotkey
  (global-set-key (kbd "s-;") 'other-window) ; s is for super

  ;; (require 'all-the-icons)

  (defadvice vc-mode-line (after strip-backend () activate)
    (when (stringp vc-mode)
      (let ((str (replace-regexp-in-string "^ Git." "" vc-mode)))
        (let ((len (length str)))
          (let ((dash-position (or (string-match "-" str (min 25 len)) 100)))
            (setq vc-mode
                   ;; (all-the-icons-octicon "git-branch" :height 1.2 :v-adjust -0.05)
                   (substring str 0 (apply 'min (list 35 len dash-position)))))))))

  (defun hs-clojure-hide-namespace-and-folds ()
    "Hide the first (ns ...) expression in the file, and also all
the (^:fold ...) expressions."
    (interactive)
    (hs-life-goes-on
     (save-excursion
       (goto-char (point-min))
       (when (ignore-errors (re-search-forward "^(ns "))
         (hs-hide-block))

       (while (ignore-errors (re-search-forward "\\^:fold"))
         (hs-hide-block)
         (next-line)))))

  (defun hs-clojure-mode-hook ()
    (interactive)
    (hs-minor-mode 1)
    (hs-clojure-hide-namespace-and-folds))

  (add-hook 'clojure-mode-hook 'hs-clojure-mode-hook)
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)

  (global-subword-mode)
  (global-wakatime-mode)


  (setq vc-follow-symlinks t)
  (spacemacs/toggle-mode-line-minor-modes-off)
  (setq neo-theme 'icons)

  (keyfreq-mode 1) ;; TODO: revisit and read stats
  (keyfreq-autosave-mode 1)

  ;; https://oremacs.com/2016/01/06/ivy-flx/
  ;;
  ;; (setq ivy-re-builders-alist
  ;;       '((t . ivy--regex-fuzzy)))
  (setq ivy-re-builders-alist
        '( ;; (ivy-switch-buffer . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))
  (setq ivy-initial-inputs-alist nil)
  (setq org-imenu-depth 2)
  (setq ivy-fixed-height-minibuffer t)
  ;; (setq org-bullets-bullet-list '("■" "◆" "▲" "▶"))

  (with-eval-after-load 'org
    (setq org-startup-indented t))

  (setq magit-repository-directories '("~/work/"))
  (setq magithub-cache t)
  ;; (setq magithub-offline-mode t)

  (spacemacs/set-leader-keys "." 'save-buffer))


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(evil-want-Y-yank-to-eol t)
 '(hl-sexp-background-color "#efebe9")
 '(package-selected-packages
   (quote
    (align-cljlet emoji-cheat-sheet-plus company-emoji slack emojify circe oauth2 ivy-todo emms transmission let-alist nix-mode company-nixos-options nixos-options org-category-capture gntp haml-mode fringe-helper git-gutter+ git-gutter marshal logito pcache ht flyspell-correct request-deferred tablist docker-tramp json-snatcher json-reformat web-completion-data know-your-http-well pos-tip inflections edn peg pythonic font-lock+ auto-complete doom-themes powerline impatient-mode multiple-cursors bbdb highlight undo-tree window-purpose skewer-mode js2-mode simple-httpd org-brain ghub+ apiwrap ghub deferred websocket json-mode diminish paredit seq memoize packed anaconda-mode auctex ghc haskell-mode company projectile counsel flycheck avy evil swiper ivy magit-popup git-commit with-editor async gh markdown-mode restclient alert log4e org-plus-contrib hydra f dash s symon string-inflection realgud test-simple loc-changes load-relative password-generator evil-org evil-lion editorconfig dante company-lua helm-bibtex ivy-bibtex biblio parsebib biblio-core meghanada groovy-mode groovy-imports gradle-mode ensime sbt-mode scala-mode browse-at-remote csv-mode sayid ujelly-theme shell-pop persp-mode ob-http jbeans-theme ivy-hydra intero info+ gotham-theme git-link exec-path-from-shell evil-nerd-commenter evil-lispy lispy dracula-theme aggressive-indent cider clojure-mode eclim smartparens yasnippet helm helm-core magit zoutline zonokai-theme zenburn-theme zen-and-art-theme yapfify yaml-mode xterm-color ws-butler winum which-key wgrep web-mode wakatime-mode w3m volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme twittering-mode twilight-theme twilight-bright-theme twilight-anti-bright-theme try tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stickyfunc-enhance srefactor sql-indent spray spaceline spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smex smeargle slim-mode seti-theme scss-mode sass-mode reverse-theme restart-emacs ranger rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme queue pyvenv pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme popwin planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme pdf-tools pcre2el pastels-on-dark-theme paradox ox-gfm orgit organic-green-theme org-projectile org-present org-pomodoro org-gcal org-download org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme ob-restclient ob-ipython noctilux-theme niflheim-theme nginx-mode neotree naquadah-theme mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magithub magit-gitflow magit-gh-pulls madhat2r-theme macrostep lush-theme lua-mode lorem-ipsum live-py-mode linum-relative link-hint light-soap-theme leuven-theme less-css-mode keyfreq jazz-theme ivy-purpose ir-black-theme inkpot-theme indent-guide hy-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation hide-comnt heroku-theme hemisu-theme help-fns+ helm-make hc-zenburn-theme haskell-snippets gruvbox-theme gruber-darker-theme grandshell-theme google-translate golden-ratio gnuplot gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe git-gutter-fringe+ gist gh-md gandalf-theme fuzzy flyspell-correct-ivy flycheck-pos-tip flycheck-haskell flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator fasd farmhouse-theme fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu espresso-theme eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav ein dumb-jump dockerfile-mode docker django-theme disaster diff-hl define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme counsel-projectile company-web company-statistics company-restclient company-quickhelp company-ghci company-ghc company-emacs-eclim company-cabal company-c-headers company-auctex company-anaconda command-log-mode column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode cmm-mode cmake-mode clues-theme clojure-snippets clojure-semantic clj-refactor clean-aindent-mode clang-format cider-eval-sexp-fu cherry-blossom-theme calfw busybee-theme bubbleberry-theme birds-of-paradise-plus-theme bbdb-vcard badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme all-the-icons alect-themes afternoon-theme adaptive-wrap ace-window ace-link ac-ispell)))
 '(safe-local-variable-values
   (quote
    ((cider-cljs-lein-repl . "(do (user/go) (user/cljs-repl))")
     (cider-refresh-after-fn . "reloaded.repl/resume")
     (cider-refresh-before-fn . "reloaded.repl/suspend"))))
 '(wakatime-cli-path "/run/current-system/sw/bin/wakatime")
 '(wakatime-python-bin nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(spacemacs-normal-face ((t (:inherit (quote mode-line) :background "#51afef" :foreground "#282c34")))))
)
