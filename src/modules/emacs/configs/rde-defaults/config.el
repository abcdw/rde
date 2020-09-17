(use-package files
  :config
  ;; Most of the files are under version control anyway.
  (setq make-backup-files nil)
  ;; Doesn't make a lot of sense for single user setup, also
  ;; modification date will notify in case someone edit file.
  (setq create-lockfiles nil)

  ;; Add newline at the end of the file on save, the reason:
  ;; https://stackoverflow.com/questions/729692/why-should-text-files-end-with-a-newline
  (setq require-final-newline t))

(use-package custom
  :config
  ;; According to XDG files with data should be placed in
  ;; ~/.local/share, btw it's not necessary to use customize at all
  (setq custom-file rde/custom-file))

(use-package emacs
  )
