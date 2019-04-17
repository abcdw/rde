;;; packages.el --- org-additions layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Andrew Tropin <abcdw@a-xps13>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `org-additions-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `org-additions/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `org-additions/pre-init-PACKAGE' and/or
;;   `org-additions/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst org-additions-packages
  '(org-gcal
    calfw
    ;; org-ref
    ))


(defun org-additions/init-org-gcal ()
  (use-package org-gcal))

(defun org-additions/init-calfw ()
  (use-package calfw-ical))

;;; packages.el ends here
