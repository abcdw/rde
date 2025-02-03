(require 'org)
(setq org-html-head-include-scripts nil)
(setq org-html-head-include-default-style nil)

(require 'ox-html-stable-ids)
(setq org-html-stable-ids t)
(org-html-stable-ids-add)
