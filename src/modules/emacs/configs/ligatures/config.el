(use-package composite
  ;; Ligatures setup: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
  ;; https://andreyorst.gitlab.io/posts/2020-07-21-programming-ligatures-in-emacs/
  :config
  (let ((ligatures `((?-  . ,(regexp-opt '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->")))
                     (?/  . ,(regexp-opt '("/**" "/*" "///" "/=" "/==" "/>" "//")))
                     (?*  . ,(regexp-opt '("*>" "***" "*/")))
                     (?<  . ,(regexp-opt '("<-" "<<-" "<=>" "<=" "<|" "<||" "<|||::=" "<|>" "<:" "<>" "<-<"
                                           "<<<" "<==" "<<=" "<=<" "<==>" "<-|" "<<" "<~>" "<=|" "<~~" "<~"
                                           "<$>" "<$" "<+>" "<+" "</>" "</" "<*" "<*>" "<->" "<!--")))
                     (?:  . ,(regexp-opt '(":>" ":<" ":::" "::" ":?" ":?>" ":=")))
                     (?=  . ,(regexp-opt '("=>>" "==>" "=/=" "=!=" "=>" "===" "=:=" "==")))
                     (?!  . ,(regexp-opt '("!==" "!!" "!=")))
                     (?>  . ,(regexp-opt '(">]" ">:" ">>-" ">>=" ">=>" ">>>" ">-" ">=")))
                     (?&  . ,(regexp-opt '("&&&" "&&")))
                     (?|  . ,(regexp-opt '("|||>" "||>" "|>" "|]" "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||")))
                     (?.  . ,(regexp-opt '(".." ".?" ".=" ".-" "..<" "...")))
                     (?+  . ,(regexp-opt '("+++" "+>" "++")))
                     (?\[ . ,(regexp-opt '("[||]" "[<" "[|")))
                     (?\{ . ,(regexp-opt '("{|")))
                     (?\? . ,(regexp-opt '("??" "?." "?=" "?:")))
                     (?#  . ,(regexp-opt '("####" "###" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" "##")))
                     (?\; . ,(regexp-opt '(";;")))
                     (?_  . ,(regexp-opt '("_|_" "__")))
                     (?\\ . ,(regexp-opt '("\\" "\\/")))
                     (?~  . ,(regexp-opt '("~~" "~~>" "~>" "~=" "~-" "~@")))
                     (?$  . ,(regexp-opt '("$>")))
                     (?^  . ,(regexp-opt '("^=")))
                     (?\] . ,(regexp-opt '("]#"))))))
    (dolist (char-regexp ligatures)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring]))))
  :hook (prog-mode-hook . auto-composition-mode)
  :init (global-auto-composition-mode -1))

