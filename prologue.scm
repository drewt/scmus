(require-extension utf8 utf8-srfi-13)

(import
  (except scheme
    string-length string-ref string-set! make-string string substring
    string->list list->string string-fill! write-char read-char display)
  (except chicken
    reverse-list->string print print*)
  (except data-structures
    ->string conc string-chop string-split string-translate
    substring=? substring-ci=? substring-index substring-index-ci)
  (except extras
    read-string write-string read-token))
