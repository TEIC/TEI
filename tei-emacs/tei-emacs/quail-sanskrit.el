;; quail package for Sanskrit transliterated-input
;; Lou Burnard, December 2001
;; from specification by Dominik Wujastyk
(quail-define-package
 "sanskrit-romanized-postfix" "Latin-1" "SK>" t
 "Romanized sanskrit with post-modifiers

    effect   | postfix| examples
 ------------+--------+----------
    tilde    |   ~    | n~ -> ,Aq(B
    macron   |   -    | a- -> ,D`(B
    udot     |   .    | t. -> $,1nM(B
    
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A-" ?,D@(B)
 ("D." ?$,1mL(B)
 ("H." ?$,1md(B)
 ("I-" ?,DO(B)
 ("L." ?$,1mv(B)
 ("M." ?$,1n"(B)
 ("N`" ?$,1n$(B)
 ("N~" ?,MQ(B)
 ("N." ?$,1n&(B)
 ("R." ?$,1n:(B)
 ("R.-" ?$,1n<(B)
 ("S'" ?,B&(B)
 ("S." ?$,1nB(B)
 ("T." ?$,1nL(B)
 ("U-" ?,D^(B)
 ("a-" ?,D`(B)
 ("d." ?$,1mM(B)
 ("h." ?$,1me(B)
 ("i-" ?,Do(B)
 ("l." ?$,1mw(B)
 ("m." ?$,1n#(B)
 ("n`" ?$,1n%(B)
 ("n~" ?,Mq(B)
 ("n." ?$,1n'(B)
 ("r." ?$,1n;(B)
 ("r.-" ?$,1n=(B)
 ("s'" ?,B6(B)
 ("s." ?$,1nC(B)
 ("t." ?$,1nM(B)
 ("u-" ?,D~(B)
 ("A--" ["A-"])
 ("D.." ["D."])
 ("H.." ["H."])
 ("I--" ["I-"])
 ("L.." ["L."])
 ("M.." ["M."])
 ("N.." ["N."])
 ("N``" ["N`"])
 ("N~~" ["N~"])
 ("R.." ["R."])
 ("S''" ["S'"])
 ("S.." ["S."])
 ("T.." ["T."])
 ("U--" ["U-"])
 ("a--" ["a-"])
 ("d.." ["d."])
 ("h.." ["h."])
 ("i--" ["i-"])
 ("l.." ["l."])
 ("m.." ["m."])
 ("n.." ["n."])
 ("n``" ["n`"])
 ("n~~" ["n~"])
 ("r.." ["r."])
 ("s''" ["s'"])
 ("s.." ["s."])
 ("t.." ["t."])
 ("u--" ["u-"])


)

