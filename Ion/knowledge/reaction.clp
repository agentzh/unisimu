;  (line 3)
(defrule reaction-3
    (and (ion Ba +2) (ion SO4 -2))
    =>
    (assert (solid BaSO4))
)

;  (line 4)
(defrule reaction-4
    (and (ion Ag +1) (ion Cl -1))
    =>
    (assert (solid AgCl))
)

;  (line 5)
(defrule reaction-5
    (and (ion Mg +2) (ion OH -1))
    =>
    (assert (solid "Mg(OH)2"))
)

;  (line 6)
(defrule reaction-6
    (and (ion Mg +2) (ion CO3 -2))
    =>
    (assert (solid "Mg(OH)2"))
    (assert (gas CO2))
)

;  (line 7)
(defrule reaction-7
    (and (ion Mg +2) (ion HCO3 -1))
    =>
    (assert (solid MgCO3))
    (assert (gas CO2))
    (assert (liquid H2O))
)

;  (line 8)
(defrule reaction-8
    (and (ion Fe +3) (ion SCN -1))
    =>
    (assert (ion FeSCN +2))
)

;  (line 9)
(defrule reaction-9
    (and (ion Fe +3) (ion OH -1))
    =>
    (assert (solid "Fe(OH)3"))
)

;  (line 10)
(defrule reaction-10
    (and (ion S +2) (ion OH -1))
    =>
    (assert (gas H2S))
)

;  (line 11)
(defrule reaction-11
    (and (ion Fe +3) (ion S -2))
    =>
    (assert (solid S))
)

;  (line 12)
(defrule reaction-12
    (and (ion NH4 +1) (ion OH -1))
    =>
    (assert (liquid "NH3.H2O"))
)

;  (line 13)
(defrule reaction-13
    (and (solid Cu) (and (ion H +1) (ion NO -3)))
    =>
    (assert (gas NO))
)

;  (line 14)
(defrule reaction-14
    (and (gas CL2) (and (ion Fe +2) (ion Br -1)))
    =>
    (assert (gas Br2))
)

;  (line 15)
(defrule reaction-15
    (and (ion HS -1) (ion H +1))
    =>
    (assert (gas H2S))
)

;  (line 16)
(defrule reaction-16
    (and (ion HSO3 -1) (ion H +1))
    =>
    (assert (gas SO2))
)

;  (line 17)
(defrule reaction-17
    (and (ion S -2) (ion H +1))
    =>
    (assert (gas H2S))
)

;  (line 18)
(defrule reaction-18
    (and (ion CO3 -2) (ion H +1))
    =>
    (assert (gas CO2))
    (assert (liquid H2O))
)

;  (line 19)
(defrule reaction-19
    (and (ion Ba +2) (ion CO3 -2))
    =>
    (assert (solid BaCO3))
)

;  (line 20)
(defrule reaction-20
    (and (ion Ca +2) (ion CO3 -2))
    =>
    (assert (solid CaCO3))
)

;  (line 21)
(defrule reaction-21
    (and (ion Ca +2) (ion SO4 -2))
    =>
    (assert (solid CaSO4))
)

;  (line 22)
(defrule reaction-22
    (and (ion Mg +2) (ion SO4 -2))
    =>
    (assert (solid MgSO4))
)

;  (line 23)
(defrule reaction-23
    (and (ion Mg +2) (ion CO3 -2))
    =>
    (assert (solid MgCO3))
)

;  (line 24)
(defrule reaction-24
    (and (ion Cu +2) (ion OH -1))
    =>
    (assert (solid "Cu(OH)2"))
)

;  (line 25)
(defrule reaction-25
    (and (ion Fe +3) (ion OH -1))
    =>
    (assert (solid "Fe(OH)3"))
)

;  (line 26)
(defrule reaction-26
    (and (ion Al +3) (ion OH -1))
    =>
    (assert (solid "Al(OH)3"))
)

;  (line 27)
(defrule reaction-27
    (and (ion H +1) (ion OH -1))
    =>
    (assert (liquid H2O))
)

;  (line 28)
(defrule reaction-28
    (and (ion CH3COO -1) (ion H +1))
    =>
    (assert (liquid CH3COOH))
)

;  (line 29)
(defrule reaction-29
    (and (ion AlO -2) (ion Al +3))
    =>
    (assert (solid "Al(OH)3"))
)

;  (line 30)
(defrule reaction-30
    (and (ion I -1) (ion Fe +3))
    =>
    (assert (solid I2))
)

;  (line 31)
(defrule reaction-31
    (and (ion S -2) (and (ion SO3 -2) (ion H +1)))
    =>
    (assert (solid S))
    (assert (liquid H2O))
)

;  (line 37)
(defrule reaction-37
    (and (ion ?X ?bond) (and (or (test (eq ?bond 1)) (test (eq ?bond 2))) (and (test (neq ?X K)) (and (test (neq ?X NH4)) (and (test (neq ?X Na)) (ion PO4 -4))))))
    =>
    (assert (solid (sym-cat ?X (div 4 ?bond) PO4)))
)

;  (line 39)
(defrule reaction-39
    (and (ion ?X +4) (ion PO4 -4))
    =>
    (assert (solid (sym-cat ?X PO4)))
)

;  (line 41)
(defrule reaction-41
    (and (ion ?X +3) (ion PO4 -4))
    =>
    (assert (solid (sym-cat ?X "4(PO4)3")))
)

;  (line 43)
(defrule reaction-43
    (solid ?X)
    =>
    (printout t "{" ?X "}" crlf)
)

;  (line 44)
(defrule reaction-44
    (gas ?X)
    =>
    (printout t "^" ?X crlf)
)

;  (line 45)
(defrule reaction-45
    (liquid ?X)
    =>
    (printout t "~" ?X crlf)
)
