;打开超大图纸，将此文件直接拖入cad即可。
(setvar "cmdecho" 0)
(dictremove (namedobjdict) "ACAD_DGNLINESTYLECOMP")
(command "_.qsave")
(command "_audit" "y")
(command "_purge" "all" "" "n")
(command "_.qsave")
(setvar "cmdecho" 1)
(princ)