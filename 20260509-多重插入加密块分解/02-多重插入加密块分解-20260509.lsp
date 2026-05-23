(defun c:unlk (/ en ent newent)
  (setq en (entsel "\n请选择被加密的图形:"))
  (if en
    (progn
      (setq en (car en))                ; 获取图元名
      (setq ent (entget en))            ; 获取图元数据
      (if (= (cdr (assoc 0 ent)) "INSERT")
        (progn
          (setq newent (entmakex (list '(0 . "INSERT") (assoc 2 ent) (assoc 10 ent))))
          (if newent
            (progn
              (command "_.explode" newent)
              (entdel en)
              (princ "\n解密成功，若仍未分解请再执行一次。")
            )
            (princ "\n创建临时块失败，解密未完成。")
          )
        )
        (princ "\n所选对象不是块，无法解密。")
      )
    )
    (princ "\n未选择任何对象。")
  )
  (princ)
)
(princ "\n输入“unlk”运行多重插入加密块分解")
(c:unlk)