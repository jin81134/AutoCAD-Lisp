;;; 随机分配图元到 1~255 图层 
;;; - 普通图元 ByBlock(0)：仅随机换层，不改色 
;;; - 块/标注 ByBlock(0)：移动到与原图层颜色同名的数字层，不改色 
;;; - 其余图元：固定颜色，块/标注移至同色数字层，其它随机 
(defun c:RLC ( / *error* ss i ent typ lay ecolor lcol final_color target_color newidx newlay elist )
 
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*"))
      (princ (strcat "\n错误: " msg))
    )
    (princ)
  )
 
  (defun LM:rand ( / mod )
    (if (not rand-seed) (setq rand-seed (getvar "DATE")))
    (setq mod 65536 
          rand-seed (rem (+ (* 25173 rand-seed) 13849) mod)
    )
    (/ rand-seed mod)
  )
 
  (princ "\n选择要处理的图元...")
  (if (setq ss (ssget "_:L"))
    (progn 
      ;; 创建数字图层 1~255（颜色 = 序号）
      (setq i 1)
      (repeat 255 
        (setq newlay (itoa i))
        (if (not (tblsearch "LAYER" newlay))
          (entmake (list '(0 . "LAYER")
                         '(100 . "AcDbSymbolTableRecord")
                         '(100 . "AcDbLayerTableRecord")
                         (cons 2 newlay)
                         '(70 . 0)
                         (cons 62 i)
                         '(6 . "Continuous")))
        )
        (setq i (1+ i))
      )
 
      ;; 处理图元 
      (setq i 0)
      (repeat (sslength ss)
        (setq ent  (ssname ss i)
              elist (entget ent)
              lay   (cdr (assoc 8 elist))
              typ   (cdr (assoc 0 elist))
              ecolor (cdr (assoc 62 elist))
        )
 
        ;; ============ ByBlock (0) 的特殊处理 ============
        (if (= ecolor 0)
          (progn 
            ;; 获取原图层的颜色（作为目标图层编号）
            (setq lcol (cdr (assoc 62 (tblsearch "LAYER" lay))))
            (if (or (not lcol) (member lcol '(0 256)))
              (setq target_color 7)            ; 兜底白色 
              (setq target_color lcol)
            )
 
            (if (member typ '("INSERT" "DIMENSION" "LEADER" "MULTILEADER"))
              ;; 块/标注 ByBlock → 移动到与原图层颜色相同的数字图层 
              (setq newlay (itoa target_color))
              ;; 普通图元 ByBlock → 随机 1~255 
              (setq newidx (1+ (fix (* (LM:rand) 255)))
                    newlay (itoa newidx))
            )
 
            ;; 只更换图层，不修改任何颜色组码 
            (setq elist (subst (cons 8 newlay) (assoc 8 elist) elist))
            (entmod elist)
            (setq i (1+ i))
          ) ; 结束 ByBlock 分支 
 
          ;; ============ 非 ByBlock 情况 ============
          (progn 
            ;; 计算最终颜色 
            (if (and ecolor (<= 1 ecolor 255))
              (setq final_color ecolor)       ; 图元自有颜色 
              ;; 否则取图层颜色 
              (progn 
                (setq lcol (cdr (assoc 62 (tblsearch "LAYER" lay))))
                (if (or (not lcol) (member lcol '(0 256)))
                  (setq final_color 7)
                  (setq final_color lcol)
                )
              )
            )
 
            ;; 强制写入最终颜色 
            (foreach dxf '(62 420 430)
              (if (assoc dxf elist)
                (setq elist (vl-remove (assoc dxf elist) elist))
              )
            )
            (setq elist (append elist (list (cons 62 final_color))))
 
            ;; 确定新图层 
            (if (member typ '("INSERT" "DIMENSION" "LEADER" "MULTILEADER"))
              (setq newlay (itoa final_color))     ; 块/标注 → 同色层 
              (progn                                ; 其他 → 随机 
                (setq newidx (1+ (fix (* (LM:rand) 255)))
                      newlay (itoa newidx))
              )
            )
 
            ;; 换层 
            (setq elist (subst (cons 8 newlay) (assoc 8 elist) elist))
            (entmod elist)
            (setq i (1+ i))
          ) ; 结束非 ByBlock 分支 
        )
      ) ; repeat 
 
      (princ (strcat "\n完成！共处理 " (itoa (sslength ss))
                     " 个图元。\n块/标注 ByBlock → 移到原图层颜色对应的数字层；"
                     "\n普通 ByBlock → 仅随机换层；"
                     "\n其余图元 → 颜色固定，块/标注移至同色数字层，其他随机。"))
    )
    (princ "\n未选择有效图元，退出。")
  )
  (princ)
)
(c:RLC)