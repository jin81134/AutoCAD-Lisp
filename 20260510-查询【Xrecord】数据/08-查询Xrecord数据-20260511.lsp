;;; ============================================================
;;; 通用扩展字典探查工具  v2.4 (终极适配版)
;;; 使用方法：加载后运行命令 DICTINFO 
;;; 支持：正常字典、Xrecord、保护实体内部块定义等任意结构 
;;; ============================================================
 
(vl-load-com)
 
;; ── 递归探查任意实体或字典 ── 
;; objEname : 实体名 / 字典实体名 
;; indent   : 缩进字符串 
(defun entity-dump-recursive (objEname indent / entry key subEname subDict objType objEnt)
  (if (not objEname)
    (princ (strcat indent "? 对象为 nil\n"))
    (progn 
      ;; 先看它是不是字典实体 
      (setq objEnt (entget objEname '("*")))
      (if (not objEnt)
        (princ (strcat indent "? 无法获取对象数据\n"))
        (progn 
          ;; 若是字典，进入标准遍历 
          (if (= "DICTIONARY" (cdr (assoc 0 objEnt)))
            (progn 
              (setq entry (dictnext objEname t))
              (while entry 
                (setq key (cdr (assoc 3 entry)))
                (cond 
                  ;; 情况1：有键名的标准条目 
                  (key 
                    (if (setq subEname (cdr (assoc 360 entry)))
                      ;; 子字典 
                      (progn 
                        (princ (strcat indent "├─ [字典] " key " (实体名: "))
                        (princ subEname)
                        (princ ")")
                        (terpri)
                        (entity-dump-recursive subEname (strcat indent "│   "))
                      )
                      (if (setq subEname (cdr (assoc 350 entry)))
                        ;; Xrecord 
                        (progn 
                          (princ (strcat indent "├─ [Xrecord] " key " (实体名: "))
                          (princ subEname)
                          (princ ")")
                          (terpri)
                          (princ (strcat indent "│   数据: "))
                          (print (entget subEname '("*")))
                          (terpri)
                        )
                        ;; 未知条目 
                        (progn 
                          (princ (strcat indent "├─ [?] " key " : "))
                          (print entry)
                          (terpri)
                        )
                      )
                    )
                  )
                  ;; 情况2：无键名 → 视作内部关联实体（如保护对象中的块记录）
                  (t 
                    (setq subEname (cdr (assoc -1 entry)))
                    (princ (strcat indent "├─ [内部实体] 类型: " (cdr (assoc 0 entry))
                                    " 句柄: " (cdr (assoc 5 entry))
                                    (if (assoc 2 entry) (strcat " 名称: " (cdr (assoc 2 entry))) "")
                                    (if subEname (strcat " 实体名: " (vl-prin1-to-string subEname)) "")))
                    (terpri)
                    ;; 递归探查该实体的扩展字典 
                    (if subEname 
                      (progn 
                        (setq subDict (cdr (assoc 360 (entget subEname '("*")))))
                        (if subDict 
                          (progn 
                            (princ (strcat indent "│   └─ 扩展字典:"))
                            (terpri)
                            (entity-dump-recursive subDict (strcat indent "│       "))
                          )
                          (princ (strcat indent "│   (该实体无扩展字典)\n"))
                        )
                      )
                      ;; 连实体名都没有，直接输出条目 
                      (print entry)
                    )
                  )
                )
                (setq entry (dictnext objEname nil))
              )
            )
            ;; 如果不是字典，但调用方传过来的是普通实体，则打印其基本信息和扩展字典 
            (progn 
              (princ (strcat indent "对象不是字典，类型: " (cdr (assoc 0 objEnt))
                              " 句柄: " (cdr (assoc 5 objEnt))))
              (setq subDict (cdr (assoc 360 objEnt)))
              (if subDict 
                (progn 
                  (princ (strcat " (有扩展字典)"))
                  (terpri)
                  (entity-dump-recursive subDict (strcat indent "    "))
                )
                (progn 
                  (princ " (无扩展字典)\n")
                )
              )
            )
          )
        )
      )
    )
  )
)
 
;; ── 主命令 ── 
(defun c:DICTINFO ( / ent entDxf extDict xdata xlist app datalist)
  (setq ent (car (entsel "\n选择要探查的实体: ")))
  (if (not ent)
    (princ "\n未选择实体。")
    (progn 
      (terpri)
      (princ "========================== 实体基本信息 ==========================")
      (setq entDxf (entget ent '("*")))
      (print entDxf)
 
      (terpri)
      (princ "========================== 扩展数据 (XDATA) ==========================")
      (setq xdata (assoc -3 entDxf))
      (if xdata 
        (progn 
          (princ "\n发现以下注册应用的 XDATA:")
          (setq xlist (cdr xdata))
          (foreach app xlist 
            (princ "\n应用名: ")
            (princ (car app))
            (princ " 数据: ")
            (setq datalist (cdr app))
            (if (and datalist (null (cdr datalist)))
              (princ (car datalist))
              (princ datalist)
            )
            (terpri)
          )
        )
        (princ "\n该实体没有 XDATA。")
      )
 
      (terpri)
      (princ "========================== 扩展字典 ==========================")
      (setq extDict (cdr (assoc 360 entDxf)))
      (if extDict 
        (progn 
          (princ "\n扩展字典入口实体名: ") (princ extDict)
          (princ "\n扩展字典入口原始数据:")
          (print (entget extDict '("*")))
          (princ "\n完整结构遍历:\n")
          (entity-dump-recursive extDict "  ")
        )
        (princ "\n该实体没有扩展字典。")
      )
    )
  )
  (princ)
)
 
(princ "\n通用扩展字典探查工具 v2.4 已加载。命令: DICTINFO")
(princ)
(c:DICTINFO)