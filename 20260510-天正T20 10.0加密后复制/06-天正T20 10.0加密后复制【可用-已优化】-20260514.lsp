(defun c:COPY-BLOCK ( / ent extDict blkRec doc curSpace ss vlaNewList vlaObj copyResult err)
  (vl-load-com)
 
  ;; ── 获取当前文档和活动空间（修正 TILEMODE 判断） ── 
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (if (= (getvar "TILEMODE") 1)
    (setq curSpace (vla-get-ModelSpace doc))
    (progn 
      ;; TILEMODE = 0 时，可能激活的是浮动模型空间（视口内）
      ;; 使用 ActiveSpace 属性获取正确的空间对象 
      (if (and (= (getvar "CVPORT") 1))  ; 1 表示图纸空间 
        (setq curSpace (vla-get-PaperSpace doc))
        (setq curSpace (vla-get-ModelSpace doc))
      )
    )
  )
 
  ;; ── 选择加密实体 ── 
  (setq ent (car (entsel "\n选择加密实体: ")))
  (if (not ent)
    (progn (princ "\n未选择实体，退出。") (quit))
  )
 
  ;; ── 追溯扩展字典链（修正：遍历字典条目，查找第一个块表记录） ── 
  (setq extDict (cdr (assoc 360 (entget ent))))
  (if (not extDict)
    (progn (princ "\n该实体没有扩展字典，可能不是加密实体。") (quit))
  )
 
  (setq blkRec nil)
  ;; 遍历扩展字典的所有条目，寻找类型为 "AcDbBlockTableRecord" 的对象 
  (vl-catch-all-apply 
    '(lambda ( / dictList item3 item360 owner)
       (setq dictList (entget extDict))
       (while (setq item3 (assoc 3 dictList))
         ;; 查找与该条目名称对应的 360 组码 
         (setq item360 (cdr (member item3 dictList))) ; 取后续列表 
         (setq item360 (assoc 360 item360))           ; 在当前条目后找第一个 360 
         (if item360 
           (progn 
             (setq owner (cdr item360))
             (if (and (vlax-ename->vla-object owner)  ; 验证对象存在 
                      (wcmatch (vla-get-ObjectName (vlax-ename->vla-object owner)) "AcDbBlockTableRecord"))
               (setq blkRec owner)
             )
           )
         )
         ;; 跳过已处理的 (3 . ...) 和 (360 . ...)，继续循环 
         (setq dictList (cdr (member item360 dictList)))
       )
     )
  )
 
  (if (not blkRec)
    (progn (princ "\n在扩展字典中未找到块记录，可能不是加密实体。") (quit))
  )
 
  ;; ── 初始化选择集 ── 
  (setq ss (ssadd))
  (setq vlaNewList '())                     ; 空列表初始 
 
  ;; ── 策略1：遍历块记录内的对象 ── 
  (setq err 
    (vl-catch-all-apply 
      '(lambda ()
         (vlax-for obj (vlax-ename->vla-object blkRec)
           (setq vlaNewList (cons obj vlaNewList))
         )
       )
    )
  )
 
  ;; 区分错误和空结果 
  (if (vl-catch-all-error-p err)
    (princ (strcat "\n遍历块记录时出错: " (vl-catch-all-error-message err)))
    (if vlaNewList 
      (progn 
        (princ (strcat "\n块记录中找到 " (itoa (length vlaNewList)) " 个内部对象，开始复制..."))
        (foreach vlaObj vlaNewList 
          (setq copyResult 
            (vl-catch-all-apply 
              'vlax-invoke 
              (list doc 'CopyObjects (list vlaObj) curSpace nil)
            )
          )
          (if (not (vl-catch-all-error-p copyResult))
            (progn 
              ;; 确保返回值是列表（复制单个对象时可能返回单个 VLA 对象）
              (if (not (listp copyResult))
                (setq copyResult (list copyResult))
              )
              (foreach newObj copyResult 
                (ssadd (vlax-vla-object->ename newObj) ss)
              )
            )
            (princ (strcat "\n复制失败: " (vl-catch-all-error-message copyResult)))
          )
        )
      )
      ;; ── 策略2：块记录为空，尝试爆炸加密实体（仅当为 INSERT） ── 
      (if (= (cdr (assoc 0 (entget ent))) "INSERT")
        (progn 
          (princ "\n块记录为空，尝试爆炸加密实体...")
          (setq explodedList 
            (vl-catch-all-apply 'vla-Explode (list (vlax-ename->vla-object ent)))
          )
          (if (not (vl-catch-all-error-p explodedList))
            (progn 
              ;; Explode 返回的对象数组也需要处理 
              (setq explodedList (vlax-safearray->list (vlax-variant-value explodedList)))
              (foreach obj explodedList 
                (ssadd (vlax-vla-object->ename obj) ss)
              )
              (princ (strcat "\n通过爆炸成功提取 " (itoa (length explodedList)) " 个图元。"))
            )
            (princ (strcat "\n爆炸失败： " (vl-catch-all-error-message explodedList)))
          )
        )
        (princ "\n块记录无对象且加密实体非 INSERT，无法处理。")
      )
    )
  )
 
  ;; ── 输出统计 ── 
  (if (> (sslength ss) 0)
    (princ (strcat "\n共成功提取 " (itoa (sslength ss)) " 个图元到当前空间。"))
    (princ "\n未提取任何图元。")
  )
  (princ)
)
(c:COPY-BLOCK)