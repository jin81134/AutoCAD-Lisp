(defun c:TMerge (/ *error* doc layers layerNames hiddenLayers ss mergeDistance 
                   textHeight defaultDist processedLayers lastSs continue 
                   textList circles mergedGroups mergedTexts count tempCircles 
                   toDelete)
  
  ;; 错误处理函数 
  (defun *error* (msg)
    ;; 恢复隐藏的图层 
    (if (and hiddenLayers (not (vlax-erased-p (car hiddenLayers))))
      (mapcar '(lambda (x) (vla-put-layeron x :vlax-true)) hiddenLayers)
    )
    ;; 删除所有临时圆 
    (foreach circle tempCircles 
      (if (entget circle)
        (entdel circle)
      )
    )
    (if (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
      (princ (strcat "\n错误: " msg))
    )
    (princ)
  )
  
  ;; 安全获取DXF组码值 
  (defun safe-cdr (code elist)
    (if (assoc code elist) (cdr (assoc code elist)) nil)
  )
  
  ;; 获取文字关键属性 
  (defun GetTextProps (ent / data props)
    (setq data (entget ent))
    (setq props (list 
      (cons 0 (safe-cdr 0 data))     ; 类型 
      (cons 8 (safe-cdr 8 data))     ; 图层 
      (cons 10 (safe-cdr 10 data))   ; 插入点 
      (cons 11 (safe-cdr 11 data))   ; 对齐点 
      (cons 1 (safe-cdr 1 data))     ; 内容 
      (cons 7 (safe-cdr 7 data))     ; 文字样式 
      (cons 40 (safe-cdr 40 data))   ; 字高 
      (cons 41 (safe-cdr 41 data))   ; 宽度因子 
      (cons 50 (safe-cdr 50 data))   ; 旋转角度（弧度）
      (cons 72 (safe-cdr 72 data))   ; 水平对齐 
      (cons 73 (safe-cdr 73 data))   ; 垂直对齐 
      (cons 71 (safe-cdr 71 data))   ; 文字生成标志（镜像等）
      (cons 51 (safe-cdr 51 data))   ; 倾斜角度
    ))
    props 
  )
  
  ;; 创建文字实体（修复对齐方式问题）
  (defun CreateTextWithProps (pos content props / newEnt horizAlign vertAlign insertPt alignPt hasAlignment)
    ;; 获取对齐方式 
    (setq horizAlign (cdr (assoc 72 props)))
    (setq vertAlign (cdr (assoc 73 props)))
    (setq insertPt (cdr (assoc 10 props)))
    (setq alignPt (cdr (assoc 11 props)))
    
    ;; 判断是否有对齐设置（非左对齐/基线对齐）
    (setq hasAlignment (or (and horizAlign (/= horizAlign 0))
                           (and vertAlign (/= vertAlign 0))))
    
    ;; 构建基本属性列表
    (setq newEnt (list 
      '(0 . "TEXT")
      (cons 8 (cdr (assoc 8 props)))     ; 图层 
      (cons 1 content)                   ; 内容 
      (cons 7 (cdr (assoc 7 props)))     ; 文字样式 
      (cons 40 (cdr (assoc 40 props)))   ; 字高 
      (cons 41 (cdr (assoc 41 props)))   ; 宽度因子 
      (cons 50 (cdr (assoc 50 props)))   ; 旋转角度 
      (cons 71 (cdr (assoc 71 props)))   ; 文字生成标志 
      (cons 51 (cdr (assoc 51 props)))   ; 倾斜角度
    ))
    
    (if hasAlignment 
      ;; 有对齐设置的情况：正确处理对齐文字 
      (progn
        ;; 对于对齐文字，必须同时设置插入点和对齐点
        (setq newEnt (append newEnt (list 
          (cons 10 insertPt)             ; 保留原始插入点 
          (cons 11 pos)                  ; 对齐点设为新位置
          (cons 72 horizAlign)           ; 水平对齐 
          (cons 73 vertAlign)            ; 垂直对齐
        )))
        
        ;; 特殊处理：对于"对齐"和"布满"方式，需要确保字高不为0
        (if (or (= horizAlign 1) (= horizAlign 3) (= horizAlign 5)) ; 对齐、中心、右对齐
          (progn 
            ;; 确保字高有效 
            (if (or (null (cdr (assoc 40 newEnt))) (= (cdr (assoc 40 newEnt)) 0.0))
              (setq newEnt (subst (cons 40 2.5) (assoc 40 newEnt) newEnt)) ; 设置默认字高 
            )
          )
        )
      )
      ;; 无对齐设置的情况：使用插入点，设为左对齐
      (progn 
        (setq newEnt (append newEnt (list 
          (cons 10 pos)                  ; 插入点 
          (cons 72 0)                    ; 水平对齐设为左对齐
          (cons 73 0)                    ; 垂直对齐设为基线对齐 
        )))
      )
    )
    
    ;; 创建文字实体
    (entmake newEnt)
  )
  
  ;; 获取文字的实际位置点（修复位置问题）
  (defun GetTextPosition (props / horizAlign vertAlign insertPt alignPt)
    (setq horizAlign (cdr (assoc 72 props)))
    (setq vertAlign (cdr (assoc 73 props)))
    (setq insertPt (cdr (assoc 10 props)))
    (setq alignPt (cdr (assoc 11 props)))
    
    ;; 如果有对齐点且对齐方式不是左对齐/基线对齐，使用对齐点 
    (if (and alignPt 
             (or (and horizAlign (/= horizAlign 0))
                 (and vertAlign (/= vertAlign 0))))
      alignPt    ; 使用对齐点作为实际位置
      insertPt   ; 否则使用插入点 
    )
  )
  
  ;; 获取文字的有效字高（处理对齐文字的特殊情况）
  (defun GetEffectiveTextHeight (props / horizAlign height)
    (setq horizAlign (cdr (assoc 72 props)))
    (setq height (cdr (assoc 40 props)))
    
    ;; 对于某些对齐方式，字高可能为0，需要处理 
    (cond 
      ((and height (> height 0.0)) height) ; 正常字高 
      ((or (= horizAlign 1) (= horizAlign 3) (= horizAlign 5)) ; 对齐、中心、右对齐 
       2.5) ; 返回默认字高 
      (t 2.5) ; 其他情况也返回默认字高 
    )
  )
  
  ;; 创建临时圆（用于可视化合并范围）
  (defun CreateTempCircle (center radius)
    (entmakex (list 
      '(0 . "CIRCLE")
      '(8 . "TEMP") ; 放在临时图层 
      (cons 10 center)
      (cons 40 radius)
      '(62 . 1) ; 红色 
    ))
  )
  
  ;; 判断点是否在圆内 
  (defun IsPointInCircle (pt center radius)
    (<= (distance pt center) radius)
  )
  
  ;; 弧度转角度 
  (defun RadToDeg (radians)
    (* radians (/ 180.0 pi))
  )
  
  ;; 角度转弧度 
  (defun DegToRad (degrees)
    (* degrees (/ pi 180.0))
  )
  
  ;; 优化函数：计算投影值（用于文字排序）
  (defun CalculateProjection (pt baseAngRad / vx vy)
    (setq vx (cos baseAngRad))  ; X方向分量 
    (setq vy (sin baseAngRad))  ; Y方向分量 
    (+ (* (car pt) vx) (* (cadr pt) vy))  ; 点积结果 
  )
  
  ;; 获取当前文档和图层集合 
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq layers (vla-get-layers doc))
  
  ;; 初始化变量 
  (setq hiddenLayers nil)    ; 存储被隐藏的图层对象 
  (setq layerNames '())      ; 存储用户选择的图层名 
  (setq mergeDistance nil)   ; 用户指定的合并距离 
  (setq processedLayers 0)   ; 已处理的图层计数 
  (setq lastSs nil)          ; 存储上次的选择集 
  (setq tempCircles nil)     ; 存储临时圆对象 
  
  ;; 主程序循环 
  (while t 
    ;; 步骤1: 选择要合并的文字图层 
    (princ "\n步骤1: 选择要合并的文字图层 (按Enter结束选择): ")
    (setq layerNames '())
    (while 
      (progn 
        (setq ent (car (entsel "\n选择图层上的一个文字对象: ")))
        (cond 
          ((null ent) nil) ; 结束循环 
          ((/= "TEXT" (cdr (assoc 0 (entget ent))))
           (princ "\n所选对象不是文字，请重新选择。") t)
          (t 
           (setq layerName (cdr (assoc 8 (entget ent))))
           (if (not (member layerName layerNames))
             (progn 
               (setq layerNames (cons layerName layerNames))
               (setq layer (vla-item layers layerName))
               (vla-put-layeron layer :vlax-false) ; 隐藏图层 
               (setq hiddenLayers (cons layer hiddenLayers))
               (princ (strcat "\n已选择图层: " layerName " (已隐藏)"))
             )
             (princ (strcat "\n图层 " layerName " 已被选择过"))
           )
           t 
          )
        )
      )
    )
    
    (if (null layerNames)
      (progn (princ "\n未选择任何图层，操作取消。") (exit))
    )
    
    ;; 步骤2: 显示隐藏的图层并选择合并范围 
    (mapcar '(lambda (x) (vla-put-layeron x :vlax-true)) hiddenLayers)
    (setq hiddenLayers nil)
    
    (princ "\n步骤2: 选择要合并的文字范围 (按Enter结束选择): ")
    (setq ss (ssget '((0 . "TEXT"))))
    
    (if (null ss)
      (progn (princ "\n未选择任何文字，操作取消。") (exit))
    )
    
    (setq lastSs ss)
    
    ;; 步骤3: 获取合并距离（使用有效字高）
    (setq textHeight (GetEffectiveTextHeight (entget (ssname ss 0))))
    (setq defaultDist (* textHeight 1.5))
    (initget 6)
    (setq mergeDistance (getdist (strcat "\n输入合并文字的最大距离 <" (rtos defaultDist 2 2) ">: ")))
    (if (null mergeDistance) (setq mergeDistance defaultDist))
    
    ;; 主合并函数 - 使用圆形范围并集方法 
    (defun MergeText (ss layerName / i ent entData textProps textPos textContent textAngle 
                           circles mergedGroups mergedTexts count tempCircle baseAng baseAngRad 
                           toDelete actualPos) ; 新增actualPos变量 
      
      ;; 收集所有符合条件的文字（包括图元名）
      (setq textList '())
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq entData (entget ent))
        (if (and (eq "TEXT" (cdr (assoc 0 entData)))
                 (eq layerName (cdr (assoc 8 entData))))
          (progn 
            (setq textProps (GetTextProps ent))
            ;; 修改：使用正确的文字位置函数 
            (setq actualPos (GetTextPosition textProps))
            (setq textList (cons (list 
                                 actualPos                ; 实际位置（修复位置问题）
                                 (cdr (assoc 1 textProps)) ; 内容 
                                 textProps                 ; 属性 
                                 ent)                      ; 图元名 
                               textList))
          )
        )
        (setq i (1+ i))
      )
      
      (if (null textList)
        (progn (princ (strcat "\n图层 " layerName " 中没有找到可合并的文字。")) (exit))
      )
      
      ;; 为每个文字创建圆形范围（包含图元名）
      (setq circles '())
      (foreach text textList 
        (setq textPos (car text))
        (setq circles (cons (list textPos mergeDistance text) circles))
        ;; 创建临时圆用于可视化 
        (setq tempCircle (CreateTempCircle textPos mergeDistance))
        (setq tempCircles (cons tempCircle tempCircles))
      )
      
      ;; 合并重叠的圆形范围 
      (setq mergedGroups '())
      (while circles 
        (setq currentCircle (car circles))
        (setq currentCenter (car currentCircle))
        (setq currentRadius (cadr currentCircle))
        (setq currentText (caddr currentCircle))
        (setq circles (cdr circles))
        
        (setq merged nil)
        (setq newGroups '())
        
        (foreach group mergedGroups 
          (setq groupMerged nil)
          (foreach circle group 
            (if (IsPointInCircle (car circle) currentCenter currentRadius)
              (setq groupMerged t)
            )
          )
          (if groupMerged 
            (setq merged (append group (list currentCircle)))
            (setq newGroups (cons group newGroups))
          )
        )
        
        (if merged 
          (setq mergedGroups (cons merged newGroups))
          (setq mergedGroups (cons (list currentCircle) mergedGroups))
        )
      )
      
      ;; 处理每个合并组 
      (setq mergedTexts '())
      (setq count 0)
      (setq toDelete '()) ; 新增：存储待删除的图元名 
      
      (foreach group mergedGroups 
        (if (> (length group) 1) ; 只有多个文字才需要合并 
          (progn 
            ;; 获取组内第一个文字的角度作为基准方向 
            (setq baseAng (cdr (assoc 50 (caddr (caddr (car group))))))
            (if baseAng 
              (setq baseAng (RadToDeg baseAng)) ; 弧度转角度 
              (setq baseAng 0.0)
            )
            
            ;; 将角度转换为弧度用于投影计算 
            (setq baseAngRad (DegToRad baseAng))
            
            ;; 优化点：使用投影值排序（确保任何角度下都能正确排序）
            (setq group 
              (vl-sort group 
                (function 
                  (lambda (a b / aPos bPos aProj bProj)
                    (setq aPos (car (caddr a))) ; 文字A位置 
                    (setq bPos (car (caddr b))) ; 文字B位置 
                    ;; 计算位置在基准方向上的投影值 
                    (setq aProj (CalculateProjection aPos baseAngRad))
                    (setq bProj (CalculateProjection bPos baseAngRad))
                    (< aProj bProj) ; 按投影值从小到大排序 
                  )
                )
              )
            )
            
            ;; 使用第一个文字的位置作为合并位置 
            (setq firstText (caddr (car group))) ; 获取排序后第一个文字 
            (setq mergedPos (car firstText))      ; 使用第一个文字的实际位置 
            (setq mergedProps (caddr firstText))  ; 使用第一个文字的属性 
            
            ;; 计算合并后的文字内容 
            (setq mergedContent "")
            (foreach circle group 
              (setq text (caddr circle))
              (setq textContent (cadr text)) 
              (setq mergedContent (strcat mergedContent textContent))
              ;; 添加到待删除列表 
              (setq toDelete (cons (nth 3 text) toDelete)) ; 存储图元名 
            )
            
            ;; 添加到合并结果 
            (setq mergedTexts (cons (list mergedPos mergedContent mergedProps) mergedTexts))
            (setq count (1+ count))
          )
        )
      )
      
      ;; ===== 修复：只删除被合并的文字 ===== 
      (foreach ent toDelete 
        (if (entget ent) ; 确保图元存在 
          (entdel ent)
        )
      )
      
      ;; 创建合并后的文字（使用修复后的创建函数）
      (foreach text mergedTexts 
        (CreateTextWithProps (car text) (cadr text) (caddr text))
      )
      
      (setq processedLayers (1+ processedLayers))
      (princ (strcat "\n图层 " layerName " 合并完成，共合并了 " (itoa count) " 组文字。"))
      (if (> (length textList) (length toDelete))
        (princ (strcat "\n保留了 " (itoa (- (length textList) (length toDelete))) " 个单独的文字。"))
      )
    )
    
    ;; 执行合并（处理每个选中的图层）
    (foreach layerName layerNames 
      (MergeText ss layerName)
    )
    
    ;; 删除所有临时圆 
    (foreach circle tempCircles 
      (if (entget circle)
        (entdel circle)
      )
    )
    (setq tempCircles nil)
    
    (princ (strcat "\n当前合并完成。共处理了 " (itoa processedLayers) " 个图层。"))
    (princ "\n是否继续合并其他文字？(Y/N) <Y>: ")
    (setq continue (getstring))
    (if (and continue (wcmatch (strcase continue) "N*"))
      (progn (princ "\n文字合并工具使用结束。") (exit))
      (princ "\n开始新一轮合并...")
    )
  )
) 
 
;; 加载提示 
(princ "\n文字合并工具已加载，请输入 TMerge 命令运行。")
(princ)
(c:TMerge)