(defun c:PROXYEXPLODE (/ *error* oldCmdEcho oldCtab oldNomutt spaces mode ss totalCount i ent space successCount failCount explodeInvisible entData proxyFlag isVisible)
  ;; 错误处理函数 
  (defun *error* (msg)
    (if oldNomutt (setvar "NOMUTT" oldNomutt))  ;; 恢复NOMUTT设置 
    (if (and oldCmdEcho (not (equal oldCmdEcho (getvar "CMDECHO"))))
      (setvar "CMDECHO" oldCmdEcho)
    )
    (if (and oldCtab (not (equal oldCtab (getvar "CTAB"))))
      (setvar "CTAB" oldCtab)
    )
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*"))
      (princ (strcat "\n错误: " msg))
    )
    (princ)
  )
  
  ;; 保存当前设置 
  (setq oldCmdEcho (getvar "CMDECHO")
        oldCtab (getvar "CTAB")
        oldNomutt (getvar "NOMUTT"))  ;; 保存NOMUTT初始值 
  
  ;; 用户选择范围 - 默认改为"选择(Select)"
  (initget "All Model Layout Current Select")
  (setq mode (getkword "\n选择范围 [全部(All)/模型(Model)/布局(Layout)/当前(Current)/选择(Select)] <Select>: "))
  (if (not mode) (setq mode "Select"))  ; 默认选择模式 
  
  (setvar "CMDECHO" 0)   ; 关闭命令回显 
  (setvar "NOMUTT" 1)    ; 关键修改：禁止命令提示（抑制错误输出）
  
  ;; 初始化计数器 
  (setq totalCount 0 
        successCount 0 
        failCount 0)
  
  (cond 
    ;; ========================
    ;; 手动选择模式 (默认模式)
    ;; ========================
    ((= mode "Select")
     (princ "\n请选择要分解的代理对象: ")
     (if (setq ss (ssget (list '(0 . "ACAD_PROXY_ENTITY"))))
       (progn 
         (repeat (setq i (sslength ss))
           (setq ent (ssname ss (setq i (1- i))))
           ;; 检查是否为代理对象 
           (if (and (setq entData (entget ent))
                    (setq proxyFlag (cdr (assoc 0 entData)))
                    (= proxyFlag "ACAD_PROXY_ENTITY"))
             (progn 
               ;; 尝试分解对象，成功则增加successCount，失败则增加failCount
               (if (vl-catch-all-error-p 
                     (vl-catch-all-apply 
                       '(lambda ()
                          (command "._EXPLODE" ent "")
                        )
                     )
                   )
                 (setq failCount (1+ failCount)) ; 分解失败计数 
                 (setq successCount (1+ successCount)) ; 分解成功计数
               )
               (setq totalCount (1+ totalCount))
             )
             ;; 如果不是代理对象，输出警告（检查句柄是否存在）
             (princ (strcat "\n警告: 对象 " (if (assoc 5 entData) (cdr (assoc 5 entData)) "未知对象") " 不是代理对象，已跳过"))
           )
         )
         (princ (strcat "\n分解了 " (itoa totalCount) " 个选择的代理对象"))
       )
       (princ "\n未选择任何代理对象，操作取消")
     )
    )
    
    ;; ================================= 
    ;; 自动选择模式（全部/模型/布局/当前）
    ;; ================================= 
    (T 
     ;; 确定要处理的空间列表 
     (cond 
       ((= mode "All")   ; 全部空间 
        (setq spaces (cons "Model" (layoutlist))))
       ((= mode "Model") ; 仅模型空间 
        (setq spaces '("Model")))
       ((= mode "Layout") ; 仅布局空间 
        (setq spaces (layoutlist)))
       ((= mode "Current") ; 当前空间 
        (setq spaces (list (getvar "CTAB"))))
     )
     
     ;; 询问是否分解无图形表示的代理对象 - 默认N 
     (initget "Y N")
     (setq explodeInvisible (getkword "\n是否分解无图形表示的代理对象? [是(Y)/否(N)] <N>: "))
     (if (not explodeInvisible) (setq explodeInvisible "N")) ; 默认不分解 
     
     ;; 检查所有空间中是否存在代理对象 
     (setq hasProxy nil)
     (foreach space spaces 
       (if (ssget "_X" (list '(0 . "ACAD_PROXY_ENTITY") (cons 410 space)))
         (setq hasProxy T)
       )
     )
     
     (if hasProxy 
       (progn 
         ;; 遍历所有选定空间 
         (foreach space spaces 
           (setvar "CTAB" space)  ; 切换到当前空间 
           
           ;; 获取当前空间中的代理对象 
           (if (setq ss (ssget "_X" (list '(0 . "ACAD_PROXY_ENTITY") (cons 410 space))))
             (progn 
               (princ (strcat "\n空间 [" space "] 中找到 " (itoa (sslength ss)) " 个代理对象"))
                
               ;; 分解当前空间中的代理对象 
               (repeat (setq i (sslength ss))
                 (setq ent (ssname ss (setq i (1- i))))
                 (setq entData (entget ent))
                 
                 ;; 检查对象可见性 (组码60)
                 (setq isVisible (if (assoc 60 entData)
                                     (zerop (cdr (assoc 60 entData)))
                                     T)) ; 没有60组码默认为可见 
                 
                 ;; 根据可见性决定是否分解 
                 (cond 
                   ((or isVisible (= explodeInvisible "Y")) ; 可见或用户选择分解不可见 
                    (if (vl-catch-all-error-p 
                          (vl-catch-all-apply 
                            '(lambda ()
                               (command "._EXPLODE" ent "")
                             )
                          )
                        )
                      (setq failCount (1+ failCount)) ; 分解失败计数 
                      (setq successCount (1+ successCount)) ; 分解成功计数 
                    )
                    (setq totalCount (1+ totalCount))
                   )
                   (T ; 不可见且用户选择不分解 
                    (princ (strcat "\n  - 跳过对象: " (if (assoc 5 entData) (cdr (assoc 5 entData)) "未知对象") " (不可见)"))
                   )
                 )
               )
             )
             (princ (strcat "\n在空间 [" space "] 中未找到代理对象"))
           )
         )
       )
       (princ "\n在所有选定空间中均未找到代理对象")
     )
    )
  )
  
  ;; 恢复原始设置 
  (setvar "NOMUTT" oldNomutt)  ;; 关键修改：恢复NOMUTT初始值 
  (setvar "CTAB" oldCtab)
  (setvar "CMDECHO" oldCmdEcho)
  
  ;; =================== 
  ;; 显示总结信息 
  ;; =================== 
  (princ "\n===========================================")
  (cond 
    ((> totalCount 0)
     (princ (strcat "\n操作完成! 共尝试分解 " (itoa totalCount) " 个代理对象"))
     (princ (strcat "\n成功: " (itoa successCount) " 个"))
     (princ (strcat "\n失败: " (itoa failCount) " 个")))
    ((= mode "Select")
     (princ "\n未分解任何代理对象"))
    (T 
     (princ "\n未找到任何代理对象"))
  )
  (princ "\n===========================================")
  (princ)
)
 
;; 加载提示
(princ "\nPROXYEXPLODE 命令已加载。输入 PROXYEXPLODE 运行。")
(princ)