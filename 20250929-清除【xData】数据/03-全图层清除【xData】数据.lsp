(defun c:XDataCleaner ( / *error* SaveLayerStates RestoreLayerStates UnlockAllLayers ClearEntXData 
                          layerMode opt ss ent count confirm origLayerStates 
                          layerName layerData layerLocked i entList flags)
  ;;; --- 错误处理函数 ---
  (defun *error* (msg)
    (if origLayerStates (RestoreLayerStates origLayerStates))
    (if (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
      (princ (strcat "\n错误: " msg))
    )
    (princ)
  )
  
  ;;; --- 图层状态管理函数 ---
  
  ;;; 保存所有图层状态
  (defun SaveLayerStates ()
    (mapcar '(lambda (layer)
               (list (cdr (assoc 2 layer))  ; 图层名
                     (cdr (assoc 70 layer)) ; 状态标志
                     (cdr (assoc 62 layer)) ; 颜色值
               )
             )
            (GetAllLayers)
    )
  )
  
  ;;; 获取所有图层
  (defun GetAllLayers ( / layers layer)
    (setq layers '())
    (tblnext "LAYER" T) ; 重置图层表
    (while (setq layer (tblnext "LAYER"))
      (setq layers (cons layer layers))
    )
    layers
  )
  
  ;;; 恢复图层状态 
  (defun RestoreLayerStates (states)
    (foreach state states
      (setq layerName (car state)
            flags (cadr state)
            color (caddr state))
      (if (tblsearch "LAYER" layerName)
        (progn 
          (setq layerEnt (tblobjname "LAYER" layerName)
                layerData (entget layerEnt))
          ; 恢复原始状态 
          (entmod (subst (cons 70 flags) (assoc 70 layerData) 
                  (subst (cons 62 color) (assoc 62 layerData) layerData)))
        )
      )
    )
    (princ "\n? 图层状态已恢复")
  )
  
  ;;; 解锁所有图层 (修复lognot问题)
  (defun UnlockAllLayers ( / layers layer layerEnt layerData)
    (setq layers (GetAllLayers))
    (foreach layer layers 
      (setq layerName (cdr (assoc 2 layer))
            layerEnt (tblobjname "LAYER" layerName))
      (if layerEnt
        (progn 
          (setq layerData (entget layerEnt)
                flags (cdr (assoc 70 layerData)))
          ; 修复：使用位运算清除锁定标志(4)
          (entmod (subst (cons 70 (logand flags 65531)) ; 65535 - 4 = 65531 
                         (assoc 70 layerData) layerData))
        )
      )
    )
    (princ "\n? 所有图层已临时解锁")
  )
  
  ;;; --- 核心功能函数 ---
  
  ;;; 清除单个实体的XData
  (defun ClearEntXData (ent / xd appList)
    (if (setq xd (assoc -3 (entget ent '("*"))))
      (progn 
        (setq appList (mapcar '(lambda (x) (list (car x))) (cdr xd)))
        (entmod (append (entget ent) (list (cons -3 appList))))
        T 
      )
      nil
    )
  )
  
  ;;; --- 主程序开始 --- 
  (setq origLayerStates (SaveLayerStates)) ; 保存初始图层状态
  
  ;;; 选择图层模式
  (initget "Unlocked AllLayers")
  (setq layerMode (getkword "\n图层模式 [仅已解锁(Unlocked)/全图层(AllLayers)] <Unlocked>: "))
  (if (null layerMode) (setq layerMode "AllLayers"))
  
  ;;; 选择清除模式 
  (initget "All Select")
  (setq opt (getkword (strcat "\n清除范围 [全部(All)/选择(Select)] <Select> (当前图层模式: " layerMode "): ")))
  (if (null opt) (setq opt "Select"))
  
  (cond
    ((= opt "Select") ; 选择清除模式 
      (prompt "\n选择要清除XData的实体: ")
      (if (setq ss (ssget))
        (progn 
          (setq count 0)
          
          ; 全图层模式需要临时解锁 
          (if (= layerMode "AllLayers")
            (UnlockAllLayers)
          )
          
          (repeat (setq i (sslength ss))
            (setq ent (ssname ss (setq i (1- i)))
                  entList (entget ent)
                  layerName (cdr (assoc 8 entList)))
            
            (setq layerLocked
              (if (setq layerData (tblsearch "LAYER" layerName))
                (= (logand (cdr (assoc 70 layerData)) 4) 4)
                nil
              )
            )
            
            (cond 
              ((or (= layerMode "AllLayers") (not layerLocked))
               (if (ClearEntXData ent)
                 (progn 
                   (prompt (strcat "\n? 已清除实体 " (cdr (assoc 5 entList)) " 的XData"))
                   (setq count (1+ count))
                 )
                 (prompt (strcat "\n? 实体 " (cdr (assoc 5 entList)) " 无XData"))
               )
              )
              (t 
               (prompt (strcat "\n? 跳过锁定图层实体: " (cdr (assoc 5 entList))))
              )
            )
          )
          
          (prompt (strcat "\n>> 操作完成! 成功清除 " (itoa count) "/" (itoa (sslength ss)) " 个实体"))
        )
        (prompt "\n? 未选择任何实体")
      )
    )
    
    ((= opt "All") ; 全图清除模式
      (prompt "\n? 警告: 即将清除图形中所有")
      (princ (if (= layerMode "Unlocked") "未锁定图层" "所有图层"))
      (prompt "实体的XData!")
      
      (setq confirm (strcase (getstring "\n确认操作? (Y/N) <N>: ")))
      (if (wcmatch confirm "Y*")
        (progn 
          ; 全图层模式需要临时解锁 
          (if (= layerMode "AllLayers")
            (UnlockAllLayers)
          )
          
          (setq count 0)
          (setq ent (entnext))
          (while ent
            (setq entList (entget ent)
                  layerName (cdr (assoc 8 entList)))
            (setq layerLocked
              (if (setq layerData (tblsearch "LAYER" layerName))
                (= (logand (cdr (assoc 70 layerData)) 4) 4)
                nil
              )
            )
            
            (if (or (= layerMode "AllLayers") (not layerLocked))
              (if (ClearEntXData ent)
                (setq count (1+ count))
            ))
            (setq ent (entnext ent))
          )
          
          (prompt (strcat "\n>> 操作完成! 共清除 " (itoa count) " 个实体的XData"))
        )
        (prompt "\n? 操作已取消")
      )
    )
  )
  
  ;;; 恢复原始图层状态
  (RestoreLayerStates origLayerStates)
  (princ)
)
(princ "\n命令已加载，输入 XDataCleaner 运行")
(c:XDataCleaner)