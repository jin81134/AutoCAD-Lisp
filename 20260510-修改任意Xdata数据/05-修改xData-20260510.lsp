;;; 通用修改扩展数据 (xdata) - 交互式菜单版（即时写入）
(defun c:XDMOD (/ en ed xdata apps appname datalist done
                  sel idx itm oldcode oldval newval tmp code
                  update-ent)
  ;; 选择对象
  (setq en (car (entsel "\n选择要修改扩展数据的对象: ")))
  (if (not en)
    (progn (princ "\n未选择对象。") (exit))
  )
 
  ;; 获取所有扩展数据 
  (setq ed (entget en '("*")))
  (setq xdata (cdr (assoc -3 ed)))
  (if (not xdata)
    (setq xdata nil)
  )
 
  ;; 提取注册应用程序名 
  (setq apps (if xdata (mapcar 'car xdata) nil))
 
  ;; ---- 内部更新函数：立即写入实体 ----
  (defun update-ent (app data)
    ;; 确保应用名已注册
    (if (not (member app apps))
      (regapp app)  ;; 注册新应用名 
    )
    (if (assoc app xdata)
      (setq xdata (subst (cons app data) (assoc app xdata) xdata))
      (setq xdata (append xdata (list (cons app data))))
    )
    (setq ed (subst (cons -3 xdata) (assoc -3 ed) ed))
    (if (assoc -3 ed)
      (entmod ed)
      (progn 
        (setq ed (append ed (list (cons -3 xdata))))
        (entmod ed)
      )
    )
    ;; 刷新应用名列表 
    (setq apps (mapcar 'car xdata))
  )
  ;; -----------------------------------
 
  (setq done nil)
  (while (not done)
    ;; ================= 应用名选择菜单 ================= 
    (princ "\n=================================")
    (princ "\n  当前对象的扩展数据应用名：")
    (if apps
      (progn
        (setq idx 1)
        (foreach a apps
          (princ (strcat "\n  " (itoa idx) ". " a))
          (setq idx (1+ idx))
        )
        (princ (strcat "\n  " (itoa idx) ". [新建应用]"))
        (princ (strcat "\n  " (itoa (1+ idx)) ". [退出]"))
        (initget 1 (strcat "1-" (itoa (1+ idx))))
        (setq sel (getint "\n请选择序号: "))
        (cond 
          ((= sel (1+ idx))
           (setq done T))
          ((= sel idx)
           (setq appname (getstring T "\n输入新应用程序名: "))
           (if (member appname apps)
             (princ "\n该应用名已存在，将使用已有应用。")
             (progn
               ;; 注册并新建应用：立即写入空数据
               (setq datalist nil)
               (update-ent appname datalist)
             )
           )
           ;; 获取该应用的数据（确保列表格式）
           (if (not datalist) (setq datalist (cdr (assoc appname xdata))))
           (if (not (listp datalist)) (setq datalist (list datalist)))
           (if (not datalist) (setq datalist nil))
          )
          (t
           (setq appname (nth (1- sel) apps))
           (setq datalist (cdr (assoc appname xdata)))
           (if (not (listp datalist))
             (setq datalist (list datalist))
           )
           (if (not datalist)
             (setq datalist nil)
           )
          )
        )
      )
      (progn
        (princ "\n该对象没有任何扩展数据。")
        (setq appname (getstring T "\n请输入新应用程序名: "))
        (if (= appname "")
          (setq done T)
          (progn
            ;; 对象无xdata，注册并创建新应用
            (setq datalist nil)
            (update-ent appname datalist)
            (if (not datalist) (setq datalist (cdr (assoc appname xdata))))
            (if (not (listp datalist)) (setq datalist (list datalist)))
            (if (not datalist) (setq datalist nil))
          )
        )
      )
    )
 
    (if (not done)
      (progn
        ;; ================= 组码选择与操作菜单 =================
        (setq tmp T)
        (while tmp
          (princ "\n---------------------------------")
          (princ (strcat "\n应用名: " appname))
          (if datalist
            (progn
              (princ "\n当前扩展数据项：")
              (setq idx 1)
              (foreach it datalist
                (if (and (listp it) (cdr it))
                  (princ (strcat "\n  " (itoa idx) ". 组码 "
                                 (itoa (car it)) " = "
                                 (vl-prin1-to-string (cdr it))))
                  (princ (strcat "\n  " (itoa idx) ". 格式异常项"))
                )
                (setq idx (1+ idx))
              )
              (princ (strcat "\n  " (itoa idx) ". [添加新数据]"))
              (princ (strcat "\n  " (itoa (1+ idx)) ". [返回上级]"))
              (initget 1 (strcat "1-" (itoa (1+ idx))))
              (setq sel (getint "\n请选择序号: "))
            )
            (progn
              (princ "\n当前无扩展数据项。")
              (princ "\n  1. [添加新数据]")
              (princ "\n  2. [返回上级]")
              (initget 1 "1 2")
              (setq sel (getint "\n请选择: "))
            )
          )
 
          (cond 
            ;; 返回上级
            ((or (and datalist (= sel (1+ idx)))
                 (and (not datalist) (= sel 2)))
             (setq tmp nil)
            )
            ;; 添加新数据
            ((or (and datalist (= sel idx))
                 (and (not datalist) (= sel 1)))
             (setq code (getint "\n输入新组码 (例如 1000, 1040, 1071): "))
             (cond 
               ((member code '(1000 1001 1002 1003 1004 1005 1006))
                (setq newval (cons code (getstring T "\n输入字符串值: "))))
               ((= code 1040)
                (setq newval (cons code (getreal "\n输入实数值: "))))
               ((= code 1041)
                (setq newval (cons code (getreal "\n输入距离值: "))))
               ((member code '(1070 1071))
                (setq newval (cons code (getint "\n输入整数值: "))))
               (t
                (setq newval (cons code (getstring T "\n输入新值: "))))
             )
             (setq datalist (append datalist (list newval)))
             (update-ent appname datalist)
             (princ "\n新数据已添加。")
            )
            ;; 修改已有数据项
            (t
             (setq itm (nth (1- sel) datalist))
             (setq oldcode (car itm))
             (setq oldval (cdr itm))
             (princ (strcat "\n当前值: " (vl-prin1-to-string oldval)))
             (cond 
               ((member oldcode '(1000 1001 1002 1003 1004 1005 1006))
                (setq newval (cons oldcode (getstring T "\n输入新字符串: "))))
               ((= oldcode 1040)
                (setq newval (cons oldcode (getreal "\n输入新实数: "))))
               ((= oldcode 1041)
                (setq newval (cons oldcode (getreal "\n输入新距离: "))))
               ((member oldcode '(1070 1071))
                (setq newval (cons oldcode (getint "\n输入新整数: "))))
               (t
                (setq newval (cons oldcode (getstring T "\n输入新值: "))))
             )
             (setq datalist (subst newval itm datalist))
             (update-ent appname datalist)
             (princ "\n数据已修改。")
            )
          )
        ) ; while tmp
      )
    ) ; if not done
  ) ; while not done
 
  (princ "\n操作完成。")
  (princ)
)
 
;; ========== 加载后自动运行 ==========
(princ "\n=================================")
(princ "\n已加载交互式扩展数据工具，命令 XDMOD 已自动启动。")
(princ "\n=================================")
(c:XDMOD)
(princ)