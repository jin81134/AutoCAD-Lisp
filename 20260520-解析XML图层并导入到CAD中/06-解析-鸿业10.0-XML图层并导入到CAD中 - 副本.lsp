;;;=============================================================
;;; ImportXMLayers.lsp 
;;; 功能：解析XML图层信息并导入AutoCAD图纸 
;;;   - 图层名称来源：读取XML后让用户从以下四个属性中选择：
;;;       1. Comment  2. DefaultLayer  3. Name  4. UserLayer（默认）
;;;   - 图层说明：取自 "Comment" 属性
;;;   - 颜色解码（两种编码体系智能识别）
;;;   - 线型：取自 "LineType" 属性
;;;   - 线宽：取自 "LineWeight" 属性
;;; 使用方法：加载后运行命令 IML，选择XML文件即可
;;;=============================================================
 
(vl-load-com)
 
(defun *error* (msg)
  (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
    (princ (strcat "\n错误: " msg))
  )
  (gc)
  (princ)
)
 
;;; 安全获取XML节点属性 
(defun GetNodeAttr (node attrName / result)
  (setq result
    (vl-catch-all-apply
      'vlax-invoke-method
      (list node 'getAttribute attrName)
    )
  )
  (if (or (vl-catch-all-error-p result) (not result))
    ""
    (vlax-variant-value result)
  )
)
 
;;; 确保线型已加载
(defun EnsureLinetype (ltypeName / doc ltypes)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ltypes (vla-get-Linetypes doc))
  (if (vl-catch-all-error-p
        (vl-catch-all-apply 'vla-item (list ltypes ltypeName)))
    (progn
      (vl-catch-all-apply 'vla-load (list ltypes ltypeName "acad.lin"))
      (if (vl-catch-all-error-p
            (vl-catch-all-apply 'vla-item (list ltypes ltypeName)))
        (vl-catch-all-apply 'vla-load (list ltypes ltypeName "acadiso.lin"))
      )
    )
  )
)
 
;;;=============================================================
;;; 颜色解码
;;; 体系一（≥16777216）：ACI = (值 & 0xFFFFFF) + 1
;;; 体系二（1~255）：直接ACI索引
;;; 体系二（>255 且 <16777216）：BGR真彩色 → AcCmColor自动匹配
;;;=============================================================
(defun DecodeColor (colorStr / val aci ver acCmColor r g b ci)
  (if (or (not colorStr) (= colorStr ""))
    7
    (progn
      (setq val (atoi colorStr))
      (cond
        ((>= val 16777216)
         (setq aci (1+ (logand val 16777215)))
         (if (or (< aci 1) (> aci 255)) 7 aci)
        )
        ((and (>= val 1) (<= val 255))
         val
        )
        (t
         (setq r (logand val 255))
         (setq g (logand (lsh val -8) 255))
         (setq b (logand (lsh val -16) 255))
         (setq ver (substr (getvar "ACADVER") 1 2))
         (setq acCmColor 
           (vl-catch-all-apply 'vlax-create-object
             (list (strcat "AutoCAD.AcCmColor." ver))
           )
         )
         (if (vl-catch-all-error-p acCmColor)
           (setq acCmColor
             (vl-catch-all-apply 'vlax-create-object (list "AutoCAD.AcCmColor"))
           )
         )
         (if (or (vl-catch-all-error-p acCmColor) (not acCmColor))
           7
           (progn
             (vlax-invoke-method acCmColor 'SetRGB r g b)
             (setq ci
               (vl-catch-all-apply
                 'vlax-get-property
                 (list acCmColor 'ColorIndex)
               )
             )
             (vlax-release-object acCmColor)
             (if (or (vl-catch-all-error-p ci) (not ci) (< ci 1) (> ci 255))
               7
               ci
             )
           )
         )
        )
      )
    )
  )
)
 
;;; 属性名 → 选项编号 映射
(setq *attr-options*
  '(("Comment"      . 1)
    ("DefaultLayer" . 2)
    ("Name"         . 3)
    ("UserLayer"    . 4)
  )
)
 
;;; 处理单个图层节点 
;;; layerAttrName: 用户选择的属性名（如 "Name"、"UserLayer" 等）
(defun ImportLayerFromNode (node layerAttrName 
                            / name comment color ltype lweight 
                              layer layers doc lw lwExist aciColor)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
 
  ;; 用用户选择的属性作为图层名称
  (setq name (GetNodeAttr node layerAttrName))
  (if (= name "")
    nil
    (progn
      (setq comment (GetNodeAttr node "Comment"))
      (setq color   (GetNodeAttr node "Color"))
      (setq ltype   (GetNodeAttr node "LineType"))
      (setq lweight (GetNodeAttr node "LineWeight"))
 
      ;; 获取或创建图层
      (setq layers (vla-get-Layers doc))
      (setq layer (vl-catch-all-apply 'vla-add (list layers name)))
      (if (vl-catch-all-error-p layer)
        (setq layer (vla-item layers name))
      )
 
      ;; 设置索引颜色 
      (setq aciColor (DecodeColor color))
      (vl-catch-all-apply 'vla-put-Color (list layer aciColor))
 
      ;; 设置线型
      (if (/= ltype "")
        (progn 
          (EnsureLinetype ltype)
          (vl-catch-all-apply 'vla-put-Linetype (list layer ltype))
        )
      )
 
      ;; 设置线宽
      (if (/= lweight "")
        (progn
          (setq lw (atof lweight))
          (cond 
            ((= lw -3) (setq lwExist -3))
            ((= lw -2) (setq lwExist -2))
            ((= lw -1) (setq lwExist -1))
            ((>= lw 0) (setq lwExist (fix (+ (* lw 100) 0.5))))
            (t        (setq lwExist -3))
          )
          (vl-catch-all-apply 'vla-put-Lineweight (list layer lwExist))
        )
      )
 
      ;; 设置图层说明
      (if (/= comment "")
        (vl-catch-all-apply 'vla-put-Description (list layer comment))
      )
 
      1 
    )
  )
)
 
;;; 让用户选择图层名称来源 
;;; 返回属性名字符串（如 "UserLayer"）
(defun SelectLayerNameAttr (firstNode / sampleComment sampleDefault sampleName sampleUser
                                        choice attrMap pair)
  ;; 读取第一行示例值
  (setq sampleComment (GetNodeAttr firstNode "Comment"))
  (setq sampleDefault (GetNodeAttr firstNode "DefaultLayer"))
  (setq sampleName    (GetNodeAttr firstNode "Name"))
  (setq sampleUser    (GetNodeAttr firstNode "UserLayer"))
 
  (princ "\n========================================")
  (princ "\n请选择作为图层名称的属性：")
  (princ (strcat "\n  1. Comment      → " sampleComment))
  (princ (strcat "\n  2. DefaultLayer → " sampleDefault))
  (princ (strcat "\n  3. Name         → " sampleName))
  (princ (strcat "\n  4. UserLayer    → " sampleUser "  ★默认"))
  (princ "\n========================================")
 
  (initget 6)  ; 不允许零或负
  (setq choice (getint "\n请输入选项 [1/2/3/4] <4>: "))
  (if (not choice)
    (setq choice 4)
  )
  (if (or (< choice 1) (> choice 4))
    (setq choice 4)  ; 越界则默认
  )
 
  ;; 根据选项返回属性名 
  (cond
    ((= choice 1) "Comment")
    ((= choice 2) "DefaultLayer")
    ((= choice 3) "Name")
    ((= choice 4) "UserLayer")
  )
)
 
;;; 主命令 
(defun C:IML ( / xmlFile xmlDoc rows firstNode layerAttrName count i node successCnt)
  (setq xmlFile (getfiled "选择图层XML文件" "" "xml" 4))
  (if (not xmlFile)
    (progn (princ "\n取消操作。") (quit))
  )
 
  ;; 加载XML
  (setq xmlDoc 
    (vl-catch-all-apply 'vlax-create-object (list "MSXML2.DOMDocument.6.0"))
  )
  (if (or (vl-catch-all-error-p xmlDoc) (not xmlDoc))
    (setq xmlDoc (vlax-create-object "MSXML2.DOMDocument"))
  )
  (vlax-put-property xmlDoc "async" :vlax-false)
 
  (if (not (vlax-invoke-method xmlDoc 'load xmlFile))
    (progn
      (princ "\nXML文件加载失败，请检查文件格式。")
      (vlax-release-object xmlDoc)
      (quit)
    )
  )
 
  ;; 获取 z:row 节点
  (setq rows
    (vl-catch-all-apply
      'vlax-invoke-method 
      (list xmlDoc 'getElementsByTagName "z:row")
    )
  )
  (if (or (vl-catch-all-error-p rows)
          (not rows)
          (= (vlax-get-property rows 'length) 0))
    (setq rows 
      (vl-catch-all-apply
        'vlax-invoke-method
        (list xmlDoc 'getElementsByTagName "row")
      )
    )
  )
 
  (if (or (vl-catch-all-error-p rows)
          (not rows)
          (= (vlax-get-property rows 'length) 0))
    (progn
      (princ "\n未找到任何图层数据行。")
      (if (and rows (not (vl-catch-all-error-p rows)))
        (vlax-release-object rows)
      )
      (vlax-release-object xmlDoc)
      (quit)
    )
  )
 
  ;; ★ 取第一行作为示例，让用户选择图层名称属性
  (setq firstNode (vlax-get-property rows 'item 0))
  (setq layerAttrName (SelectLayerNameAttr firstNode))
  (princ (strcat "\n已选择: 使用 \"" layerAttrName "\" 作为图层名称。"))
 
  ;; 静默遍历导入
  (setq count   (vlax-get-property rows 'length))
  (setq i 0)
  (setq successCnt 0)
  (princ (strcat "\n正在导入 " (itoa count) " 个图层..."))
  (while (< i count)
    (setq node (vlax-get-property rows 'item i))
    (if (ImportLayerFromNode node layerAttrName)
      (setq successCnt (1+ successCnt))
    )
    (setq i (1+ i))
  )
 
  (princ (strcat "\n导入完成：" (itoa successCnt) " / " (itoa count) " 个图层已成功处理。"))
  (vlax-release-object rows)
  (vlax-release-object xmlDoc)
  (gc)
  (princ)
)
(princ "\nIML 命令已加载。输入 IML 并选择XML文件即可导入图层。")
(c:IML)