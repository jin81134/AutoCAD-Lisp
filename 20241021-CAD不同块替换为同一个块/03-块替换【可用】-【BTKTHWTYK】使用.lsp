(defun c:BTKTHWTYK (/ ssOldBlocks newBlock newBlockName ss ent)
  ;; 提示用户选择要替换的多个块
  (setq ssOldBlocks (ssget '((0 . "INSERT"))))
  (if ssOldBlocks
    (progn
      ;; 提示用户选择新的块
      (setq newBlock (car (entsel "\n选择新的块: ")))
      (if newBlock
        (progn
          ;; 获取新的块名称
          (setq newBlockName (cdr (assoc 2 (entget newBlock))))
          ;; 遍历选择集中的每个块进行替换
          (repeat (setq i (sslength ssOldBlocks))
            (setq ent (ssname ssOldBlocks (setq i (1- i))))
            ;; 替换块
            (entmod (subst (cons 2 newBlockName) (assoc 2 (entget ent)) (entget ent)))
          )
          (princ "\n块替换完成。")
        )
        (princ "\n未选择新的块。")
      )
    )
    (princ "\n未选择任何块。")
  )
  (princ)
)