(defun c:RYDDFZTY ()
  (prompt "\n请选择一些线段: ")
  (setq ss (ssget '((0 . "LINE"))))

  (if ss
    (progn
      ;; 获取有效的图元
      (setq ent nil)
      (while (not ent)
        (setq ent (car (entsel "\n请选择一个图元来复制到交叉点处: ")))
        (if (not ent)
          (prompt "\n未选择有效的图元，请重新选择。")
        )
      )
      (setq entData (entget ent))

      ;; 获取用户输入的交点次数
      (setq valid-input nil)
      (while (not valid-input)
        (setq input (getint "\n请输入交点次数 (只能输入数字): "))
        (if (numberp input)
          (setq valid-input t)
          (prompt "\n无效输入，请重新输入一个数字。")
        )
      )
      
      (setq pointsList nil)
      (setq count 0)

      ;; 获取线段的起点和终点
      (while (< count (sslength ss))
        (setq entLine (ssname ss count))
        (setq p1 (cdr (assoc 10 (entget entLine))))
        (setq p2 (cdr (assoc 11 (entget entLine))))
        (setq pointsList (append pointsList (list p1 p2)))
        (setq count (1+ count))
      )

      ;; 手动去重
      (setq uniquePoints nil)
      (foreach pt pointsList
        (if (not (member pt uniquePoints))
          (setq uniquePoints (cons pt uniquePoints))
        )
      )

      ;; 检查点的重复出现次数，并复制包含用户输入交点次数的图元
      (foreach pt uniquePoints
        (setq matchCount 0)
        (foreach pt2 pointsList
          (if (equal pt pt2)
            (setq matchCount (1+ matchCount))
          )
        )
        (if (= matchCount input)
          (progn
            ;; 复制图元并插入新位置
            (setq newEntData (subst (cons 10 pt) (assoc 10 entData) entData))
            (entmake newEntData)
          )
        )
      )
      (princ "\n图元已复制到交点处。")
    )
    (prompt "\n未选择任何线段。")
  )
  (princ)
)
