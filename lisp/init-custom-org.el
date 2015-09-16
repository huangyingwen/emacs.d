;;; pachkage --- Summary: 自定义org-mode配置
;;; Commentary:
;;; Code:

;;只高亮显示最后一个代表层级的 *
(setq org-hide-leading-stars t)
;;大纲显示缩进（默认显示没有缩进 [M-x org-indent-mode]）
(setq org-startup-indented t)
;;Org缓冲区之间切换
(define-key global-map "\C-cb" 'org-iswitchb)
;; 为了使用Org-mode的日程表功能，首先需要把todo.org加入到日程表文件中
(setq org-agenda-files (list "e:/org/todo.org"))

;;----------------------------------------------------------------------------
;; 自动更新上级任务的完成状态
;;----------------------------------------------------------------------------
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)


;;----------------------------------------------------------------------------
;; 事件
;;----------------------------------------------------------------------------
;; 事件状态变化
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)" "PHONE"))))

;; 事件状态颜色
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold) ;;等待其他人完成
              ("SOMEDAY" :foreground "magenta" :weight bold)  ;;延期
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

;; 事件状态改变，自动添加标签
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("SOMEDAY" ("WAITING" . t) ("SOMEDAY" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("SOMEDAY"))
              ("NEXT" ("WAITING") ("CANCELLED") ("SOMEDAY"))
              ("DONE" ("WAITING") ("CANCELLED") ("SOMEDAY")))))

;; 事件状态通过S-LEFT和S-RIGHT改变不需要时间戳和改变原因
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; 事件更改记录放在抽屉中
(setq org-log-into-drawer t)

;; 时间记录会放到一个名为LOGBOOK的抽屉(drawer)中
(setq org-clock-into-drawer t)


;; Agenda clock report(C-c a R) 参数
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

                                        ; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ("@office" . ?o) ;; 公司事情
                            ("@home" . ?H) ;; 家里事情
                            (:endgroup)
                            ("PHONE" . ?p)
                            ("WAITING" . ?w)
                            ("SOMEDAY" . ?h)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("FARM" . ?F)
                            ("ORG" . ?O)
                            ("NORANG" . ?N)
                            ("crypt" . ?E)
                            ("MARK" . ?M)
                            ("NOTE" . ?n)
                            ("BZFLAG" . ?B)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??))))

;;----------------------------------------------------------------------------
;; 自动归档任务
;;----------------------------------------------------------------------------
(global-set-key "\C-cr" 'org-capture)
(setq org-capture-templates
      `(("t" "Task" entry (file+headline ,"e:/git/org/todo/todo.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("b" "Blog" entry (file+headline ,"e:/git/org/todo/todo.org" "Blog")
         "* TODO %?\n  %i\n  %a")
        ("s" "Study" entry (file+headline ,"e:/git/org/todo/todo.org" "Study")
         "* TODO %?\n  %i\n  %a")
        ("n" "Note" entry (file+headline ,"e:/git/org/todo/note.org" "Index")
         "* Note %?\n  %i\n  %a")))


;; 标签搜索忽略任务计划和截止日期
(setq org-agenda-tags-todo-honor-ignore-options t)

;;----------------------------------------------------------------------------
;; 用Org-mode实践《奇特的一生》
;;----------------------------------------------------------------------------
;; used by org-clock-sum-today-by-tags
(defun filter-by-tags ()
  (let ((head-tags (org-get-tags-at)))
    (member current-tag head-tags)))

(defun org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
  (interactive "P")
  (let* ((timerange-numeric-value (prefix-numeric-value timerange))
         (files (org-add-archive-files (org-agenda-files)))
         (include-tags '("ACADEMIC" "ENGLISH" "SCHOOL"
                         "LEARNING" "OUTPUT" "OTHER"))
         (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
         (output-string "")
         (tstart (or tstart
                     (and timerange (equal timerange-numeric-value 4) (- (org-time-today) 86400))
                     (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "Start Date/Time:"))
                     (org-time-today)))
         (tend (or tend
                   (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "End Date/Time:"))
                   (+ tstart 86400)))
         h m file item prompt donesomething)
    (while (setq file (pop files))
      (setq org-agenda-buffer (if (file-exists-p file)
                                  (org-get-agenda-file-buffer file)
                                (error "No such file %s" file)))
      (with-current-buffer org-agenda-buffer
        (dolist (current-tag include-tags)
          (org-clock-sum tstart tend 'filter-by-tags)
          (setcdr (assoc current-tag tags-time-alist)
                  (+ org-clock-file-total-minutes (cdr (assoc current-tag tags-time-alist)))))))
    (while (setq item (pop tags-time-alist))
      (unless (equal (cdr item) 0)
        (setq donesomething t)
        (setq h (/ (cdr item) 60)
              m (- (cdr item) (* 60 h)))
        (setq output-string (concat output-string (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
    (unless donesomething
      (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
    (unless noinsert
      (insert output-string))
    output-string))

;;每个标签的时间统计
(define-key global-map "\C-c\C-xt" 'org-clock-sum-today-by-tags)

(provide 'init-custom-org)
;;; init-custom-org.el ends here
