;;; pachkage --- Summary: 偏好设置
;;; Commentary:
;;; Code:

;; 修改中文文本的行距,3个象素就可以了吧
(setq-default line-spacing 3)
;; 启用C-x,C-v,C-s这些通用设置
(cua-mode t);; 将默认模式从fundemental-mode改为text-mode
(setq default-major-mode 'text-mode)
;; 鼠标自动避开指针，如当你输入的时候，指针到了鼠标的位置，鼠标有点挡住视线了
(mouse-avoidance-mode 'animate)
;; 允许自动打开图片，如wiki里面
(auto-image-file-mode t)
;; 指针不要闪，我得眼睛花了
(blink-cursor-mode -1)
;; 设置 sentence-end 可以识别中文标点。不用在 fill 时在句号后插入两个空格。
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
;; 改变emacs标题栏的标题,显示buffer的名字
;;(setq frame-title-format "emacs-snapshot@%b")

;;----------------------------------------------------------------------------
;; 备份设置
;; emacs还有一个自动保存功能，默认在~/.emacs.d/auto-save-list里，这个非常有用，我
;; 这里没有改动，具体可以参见Sams teach yourself emacs in 24hours(我简称为sams24)
;;----------------------------------------------------------------------------
;; 启用自动备份
(setq make-backup-file t)
;; 启用版本控制，即可以备份多次
(setq version-control t)
;; 备份最原始的版本两次，记第一次编辑前的文档，和第二次编辑前的文档
(setq kept-old-versions 2)
;; 备份最新的版本十次，理解同上
(setq kept-new-versions 10)
;; 删掉不属于以上12种版本的版本
(setq delete-old-versions t)
;; 设置备份文件的路径
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
;; 备份设置方法，直接拷贝
(setq backup-by-copying t)


;;----------------------------------------------------------------------------
;; 自动补全功能，从王垠的网站直接Copy过来的，引用一些他对此的说明
;; 你可以设置以下 hippie-expand 的补全方式。它是一个优先列表， hippie-expand 会优
;; 先使用表最前面的函数来补全这是说，首先使用当前的buffer补全，如果找不到，就到别的可见
;; 的窗口里寻找，如果还找不到，那么到所有打开的buffer去找，如果还那么到kill-ring
;; 里，到文件名，到简称列表里，到list, 当前使用的匹配方式会在 echo 区域显示。
;; 特别有意思的是 try-expand-line，它可以帮你补全整整一行文字。我很多时后有两行文字大致
;; 相同，只有几个字不一样，但是我懒得去拷贝粘贴以下。那么我就输入这行文字的前面几个字。然后
;; 多按几下 M-/ 就能得到那一行。
;;----------------------------------------------------------------------------
(global-set-key [(meta ?/)] 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(
        ;;senator-try-expand-semantic ;优先调用了senator的分析结果-很慢-还是使用吧
        try-expand-line ; 补全当前行
        try-expand-line-all-buffers
        try-expand-list ; 补全一个列表
        try-expand-list-all-buffers
        try-expand-dabbrev ; 搜索当前 buffer
        try-expand-dabbrev-visible ; 搜索当前可见窗口
        try-expand-dabbrev-all-buffers ; 搜索所有 buffer
        try-expand-dabbrev-from-kill ; 从 kill-ring 中搜索
        try-complete-file-name ; 文件名匹配
        try-complete-file-name-partially ; 文件名部分匹配
        try-complete-lisp-symbol ; 补全 lisp symbol
        try-complete-lisp-symbol-partially ; 部分补全 elisp symbol
        try-expand-whole-kill
        )
      )

;;----------------------------------------------------------------------------
;;一般来说我们按下home是回到行的最开头。但其实若大家是在写程式，通常是会缩排(indent)的，
;;因此总是还要往回按。这段code的用意是你一按home，就会到该行第一个有字的地方，而不是最
;;前头。若要传统的home钮，只需要再多按一次即可
;;----------------------------------------------------------------------------
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-l."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key [home] ' smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

;;----------------------------------------------------------------------------
;; 时间戳设置(time-stamp)，设定文档上次保存的信息
;; 只要里在你得文档里有Time-stamp:的设置，就会自动保存时间戳
;;----------------------------------------------------------------------------
;; 启用time-stamp
(setq time-stamp-active t)
;; 去掉time-stamp的警告？
(setq time-stamp-warn-inactive t)
;; 设置time-stamp的格式，我如下的格式所得的一个例子：
(setq time-stamp-format "%:u %02m/%02d/%04y %02H:%02M:%02S")
;; 将修改时间戳添加到保存文件的动作里。
(add-hook 'write-file-hooks 'time-stamp)


;;----------------------------------------------------------------------------
;; 时间显示设置
;;----------------------------------------------------------------------------
;; 启用时间显示设置，在minibuffer上面的那个杠上
(display-time-mode 1)
;; 时间使用24小时制
(setq display-time-24hr-format t)
;; 时间显示包括日期和具体时间
(setq display-time-day-and-date t)
;; 时间栏旁边启用邮件设置
(setq display-time-use-mail-icon t)
;; 时间的变化频率
(setq display-time-interval 10)
;; 显示时间的格式
(setq display-time-format "%m月%d日%A%H:%M")

(provide 'init-custom-basic)
;;; init-custom-basic.el ends here
