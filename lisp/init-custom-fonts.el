;;; pachkage --- Summary: 自定义字体设置，参考http://coldnew.github.io/blog/2013/11/16_d2f3a.html
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; 系统默认字体
;;----------------------------------------------------------------------------
(require 'cl) ;; find-if is in common list package

(defun qiang-font-existsp (font)(if (null (x-list-fonts font)) nil t))

(defvar font-list '("微软雅黑" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体"))

(find-if #'qiang-font-existsp font-list)

(defun qiang-make-font-string (font-name font-size)
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))

(defun qiang-set-font (english-fonts
                       english-font-size
                       chinese-fonts
                       &optional chinese-font-size)
  "english-font-size could be set to \":pixelsize=18\" or a integer. If set/leave chinese-font-size to nil, it will follow english-font-size"
  (let ((en-font (qiang-make-font-string
                  (find-if #'qiang-font-existsp english-fonts)
                  english-font-size))
        (zh-font (font-spec :family (find-if #'qiang-font-existsp chinese-fonts)
                            :size chinese-font-size)))
    ;; Set the default English font
    ;; The following 2 method cannot make the font settig work in new frames.
    ;; (set-default-font "Consolas:pixelsize=18")
    ;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=18"))
    ;; We have to use set-face-attribute
    (message "Set English Font to %s" en-font)
    (set-face-attribute 'default nil :font en-font)
    ;; Set Chinese font
    ;; Do not use 'unicode charset, it will cause the english font setting invalid
    (message "Set Chinese Font to %s" zh-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        zh-font))))

(defun set-font-default ()
  "默认字体"
  (interactive) (qiang-set-font
                 '("Consolas" "Monaco" "DejaVu Sans Mono" "Monospace" "Courier New") ":pixelsize=14"
                 '("微软雅黑" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体") 12))

(set-font-default)

;;----------------------------------------------------------------------------
;; org-mode字体，表格对齐用的
;;----------------------------------------------------------------------------
;; 定義自己想使用的中英文字體
(defvar emacs-english-font  "Monaco" "The font name of English." )
(defvar emacs-cjk-font  "微软雅黑" "The font name for CJK." )

(defvar emacs-font-size-pair '( 13 . 16 )
  "Default font size pair for (english . chinese)." )

(defvar emacs-font-size-pair-list '(( 5 .   6 ) ( 10 . 12 )
                                    ( 13 . 16 ) ( 15 . 18 ) ( 17 . 20 )
                                    ( 19 . 22 ) ( 20 . 24 ) ( 21 . 26 )
                                    ( 24 . 28 ) ( 26 . 32 ) ( 28 . 34 )
                                    ( 30 . 36 ) ( 34 . 40 ) ( 36 . 44 ))
  "This list is used to store matching (englis . chinese) font-size." )

(defun font-exist-p (fontname)
  "Test if this font is exist or not."
  (if (or (not fontname) (string= fontname "" ))
      nil
    (if (not (x-list-fonts fontname)) nil t)))

(defun set-font (english chinese size-pair)
  "Setup emacs English and Chinese font on x window-system."
  (if (font-exist-p english)
      (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t))

  (if (font-exist-p chinese)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font) charset
                          (font-spec :family chinese :size (cdr size-pair))))))

(defun emacs-step-font-size (step)
  "Increase/Decrease emacs's font size."
  (let ((scale-steps emacs-font-size-pair-list))
    (if (< step 0 ) (setq scale-steps (reverse scale-steps)))
    (setq emacs-font-size-pair
          (or (cadr (member emacs-font-size-pair scale-steps))
              emacs-font-size-pair))
    (when emacs-font-size-pair
      (message "emacs font size set to %.1f" (car emacs-font-size-pair))
      (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair))))

(defun increase-emacs-font-size ()
  "Decrease emacs's font-size acording emacs-font-size-pair-list."
  (interactive) (emacs-step-font-size 1 ))

(defun decrease-emacs-font-size ()
  "Increase emacs's font-size acording emacs-font-size-pair-list."
  (interactive) (emacs-step-font-size -1 ))

(defun set-font-org ()
  "切换字体，适应org-mode表格对齐"
  (interactive) (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair))


(global-set-key (kbd "C-|" ) 'set-font-default)
(global-set-key (kbd "C-:" ) 'set-font-org)
(global-set-key (kbd "C-=" ) 'increase-emacs-font-size)
(global-set-key (kbd "C--" ) 'decrease-emacs-font-size)

(provide 'init-custom-fonts)
;;; init-custom-fonts.el ends here
