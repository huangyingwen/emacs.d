;;; pachkage --- Summary: 日历配置
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; 偏好设置
;;----------------------------------------------------------------------------
;; 设置我所在地方的经纬度，calendar里有个功能是日月食的预测，和你的经纬度相联系的。
(setq calendar-latitude +30.3)
(setq calendar-longitude +120.2)
;; 我的所在地－北京
(setq calendar-location-name "Hangzhou")
;; 当退出日日历的时候把它自己建立的frame删除
(setq calendar-remove-frame-by-deleting t)
;; 设定一周的开始为周一
(setq calendar-week-start-day 1)
;; 节日和生日提醒设置
;; 我不过基督徒的节日、希伯来人的节日和伊斯兰教的节日。
;; 我是无神论者，不过我喜欢神话，大家有兴趣也可以探讨一下，发email给我吧
(setq christian-holidays nil)
(setq hebrew-holidays nil)
(setq islamic-holidays nil)
;; 日历不和日记相连，我不用Calendar自带的diary记日记
(setq mark-diary-entries-in-calendar nil)
;; 我使用diary功能
;; 这个又忘了，我真是忘性好！
;; emacs22抛弃了这个用法,使用appt-activate
;;(setq appt-issue-message nil)
;;(appt-activate 1)
;; 在日历中突出标记节日和生日
(setq mark-holidays-in-calendar t)
;; 打开calendar自动打开节日和生日列表
(setq view-calendar-holidays-initially t)

;;----------------------------------------------------------------------------
;; 中国传统农历扩展，农历生日设置
;;----------------------------------------------------------------------------
(require 'cal-china-x)
(setq mark-holidays-in-calendar t)
(setq cal-china-x-general-holiday cal-china-x-chinese-holidays)
;; 设定一些自定义的生日和节日
(setq cal-china-x-important-holidays
      '((holiday-lunar 11 23 "黄训伦生日(1954)")
        (holiday-lunar 7 2 "周庆枝生日(1957)")
        (holiday-lunar 12 22 "周庆枝忌日(2009)(13:45)")
        (holiday-lunar 5 16 "黄颖桢生日(1978)")
        (holiday-lunar 12 4 "黄颖武生日(1979)")
        (holiday-lunar 8 14 "黄颖平生日(1982)")
        (holiday-lunar 11 16 "黄翠华生日(1986)")
        (holiday-lunar 11 3 "黄颖文生日(1988)")
        (holiday-fixed 12 31 "生日")
        ))
(setq calendar-holidays (append cal-china-x-important-holidays cal-china-x-general-holiday))

;;下面两个是设置年份为中国年，好像默认的是用英文写的，由王垠修改的。
;;这个设置在节日列表的春节那天能看到，如今年的春节他就写着
;;Thursday, January 22, 2004: Chinese New Year (甲-申)
(setq chinese-calendar-celestial-stem ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
(setq chinese-calendar-terrestrial-branch ["子" "丑" "寅" "卯" "辰" "巳" "戊" "未" "申" "酉" "戌" "亥"])

(provide 'init-custom-calendar)
;;; init-custom-calendar.el ends here
