(in-package :chinese-parser)

;; for time-name
(def-word-class time-take-num () ())
(def-word-class time-take-ge () ())

;;
(def-tagged-words *times-list* *times-hash*
  (@ time-name
   (time-period
    (time-period-unit)
    ;; different sizes
    (time-century)
    (time-generation) (time-decade)
    (time-year) (time-season)
    (time-month)
    (time-week)
    (time-day)
    (time-within-day) ;; e.g. morning, afternoon, night
    (time-hour)
    (time-minute)
    (time-second)
    ;; a time period by itself
    ;; TODO: explicitly label those that can stand alone?
    (ad-hoc-time-period
     ((ad-hoc-time-period-day time-day)
      "天天" "三天兩頭" "时日")
     ((ad-hoc-time-period-season time-season) "冬閒" "冬忙")
     ((ad-hoc-time-period-within-day time-within-day)
      "夜間" "日間" "晚間" "漫漫長夜")
     (ad-hoc-time-period-word-during "期間")
     (ad-hoc-time-period-word-zhou1-qi2 "周期")
     "時間段" "時段" "天時"
     "多時" "歲月" "讀秒" "輩子" "一輩子" "光陰"
     "片刻" "宙"
     "古" "彈指" "羅預" "須臾"
     (ad-hoc-time-that-moment
      ;; used like '那 剎那'
      "頃刻" "剎那" "瞬間" "一瞬間" "一瞬" "時刻"
      (word-time-shi2-jian1 "時間")
      (word-time-shi2-hou4 "時候" "時候兒")
      )
     "終身" "时光""上古" "幼時"
     "幼年期" "少年期" "青年期" "壯年期" "中年期" "老年期"
     "長日" "空穿期" "徒期" "空窗期"
     "不一會兒" "一會" "一會兒" "多會兒" "一陣子" "陣子"
     )
    ;;
    ((time-period-num time-take-num) ;; take only number
     ((time-period-num-year time-year) "周歲")
     ((time-period-num-second time-second) "閏秒")
     ((time-period-num-within-day time-within-day) "旦")
     ((time-period-num-ind ind)
      ((time-period-num-ind-within-day time-within-day) "晚" "宿")
      ((time-period-num-ind-season time-season) "季")
      ;;
      ((time-period-num-ind-year time-year time-period-unit)
       "年" "載")
      ((time-period-num-ind-day time-day time-period-unit) "日" "天")
      ((time-period-num-ind-minute time-minute time-period-unit) "分鐘")
      ((time-period-num-ind-second time-second time-period-unit) "秒" "秒鐘")
      ((time-period-num-ind-week time-week time-period-unit) "週")
      ((time-period-num-ind-unit time-period-unit)
       "刻" "刻鐘")))
    ((time-period-num-ge time-take-ge) ;; take only "number 個"
     ((time-period-num-ge-ind ind)
      ((time-hour-ind time-hour) "鐘")
      ((time-period-num-ge-ind-year time-year)
       "年份" "寒暑")
      ((time-period-num-ge-ind-season time-season) "季節")
      ((time-period-num-ge-ind-month time-month)
       "月" "閏月" "月份")
      ((time-period-num-ge-ind-hour time-hour) "鐘頭")
      ((time-period-num-ge-ind-generation time-generation) "世代")
      ((time-period-num-ge-ind-decade time-decade) "年代")
      ))
    ((time-period-num-opt-ge time-take-num time-take-ge) ;; take number [個]
     ((time-period-num-opt-ge-ind ind)
      ((time-period-num-opt-ge-ind-century time-century)
       "世紀" "紀元")
      ((time-period-num-opt-ge-ind-year time-year)
       "年度" "財年" "學年" "財年度" "學年度")
      ((time-period-num-opt-ge-ind-season time-season) "季度")
      ((time-period-num-opt-ge-ind-day time-day) "工日")
      ((time-period-num-opt-ge-ind-hour time-hour) "小時" "時段")
      ((time-period-num-opt-ge-ind-week time-week) "星期" "禮拜")
      (time-period-num-opt-ge-ind-shi2-qi2 "時期")
      (time-period-num-opt-ge-word-cai3-shou1-qi2 "採收期")
      "期"))
    ) ;; end time-period
   ;;
   (time-pt
    ;; a time point by itself
    (time-date)
    (ad-hoc-time-pt
     ((ad-hoc-time-pt-year time-year)
      "去年度" "來年" "明年" "往年" "去年" "昨年"
      "當年" "今年")
     ((ad-hoc-time-pt-month time-month)
      "本月" "當月")
     ((ad-hoc-time-pt-day time-day)
      "壽辰" "生辰" "誕辰" "今日" "今天"
      "明天" "明日" "昨天" "昨日" "當日" "當天" "那天")
     ((ad-hoc-time-pt-within-day time-within-day)
      "昨晚" "半夜" "今早" "一大早" "一早"
      "曙" "曉" "午" "天明" "天亮" "天黑")
     ;;
     (ad-hoc-time-pt-word-tong2-shi2 "同時")
     "時節" "今昔" "辰" "史前"
     "今後" "末年" "古早" "當今"
     "過往" "昨" "最初" "不久" "歷年" "歷年來"
     (word-current "今") (word-time-shi2 "時")
     "古今" "期中" "期末"
     "有朝一日" "其後" "當初" "日後"
     "那會" "剛才" "近來" "先前"
     "稍早" "稍後" "早前" "以後" "之前" "之後" "此後" "此前"
     "以往" "自古" "萬古" "陰曆" "陽曆" "平時"
     (word-time-kai1-shi3 "一開始" "開始")
     "起初" "一時間" "此時" "此刻" "目前" "眼前"
     "此際" "早"
     "現下" "當下" "眼下" "而今" "這會" "是時" "時下"
     "成幾何時" ;; "曾幾何時" ;; this is adverb in CTB
     "青天白日" "來日" "光天化日"
     ;; "與其同時" ;; this is adverb in CTB
     (word-time-future "未來" "將來")
     (word-time-guo4-qu4 "過去") ;; also a verb
     "當前"
     "當時" "以前" "從前" "如今" "後來"
     "現在" "最近" "小時候" "最後"
     )
    ;; time points could also take number or with 個 to become time period
    ((time-pt-num-ge time-take-ge) ;; take only "number 個"
     ((seasons time-season)
      "盛暑"
      "春" "夏" "秋" "冬" "隆冬" "盛夏" "早春"
      "春天" "夏天" "秋天" "冬天" "春季" "夏季" "秋季" "冬季"
      "青陽" "芳春" "陽春" "陽中" "三春" "九春"
      "朱夏" "朱明" "長贏" "昊天" "三夏" "九夏"
      "商秋" "商節" "素節" "素商" "素秋" "金天" "九秋")
     ;;
     ((time-pt-num-ge-century time-century)
      "古近紀" "新近紀" "第四紀" "成鐵紀" "層侵紀" "造山紀"
      "固結紀" "蓋層紀" "延展紀" "狹帶紀" "拉伸紀" "成冰紀"
      "埃迪卡拉紀" "寒武紀" "奧陶紀" "志留紀" "泥盆紀"
      "石炭紀" "二疊紀" "三疊紀" "侏羅紀" "白堊紀"
      "中世紀"
      )
     ((time-pt-num-ge-year time-year)
      "元年" "凋年" "年頭" "閏年"
      "千禧年" "千禧" "Y2K"
      "鼠年" "牛年" "虎年" "兔年" "龍年" "蛇年"
      "馬年" "羊年" "猴年" "雞年" "狗年" "豬年")
     ((time-pt-num-ge-month time-month)
      "元月" "臘月"
      "正月" "一月" "二月" "三月" "四月" "五月" "六月"
      "七月" "八月" "九月" "十月" "十一月" "十二月"
      ;;
      "January" "February" "March" "April" "May" "June"
      "July" "August" "September" "October" "November"
      "December"
      "Jan" "Feb" "Mar" "Apr" "Jun" "Jul"
      "Aug" "Sep" "Oct" "Nov" "Dec"
      )
     ((time-pt-num-ge-season time-season)
      "春夏之交" "夏秋之交" "秋冬之交" "冬春之交"
      "春日" "夏日" "秋日" "冬日")
     ((time-pt-num-ge-day time-day)
      "元日" "吉日" "忌日" "黄道吉日"
      ;;
      ;; "清明" is also a holiday name
      "立春" "雨水" "驚蟄" "春分" "穀雨"
      "立夏" "小滿" "芒種" "夏至" "小暑" "大暑"
      "立秋" "處暑" "白露" "秋分" "寒露" "霜降"
      "立冬" "小雪" "大雪" "冬至" "小寒" "大寒"
      )
     ((time-pt-num-ge-within-day time-within-day)
      "清早" "宵")
     ((time-pt-num-ge-hour time-hour)
      "鼠時" "牛時" "虎時" "兔時" "龍時" "蛇時"
      "馬時" "羊時" "猴時" "雞時" "狗時" "豬時"
      ;;
      "子時" "丑時" "寅時" "卯時" "辰時" "巳時"
      "午時" "未時" "申時" "酉時" "戌時" "亥時"
      ;;
      "子初" "丑初" "寅初" "卯初" "辰初" "巳初"
      "午初" "未初" "申初" "酉初" "戌初" "亥初"
      ;;
      "子正" "丑正" "寅正" "卯正" "辰正" "巳正"
      "午正" "未正" "申正" "酉正" "戌正" "亥正")
     ;; "高商" is ambiguous with shorthand for 高級商業
     "玄英" "三冬" "九冬"
     "陰天" "晴天" "雨天" "下雨天"
     "臘" "时分" "末日"
     "大年"
     ((lunar-day time-day)
      "初一" "初二" "初三" "初四" "初五"
      "初六" "初七" "初八" "初九" "初十")
     "童年" "幼年" "盛年" "壯年" "晚年" "常年" "農曆年"
     "志學" "而立" "不惑" "知天命" "耳順" "花甲" "古稀"
     "志學之年" "而立之年" "不惑之年" "知天命之年" "耳順之年"
     "花甲之年" "古稀之年"
     ;; the valid combination of 天干地支
     ((traditional-year-name name time-year)
      "甲子" "乙丑" "丙寅" "丁卯" "戊辰" "己巳" "庚午" "辛未" "壬申" "癸酉"
      "甲戌" "乙亥" "丙子" "丁丑" "戊寅" "己卯" "庚辰" "辛巳" "壬午" "癸未"
      "甲申" "乙酉" "丙戌" "丁亥" "戊子" "己丑" "庚寅" "辛卯" "壬辰" "癸巳"
      "甲午" "乙未" "丙申" "丁酉" "戊戌" "己亥" "庚子" "辛丑" "壬寅" "癸卯"
      "甲辰" "乙巳" "丙午" "丁未" "戊申" "己酉" "庚戌" "辛亥" "壬子" "癸丑"
      "甲寅" "乙卯" "丙辰" "丁巳" "戊午" "己未" "庚申" "辛酉" "壬戌" "癸亥")
     (holiday
      ((holiday-day time-day)
       "節日" "假日"
       ((holiday-ind ind) "假" "節" "誕")
       ((holiday-name name)
        "臘八"
        "下元"
        "盂蘭" "中元"
        "端陽" "重午" "重五" "端午"
        "聖誕" "耶誕" "中秋" "清明" "重陽" "重九" "國慶"
        "元宵"
        "雙十" "重十"
        "乞巧" "七巧"
        "华伦泰" "圣华伦泰" "圣瓦伦" "瓦伦丁" ;; (Saint) Valentine
        )
       "浴佛節" "佛圓滿日" "川主誕"
       "祭灶日" "謝節" "灶王節" "祭灶" "送神日" "小年"
       "尾牙"
       "消災日" "水官誕" "水官節" "下元水官節"
       "豐收節"
       "寒衣節" "授衣節" "冥陰節"
       "菊花節"
       "秋夕" "八月節" "月節" "團圓節"
       "財神節"
       "盂蘭盆節" "鬼節"
       "玉帝誕" "玉皇誕" "地官誕"
       "五月節" "五日節" "午日節" "艾節" "夏節" "蒲節"
       "冥誕" "雙十節" "平安夜"
       "元夕"
       "七姐誕" "七夕" "７夕"
       "春節" "感恩節" "復活節" "情人節" "勞動節"
       "婦女節" "兒童節" "母親節" "父親節" "植樹節" "上巳節"
       "元旦"
       "年夜" "除夕" "新年" "歲除" "大晦日"
       )
      ;; below are holidays that do not specify a day
      "暑假" "暑期")
     )
    ((time-pt-num-opt-ge time-take-num time-take-ge) ;; take number [個]
     ((time-pt-num-opt-ge-within-day time-within-day)
      (time-word-stage "階段")
      "上午" "下午" "凌晨" "零晨" "清晨" "晨"
      "早上" "晚上" "深夜" "夜晚" "白晝" "晝"
      "黑夜" "夜" 
      "朝" "暮" "白天" "夕" "旦夕" "朝夕"
      ;; more names for hours
      "夜半" "子夜" "中夜" "午夜" "夜分" "未旦" "宵分" "未央"
      "荒雞"
      "平旦"
      "平明" "旦明" "黎明" "早旦" "日旦" "昧旦" "騎旦" "早晨" "早夜" "早朝" "昧爽" "旦日" "旦時"
      "日出" "日生" "日升" "日始" "日晞" "旭日" "破曉" "拂曉"
      "食時" "早食" "宴食" "蚤食"
      "隅中" "日隅" "禺中" "日禺"
      "日正" "日午" "日高" "正午" "中午" "亭午" "日當" "平午" "平晝"
      "日昳" "日昃" "日仄" "日側" "日跌" "日斜" "日映" "日央"
      "晡時" "餔時" "日餔" "日晡" "日鋪" "日稷" "日夕" "夕食"
      "日入" "日沒" "日沉" "日西" "日落" "日逝" "日晏" "日旴" "日晦" "傍晚"
      "黃昏" "日末" "日暮" "日闇" "日墮" "日暗" "日曛" "曛黃"
      "人定" "定昏" "夤夜"
      ;; confusing: "日晚", want e.g. '(23 日) 晚'
      ;; confusing: "日上", want e.g. '(28 日) (上午)'
      ;; confusing:  "日中"
      )
     ((time-pt-num-opt-ge-day time-day)
      ;; special weekdays, others are composed in grammar
      "周末" "周日" "星期日" "星期天" "禮拜日" "禮拜天"
      "星期一" "星期二" "星期三" "星期四" "星期五" "星期六"
      "周一" "周二" "周三" "周四" "周五" "周六"
      "太陽日" "月亮日" "火星日" "水星日" "木星日" "金星日" "土星日"
      "日曜日" "月曜日" "火曜日" "水曜日" "木曜日" "金曜日" "土曜日"
      "主的日" "主日" "提爾日" "奧丁日" "索爾日" "弗麗嘉日"
     
      "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"
      "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"
      ;;
      "日子"
      )
     ((time-pt-num-opt-ge-year time-year) "周年" "週年")
     )
   ) ;; end time-pt
  ))
;; end times-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-tagged-words *dynasties* *dynasties-hash*
  ((dynasty noun)
   "現代" "古代" "當代" "近代" "民初" "民末" "天朝"
   ((dynasty-or-period-ind ind) "時代")
   ((dynasty-ind ind) "朝" "朝代" "代")
   ((dynasty-name nr-name)
    (dynasty-shang1 "商")
    (dynasty-han4 "漢")
    "新朝" "南北朝" "南朝" "北朝" "五代" "金朝"
    "夏" "周" "西周" "東周" "春秋" "戰國" "秦" "楚" "西漢" "東漢"
    "三國" "曹魏" "蜀漢" "孫吳" "晉" "西晉" "東晉" "十六國"
    "前趙" "成漢" "前涼" "後趙" "前燕" "前秦" "後秦" "後燕" "西秦" "後涼" "南涼" "南燕" "西涼" "北燕" "北涼"
    (dynasty-word-liang "梁")
    (dynasty-word-tang2 "唐")
    "宋" "齊" "陳" "北魏" "東魏" "北齊" "西魏" "北周" "隋" "五代十国"
    "後梁" "後唐" "後晉" "後漢" "後周" "十國" "吳越" "閩國" "南平" "馬楚" "南吳" "南唐" "南漢" "北漢" "前蜀" "後蜀"
    "北宋" "南宋" "遼" "西夏" "大蒙古國" "元" "明" "後金" "清"
    "民國")
   ))

;; end dynasty
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
