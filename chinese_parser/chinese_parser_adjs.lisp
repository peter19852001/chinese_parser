(in-package :chinese-parser)

(def-tagged-words *adjectives-list* *adjectives-hash*
  ;; categorized mostly following that of nouns, but usually coraser.
  ((adj noun)
   ;; these adjectives, if mono, should not be followed by 'de'
   ;; use NM as short-hand below
   (adj-no-mono-de)
   ;; to indicate an adj is unlikely to be adv, so that it is
   ;; penalized when converting into adv. use UA as short-hand below.
   (adj-unlikely-adv)
   ;; to indicate an adj could be used like a verb on a noun, use CV
   ;; as short-hand below. And if there is already a verb for it, need
   ;; not include the adj. Currently only a small number of adjs.
   (adj-can-be-verb)
   ;;
   (adj-on-noun ;; very general, as opposed to unexpected combinations
    ;;
    ((adj-on-noun-NM adj-no-mono-de)
     "悄" "僻" "悠" "慎" "緩" "齊"
     "牢" "偏" "絕" "險"
     "先" "暴" "俊" "駿" "驟"
     "靡" "艱" "驕" "逸" "恬" "齷"
     "齪" "躁" "贅" "懇" "虔"
     "頻" "慧" "暢"
     "殊" "慈" "歹" "雅" "蠻"
     "稀" "勃" "猛" "摯"
     "悲" "狂" "渾" "奇"
     "準"
     "常" "急" "亂" "迂"
     (adj-word-wei3 "偉")
     (adj-word-miao4 "妙")
     (adj-word-zhuan1 "專")
     (adj-word-god-like "神")
     (adj-word-jing1 "精")
     (adj-word-prestige "威")
     (adj-word-bang4 "棒")
     ;;
     ((adj-on-noun-NM-UA adj-unlikely-adv)
      "殆" "宜" "喜" "劣" "雜" "初" "傲"
      "饒" "芳" "遙" "詭" "陋" "鉅" "卓"
      "蕃" "穢" "盈" "瑣" "炎" "鴻" "潔" "愉"
      "遐" "頂" "焦" "番" "傑" "艷" "頑" "危"
      "廉" "佳" "雙" "冥" "吉" "凡" "異"
      "冗" "單" "瑰" "厲" "污" "聖" "優"
      (adj-word-yao4 "要") ;; also a verb
      (adj-word-ci4 "次") ;; could be a unit
      (adj-word-bian4 "便")
      (adj-word-si1 "私")
      (adj-word-te4 "特")
      (adj-word-inside "内")
      (adj-word-wei4 "外")
      (adj-word-jian4 "健")
      (adj-word-original "原")
      (adj-word-famous "名")
      (adj-word-former "前")
      (adj-word-gu3 "古")
      (adj-word-he2 "和")
      (adj-word-xing2 "行")
      (adj-word-liang2 "良")
      (adj-word-secondary "副")
      (adj-word-master "主")
      (adj-word-chief "總")
      )
     ;;
     )
    ;;
    ((adj-on-noun-UA adj-unlikely-adv)
     (adj-word-xiang1 "香")
     (adj-word-mu3 "母")
     (adj-word-negative "負")
     (adj-word-qu1-qu1 "區區") ;; already has an adv for this
     ;;
     (adj-on-position
      "國家級" "省級" "縣級" "市級" "村級"
      "國家等級" "省等級" "縣等級" "市等級" "村等級")
     ;;
     "大補"  "十全大補"
     "關貿" "上述" "三好" "著名" "協同"
     "最後" "最遲" "近距離" "同理" "禮節性"
     "來得及" "有幸" "徒然" "跨省" "說不准"
     "到家" "跨地區" "源源" "越洋" "畢生"
     "遠近" "沒什麼" "稍候" "依稀" "另外一方面"
     "原樣" "逾期" "確實" "的確" "空手" "跨行業"
     "稅前" "壓陣" "跨部門" "精裝" "遠距"
     "不盡" "計畫式" "闔家" "新舊" "凝靜" "臨陣"
     "在地" "臨危" "無量" "原機" "電腦化" "有條件"
     "畏罪" "滿城" "順路" "滿天" "轉天" "改良式"
     "等價" "散開" "定心" "相隨" "曲彎" "職業性"
     "刻骨" "鐵定" "保不住" "永生" "等額" "搶鮮"
     "枉法" "毋容置疑" "保不准" "順水" "所謂" "跨國"
     "實質" "免稅" "堂堂" "現場" "以下" "如下"
     "優惠" "軍事" "牽頭" "專項" "累積" "總的" "後續"
     "長途" "僅有" "跨海" "露天" "專案" "首席" "勝利"
     "明眼" "歷次" "前期" "關鍵" "私營" "遠程" "無罪"
     "在線" "長足" "過渡" "跨越式" "不夠" "專題" "就近"
     "更多" "新近" "不少" "如是" "最高" "通俗"
     "似" "最新" "隨身" "無比" "平日" "最初" "還好"
     "平時" "易" "起碼" "像" "臭" "肥" "乾" "傳家"
     "禿" "夾生" "芳香" "邊陲" "邊垂" "私房" "醇"
     "榮華" "謬誤" "甘" "興榮" "芬芳" "殘缺" "年老"
     "年少" "繽紛" "私立" "公立" "真空" "狹" "靚"
     "民用" "軍用" "耐用" "前沿" "澀" "苗條" "旱"
     "有形" "現役" "崢嶸" "安康" "小康" "臨床" "善"
     "公募" "私募" "經典" "非典" "貴" "新潮" "光明"
     "主流" "出名" "私有" "公有" "鄉土" "無價" "旺"
     (adj-word-new "新")
     (adj-word-important "重要")
     "中等" "不足" "花花" "機動" "唯一" "保税" "無核"
     "本土" "舶來" "霉" "銳" "濁" "有名" "舊" "老"
     "惟一" "有關" "不行"
     )
    ;;
    ((adj-on-noun-CV)
     (adj-word-qing1 "清")
     (adj-word-tall "高") "低"
     (adj-word-big "大")
     (adj-word-small "小")
     (adj-word-strong "強")
     )
    (adj-word-handsome "帥")
    ;;
    "各式各樣" "一式一樣" "陌生"
    "柔" ;; could also mean the manner of touching, e.g. '柔柔地 撫摸'
    "集約" "小眾" "大眾" "集體" "十全" "過剩" "全能"
    "大幅" "大幅度"
    "倉促" "低速" "穩妥"
    "倉惶" "徐徐" "更快"
    "不小心" "硬性"
    "破格" "足額"
    "逆勢" "赤膊" "多方" "迎頭" "辛勤"
    "逆風" "多頭"
    "或快或慢" "清一色" "一迳" "雜亂"
    "免不了" "趕忙" "秉公"
    "並肩" "多方面"
    "逆向" "慢慢" "縱向" "低聲"
    "留心" "最大限度" "朝天" "熱切" "報復性"
    "斗膽" "賣力" "虛心" "神速"
    "逆市" "七嘴八舌" "定量" "快步" "手動"
    "全線" "僥倖" "親熱" "超常" "苦口婆心"
    "不費吹灰之力" "三位一體" "最先"
    "超基數" "恢復性" "不規則" "大批量"
    "尖聲" "應急性" "大範圍" "有步驟"
    "始終如一" "超時" "倉卒" "盡興"
    "沉著" "翹首" "限量" "兢兢業業"
    "不定期" "不旋踵" "同聲" "齊頭"
    "衝動性" "殷勤" "慢條斯理"
    "曖曖" "隨興" "熠熠" "焦灼" "勤懇"
    "光明正大" "實驗性" "照實" "恭謹"
    "一本正經" "自發性" "有意無意"
    "堅絕" "重度" "中度" "輕度" "完整" "巨量"
    "存心" "迎面" "穩當" "反手" "青一色"
    "限時" "一生一世" "壯烈" "超負荷"
    "扼要" "斷續" "零距離"
    "湊巧" "時有" "週期性" "狂亂" "假設性"
    "大篇幅" "重複性" "隨風" "奮激" "規矩"
    "空口" "不宜" "小幅度" "不難" "小聲"
    "越少" "實再" "荷槍實彈" "不間斷"
    "通宵達旦" "不由自主" "廣義上" "輕言細語"
    "未料" "踉蹌" "雄赳赳"
    "一心一意" "無休止" "鬼祟"
    "盡職" "較少" "不慌不忙" "親蜜"
    "死心踏地" "集體性" "忽悠" "殺氣騰騰"
    "煞費苦心" "規模化" "一般性" "機靈"
    "周全" "齊心協力" "不定" "心無旁鶩"
    "心無旁騖" "挖空心思" "成群結隊" "不經意"
    "無妄" "依規" "含辛茹苦" "憂傷"
    "利索" "游刃有餘" "徒手"
    "不折不扣" "碰巧" "急忙忙"
    ;;
    "流暢" "完滿"
    "不當" "英勇" "痛" "遲" "書面"
    "堅定" "成套" "片面" "惡性" "善意"
    "永久性" "緊" "深度"
    "超前" "面對面" "天生"
    "習慣性" "不懈" "完美" "額外" "地下"
    "滾滾" "深切" "刻苦" "零星"
    "誠實" "連日" "協調" "分散"
    "持槍" "雙向" "歷史性" "簡要"
    "系統性" "單向" "良性" "有機"
    "突出" "平衡" "無恥" "象徵性" "強勢"
    "動態" "便捷" "狂妄" "分類"
    "分區" "同等" "必須" "對口" "莊嚴" "妥當"
    "重疊" "兼職" "劇烈"
    "普及" "無序" "大型" "彈性"
    "局部" "模糊" "確切" "無畏" "低調"
    "風光" "悠哉" "過往"
    "充裕" "原封不動" "爽快" "傳統"
    "經常性" "自助" "無情" "籠統"
    "有償" "規範化" "不力" "紮實"
    "頑固" "粗放" "離奇" "全額" "高齡"
    "武裝" "自如" "靜態" "商業"
    "消極" "忠心" "超值" "階段性" "完善" "縝密"
    "全國性" "官方" "流動" "政策性" "理智"
    "由衷" "出奇" "團結" "冷血"
    "煽動性" "猖狂" "熊熊" "惡毒"
    "激動" "個性化" "敏銳"
    "強制性" "牢固" "人性化" "結構性"
    "無盡" "畸形" "圓潤" "堅強" "合情" "合理"
    "垂直" "有限" "總體" "合情合理"
    "親筆" "十足" "慷慨" "坦白"
    "依序" "無照" "無限期" "草率"
    "一齊" "頑強" "幕後" "同期"
    "過量" "耐心" "合資" "較早" "固定"
    "不等" "實時" "最少" "不堪"
    "從寛" "不慎" "小幅" "從容" "不好意思"
    "急速" "火速" "全天候" "義務" "從嚴"
    "常年" "有序" "有望"
    "異地" "很快" "全方位" "慎重"
    "較好" "獨資" "永續" "緊張" "短暫"
    "緩慢" "即時" "理所當然"
    "無奈" "放心" "口頭" "急切"
    (adj-word-you2-ke2-neng2 "有可能")
    (adj-word-ming2-bai2 "明白")
    "無償" "迅猛" "意外" "蓄意" "永世"
    "民主" "不便" "任意" "瞎" "當眾" "隱約"
    "注定" "鄭重" "無私" "成倍" "違規" "實在"
    "準時" "熱烈" "相應" "踴躍" "全程"
    "再來" "飛速" "過多" "大批" "和諧" "整體"
    "審慎" "來不及" "過早" "肆意" "勤"
    "衷心" "自覺" "自然而然" "安心"
    "難得" "随手" "先行" "表面上"
    "空前" "私自" "綜合" "無力" "連帶"
    "緊接著" "同比" "突然間"
    "實則" "瞬間" "一瞬間" "理性" "接連"
    "層層" "欣然" "密集" "死" "全心全意"
    "竭力" "第一時間" "超額"
    ;;"對外" ;; this adj is sometimes as JJ in CTB, but sometimes as (PP (P 對) (NP (NN 外)))
    "實質上" "如實" "全盤" "惡意" "強力"
    "大不了" "純粹" "悄然" "終身"
    "同步" "高調" "適度" "隆重"
    "定期" "這樣子" "空腹" "盡情"
    "無限" "大規模" "重複" "定" "瘋狂"
    "先前" "蓬勃" "平穩" "盲目"
    "集中" "足夠" "適時" "勉強"
    "獨立" "間接" "生前" "急劇" "大膽"
    "大聲" "自主" "特意" "人工" "詳細"
    "異常" "大致" "強制" "反覆"
    "科學" "免費" "統一" "極端"
    "嚴厲" "優先" "公然" "單獨" "廣泛"
    "深入" "私下" "私底下" "妥善"
    "刻意" "歷來" "相反" "精心"
    "不幸" "原先" "干脆" "切實"
    "故意" "緊急" "一致" "強行" "重點"
    "隨後" "高度" "夠" "持續" "密切"
    "暫時" "明確" "聯合" "初步" "平均"
    "原本" "快速" "及時" "順利" "通常"
    (adj-word-da4-li4 "大力")
    "徹底" "明顯" "專門" "相對"
    "無所不能" "迅速" "公開" "逐步"
    "傻" "榮幸"
    (adj-word-zi4-wo3 "自我")
    (adj-word-unjust "不義")
    "專業" "自信" "苦"
    "無賴" "自由" "傷心" "低俗"
    "老土" "低端" "尖端" "高貴"
    "肉麻" "踏實" "欣慰"
    "富饒"
    "儒雅" "文雅" "寂靜" "偏頗"
    "艱險" "智障" "殘障"
    "黑心" "沉悶" "悶" "鬱悶"
    "辛酸" "莊重" "穩重" "謹慎" "遲鈍"
    "恬靜" "恬謐" "恬淡" "暢通"
    "恬適" "恬然" "安然" "坦然" "安逸"
    "心狠手辣" "心狠" "手辣" "窘迫"
    (adj-word-di1-mi2 "低迷")
    "浮躁" "疲軟" "泛泛"
    "富足" "雀躍" "齷蹉" "齷齪"
    "刻板" "搶手" "赤貧" "賤"
    "虔誠" "錯誤" "科幻" "無稽"
    "甜蜜" "熱血" "首要" "長久" "單調"
    "低落" "顯著" "野蠻"
    "極致" "精良" "艱難" "困苦"
    "準繩" "滔天"
    "頻仍" "單純" "正經" "秘絕"
    "好笑" "另類" "糟" "頻繁"
    "孬" "淫穢" "狹窄" "正統"
    "狼狽" "天真" "率真" "數碼" "強硬"
    "茁壯" "豐盛" "興盛" "昌盛" "平直" "耿直"
    "癢" "腫" "狗娘養" "狗養" "充盈"
    "通暢" "順暢"
    "絢爛" "狂熱" "輝煌" "繁瑣" "蕭瑟"
    "下三濫" "冷漠" "散漫" "浪漫" "偏激"
    "平淡" "暗淡" "優游" "圓滿" "美滿"
    "堅毅" "酥" "空洞"
    "拘束" "拘謹" "蕭條" "陰柔"
    "速效" "高效" "內斂" "古樸" "簡樸"
    "傲慢" "憤慨" "兒戲" "重頭" "現成"
    "悲慘" "歡愉" "如意" "悲憤" "仁慈"
    "永恆" "可惡" "邪惡" "賢惠" "實惠" "淒慘"
    "千篇一律" "殘忍" "可怕" "驚怵"
    "誇張" "脆弱" "恰當" "光彩"
    "高尚" "夢幻"
    "典型" "緊密" "貧寒" "時尚"
    "透天" "荒唐" "滄桑" "稀有"
    "個別" "給力" "自動" "被動"
    "專供" "市儈" "公允"
    "匱乏" "凌亂" "慌亂" "紛亂" "虛盈實虧"
    "三D" "全D" "人為" "絢麗" "艷麗" "驚人"
    "迷幻" "法定" "紊亂" "太平" "1D" "2D" "3D" "4D"
    "原始" "新奇" "頹" "寫實"
    "犀利" "吉利" "温和" "经适"
    "柔情" "煽情" "外來" "熱門" "冷門" "虛"
    "威風" "浮誇" "熱情" "激情" "温情" "深情"
    "軟" "廢"
    "爆烈" "性感" "不道德" "鈍"
    (adj-word-shi2 "實")
    (adj-word-fake "假")
    (adj-word-ren2-dao4 "人道")
    "不凡"
    "邪" "歪" "噁心" "精彩" "吉祥"
    "普通" "平凡" "俗" "文明" "恐怖" "不法"
    "可靠" "常見" "必要" "神秘" "靈活" "廉潔"
    "特殊" "多樣" "隨機" "神聖" "臨時"
    "親和" "特定" "特别"
    "均衡" "弱" "薄弱" "共同" "合格" "無厘頭"
    "怪" "特派" "入超" "敵對" "多極"
    "單極" "世俗" "兩極" "雙極" "防暴" "成熟"
    "粗俗"
    (adj-word-jin4 "勁")
    "靈敏" "知名" "負面" "正面" "優等" "劣等" "高壓"
    "真" "奢侈" "地道" "厚道" "公道"
    "霸氣" "寒酸" "豪氣" "帥氣"
    "大氣" "土里土氣" "喜氣" "自來" "暗"
    ;;
    (adj-word-good
     "好"
     (adj-word-mei3 "美")
     ((adj-shang4 adj-no-mono-de) "上"))
    (adj-word-cha4 "差")
    (adj-word-cuo4 "錯")
    (adj-on-distance "遠" "近")
    (adj-word-dirty "髒")
    (adj-word-dui4 "對")
    "有用"
    "壞" "不錯" "中式" "合適" "糟糕" "一樣" "不同" "美麗" "主要"
    "熱" "涼快" "古典" "優美" "安靜" "靜" "安寧" "寧靜" "正常" "一般" "平常" "偉大" "珍貴" "簡單" "奇怪" "尋常"
    "地道" "透明" "和平"
    "痛苦" "慘" "完全" "獨特" "直接"
    "成千上萬" "有趣" "不安全" "安全" "不得了"
    "酷" "少數" "多餘"
    "天然" "超級" "危險" "無窮" "整個" "古怪"
    "厲害" "漂亮" "討厭" "可愛" "正宗"
    (adj-more-less "多" "少")
    (adj-on-weight "重" "輕")
    (adj-word-gan1-jing4 "乾淨")
    (adj-word-zheng3-qi2 "整齊")
    )
   ;;
   (adj-on-place
    ((adj-on-place-NM adj-no-mono-de)
     ((adj-on-place-NM-UA adj-unlikely-adv) "沃")
     )
    ((adj-on-place-UA adj-unlikely-adv)
     (adj-word-huang1 "荒")
     "開闊" "偏僻" "肥沃"
     ))
   ;;
   (adj-on-physical
    ((adj-on-physical-NM adj-no-mono-de)
     "疼"
     ((adj-on-physical-NM-UA adj-unlikely-adv)
      "糙" "野" "淨"))
    ((adj-on-physical-UA adj-unlikely-adv)
     "滾圓" "沸" "脆"
     )
    ;;
    "好看" "直" "舒適"
    "好吃懶做" "醜"
    (adj-color
     ((adj-color-NM adj-no-mono-de)
      ((adj-color-NM-UA adj-unlikely-adv)
       "彩" "蒼" "烏" "朱" "碧" "丹"))
     ((adj-color-UA adj-unlikely-adv)
      "白" "橙" "黃" "綠" "青" "藍" "紫" "褐"
      "朱紅" "灰藍" "銀灰" "棕"
      (adj-gold-silver "金" "銀")
      )
     "黑" ;; could also mean unlucky
     "紅" ;; could also mean famous
     "鮮紅"
     (adj-grey "灰") ;; could also mean frustrated
     )
    (adj-shape
     ((adj-shape-NM adj-no-mono-de)
      ((adj-shape-NM-UA adj-unlikely-adv)
       "畸" "菱" "矩"))
     (adj-word-pointy "尖")
     (adj-word-fang1 "方")
     (adj-thin-or-thick ;; could also modify money-related things, e.g. 薄利
      "薄" "厚")
     (adj-on-dimension
      ((adj-on-dimension-UA adj-unlikely-adv) "粗壯")
      "寬闊" "寬" "粗")
     ((adj-shape-UA adj-unlikely-adv)
      (adj-word-geometric "幾何")
      "箱型" "微型" "方塊" "長條" "平面"
      )
     "圓"
     "弧" "立體" "斜" "螺旋" "凹"
     (adj-word-flat "平"))
    (adj-size
     ((adj-size-NM adj-no-mono-de)
      ((adj-size-NM-UA adj-unlikely-adv) "巨"))
     (adj-word-xi4
      "細" ;; could also mean manner
      )
      ((adj-size-UA adj-unlikely-adv)
       "高大" "矮小" "矮"))
    (adj-temperature
     "冷" ;; could also mean the manner of someone, e.g. in speaking
     ((adj-temperature-UA adj-unlikely-adv)
      (adj-word-wen1 "溫")
      (adj-word-below-zero "零下")
      "溫暖" "暖" "暖和" "涼" "寒冷" "寒" "嚴寒"))
    (adj-on-physical-or-time
     (adj-on-length
      ;; could also describe the manner, e.g. 長長地 嘆氣
      (adj-word-long "長")
      "短")
     ))
   ;;
   (adj-on-food
    ((adj-on-food-NM adj-no-mono-de)
     ((adj-on-food-NM-UA adj-unlikely-adv) "膩"))
    ((adj-on-food-UA adj-unlikely-adv)
     (adj-word-sheng1 "生")
     (adj-word-xian1 "鮮")
     "好吃" "鹹" "辣" "腥")
    "甜" "酸")
   ;;
   (adj-on-animate
    ((adj-on-animate-NM adj-no-mono-de)
     "憨" "狡" "呆" "殘" "疾" "惰"
     ((adj-on-animate-NM-UA adj-unlikely-adv)
      (adj-word-qin1 "親")
      "壯" "幼"))
    ((adj-on-animate-UA adj-unlikely-adv)
     "聾" "駐華" "雌" "雄" "年輕" "餓" "公" "純種" "警醒"
     "肥胖" "胖" "瘦" "渴" "殘疾" "傷殘")
    ;;
    "窩囊" "健忘" "鮮活"
    "疲勞" "忠厚" "憨厚" "寂寞"
    "爱面" "爱美" "腦殘" "乖" "醒目" "狡滑"
    "舒服" "友好" "好奇" "活" "瘋" "淘氣" "高傲" "可憐" "忠實"
    "嘰喳" "不安" "清醒" "般配"
    "憤怒" "煩躁" "焦急" "無辜" "懶" "蠢" "笨" "癡"
    "膽小" "短命" "懶惰" "忠誠"
    (adj-word-healthy "健康")
    (adj-on-animate-or-organization ;; (e.g. company, country)
     "窮" "富裕" "富" "富有" "好客" "豪爽" "奸" "貧窮"
     "獨栽" "貧困" "窮困" "殘暴"
     ((adj-on-animate-or-organization-UA adj-unlikely-adv)
      "貧"))
    )
   ;;
   (adj-on-human
    ((adj-on-human-NM adj-no-mono-de)
     "勇" "懦" "刁" "鬱" "庸"
     ((adj-on-human-NM-UA adj-unlikely-adv)
      "嫡" "庶" "偽" "孤"))
    ((adj-on-human-UA adj-unlikely-adv)
     "嫡出" "庶出" "高個" "資優" "嬌嫩" "英俊" "年青" "有錢"
     "出身低微" "低保" "單身" "新來" "有产" "无产" "善弈"
     "私生" "未婚" "非婚生" "婚生" "親生")
    (adj-word-hao3-xin1 "好心")
    "狼子野心" "斯文" "果斷"
    "無怨" "無尤" "懦弱"
    "笑裡藏刀" "奮勇" "武勇" "勇武" "婆婆媽媽"
    "狼子野心" "專心" "陽剛"
    "客氣" "努力" "謙虛" "驕傲" "大方" "能幹" "羅嗦" "勇敢"
    "自在" "慈祥" "孝順" "主動" "委屈" "驚訝"
    "憂愁" "頹唐" "有緣" "猶豫" "清白" "善良" "熱心"
    "有心" "有情" "幸運" "悲觀"
    "無知" "自願" "變態" "匿名" "敗家"
    ((adj-on-human-or-food adj-on-food) "熟")
    )
   (adj-on-occupation-or-title
    ((adj-on-occupation-or-title-NM adj-no-mono-de)
     ((adj-on-occupation-or-title-NM-UA adj-unlikely-adv) "賢")))
   (adj-on-animal
    ((adj-on-animal-or-food adj-on-food)
     ((adj-on-animal-or-food-UA adj-unlikely-adv) "嫩")))
   ;;
   (adj-on-inanimate
    ((adj-on-inanimate-NM adj-no-mono-de)
     "茂" ;; could mean the growing of plants, e.g. '茂茂地 生長'
     ((adj-on-inanimate-NM-UA adj-unlikely-adv)
      (adj-word-slim "纖")
      (adj-word-valuable "寶")
      (adj-word-ma2 "麻") ;; could also describe feeling, e.g. 麻麻的
      "豪" "贗"))
    ((adj-on-inanimate-UA adj-unlikely-adv)
     (adj-word-sharp "利")
     (adj-on-cloths "短袖" "長袖")
     "卷心" "大宗" "機織" "手織" "高幫" "中幫" "低幫"
     "破" "陳舊" "陳年" "蒼蒼" "碎" "野生" "紙製" "仿精"
     "人行")
    "大件" "小件"
    (adj-word-oily "油")
    (adj-word-light "光")
    (adj-word-full "滿")
    "閃" "卑劣"
    "扁" "高速" "亮"
    "茂盛" "晶瑩" "透"
    "純"
    (adj-price "便宜" "高價" "低價" "廉價")
    "韌" "柔韌" "手工"
    "完好" "精致" "電動")
   ;;
   (adj-on-weather
    ((adj-word-qing2 adj-no-mono-de adj-unlikely-adv) "晴"))
   (adj-on-language "流利")
   ;;
   (adj-on-abstract
    ((adj-on-abstract-NM adj-no-mono-de)
     "窘" "詳" "巧" "久" "激" "微" "穩"
     (adj-time-period
      ;; e.g. '年薪'
      "年" "月" "日" "時" "週" "季")
     ((adj-on-abstract-NM-UA adj-unlikely-adv)
      (adj-word-mo4 "末")
      "噩" "謬" "幻" "歧" "趣" "弊"))
    ((adj-on-abstract-UA adj-unlikely-adv)
     "明朗" "新興" "可行" "國營" "諸多" "人均" "動亂" "中性"
     "賣座")
    (adj-word-difficult "難")
    (adj-word-chang2-qi2 "長期")
    (adj-word-qiao1-qiao1 "悄悄")
    (adj-word-qing1-chu3 "清楚" "一清二楚")
    (adj-word-lei3-jin4 "累進")
    (adj-word-zero-sum "零和")
    "方便" "直覺"
    "錯綜複雜" "直觀" "百姓通"
    "糗" "虚幻" "短期" "無上"
    "容易" "簡易" "熱鬧" "正式" "感人"
    "正好" "絕對" "穩定" "安定" "複雜"
    "突然" "理想" "平等" "同樣" "激烈" "強烈" "平安" "滿意"
    "小康" "可惜" "難忘" "不利" "惋惜" "漫漫" "震撼"
    "自費" "虛榮" "要緊" "真正" "豐富" "嚴重" "輕易" "全球"
    "過度" "和睦" "美好" "安穩" "偶然" "妥帖"
    "殘酷" "適當" "輕而易舉" "艱巨" "紛擾" "混亂"
    "常規" "急迫" "一貫" "真確" "必然" "有效" "深刻" "迫切"
    "多元" "緊迫" "嚴峻" "獨家"
    "困難" "均匀" "單方面" "單邊" "多邊" "雙邊"
    ((adj-on-abstract-or-place adj-on-place)
     ((adj-on-abstract-or-place-NM adj-no-mono-de)
      (adj-word-quan2 "全")
      ))
    (adj-on-abstract-or-movable
     "慢" "快" "有力")
    ((adj-on-abstract-or-food adj-on-food)
     "新鮮"
     ((adj-on-abstract-or-food-NM adj-no-mono-de) "淡"))
    ;;
    ((adj-on-abstract-or-inanimate adj-on-inanimate)
     ((adj-on-abstract-or-inanimate-NM adj-no-mono-de)
      "烈" "密"
      ((adj-on-abstract-or-inanimate-NM-UA adj-unlikely-adv)
       "盛" "宏" "峻" "秘"))
     ((adj-on-abstract-or-inanimate-UA adj-unlikely-adv)
      "通用" "適用" "人山人海" "天長地久" "公共"
      "繁華" "沉重" "迫" "許久"
      "基本" "多用途" "中级" "濃" "飽和"
      "等温" "康莊"
      (adj-word-night-or-late "晚")
      (adj-word-morning-or-early "早")
      (adj-word-chemical "化學")
      )
     (adj-deep-shallow "深" "淺")
     (adj-word-kong1 "空")
     (adj-word-curved "曲")
     (adj-on-wideness
      ((adj-on-wideness-NM adj-no-mono-de) "廣")
      "窄" "闊" "廣闊")
     (adj-word-lian2-xu4 "連續")
     (adj-word-shui3-ping2
      ;; also means position-of, or standard-of
      "水平")
     "具體" "普遍" "精確" "冷清" "豪華" "秘密"
     "實用" "標準" "燦爛" "硬" "爛" "連貫"
     "對稱" "準確" "非法" "合法" "私密"
     "永久" "違法" "空頭" "低碳"
     "清晰" "平行"
     )
    ;; abstract or animate
    ((adj-on-abstract-or-animate adj-on-animate)
     ((adj-on-abstract-or-animate-NM adj-no-mono-de)
      "樸" "忠" "愚" "謙" "燥" "累"
      (adj-word-se4 "色")
      ((adj-on-abstract-or-animate-NM-UA adj-unlikely-adv)
       "淑" "睿"))
     ((adj-on-abstract-or-animate-UA adj-unlikely-adv)
      "激奮" "濕" "煩惱" "煩擾" "優越" "虛實" "要害"
      (adj-on-competition
       "男單" "男雙" "女單" "女雙" "混雙" "男女混合" "雙打"))
     ;;
     (adj-word-qing1-chun1 "青春")
     (adj-word-you1-mo4 "幽默")
     (adj-word-successful "成功")
     "弱智" "瀟灑" "精深" "高深" "小氣" "愚昧"
     "自戀" "鎮定" "實幹" "費幹" "軟弱" "猥瑣"
     "嚴明" "功利" "務實" "騎牆" "騷" "激進" "達觀"
     "淫" "尷尬" "斯文" "英明"
     "虧心" "誠心" "細心" "真心" "痴心" "狠" "狠心"
     "著急" "急忙" "倒霉" "認真" "高明" "正確" "艱苦" "樸素"
     "勤儉" "愉快" "幸福" "幸福安寧" "別扭" "公平" "誠摯"
     "優秀" "聰明" "堅決" "糊塗"
     "業餘" "實際" "了不起" "冷淡" "乾燥" "嚴格" "樂觀" "過分"
     "煩" "誠懇" "絕望" "悲哀" "親切"
     "開心" "開朗" "積極" "長壽" "真誠" "幼稚" "慘淡"
     "仔細" "小心" "入時" "凄涼" "痛快" "匆忙" "忙" "安好"
     "安閒" "安詳" "閒"
     "安適" "煩悶" "厭煩" "輕鬆" "隨便" "馬虎" "溫柔" "沉默"
     "純潔" "無聊" "老實" "快樂" "悲痛"
     "悲傷" "難過" "高興" "辛苦" "惡" "醜惡" "失敗" "落伍"
     "現實" "官僚" "保守" "白痴" "厚黑" "暴力" "嚴" "嚴酷"
     "客觀" "主觀" "嚴肅" "合理" "公正" "敏感" "随意" "正當"
     "真實"
     "權威" "穩健" "正義" "小家" "盲" "坦率" "大度" "精明" "大方"
     "霸道" "和氣" "利害"
     )
    )))

;; end adjectives-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
