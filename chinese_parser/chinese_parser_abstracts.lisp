(in-package :chinese-parser)

(def-tagged-words *abstract-list* *abstract-hash*
  ;; merge animate-attribute and inanimate-attribute to here
  (@ abstract
     ((hua-verb verb)
      ;; for * + 化 --> verb, and is also an abstract noun
      )
     "課文" "軍事" 
     "練習" "口語"
     (dependency-of "依賴")
     (grammar "語法" "文法")
     ((framework furniture)
      ;; could be abstract framework, or a physical frame
      "框架" "框框")
     "問答"
     "太極拳" "公分" "航空" "海運" "通知" "海關"
     "紀念"  "空白"
     "彌補"
     ((short-hand-name name)
      ;; not sure what category to put it, but these are common short hands
      "P2P" "HDMI" "IPO" "GDP" "GNP" "OFFER"
      )
     (@ can-be-job ;; things we can put after '做' or '從事' to be a job
      (advertising "廣告")
      (media "媒體" "外媒")
      (technology "科技")
      "幕前" "幕後" "傳銷"
      "相聲" "性工作" "電影" "金融" "網管"
      "漁護" "懲教" "高監" "海監" "消保"
      (politics
       ((politics-ind ind) "政")
       "政治" "内政")
      ((fund-ind ind) "基金")
      ;; things end with 業 are often an industry, and can be job
      (industry
       "林業" "手工業" "工業" "農工業" "實業"
       "電子業" "石化業" "輕工業" "重工業"
       ;; general reference, not specific industry
       "百業" "正業" "本業" "各行各業" "主業"
       "行業" "產業" "業內"
       ;;
       ((industry-ind ind) "業")
       ((industry-name name)
        (industry-word-shang1 "商")
        ((industry-word-logistics compound) "物流")
        "會計" "殯儀" "IT" "工商" "政經" "財經"
        "農" "農耕" "漁" "畜牧" "水族" "零售" "餐飲"
        "飲食" "郵電" "化工" "電信" "重化工" "批發"
        (housing-industry
         "房地產" "地產"
         ((housing inanimate) "房產" "不動產")
         )
        (clothing-industry "成衣"))
       ))
     (@ can-be-fang4 ;; can be used with 放
        (@ study
           ((study-ind ind) "學"))
        (@ work
           ;; 工 could mean work, or as suffix of animate to mean worker
           ;; but we do not distinguish between them very clearly because
           ;; they are usually used interchangeably.
           ((work-ind ind) "工")
           (work-huo2 "活")
           "夜班" "日班")
        "晴" "牧" )
     (hatred
      "仇" "怨" "恩怨" "恩怨情仇" "仇怨" "仇恨" "刻骨仇恨"
      "深仇大恨" "憾恨")
     (marks "分數" "比分" "零蛋" "評分")
     ;;(route "航") ;; TODO
     ;;
     ;;(noun-time "時間")
     (weather
      "天氣"
      "氣象" ;; could also mean phenomenon
      )
     (@ audible ;; basically things that can be 聽
      )
     (@ written-character
      "字" ;; "詞" elsewhere
      "漢字" "生詞" "詞語" "詞條" "詞彙" "辭彙"
      "錯別字" "別字" "字幕" "字母" "字彙" "字眼"
      "措詞" "用字" "用詞" "詞藻"
      "動詞" "名詞" "代名詞" "副詞"
      (@ character-style
       "楷書" "標楷體" "正體" "殘體"
       "歐體" "顏體" "柳體" "趙體" "草書" "隸書"
       "篆書" "篆文" "行書" "行楷" "行草"
       ((character-style-ind ind) "體" "字體")
       ((character-style-name name)
        "楷" "正楷" "小楷" "大楷" "篆" "大篆" "小篆" "隸"
        )))
     (@ info-report
        ((info-warning sign-signal)
         "預警" "警報" "警示" "警告" "警號")
      "報告" "訃告" "訃聞" "告示" "諜" "諜報"
      "浮報" "情報" "特報" "捷報" "戰報" "公報" "預報")
     (@ creative-work
      "作品" "創作" "對子" "力作" "勞作" "代表作"
      "動漫" "伏筆" "手筆" "敗筆" "專輯" "特輯"
      "夜譚" "天方夜譚" "奇譚" "選輯" "集錦" "全集"
      "續集" "頌" "輯" "大作" "創造" "原創" "製作"
      ((creative-work-ind ind) "作")
      (@ article-like
       "文章" "小說" "論文" "稿" "行文" 
       "貼子" "楔子" "引子" "帖子" "經文" "經"
       "散文" "捐体文" 
       "上文" "下文" "前文" "列傳" "著作"
       "典故" "掌故" "通稿" "簡述" "自述" "講述"
       "描寫" "描述" "序述" "闡述" "表述"
       "陳述" "論述" "描述"
       (preface ;; "序" in the process-or-order
        "目錄" "根目錄" ;; table of content is similar to preface
        )
       (page-like
        ((page-like-ind ind) "頁")
        (website ;; or webpage, or internet
         ((website-ind ind) "网站")
         "互联网" "因特网" "因特网站"
         ;; names of websites
         ((website-name nr-name)
          "奇摩站" "奇摩" "新浪网" "中新网" "新华网" "东网")
         ;;
         "万象网" "红网" "酷才网"
         ;;
         "官网"
         ;;
         "網頁" "主頁" "首頁"
         "博客" "博客站" "部落格" "微博" "博文")
        )
       (outline ;; or like table of content, a brief summary
        "大綱" "政綱" "提綱" "史綱" "小結" "總結" "摘要"
        "綱領" "概說")
       (@ chapter ;; 章 also means a stamp, so elsewhere
        "章節" "華章" "簡章" "篇章" "篇" "編" "段落")
       ((newspaper-like info-report)
        "報紙" "報章"
        ;;
        "乡亲报" "邮报" "电讯报" "先驱报" "参考报"
        "学生报" "晨报" "晚报" "时报" "早报" "日报"
        "英文报" "中文报" "快报" "都市报" "小报"
        "导报" "观察家报" "宪报" "大字报" "电子报"
        "论坛报" "周报" "社区报" "华侨报"
        ;;
        ((newspaper-like-name nr-name)
         "太阳报" "新闻报" "每日镜报" "民生报" "文汇报"
         "民报" "南街村报" "红星报" "星报" "明报"
         "托拉斯报" "放屁报" "泰晤士报" "联合报" "卫报"
         "大公报" "新京报" "联合早报" "联合晚报")
        ;;
        ((newspaper-like-ind ind) "報")
        )
       (@ legal-document
        ((legal-doc-ind ind)
         "證書" "文書" "文憑" "契" "約" "證明")
        ((legal-doc-or-situation-or-shape situation shape-of)
         ((legal-doc-or-situation-or-shape-ind ind) "狀"))
        "聲明" "協議" "協定"
        "條文" "條款" "换文" "陳詞" "陳詞書" "條例" "合同"
        "遺囑" "合約" "契約" "地契" "令狀" "條約" "狀紙")
       ((audible-article-like audible)
        "故事" "神話" "段子" "童話" "佳話" "寓言" "草稿"
        (news
         ((news-ind ind) "聞")
         "新聞" "報導" "報道" "新聞報導" "頭條" "緋聞" "醜聞"
         ((news-or-ba1-gua4 tool)
          ;; 八卦 could be a gadget, or some sorts of not very
          ;; important news.
          "八卦")
         )
        (poem-like
         "詩" "詩作" "詩句" "對聯" "上聯" "下聯" "春聯"
         "横批" "橫披" "詩篇" "打油詩" "詞賦" "詩賦"
         "歌賦" "詩詞" 
         ;; 賦 is something like" poem, but also means a kind of tax
         ((written-character-poem-like written-character)
          "詞" "詞句"))))
      (@ sound
       ;; "聲音"
       (sound-literal)
       ((sound-ind ind) "音" "聲" "聲音")
       "噪音" "鼾" "風聲" "嚎" "噪聲" "響"
       "喧囂" "囂" "聲息" "音效" "聲浪"
       "天籟" "靡靡之音" "鳴" "發音"
       "呼叫" "漫罵" "呼喊" "喊叫" "喝彩" "喊話"
       "錄音" "吶喊"
       (@ music
        ((music-ind ind) "樂" "曲")
        "音樂" "調子" "快板" "慢板" 
        "交響樂" "旋律" "搖滾" "配樂"
        "聲部" "合音" "弦樂" "管弦樂" "音韻" "樂曲"
        (song
         ((song-ind ind) "歌" "調")
         ((rumor-or-song rumor) "謠")
         "歌曲" "秧歌" "歌仔" "民歌" "紅歌" "歌調"
         "北調" "曲調" "濫調" "歌謠" "民謠" "童謠")
        ))
      (dance
       ((dance-ind ind) "舞")
       ((dance-name name)
        "探戈" "芭蕾" "華爾茲" "狐步" "桑巴" "森巴"
        "佛朗明哥" "弗拉明戈" "舞踏" "倫巴" "恰恰恰"
        "嘻哈" "迪斯科" "恰恰")
       "熱舞" "笑舞" "交際舞"
       "舞蹈" "拉丁舞" "土風舞" "秧歌舞" "霹靂舞"
       "爵士舞" "踏板舞" "草裙舞" "肚皮舞"
       "現代舞" "豔舞" "快步舞" "鬥牛舞" "牛仔舞"
       "捷舞" "踢踏舞" "機械舞" "鎖舞" "鋼管舞")
      (@ video
       "影片" "廣告片" "影視" "影視片" "西片" "外片"
       "影像" "動畫" "錄相" "卡通" "視頻" "錄像"
       "剪輯"
       )
      ((drama audible)
       ((drama-ind ind) "戲" "劇")
       "过场" ;; some intermediate part between main scenes
       "節目"
       "肥皂劇" "傩剧" "的笃班" "劇作" "馬戲"
       "莆仙戲" "野台戲" "歌仔戲" "折子戲" "連臺本戲"
       "京劇" "越劇" "地方戲" "電視劇" "話劇" "戲劇" "小品"))
     (sentence ;; written or spoken, can be quantified with 句
      "句子" "台詞" "順口溜" "句" "字字句句" "語句"
      "謎語" "潛台詞"
      ((spoken audible)
       (lengthy-spoken ;; can be quantified with 席
        "談話" "談" "發言" "講話")
       (spell
        ;; could also be curse
        ((spell-ind ind) "咒" "咒語")
        "緊箍咒" "詛咒")
       ((speech-like belief)
        "格言" "梟言" "至理名言" "鑑言"
        "言論" "金玉良言" "諺語" "座右銘"
        (vow-or-promise
         ((vow-ind ind) "誓" "諾")
         "山盟海誓" "山盟" "海誓" "諾言" "承諾" "誓言" "保證")
        ((speech-like-ind ind) "言" "諺"))
       "一面之詞" "p話"
       "獻辭" "演辭" "言辭" "說辭" "辭" "文辭" 
       "口供" "對白" "獨白" "自白" "口頭禪"
       "警語" "言語" "歡聲笑語" "母語"
       "口號" "標語" "術語" "用語" "成語" "咒語"
       (spoken-language
        ((spoken-language-ind ind) "話" "語")
        ((article-or-language article-like)
         ((article-or-language-ind ind)  "文"))
        (lie "謊" "謊話" "謊言" "彌天大謊" "騙局")
        (rumor
         ;; 謠 could also mean song
         "謠言" "流言" "傳言" "傳聞" "謠傳")
        "話語" "實話" "廢話" "對話"
        ;; TODO: how to handle union like 国台语 --> 国语 + 台语
        "客語" "国台语" "俚語" "方言"
        "别话" "後話" ;; not exactly really about language or speaking
        "行話" "反話" "語族"
        "語言" "白話" "外語" "外文" "會話" "瞎话" "套话"
        "中文" "英文" "日文" "韓文" "法文" "德文" "泰文"
        "英語" "日語" "韓語" "法語" "德語" "泰語"
        (jokes "笑話" "玩笑")
        (foul-language "髒話" "粗口")
        "普通話")))
     (knowledge-domain ;; can be quantified with 門
      ((knowledge-domain-ind ind) "術" "藝")
      ((word-history-of ind) "史")
      ((knowledge-domain-compound compound)
       "理學")
      "建管" ;; shorthand for 建築管理
      "理化" "化理"
      "學術" "青史" "生態" "科研" "手外科"
      "易學" "法醫學" "儒學" "企管" "商研"
      "法學" "理工" "美學" "金石学" "顯學"
      "理科" "文科" "本科" "科學" "神學"
      "藝術" "園藝" "學問" "心理學" "美術" "文學"
      "哲學" "數學" "物理" "化學" "地理" "醫學"
      "藥理" "史學" "歷史" "概率"
      "機率" "數理" "體育" "電氣")
     (horoscope
      "星座"
      "仙女座" "寶瓶座" "天鷹座" "天壇座" "南船座"
      "白羊座" "御夫座" "牧夫座" "巨蟹座" "大犬座"
      "小犬座" "摩羯座" "仙后座" "半人馬座" "仙王座"
      "鯨魚座" "南冕座" "北冕座" "烏鴉座" "巨爵座"
      "天鵝座" "海豚座" "天龍座" "小馬座" "波江座"
      "雙子座" "武仙座" "長蛇座" "獅子座" "天兔座"
      "天秤座" "天狼座" "天琴座" "蛇夫座" "獵戶座"
      "飛馬座" "英仙座" "雙魚座" "南魚座" "天箭座"
      "人馬座" "天蠍座" "巨蛇座" "金牛座" "三角座"
      "大熊座" "小熊座" "室女座"
      )
     (seat
      ((seat-or-furniture-or-horoscope furniture horoscope)
       ((seat-or-furniture-or-horoscope-ind ind) "座"))
      "座位" "雅座" "位子" "前排" "後排")
     (baseball-strike ;; can be quantified with 支
      "安打" "全壘打")
     (prize-or-penalty
      "賞罰" "賞" "罰" "嘉獎" "賞賜" "處罰" "懲罰"
      "獎勵" "獎賞" "處分" "年終獎" "懲戒"
      ((prize-ind ind) "獎")
      ((prize-ind-word-cai3 ind) "彩")
      (marked-prize-or-penalty
       (contributions
        ((contributions-or-martial-arts martial-arts)
         ((contributions-or-martial-arts-ind ind) "功"))
        "功勳" "勳" "豐功" "功績" "苦勞"
        "小功" "大功" "汗馬之功" "功勞" "汗馬功勞" "頭功")
       (penalty-word-guo4 "過")
       "小過" "大過"))
     (benefit "好處" "利" "益" "利益" "蠅頭小利" "天时地利"
              "名利" "利潤" "紅利" "營利" "利祿" "津貼"
              "便宜" "獲益" "收獲" "收益" "優惠" "斬獲"
              "盈利" "補貼" "饋贈" "收益" "捐贈" "暴利"
              "奉獻" "獲利" "收穫" "效益")
     (cuteness "嬌")
     (content "内容")
     (business "業務")
     ((political-party human)
      "金光党" "反對派"
      "保守派" "資改派" "走资派" "花派" "支派"
      "海归派" "极左派" "左派" "左中右派" "右派" "极右派"
      ((political-party-name nr-name)
       "逊尼派" "什叶派" "拉罗什派" "阿拉维派"
       "共党" "国家安全党"
       "ｇｏｏｎ党" "三Ｋ党" "自民党" "涉民党" "神宇党" "国大党"
       "工黨" "共和黨" "社會黨" "共產黨" "民主黨" "致公黨" "革命黨"
       "公黨" "自由黨" "新民黨" "民主建港協進聯盟" "民建聯"
       "香港工會聯合會" "工聯會" "公民黨" "民進黨" "民主進步黨"
       "國民黨" "親民黨" "新黨" "建國黨" "勞動黨" "民政黨" "民主鬥爭黨"
       "人民行動黨" "工人黨" "憲法黨" "綠黨" "保守黨"
       )
      ((political-party-ind ind)
       ((political-party-or-group group-of-people)
        ((political-party-or-group-ind ind) "派"))
       "黨" "政黨" "教派" "幫派" "宗派" "派閥")
      )
     (@ race
      "阿族" "赛族" "在下族" "希族" "阿美族" "塞族"
      "邵族" "土族" "回族" "亞裔"
      ((race-ind ind)
       "民族" "種族" "裔" "族裔"
       ((race-or-group group-of-people)
        "部落"
        ((race-or-group-ind ind) "族")))
      (@ race-name
         (confusing-race-name
          (race-han4 "漢")
          "唐")
       "雅美" "排湾" "鲁凯" "布农" "凯达格兰" "蠻夷" "蠻" "夷"
       "祖鲁" "泰雅" "圖西" "滿" "客家" "猶太" "胡圖"
       "中華" "華" "康巴" "印地安" "印第安" "赛德克" "拉丁"))
     (doctrine
      ((doctrine-ind ind) "主義")
      ((doctrine-name nr-name)
       "納粹" "毛主義")
      "三民主義" "馬克斯主義" "人道主義"
      "沙文主義" "達爾文主義" "馬克思主義" "列寧主義"
      "馬列主義" "陸軍主義" "犬儒" "馬列"
      )
     ((abstract-or-human human)
      (combination "組合")
      (a-power "勢力")
      "主角" "化身" "監督" "主宰"
      "高商" ;; 高級商業
      (prize-ranks "冠軍" "亞軍" "季軍" "殿軍")
      )
     "愛情"
     "天氣預報"
     (a-change "變化" "興衰")
     (@ resulting-effect
      ((resulting-effect-ind ind) "效" "績")
      "結果" "成果" "效果" "實效" "功效" "成效"
      "時效" "績效" "後果" "效用" "成績" "業績"
      "偉績" "政績" "效能" "作用" "成敗")
     (word-act "動作")
     "書法"
     "表示" "志趣" "情趣" "趣"
     "樂趣" "心靈" "民間" "武術"
     "秘訣" "平方" "名勝" "古跡"
     "自然" "隱私" "競爭"
     "外交" "傳說"
     "玩味" "兴味"
     (word-condition "條件")
     (relationship-of "關係")
     (teaching "教育" "教誨" "教導")
     ((smell-or-flavor smell)
      ;; not only physical taste or smell or flavor, as in food,
      ;; but also abstract flavor
      "味道"
      "風味" "韻味"
      ((smell-or-flavor-ind ind) "味"))
     (word-research "研究")
     (fate-or-chance
      ((fate-or-chance-ind ind) "緣")
      (luck ;; "運" is also shorthand of 運輸, see transportation-of
       "運氣" "不幸" "幸" "大幸" "幸運" "晦運")
      (opportunity
       (word-shi2-ji1 "時機")
       "機會" "轉機" "良機" "生機" "新機" "契機" "商機"
       "可乘之機")
      "殺機" "危機" "天機" "先機"
      "緣分" "緣份"
      "機緣" "緣起" "命運" "機遇" "天運")
     (encounter-of
      ((encounter-of-ind ind) "遇")
      "境遇" "偶遇" "邂逅")
     (influence "影響")
     (society "社會")
     (reform "改革" "改革開放")
     (production "生產")
     "生命" "商品經濟"
     "享受" "交際" "氣候" "血緣"
     "計劃生育" "體統"
     "家庭" "單位" "封建" "矛盾"
     "結餘" "寫法" 
     "頭班車" "作業"
     "補習" "收成"
     "義務" "小組" "年代"
     "交通" "傳統"
     "說明" "負增長" "聽診" "理論"
     "整體" "針灸" "醫術" "和平"
     "母愛" "雙數" "早班"
     "衛生" "所在"
     "象徵"
     "資源" "表層"
     (space-of ;; could be physical or abstract
      "空間")
     (difficulty
      "瓶頸" "難點" "疑難雜症" "難處" "兩難" "疑難"
      (great-difficulty "難關" "大關"))
     (project-or-item "項目" "專案")
     "前奏" "貢獻" "升華" "時刻" "反復" "光景" "奇跡"
     "學期" "邊防" "往常" "源流" "測試" 
     (abstract-road
      "歸途")
     "好報" "所見所聞"
     (@ plan "計劃" "規劃" "企劃")
     (marriage "婚")
     (information ;; "料" also in material-of
      ((information-ind ind) "訊")
      (information-word-zi1-liao4 "資料")
      "史料" "利空" "消息" "信息" "訊息" "噩耗"
      "貼士" "資訊")
     (job-shift "輪值" "輪班")
     (@ money-related
      ;; "錢" "金錢"
      (money-related-organization-short-hand
       ;; these often used as shorthand of an organization
       "外資" "國資")
      "抵押" "賠償" "補償" "理賠" "捐款" "撥款"
      "私己" "收入" "赤字" "進帳" "融資"
      "本錢" "香油錢" "零用" "零錢" "軍餉" "餉"
      "薪俸" "起薪" "薪資" "報酬" "酬" "片酬"
      "俸祿" "民脂" "民膏" "民脂民膏" "薪"
      "營收" "銀根" "賭注" "注碼" "俸" "祿"
      "回扣" "入息" "折扣" "按揭" "收支" "折"
      "利息" "毛利" "孳息" "息" "本息"
      "紅包" "歲幣" "存底" "勾當" "外快"
      "款項" "橫財" "罰款" "身家" "家當"
      "資本" "資金" "外匯" "存款" "貸款" "匯款"
      "首付" "首期"
      "資" ;; 資 also mean 資格, which is also abstract
      "工資" "股本" "帳單" "賬" "帑"
      (word-hui2-long2 "回笼")
      ((tent-or-account-related tent-like)
       "帳")
      (tax
       "賦稅" "版稅" "个税" "税收" "稅賦" "田賦"
       "退税"
       ((tax-or-poem-like poem-like)
        ((tax-or-poem-like-ind ind) "賦"))
       ((tax-ind ind) "税"))
      (@ cost-or-budget
       ((cost-or-budget-ind ind) "費")
       (abstract-cost-to-pay "代價")
       "花費" "支出"
       "費用" "家用" "租" "租金" "開銷" "開支"
       "壓金" "佣" "價金" "薪金" "薪水" "成本"
       "重金" "黑金" "酬金" "公積金" "年金" "獎學金"
       "經費" "預算" "機要費" "郵費" "學費" "水電費"
       "房租" "醫藥費" "旅費" "運費" "話費" "月租"
       "学杂费" "收費" "人工" "小費" "人头费" "團費"
       "會費"
       )
      ((loan-related-job can-be-job) "信貸")
      ((money-related-can-be-fang4 can-be-fang4)
       ;; "給付" ;; TODO: whether to give a noun to this, which is mostly a behavior, as a debt
       "債" "貸" "款" "内債" "外債" "債務")
      (@ currency
       ((currency-ind ind) "幣" "元" "鈔" "貨幣")
       ((currency-or-marks marks)
        ;; 分 means 10 cents as money. also means marks.
        ;; In "贵贱 之 分", '分' also means difference
        ((currency-or-marks-ind ind) "分"))
       (@ metal-currency
        "銀兩" "銀倆" "孔方兄" "元寶" "文銀")
       "交子" 
       "新元" "分幣" "外幣"
       (currency-maybe-unit
        "美金" "歐元" "港幣"
        (currency-unit
         "美元" "日元" "加元" "英鎊" "港元" "澳元" "澳幣"
         "馬克" "克朗" "瑞尔" "埃居" "盾" "比索" "新幣"
         "第纳尔" "先令")
        )
       "現金"
       "人民幣" "日幣" "加幣" "台幣" "新台幣"
       "盧布" "里拉" "印尼盾" "法郎" "盧比")
      ((transaction can-be-job)
       ((transaction-ind ind) "貿")
       (import-export "進口" "出口" "入口" "進出口" "出入口")
       "外經貿" "經貿" "交投" "科工貿" "邊貿" "轉讓" "租賃"
       "交易" "買賣" "生意" "投资" "貿易" "商貿" "外貿"))
     (emotion
      (emotion-hatred "恨")
      "喜怒哀樂" "悲" "傷悲" "愁" "鄉愁" "憤" "愛憎" "怒火"
      "情緒" "笑" "微笑" "冷笑" "喜" "怒" "哀" "驚" "恐"
      "愛" "寵愛"
      "喜悅" "憤怒" "悲傷" "恐懼" "厭惡" "驚奇" "窘迫" "內疚" "害羞"
      "驕傲" "嫉妒" "慚愧" "羞恥" "自豪" "驚喜" "期待" "傷心"
      "恐慌" "恥" "辱" "奇恥" "大辱" "恥辱" "屈辱" "牢騷")
     (details "細節")
     (situation
      ((situation-ind ind) "況" "態")
      ((situation-or-place place scenery) "境")
      ((situation-details details) "詳情")
      "情況" "輿情" "内情" "境況" "市況" "景況"
      "概況" "狀況" "現況" "場合" "處境" "家境" "心境"
      "化境" "情境" "意境" "止境" "環境" "窘境" "絕境"
      "苦境" "語境" "情形" "情景" "榮景" "地步"
      "事態" "動態" "勢態" "狀態" "現狀" "節骨眼"
      "腥風血雨" "表狀" "場面"
      (politics-state "政局")
      "敗局" "結局" "牌局" "殘局" "格局"
      "时局" "開局" "平局" "布局" "定局" "局面"
      "大局" "困局" "變局" "危局" "全局" "僵局"
      )
     (feeling-or-sense
      (reaction "反應")
      ((feeling-or-sense-or-situation situation)
       "群情"
       ((feeling-or-sense-or-situation-ind ind) "情"))
      "感情" "幽情" "思古之幽情" "誼" "厚誼" "情誼"
      "直覺"
      ((feeling-or-sense-ind ind) "感"))
     "風情" "行情"
     (order-of "次序" "秩序" "顺序")
     (process ;; can be quantified with 道
      "程序" "禮節" "儀式" "手續" "進程" "過程"
      "規程" "章程" "流程" "日程"
      "历程" "制程" "禮儀" "後續" "行動" "經過"
      "手術"
      "工程"
      (trip
       ;; '旅' could mean a trip or a unit of army
       ((trip-ind ind) "旅")
       "行程" "旅途" "旅程")
      ((process-or-order-or-preface preface)
       ;; 序 also means an order of things
       ((process-or-order-or-preface-ind ind) "序"))
      (process-or-range
       ;; 程 could mean process as in 过程, or range as in 射程
       ((process-or-range-ind ind) "程"))
      (steps ;; can be quantified with 步
       (word-steps-bu4 "步")
       (steps-word-cuo4-shi1
        "措施" "舉措")
       "腳步" "台步" "正着" "臨門一腳"
       "工序" "步驟"))
     (operation
      "操作" "運作"
      )
     (concern-understanding
      ((concern-understanding-ind ind) "憂")
      (knowledge-or-understanding-of
       ((knowledge-or-understanding-of-ind ind) "識")
       "共識" "了解" "知識" "常識" "見識" "識見" "學識")
      (word-you1-lu4 "憂慮")
      "考慮" "擔憂" "焦慮"
      "顧慮" "隱憂" "內憂" "憂患" "後顧之憂" "疑慮")
     (@ event
      ((event-ind ind) "事件")
      (abstract-matter
       ;; 事 as suffix could sometimes mean ability (本事),
       ;; but these are treated individually elsewhere
       ((abstract-matter-ind ind) "事" "務")
       "家務" "事務" "礦務" "正務" "事情" "事宜"
       "外務" "公務" "商務" "勤務" "法務" "內務"
       "軼事" "私事" "正事" "易事" "時事" "政事"
       "咄咄怪事" "心事" "幸事" "差事" "實事"
       "好人好事" "啟事" "後事" "身後事" "事事"
       "樂事" "世事" "萬事" "婚事" "親事" "往事"
       "雞毛蒜皮" "重中之重" "前車之鑑" "殷鑑"
       "喪事" "事故"
       )
      (big-change-event
       ((big-change-event-ind ind) "變")
       "遽變" "裂變"
       "政變" "事變" "兵變" "天變" "革命" "文化大革命")
      "車禍" "饑饉" "陰謀" "擴張" "民運" "大躍進"
      "勝利" "虛驚" "遊行" "操演" "演練"
      "亂局" "風波" "軒然大波" "內亂" "叛亂" "大亂"
      "文革" "塞机" "海事" "塞車" "暴動" "動亂" "示威"
      (@ argument "紛爭" "爭端" "衝突" "訴訟" "扞格" "杆格" "磨擦"
                "爭歧" "歧" "分歧" "事端" "糾紛" "訌"
                "公訴" "訟訴" "爭吵" "爭拗" "爭執" "爭論")
      (@ disaster "浩劫" "劫" "意外" "爆炸")
      (legal-case
       ;; 案 could also indicate a proposal, handled elsewhere
       "官司" "冤案" "謀殺案" "案件" "抗告"
       "購案" "窝案" "兴票案" "疑案" "懸案" "谍嫌案"
       "大案" "命案" "劫案" "凶案" "定案" "個案")
      (forced-labor "徭役" "兵役")
      (battle ;; also war
       ((battle-ind ind) "戰" "仗" "鬥")
       ((battle-or-forced-labor forced-labor)
        ((battle-or-forced-labor-ind ind) "役"))
       "戰爭" "決戰" "混戰" "大戰" "作戰"
       "格鬥" "決鬥" "爭鬥" "鬥爭" "博戰" "內戰"
       "窩裡鬥" "打鬥" "對決" "對戰" "決鬥" "搏鬥"
       "点穴战" "二戰" "一戰" "戰役" "戰火"
       "陸戰" "特戰" "地道戰" "車輪戰" "劳资战"
       "戰鬥" "戰局" "困獸之鬥" "困獸鬥"
       "選戰" "戰事" "爭霸" "爭霸戰" "拉鋸" "拉鋸戰"
       "角力" "攻堅戰" "爭奪" "爭奪戰" "攻守戰" "攻防")
      (@ game-like
       (gamble "賭局" "賭博" "搏弈")
       (@ competition ;; can be quantified with 局
        ((competition-ind ind) "賽")
        "預選賽" "預賽"
        "比賽" "賽事" "決賽" "棋局" "奥赛" "世乒"
        "友誼賽" "初賽" "角逐" "競舉" "競賽"
        "競技" "拍賣" "競拍")
       "躲貓貓" "捉迷藏" "九連環" "遊戲"
       )
      (examination
       ((examination-ind ind) "試" "考")
       "會考" "高考" "考試" "科舉" "答辯" "公考"
       "測驗" "小測" "考核")
      (@ arranged-event
       ((arranged-event-ind ind)
        "展" "典" "慶典" "慶" "祭" "祭祀")
       ((arranged-event-or-sports sports) "運動")
       "公投" "選舉" "競選"
       ;; 制作新木鼓时，首先需要从森林中拉回一段大树干，称之“拉木鼓”。
       ;; 拉木鼓是佤族人民生活中的一件大事.
       "拉木鼓"
       ;;
       (word-huo2-dong4"活動")
       "會慶" "台慶" "彌撒" "匯演" "牙祭" 
       "聚會" "展覽會" "夏令營" "宴席"  "展覽" "婚禮" "學運"
       "春晚" "晚會" "祭典" "典禮" "法會" "茶會" "記者會"
       "演出" "博覽" "盛會" "聯歡" "年會" "簡布會"
       "嘉年華" "九冬会" "慶祝" "展演" "巡遊" "集會"
       "派對" "聆訊" "審訊" "大拷問" "聯誼" "喜慶"
       "演習" "軍演" "拍賣" "大選" "談判"
       (performance-show "表演")
       (discussion-meeting
        "會議" "討論" "大會" "高峰會" "峰會" "全会" "例會")
       (training ;; 訓 also means regulations, so in law
        ;; '訓練' and 'training' could mean physical training, or the intellectual background of a person.
        "訓練" "集訓" "鍛煉")
       (promotion "宣傳" "宣傳會" "文宣" "秀" "推廣")
       ((audible-arranged-event audible)
        "音樂會" "演唱會" "公聽會" "講座" "辯論"
        "演說" "演講" "演奏"
        "講演" "座談" "座談會" "訪談" "研討" "研討會" "對談"
        "爭辯" "會談" "議壇" "講壇" "論壇")
       (@ meal-like-event
        ((meal-like-event-ind ind) "宴")
        "宴會" "喜宴" "酒宴" "飯局")
       ;; these are mostly short-hands
       "十运会" "冬运会" "全运会" "九运会"
       ((arranged-event-name nr-name)
        "大运会" "亚运会" "世博会" "中博会"
        "残奥" "奥运" "奥运会"
        ((arranged-event-compound compound) "奥会")
        )
       ;;
       ))
     ;; TODO: these are not arranged events, but may be organizations, or other abstract concepts
     ;; TODO: Not sure how to distinguish arranged events and organizations, both of which could end with "會"
     (@ organization
      ((organization-or-event arranged-event)
       ((organization-or-event-ind ind) "會"))
      ;;
      (@ place
         ;; TODO: consider recording the original place or noun and the dir
         (place-with-dir)
         (platform))
      ((organization-or-platform platform)
       ((organization-or-platform-ind ind) "台")
       "无线台" "平台" "巴台" "電台"
       ;; 台 is also shorthand of Taiwan, elsewhere
       )
      (@ company
         (@ company-name
          ;; some well known companies, so they sometimes appear in
          ;; text.

          ;; extracted mostly from the 2017 list at
          ;; https://zh.wikipedia.org/zh-hk/%E8%B4%A2%E5%AF%8C%E4%B8%96%E7%95%8C500%E5%BC%BA

          ;; But some names deemed too general, or confusing, or
          ;; already a name elsewhere, or is easily composed of base
          ;; nouns, are commented out. For some, a subpart of its name
          ;; is extracted.
          "沃爾瑪" "WALMART"
          "國家電網公司" "STATE GRID" ;;國家電網公司（STATE GRID)  	
          "中國石油化工" "SINOPEC" ;;中國石油化工集團( SINOPEC GROUP)
          "CHINA NATIONAL PETROLEUM" ;;中國石油天然氣集團（CHINA NATIONAL PETROLEUM)  	
          "TOYOTA" "豐田汽車" "TOYOTA MOTOR"
          "大眾集團" "VOLKSWAGEN" ;;大眾集團（VOLKSWAGEN GROUP)  	
          "蜆殼公司" "ROYAL DUTCH SHELL" ;;蜆殼公司（ROYAL DUTCH SHELL) 	
          "波克夏·海瑟威" "BERKSHIRE" "HATHAWAY"
          "蘋果公司" ;;蘋果公司（APPLE）  	
          "埃克森美孚" "EXXON"
          "麥克森" "MCKESSON"
          ;;英國石油公司（BP）  	
          "聯合健康保險" "UNITEDHEALTH GROUP"
          "CVS Health" ;;CVS Health公司（CVS Health) 	
          "SAMSUNG" "三星電子" "三星"
          "嘉能可" "GLENCORE"
          "戴姆勒" "DAIMLER"
          "通用" "通用汽車" "GENERAL MOTORS" ;;通用汽車 GENERAL MOTORS
          "AT&T" ;;美國電話電報公司 AT&T
          "EXOR" ;;EXOR集團 EXOR GROUP
          "福特" "福特汽車" "FORD" "FORD MOTOR" ;;福特汽車 FORD MOTOR

          "美源伯根" "AMERISOURCEBERGEN" ;;美源伯根公司 AMERISOURCEBERGEN
          "中國建築工程總公司" "CHINA STATE CONSTRUCTION ENGINEERING"
          "安盛" "AXA"
          ;;亞馬遜 AMAZON.COM
          "鴻海" "鴻海科技" "HON HAI PRECISION INDUSTRY" ;;鴻海科技集團 HON HAI PRECISION INDUSTRY

          "HONDA" "HONDA MOTOR" "本田汽車"
          "道達爾" ;;TOTAL
          "通用電氣" "GENERAL ELECTRIC"
          "VERIZON" "威訊" "VERIZON COMMUNICATIONS"
          ;;日本郵政 JAPAN POSTHOLDINGS
          "安聯" "安聯保險集團" "ALLIANZ" ;;安聯保險集團 ALLIANZ
          "康德樂" "CARDINAL HEALTH"
          "好市多" "COSTCO" "COSTCO WHOLESALE"
          "沃博聯" "Walgreens" "Walgreens Boots Alliance" "沃爾格林" 	

          "中國平安" "PING AN INSURANCE"
          "克羅格" "KROGER"
          "上汽" "SAIC MOTOR" "上汽集團"

          "BNP PARIBAS" ;;法國巴黎銀行 BNP PARIBAS
          "NISSAN" "NISSAN MOTOR" ;;日產汽車 NISSAN MOTOR
          "雪佛龍" "CHEVRON"
          "房利美" "FANNIE MAE"
          "中國移動" "CHINA MOBILE" "CHINA MOBILE COMMUNICATIONS"
          "摩根" "摩根大通" "JPMORGAN CHASE & CO."
          "法通" "法通保險" "LEGAL & GENERAL GROUP"
          "日本電信電話" "NIPPON TELEGRAPH & TELEPHONE" ;;日本電信電話 NIPPON TELEGRAPH & TELEPHONE

          "中國人壽" "中國人壽保險" "CHINA LIFE INSURANCE"
          "寶馬集團" "BMW Group"
          "快捷藥方控股" "EXPRESS SCRIPTS HOLDING" ;;美國快捷藥方控股公司 EXPRESS SCRIPTS HOLDING
          "托克" "Trafigura" "托克集團" "Trafigura Group"
          "中國中鐵" "China Railway Engineering"
          "保誠" "英國保誠" "PRUDENTIAL"
          "忠利" "ASSICURAZIONI GENERALI" "忠利保險"
          "中國鐵建" "CHINA RAILWAY CONSTRUCTION"
          "家得寶" "HOME DEPOT"
          "波音" "BOEING"

          "GAZPROM" ;; 	俄羅斯天然氣工業股份公司 GAZPROM
          "NESTLÉ" 	;;雀巢 NESTLÉ
          ;; 	Alphabet Alphabet
          "西門子" "SIEMENS"
          "家樂福" "CARREFOUR"
          "東風集團" "DONGFENG" "DONGFENG MOTOR" "DONGFENG MOTOR GROUP"
          "微軟" "MICROSOFT"
          "偉彭醫療網" "Anthem"
          "日立" "HITACHI"
          "軟銀" "軟銀集團" "SoftBank Group"
          "桑坦德" "桑坦德銀行" "BANCO SANTANDER"
          "花旗" "花旗集團" "CITIGROUP"
          "PETROBRAS" ;;巴西石油 PETROBRAS
          "勞勃·博世" "ROBERT BOSCH" ;;勞勃·博世公司 ROBERT BOSCH
          "DEUTSCHE TELEKOM" ;;德國電信 DEUTSCHE TELEKOM
          "HYUNDAI MOTOR" ;;現代汽車 HYUNDAI MOTOR
          "康卡斯特" "COMCAST"

          "國際商業機器" "IBM" ;;國際商業機器公司 IBM
          "ÉLECTRICITÉ DE FRANCE" ;;法國電力公司 ÉLECTRICITÉ DE FRANCE
          "華為" "HUAWEI" "HUAWEI INVESTMENT & HOLDING"
          "ENEL" ;;意大利國家電力公司 ENEL
          ;;州立農業保險公司 STATE FARM INSURANCE COS.
          "華潤" "華潤集團" "CHINA RESOURCES NATIONAL"
          "永旺" "永旺集團" "AEON"

          "太平洋建設集團" "PACIFIC CONSTRUCTION GROUP"
          "英傑華" "英傑華集團" "AVIVA"
          "UNIPER" ;;Uniper公司 UNIPER
          "樂購" "TESCO"
          "蘇伊士" "蘇伊士集團" "Engie" ;;法國天然氣蘇伊士集團 Engie
          "空中客車集團" "Airbus Group"
          "SK集團" "SK HOLDINGS"
          "Phillips" 	
          "強生" "JOHNSON & JOHNSON"
          "寶潔" "PROCTER & GAMBLE"
          ;;美國郵政署 U.S. POSTAL SERVICE
          "中國南方電網" "CHINA SOUTHERN POWER GRID"

          ;;中國兵器裝備集團 CHINA SOUTH INDUSTRIES GROUP
          "盧克石油" "LUKOIL"
          ;;中國交通建設 CHINA COMMUNICATIONS CONSTRUCTION

          "索尼" "Sony"
          "瓦萊羅" "瓦萊羅能源" "VALERO ENERGY"
          "塔吉特" "TARGET"

          "慕尼黑再保險" "MUNICH RE GROUP"
          "松下" "Panasonic"
          "NIPPON LIFE INSURANCE" ;;日本生命保險 NIPPON LIFE INSURANCE
          "蘇黎世金融服務集團" "ZURICH INSURANCE GROUP"

          "中國人民保險集團" "People's Insurance Co. of China"
          "中國海洋石油總公司" "CHINA NATIONAL OFFSHORE OIL"
          "丸紅" "MARUBENI"
          ;;德國郵政 DEUTSCHE POST
          "房地美" "FREDDIE MAC"
          ;;中國郵政 CHINA POST GROUP
          "中國五礦" "CHINA MINMETALS"

          "美國勞氏" "LOWE'S"
          "麥德龍" "METRO"
          "DELL" "戴爾" "DELL TECHNOLOGIES"
          "CHINA FAW GROUP" ;;中國第一汽車集團 CHINA FAW GROUP
          "巴斯夫" "BASF"
          "JXTG" "JXTG控股" "JXTG HOLDINGS"
          "大都會人壽保險" "METLIFE"
          "天津物產集團" "TEWOO GROUP"
          "安泰保險" "Aetna"
          "百事公司" "PEPSICO"
          "埃尼" "埃尼石油" "ENI" ;;埃尼石油公司 ENI
          "中國電信集團" "CHINA TELECOMMUNICATIONS"
          "阿徹丹" "尼爾斯" "米德蘭" "ARCHER DANIELS MIDLAND" ;;美國阿徹丹尼爾斯米德蘭公司 ARCHER DANIELS MIDLAND
          ;;中國兵器工業集團 CHINA NORTH INDUSTRIES GROUP
          "中糧集團" "COFCO"
          "北汽集團" "BEIJING AUTOMOTIVE GROUP"
          "聯合包裹" "UPS"
          "安邦保險" "ANBANG INSURANCE GROUP"
          "標緻雪鐵龍" "PEUGEOT" "標緻雪鐵龍集團"
          "艾伯森" "ALBERTSONS COS."
          "DAI-ICHI LIFE HOLDINGS" ;;第一生命保險 DAI-ICHI LIFE HOLDINGS
          "中化集團" "SINOCHEM" "SINOCHEM GROUP"
          "英特爾" "INTEL"
          "三菱" "三菱商事" "MITSUBISHI"
          "歐尚" "AUCHAN HOLDING"
          "AEGON" "全球保險集團"
          "保德信" "PRUDENTIAL FINANCIAL" ;;保德信金融集團 PRUDENTIAL FINANCIAL
          "沃達豐" "VODAFONE" "VODAFONE GROUP"
          "聯合利華" "UNILEVER"

          "PEMEX" ;;墨西哥石油公司 PEMEX
          "TELEFÓNICA" ;;西班牙電信 TELEFÓNICA

          ;;聯合技術公司 UNITED TECHNOLOGIES
          "安賽樂米塔爾" "ARCELORMITTAL"
          "雷諾" "RENAULT"
          "ROSNEFT OIL" ;;俄羅斯石油  ROSNEFT OIL
          "魏橋創業" "SHANDONG WEIQIAO PIONEERING GROUP"
          ;;馬拉松石油 MARATHON PETROLEUM

          "中國航空工業集團" "AVIATION INDUSTRY CORP. OF CHINA"
          "荷蘭國際集團" "ING"
          "三菱日聯金融集團" "MITSUBISHI UFJ FINANCIAL GROUP"
          "皇家阿霍德德爾海茲" "皇家阿霍德德爾海茲集團" "ROYAL AHOLD DELHAIZE"
          "哈門那" "HUMANA"
          "7&I控股" "SEVEN & I HOLDINGS"
          ;;印度石油公司 INDIAN OIL
          "ROCHE GROUP" ;;羅氏 ROCHE GROUP
          "海航集團" "HNA GROUP"

          "中信" "中國中信集團" "CITIC GROUP"
          "輝瑞" "PFIZER"
          "拜耳" "BAYER"
          "美國國際集團" "AIG"
          "AMÉRICA MÓVIL" ;;美洲電信 AMÉRICA MÓVIL
          ;;韓國電力公社 KOREA ELECTRIC POWER
          "洛歇·馬丁" "LOCKHEED MARTIN"
          "西斯科" "SYSCO" ;;西斯科公司 SYSCO
          "聯邦快遞" "FEDEX"
          "慧與科技" "HEWLETT PACKARD ENTERPRISE"
          "路易達孚" "LOUIS DREYFUS"
          "正威集團" "AMER INTERNATIONAL GROUP"
          "PETRONAS" ;;馬來西亞國家石油公司 PETRONAS
          ;;東京電力 TOKYO ELECTRIC POWER
          "諾華公司" "NOVARTIS"
          "思科" "CISCO" "思科系統" "CISCO SYSTEMS"
          "三井住友海上集團" "MS&AD INSURANCE GROUP HOLDINGS"

          "中國電建" "POWERCHINA"
          "JBS股份" "JBS"
          ;;泰國國家石油股份 PTT
          "東京海上控股" "TOKIO MARINE HOLDINGS"
          "惠普" "HP"
          "萊茵集團" "RWE"
          "DOW CHEMICAL" ;;陶氏化學 DOW CHEMICAL
          "Finatis" ;;法國Finatis 	
          "西農集團" "WESFARMERS"
          "SINOPHARM" "國藥控股" "中國醫藥集團"
          "CNP ASSURANCES" ;;法國國家人壽保險公司 CNP ASSURANCES

          "LG" "LG電子" "LG ELECTRONICS"
          "三井住友金融集團" "SUMITOMO MITSUI FINANCIAL GROUP"
          "信實工業" "RELIANCE INDUSTRIES"
          "寶武鋼鐵集團" "BAOSTEEL GROUP" ;;中國寶武鋼鐵集團有限公司 BAOSTEEL GROUP
          "來寶集團" "NOBLE GROUP"
          "安海斯" "布希英博" "ANHEUSER-BUSCH INBEV" ;;安海斯-布希英博集團 ANHEUSER-BUSCH INBEV
          ;;挪威國家石油公司 STATOIL
          "浦項鋼鐵" "POSCO"
          "起亞汽車" "KIA MOTORS"
          ;;Orange
          "中國化工" "CHEMCHINA" "中國化工集團"
          "DEUTSCHE BAHN" ;;德國聯邦鐵路公司 DEUTSCHE BAHN
          ;;大陸集團 CONTINENTAL
          "HCA HOLDINGS" "美國醫院有限公司"
          "伊藤忠商事" "ITOCHU"

          ;;印度國家銀行 STATE BANK OF INDIA
          "沃爾沃斯" "WOOLWORTHS"
          "KDDI" "KDDI電信" "日本KDDI電信"
          "瑞士再保險" "SWISS RE"
          "河鋼集團" "HBIS GROUP"
          "華信" "華信能源" "CEFC CHINA ENERGY"
          "BANCO BILBAO VIZCAYA ARGENTARIA" ;;西班牙對外銀行 BANCO BILBAO VIZCAYA ARGENTARIA
          "蒂森克虜伯" "THYSSENKRUPP"
          "聖戈班" "SAINT-GOBAIN"
          "LENOVO" "聯想集團" "LENOVO GROUP"
          "萬喜集團" "VINCI"
          "新日鐵住金" "NIPPON STEEL & SUMITOMO METAL"
          "邦吉" "Bunge" "Bunge Limited" ;;邦吉公司 Bunge Limited

          "意昂集團" "E.ON"
          "SBERBANK" ;;俄羅斯聯邦儲蓄銀行 SBERBANK
          "中國船舶重工" "中國船舶重工集團" "CHINA SHIPBUILDING INDUSTRY"
          "克里斯汀·迪奧" "CHRISTIAN DIOR"
          "可口可樂公司"
          "DENSO" ;;電裝 DENSO
          "富士通" "FUJITSU"
          "廣汽集團" "GUANGZHOU AUTOMOBILE INDUSTRY GROUP"
          "豐益" "豐益國際" "WILMAR INTERNATIONAL"
          "賽諾菲" "SANOFI"
          "聯通" "中國聯通" "CHINA UNITED NETWORK COMMUNICATIONS"
          "住友生命保險" "SUMITOMO LIFE INSURANCE"
          "紐約人壽" "NEW YORK LIFE INSURANCE"
          "Centene" "Centene Corp" "Centene公司"

          "韓華" "韓華集團" "HANWHA"
          "塔塔汽車" "TATA MOTORS"
          "ALUMINUM CORP. OF CHINA" ;;中國鋁業 ALUMINUM CORP. OF CHINA
          "三井物產" "MITSUI"
          "宏利" "MANULIFE" "宏利金融" "MANULIFE FINANCIAL"

          "中國太平洋保險" "CHINA PACIFIC INSURANCE GROUP"

          "美國航空集團" "AMERICAN AIRLINES GROUP"
          "美國全國保險公司" "NATIONWIDE"
          "默克" "默克藥廠" "MERCK"
          "信諾" "CIGNA"
          "達美" "DELTA" "達美航空" "DELTA AIR LINES"
          "百思買" "BEST BUY"
          "中國建築材料集團" "CHINA NATIONAL BUILDING MATERIAL GROUP"
          "霍尼韋爾" "HONEYWELL INTERNATIONAL"
          "京東商城" "JD.COM"
          "三菱電機" "MITSUBISHI ELECTRIC"
          "采埃孚" "ZF FRIEDRICHSHAFEN"
          "卡特彼勒" "CATERPILLAR"
          "利寶" "利寶互助保險集團" "LIBERTY MUTUAL INSURANCE"
          "鮑爾集團" "POWER CORP. OF CANADA" ;;加拿大鮑爾集團 POWER CORP. OF CANADA
          "摩根士丹利" "MORGAN STANLEY"
          "恆力集團" "HENGLI GROUP"
          ;;南蘇格蘭電力 SSE
          "萬通" "美國萬通金融集團" "MASSACHUSETTS MUTUAL LIFE INSURANCE"
          "高盛" "GOLDMAN SACHS" "GOLDMAN SACHS GROUP"
          "江森" "江森自控" "JOHNSON CONTROLS INTERNATIONAL"
          "葛蘭素史克" "GLAXOSMITHKLINE"
          "華能" "中國華能" "CHINA HUANENG GROUP" "中國華能集團" "中國華能集團公司"
          ;;ENERGY TRANSFER EQUITY 	
          "神華" "神華集團" "SHENHUA GROUP"
          "綠地控股" "綠地控股集團" "GREENLAND HOLDING GROUP"
          ;;美國教師退休基金會 TIAA
          "怡和" "怡和洋行" "JARDINE MATHESON"
          "甲骨文公司" "ORACLE"
          "ACS集團" "ACS"
          "住友商事" "SUMITOMO"
          "泰森食品" "TYSON FOODS" "泰森食品公司"
          "巴克萊" "巴克萊銀行" "BARCLAYS"
          ;;意大利郵政 POSTE ITALIANE
          "森特理克" "CENTRICA"
          "美國聯合大陸控股聯合航空" "UNITED CONTINENTAL HOLDINGS"
          "好事達" "ALLSTATE"
          "PERTAMINA" ;;印尼國家石油公司 PERTAMINA
          "麥格納" "MAGNA INTERNATIONAL"
          "瑞銀" "UBS" "瑞銀集團"

          "喬治威斯頓公司" "GEORGE WESTON" ;;加拿大喬治威斯頓公司 GEORGE WESTON
          "三菱重工" "三菱重工業" "MITSUBISHI HEAVY INDUSTRIES"
          "拉加什出口有限公司" "RAJESH EXPORTS"
          "和碩" "和碩聯合科技" "PEGATRON"
          "明治安田生命保険" "MEIJI YASUDA LIFE INSURANCE"
          "馬士基" "MAERSK GROUP"
          "沙基工業" "沙基工業股份" "SABIC"
          "布依格" "法國布依格集團" "BOUYGUES"

          "VOLVO" ;;富豪集團 VOLVO
          "Talanx" 	
          "漢莎" "漢莎航空" "LUFTHANSA GROUP"

          "埃森哲" "ACCENTURE"
          "REPSOL" ;;西班牙國家石油公司 REPSOL
          "萬科" "CHINA VANKE"
          "PUBLIX SUPER MARKETS" "大眾超級市場公司"
          "艾德卡公司" "EDEKA ZENTRALE"
          "森寶利" "J.SAINSBURY"
          "ALIMENTATION COUCHE-TARD"
          "中國能建" "CHINA ENERGY ENGINEERING GROUP"
          "HYUNDAI HEAVY INDUSTRIES" ;;現代重工業 HYUNDAI HEAVY INDUSTRIES
          "ABB"
          "美國運通" "AMERICAN EXPRESS"
          "力拓集團" "RIO TINTO GROUP"
          "SNCF MOBILITÉS" ;;法國國家鐵路 SNCF MOBILITÉS
          "中國中車" "CRRC"
          "和記" "和記實業" "長江和記實業" "CK HUTCHISON HOLDINGS"
          "冀中能源" "冀中能源集團" "JIZHONG ENERGY GROUP"
          "TJX公司" "TJX"
          "新興際華" "XINXING CATHAY INTERNATIONAL GROUP" "新興際華集團"
          "摩比斯" "現代摩比斯" "HYUNDAI MOBIS"
          "愛信精機" "AISIN SEIKI"
          "來愛德" "RITE AID"
          "延長石油集團" "SHAANXI YANCHANG PETROLEUM GROUP"

          "中國光大集團" "CHINA EVERBRIGHT GROUP"
          "大和房屋工業" "DAIWA HOUSE INDUSTRY"
          "耐吉" "NIKE"
          "伊比德羅拉" "IBERDROLA"
          "COMMONWEALTH BANK OF AUSTRALIA" "澳洲聯邦銀行"
          "國機集團" "SINOMACH"
          "費森尤斯" "FRESENIUS"
          "中國航天科技集團" "CHINA AEROSPACE SCIENCE & TECHNOLOGY"
          "煤業化工" "SHAANXI COAL & CHEMICAL INDUSTRY" ;;陝西煤業化工 SHAANXI COAL & CHEMICAL INDUSTRY
          "中國恆大集團" "CHINA EVERGRANDE GROUP"
          "江西銅業股份" "JIANGXI COPPER"
          "損保日本興亞控股" "SOMPO HOLDINGS"
          "保利集團" "CHINA POLY GROUP"
          "安達" "安達集團股份" "CHUBB"
          "吉利汽車" "ZHEJIANG GEELY HOLDING GROUP"
          "愛克斯龍" "愛克斯龍電力" "EXELON"
          "通用動力" "GENERAL DYNAMICS"
          ;;英國電信 BT GROUP
          "佳能" "CANON"
          "物產中大集團" "WUCHAN ZHONGDA GROUP"
          "三菱化學控股" "MITSUBISHI CHEMICAL HOLDINGS"
          "必和必拓" "BHP BILLITON"

          ;; some are bank, but usually used for company name only
          "滙豐銀行控股" "HSBC HOLDINGS" "滙控" "滙豐控股"
          "ITAÚ UNIBANCO HOLDING"
          "UNICREDIT GROUP"
          "LLOYDS BANKING GROUP" "勞埃德銀行集團"
          "GROUPE BPCE" ;;法國BPCE銀行集團 GROUPE BPCE 法國外貿銀行

          ;;
          "google" "谷歌" "雅培"
          ;; some more in Hong Kong, some names are listed in
          ;; company-as-place
          )
         "株式會社" "會社" "银金社" "旅社" "集團" "控股"
         )
      "總承包"
      ;;
      ;; 局 could mean an organization such as 郵局 (post office)
      ;; or could mean state of affair. Currently the ind is used
      ;; for the organization meaning.
      ((word-organization-ju2 ind) "局")
      ((organization-ind ind)
       "議會" "部会" "委員會" "協會" "交会" "司"
       "部" "署" "公署" "組" "社" "社團" "組織" "邦"
       "部門" "衙" "衙門" "盟" "幫" "辦" "同盟" "聯盟"
       "公社"
       ;; 團 could mean an army unit, or simply a group of people,
       ;; it could also be a measure word to describe a particular shape
       "團"
       )
      ((organization-or-poem-like poem-like)
       ;; 聯 could be shorthand for 對聯
       "聯")
      (organization-or-place-chu4
       ;; 處 could mean an organization similar to 署, but could also be
       ;; a general reference to a place, i.e. somewhere, and could also be
       ;; an abstract place
       ((organization-or-place-chu4-ind ind) "處")
       )
      ((knowledge-domain-or-organization knowledge-domain)
       ((knowledge-domain-or-organization-or-type
         abstract-type-of)
        ((knowledge-domain-or-organization-or-type-ind ind)
         "系" "科")
        "派系" "科系")
       )
      ;; many of these are short-hands
      "國務院" "眾議院" "下議院" "中科院" "中研院"
      "內閣" "體協" "政協" "黑幫" "朝廷" "朝野"
      "济公团" "使团" "樂團" "炮团"
      "銀團" "財團" "陆航团" "工团" "陪審團"
      "计财处" "林管处" "企業" "企市" "院团"
      "中企" "国企" "外企" "民企" "私企" "车企" ;; short form
      "分局" "水資局" "外管局" "中情局" "總局"
      "當局" "国安局" "環保局"
      "學會" "勞顧會" "装部会" "中选会" "安理會"
      "退辅会" "国统会" "考纪会" "证管会" "銀監會"
      "政会" "總會" "央視" "世衛"
      (church-like-organization
       ((church-like-organization-compound compound) "主教會")
       "教會")
      "中常會" "總工會" "工會"
      "陆委会" "经委会" "组委会" "管委会" "筹委会"
      "混委会" "國會" "總商會"
      "商會" "工商會" "经合会" "季分会"
      "分會" "农会" "公會" "军停会" "消保会"
      "市代会" "宗親會" "昆交会" "广交会" "展交会"
      "监事会" "高法" "自衛隊" "防衛省" "防衛廳" "皇家"
      "俱樂部" "中聯部" "中組部" "農林部" "指揮部"
      "本部" "支部" "總部" "分部" "夜間部"
      "群體" "政體" "共同體" "聯合體" "機構"
      "NGO" "黨中央" "團契" "足總" "邦聯" "證鑑" "政府"
      "中共"
      (word-da4-bu4 "大部")
      ((organization-name nr-name)
       "塔利班" "聯合國" "紅十會" "海基会" "海协会"
       "天地会" "紅十字會" "狮子会" "乌洽会" "共濟會"
       "義和團" "教科文"
       "作协" ;; shorthand for 作家协会
       ;; names
       ((organization-name-newpaper-like newspaper-like)
        "中新社" "塔斯社" "光启社" "新华社"
        "博联社" "放索社" "新潮社" "中旅社" "法新社")
       )
      (governance-power "政權")
      "劇團" "基金會"
      )
     ;;
     "老鼠會" "司改" "卷包会" "體會"
     ;;
     (legal-sentence-or-punishment
      ((punishment-ind ind) "刑" "判")
      "仲裁"
      "判決" "言判" "嚴刑" "死緩" "判刑" "裁定" "裁決"
      "死刑" "緩刑" "徒刑" "宫刑" "本刑" "從刑" "殛刑")
     (sin-or-crime "罪" "罪惡")
     (@ product
      ;; "產品"
      "應用程式" "程式" "瀏覽器")
     (order
      ;; "命令"
      "指令" "命令" "條令" "號令" "口令"
      ((order-ind ind) "令"))
     (posed-question
      "題目" "難題" "考題" "命題" "思考題" "問題")
     (scenery "景象" "畫面" "夢境" "景" "風景" "景觀"
              "奇觀" "景像" "山水" "佈景" "景緻"
              (background "背景"))
     (@ long-thin ;; can be quantified with 根, 條
        (@ abstract-line
           (word-curve-line
            ;; could also mean hyperbolic, sarcastic
            "曲線")
           "趨勢線" "界線" "分界線" "航線" "熱線" "底線"
           "導火線" "日界线" "經線" "緯線" "航段")
        (long-thin-fiber "纖")
        "引芯")
     ;;
     "路子" "空子" "點子" "梁子" "根子" "花架子" "月子"
     "拍子" "擔子" "棄子" "幌子" "婁子" "因子" "話匣"
     (example
      ((example-ind ind) "例")
      "個例" "前例" "病例" "範例")
     (system "系統")
     "天文" "陰" "陽"
     "階級" "差異" "節奏" "趣味" "主體"
     "社交"
     ;; those words ending with 力 could mean ability to do something,
     ;; or force due to something, or other abstract attributes
     (@ attribute
        ;; to prevent ambiguity
        "代表性" "血性" "自性" "秉性" "理性" "特性"
        "靈性" "水性" "母性" "樣性" "感性"
        "心性" "屬性" "定性" "塑性"
        "習性" "知性"
        (word-xing4 "性")
        (perspective "角度")
        (word-growth "成長")
      ((attribute-compound compound) "受力")
      "效力" "威力" "助力" "公信力"
      "主力" "力道"
      (ability
       "能力"
       (strength-or-ability
        ((strength-or-ability-ind ind) "力")
        "物力" "经济力" "重力" "运力" "行力" "警力"
        "政治力" "向心力" "張力" "应力" "外力"
        "源動力" "原動力" "核動力" "勞動力"
        ))
      )
     (word-part-of "部分" "部份")
     (capability-of
      "職能" "產能" "體能" "功能" "動能" "性能" "才能"
      "潛能" "機能" "運能" "道行" "造詣" "神通" "本領")
     (energy
      "核能" "電能" "耗能" "太陽能" "能源" "能量"
      (word-electricity "電")
      ((capability-or-energy capability-of)
       ((capability-or-energy-ind ind) "能"))
      )
     ;;
     (regularity ;; as in physical law
      "定律" "規律"
      )
     (@ law ;; including regulations or discipline
      ((law-ind ind) "律法" "法例" "規例" "規" "紀" "誡")
      ((law-or-training training)
       ((law-or-training-ind ind) "訓"))
      ((law-or-regularity regularity)
       ((law-or-regularity-ind ind) "律"))
      ((law-can-be-job can-be-job) "法律")
      "紀要" "交規" "法規" "朝綱" "規定" "門禁" "指引"
      "家規" "規則" "政法" "峻法" "規章" "黨紀" "法紀"
      "选霸法" "选罢法" "王法" "母法" "子法" "基本法" "家法"
      "公安法" "公法" "工作法" "勞資法" "刑诉法" "憲法" "憲章"
      "社保法" "实体法" "潛規則" "規範" "規矩")
     (word-method
      (word-suan4-fa3 "算法")
      "演算法" "章法" "區田法" "用法" "活法" "手法"
      "工法" "土法" "叫法" "变法" "兵法" "辦法" "法子" "技法"
      "套袋农法" "手段" "途徑" "做法" "作法"
      (method-therapy "療法" "治法")
      ;;
      "良方" "秘方" "妙方"
      ;;
      (strategy
       ((strategy-ind ind) "策" "略" "謀")
       ((strategy-or-measuring-tool tool)
        ((strategy-or-measuring-tool-ind ind) "計"))
       (policy
        ;; this is really policy, only similar to strategy
        "政策")
       "戰略" "策略" "方略" "計謀" "謀略" "計策"
       "對策" "長久之計" "為今之計" "權宜之計"
       "百年大計" "心計" "方針" "步署" "部署")
      (magic-like
       (divination "乩" "扶箕" "扶乩")
       "魔法" "法術" "道術" "方術" "點金術" "術數"
       "五鬼搬運" "五鬼運財")
      ((method-ind ind) "方法")
      ((method-or-law law)
       ;; nouns ending with 法 could be a method, or a law, don't know how to distinguish for now
       "分法"
       ((method-or-law-ind ind) "法" "大法")
       ))
     ;;
     (amount-of
      ((amount-of-ind ind) "量" "總量" "額" "幅")
      "藏量" "吃稿量" "載客量" "定量" "劑量" "總額"
      "產量" "吞吐量" "毛额" "數額" "員額" "名額"
      "用電量"
      "份額" "分額"
      "總產" "幅度" "增幅" "篇幅" "分毫" "絲毫"
      (visibility "能見度")
      (likelihood-of "勝算")
      (distance-of
       ((distance-of-ind ind) "距")
       "距離" "焦距" "差距")
      (limit-of
       ((limit-of-ind ind) "限")
       "界限")
      (speed-of
       ((speed-of-ind ind) "速")
       "時速" "秒速" "亞音速" "音速")
      (consumption-of
       "能耗" "損耗" "內耗" "消耗"
       ((consumption-of-ind ind) "耗"))
      (number-of
       "開數" "基數" "參數" "多半數" "為數" "數量"
       "負數" "約數" "係數" "點數" "指數" "總數"
       "批次" "架次" "數字"
       "百分點" "百份點"
       (word-sum-he2 "和") "總和"
       ((number-of-ind ind) "數" "次數")
       )
      )
     ;;
     "紀年" "天年" "聯邦" "误区" "盲区"
     "裂度区" "主產"
     (explanation
      ((explanation-ind ind)
       ;; 解 also means solution to a problem
       "解")
      "正解"
      "說法" "講法" "注解" "解說" "解釋" "詮釋" "闡釋")
     (theory
      "陰陽家" "儒家" "道家" "法家" "墨家" "名家" "縱橫家"
      "雜家" "諸子百家" "百家" "道学" "儒門" "佛門"
      (theory-word-dao4
       ((theory-word-dao4-or-street street)"道")))
     (rate
      ((rate-ind ind) "率")
      (power-rate "功率")
      "頻率" "速率" "匯率" "比率" "市占率"
      "利率" "产值率" "费率" "零税率" "税率"
      )
     (proposal-like
      ((proposal-like-ind ind) "方案")
      "預案" "行銷案" "草案" "腹案" "提案"
      "法案" "教案" "議案"
      ;;
      ((legal-case-or-proposal legal-case)
       ((legal-case-or-proposal-ind ind) "案")
       "公案"
       ))
     ;;
     (degree-of
      ((degree-of-ind ind) "度" "程度" "幅度")
      "速度" "進度" "精度" "烈度" "尺度" "公信度"
      "強度" ;; confused with (verb-x (adv 強) (verb-a 度))
      "高度" ;; also a adj and adv
      (concentration-of "濃度")
      )
     (targeted-subject "對象")
     ;;
     "提法" "曆法" "司法" "刑法" "佛法" "國際"
     "海防" "國防" "區間" "憲" "實體"
     "霸權" "金权" "客運"
     "負荷" "變量"
     "有生力量" "慈善" "余地" "消防" "編隊" "外景"
     "儀仗" "時間點" "鐘點" "重點" "迷點" "論點"
     "缺點" "疑點" "焦點" "死点" "利基" "利基点"
     "基点" "賣點" "位點"
     "節點" "收視" "表率"
     "通脹" "通貨膨脹" "通漲" "通貨膨漲" "通縮"
     "效率" "工傷"
     "龍頭" "風頭" "陣頭" "鋒頭" "跟头"
     "詞頭" "講頭" "苗頭" "線頭" "矛頭"
     "看頭" "由頭" "熱頭"
     "搞頭" "戶頭" "念頭" "開頭" "寡头" "噱頭"
     "勁頭" "關頭" "卡拉ok"
     "民航"
     "工时"
     "原產" "邦交" "總長" "配化"
     "法治" "课业"
     "家業" "學業" "炊事" "局部"
     "環保"
     (word-tian2-tou "甜頭")
     (administration "行政")
     (drainage-related "水利" "給排水")
     (concept "概念")
     (quality
      ;; 質 could also mean the material, but it is a different sense of quality
      ((quality-ind ind) "質")
      "質素")
     (source
      (kinship-related-source "血源" "渊源")
      "根源" "源頭" "起源" "水源" "來源"
      ((source-ind ind) "源"))
     ;;
     (insurance ;; could be social insurance
      ((insurance-industry industry) "保險" "險")
      ((insurance-ind ind) "保" "保障")
      "醫保" "健保" "低保" "共保" "半保" "社保"
      "兩全險" "兩全保險"
      )
     "额度" "維度" 
     ;;
     (@ aspect
      ((aspect-word-mian4-ind ind) "面")
      ((aspect-ind ind) "方面" "層面" "疇")
      (topic
       ((topic-or-question posed-question) "題")
       "主題" "標題" "議題" "話題"
       )
      ;; "項" also means neck, so in body-part
      "方方面面" "正反面" "反面" "基本面" "範疇"
      "弱項" "強項" "要項"
      )
     "頁面" "版面" "封面" "世面"
     "門面" "赢面" "胜面" "海平面" "字面"
     "模範" "浮生" "民生" "国计" "天際" "陣線"
     "禮" "禮數" "典範"
     "營養"
     ;; 方, 道 could also mean method, here treated as special case individually
     "配方" "見方" "立方"
     (abstract-dao4 ;; can be used with 條
      "頻道" "門道" "邪道" "歪門邪道" "外門邪道"
      "管道" "王道" "渠道" "康莊大道" "声道")
     "歪門" "家道" "公道" "交道" "因應之道" "世道"
     ;;
     "比方"
     ;;
     (meaning
      ((meaning-ind ind) "涵義" "含義" "義含")
      "精義" "疑義" "意義" "定義" "意涵")
     (reason ;; also similar to 'factor'
      ((reason-ind ind) "因" "理由" "故" "由")
      "主因" "原因" "因由" "原由" "内因" "外因"
      "動因" "因素" "成因" "死因" "誘因" "敗因"
      "起因" "變故" "緣故" "緣由" "原故" "前因"
      "青紅皂白" "初衷" "苦衷" "邏輯")
     (ethics
      "道義" "正義" "忠義" "公義" "信義" "仁義"
      "仁" "品德" "道德" "私德" "美德" "明德" "師德"
      "三從四德" "人倫" "天倫" "情操" "良知" "天良"
      "行誼" "風化"
      ((ethics-ind ind) "倫")
      ((reason-or-ethics reason)
       "倫理" "歪理" "真諦"
       "道理" "法理" "定理" "天理" "哲理" "原理"
       ((reason-or-ethics-ind ind) "理" "諦"))
      ((meaning-or-ethics meaning)
       ((meaning-or-ethics-ind ind) "義")
       ))
     ;;
     "歪風邪氣" "節氣" "斜風歪氣" "惡氣" "買氣"
     "紋" "花紋"
     ;; abstract meaning of roads, sometimes means different ways
     "陸路" "門路" "漫漫長路" "邪路" "財路"
     "絕路" "電路" "生路" "活路" "死路"
     "歪路" "正路" "窮途" "末路" "窮途末路"
     "套路" "回頭路" "頭路" "回路" "出路"
     "必由之路" "一路"
     ;;
     (@ abstract-heart
      ;; 心 as suffix could also mean the middle part, but treated
      ;; individually elsewhere
      "重心" "核心"
      ;; some are animate-attributes, and are put elsewhere
      )
     (standard-of
      ((standard-of-ind ind) "準" "準則" "則")
      (word-standard "標準")
      "基準" "原則" "守則" "法則"
      "國標" ;; short hand
      )
     ;;
     "恥" "國恥" "國殤" "廉恥"
     "處處" "短處" "益處" "用處" "不足之處"
     "工夫" "的物" "風水" "苦水" "油水"
     "使命" "牛市" "熊市" "後市" "盹" "正理"
     "音標" "玩意" "勁兒" "低潮" "孕" "謎團"
     "謎" "军练" "榜首" "輿論" "靠山"
     "諮商" "咨商"
     (@ a-sign
      ((a-sign-ind ind) "標")
      "台標" "商標" "指標" "鼠標" "標記" "標誌"
      ;; 號 could mean a sort of id number, or a name, or a sign, or a day in month
      (@ sign-signal
       (musical-note "樂符" "音符" "休止符" "終止符")
       "眼色" "公示" "啟示"
       "訊號" "等號" "符號" "標號" "引號" "句號" "信號")
      )
     (reputation
      (evaluation-of
       ((evaluation-of-ind ind) "評")
       ((evaluation-can-be-job can-be-job) "影評")
       "批評" "評價" "評論" "評級"
       "環評" "社評" "測評" "品評" "點評" "好評" "劣評")
      "排名" "榮辱"
      "名聲" "英名" "功名" "聲名" "人望" "名望" "口碑"
      "榮" "譽" "榮譽" "信譽" "名譽" "商譽" "聲譽" "尊榮")
     (name-of
      ((name-of-ind ind) "稱")
      "學名" "化名" "名目" "名堂" "稱謂" "簡稱"
      "代稱" "稱呼" "名稱" "統稱" "職稱"
      "名字" "名兒" "姓名" "题名" "藝名" "筆名"
      (title-of
       ((title-of-ind ind) "銜")
       "職銜" "軍銜" "頭銜" "銜頭")
      ((name-or-reputation reputation)
       ((name-or-reputation-ind ind) "名"))
      (name-as-sign
       "綽號" "稱號" "旗號" "外號" "名號" "代號"
       "字号" "域名"))
     (id-number
      "號碼" "編號"
      "连号" "行号" "番号" "文号" "批号" "座号"
      ((name-as-number-or-sign sign-signal name-as-sign)
       ((name-as-number-or-sign-ind ind) "號")
       ))
     "人文" "大器" "凶"
     (pressure
      ;; could be physical or mental pressure
      (word-pressure-voltage "壓") ;; (physical and mental) pressure, voltage
      "水壓" "眼壓" "血壓")
     (lesson-class
      ((class-organization organization)
       ((class-organization-ind ind) "班"))
      "班級" "安亲班"
      (lesson "課" "課程" "设程"))
     (system-policy-mechanism-made
      ((system-policy-mechanism-made-ind ind) "制")
      "制度" "AA制" "机制" "形制" "建制" "宜制" "體制"
      "一國兩制" "法制" "立憲制"
      "所有制"
      )
     ;;
     (@ sports
      "田徑" "相撲" "陸跑" "力舉" "摔跤" "拳擊" "體操"
      "瑜珈" "跳遠" "跳高" "馬拉松" "賽跑"
      (swimming "泳" "冬泳" "渡海泳"))
     ;;
     (bath "澡" "涼" "浴" "三温暖" "桑拿" "芬蘭浴"
           "維琪浴" "維其浴")
     (price
      "等價" "天價" "特價" "底價" "差價" "估價" "定價"
      ((price-ind ind) "價錢" "價" "價格" "市價" "價碼"))
     (domain-circles
      "年界"
      ((domain-circles-ind ind) "界")
      )
     (style-trend
      "風格" "作風" "新風" "風氣" "歪風"
      "形式" "把式" "招式" "服式" "樣式"
      "格式" "模式" "款式" "範式" "式樣"
      "不正之風"
      (heat-or-trend
       ((heat-or-trend-ind ind) "熱"))
      ((trend-or-wave wave)
       ((trend-or-wave-ind ind) "潮"))
      ((style-or-process process)
       "方式" "行式"
       ((style-or-process-ind ind) "式"))
      ((style-trend-ind ind) "風"))
     (intention-or-feel ;; also include concept like 诗意
      "言外之意" "詩情畫意"
      ((intention-or-feel-ind ind) "意"))
     (@ evidence
      "據" "證據" "鐵證" "表證" "明證" "依憑" "依據" "憑據"
      "數據" "真憑實據" "論據" "根據")
     (position-of ;; physical or metaphorical position or level
      "分位" "份位"
      "首位" "層位" "末位" "等級" "頭等" "基層"
      "名次" "層次" "席次" "檔次" "中等" "前茅"
      "階" "位階" "層級" "位置" "級別"
      "幾何級數" "級數"
      "水準" "水平"
      ;;
      ((position-or-count number-of)
       ((position-or-count-ind ind) "次")
       )
      ((position-or-layer layer-like)
       ((position-or-layer-ind ind) "層"))
      ((position-of-ind ind) "位" "級" "等"))
     (network-like ;; abstract net or network
      "網路" "網絡" "天羅地網"
      ((network-or-website website net-like)
       ((network-or-website-ind ind) "網")))
     (flow-of ;; either physical or metaphorical
      "車流" "人流" "客流"
      ((flow-of-ind ind) "流"))
     (gesture "手勢" "姿勢")
     (tendency-or-momentum
      "形勢" "態勢" "局勢" "後勢"
      "勢頭" "陣勢" "走勢" "氣勢" "架勢"
      "優勢" "劣勢"
      ((tendency-or-gesture gesture)
       ((tendency-or-gesture-ind ind) "勢")))
     (@ belief "信條" "教條" "學說" "假說" "邪說" "主張")
     (@ belief-or-argument
        "結論" "立論" "游论" "定論"
        )
     (abstract-type-of
      "種類" "綱目" "屬種" "物種" "品種" "險種"
      "門類" "體系" "布種" "樹種" "分類"
      "體裁" ;; type of articles or writings
      ((abstract-type-or-outline-or-law outline law)
       ((type-or-outline-or-law-ind ind) "綱"))
      ;; 系 and 科 are also knowlege domain or organization
      ((abstract-type-of-ind ind) "類" "類型" "亞種" "種"))
     (@ brand "牌子" "品牌")
     (personality
      "性格" "国格" "品格" "人格" "本性" "性子" "個性"
      "品性" "稟性" "天性"
      (negative-personality
       "奴性" "劣根性" "德性" "奴洋性")
      (personality-or-space
       ;; 格 could mean personality as in 人格, or a (usually small and
       ;; rectangular) space
       ((personality-or-space-ind ind) "格")))
     ;;
     (counting-name
      (word-jia3 "甲") "乙" "丙" (word-ding1 "丁")
      "戊" "己" "庚" "辛" "壬" "癸")
     ;; the 地支 are included in the times. And the combination of 天干
     ;; and 地支 are also in the times for years.
     (@ field-or-column ;; as in newspaper or magazine
      "欄位" "欄目" "專欄" "榜")
     (value-of
      ((value-of-ind ind) "值")
      "產值" "定值" "差值" "市值" "數值" "面值" "總值"
      "淨值")
     (custom
      ((custom-ind ind) "俗")
      "風俗" "習俗")
     (aura-or-symptom
      ((aura-or-symptom-ind ind) "兆" "兆頭" "徵兆")
      "先兆" "前兆" "預兆" "症狀" "徵狀"
      )
     (series
      ((series-ind ind) "列")
      "系列" "序列" "行列")
     (difference-of
      ((difference-of-ind ind) "别" "差" )
      "分別" ;; could mean difference. As verb, could mean "leave each other" or "differentiate"
      "出入" ;; could mean small difference. As verb, could mean going in and out.
      "區分" "區別" "歧異"
      "天壤之別" "差別" "偏差" "利差" "反差"
      "時差" "比差" "落差")
     (motivation-or-momentum
      ((motivation-or-momentum-ind ind) "勁")
      "衝勁" "後勁" "幹勁" "拼勁")
     (shape-of
      ((shape-of-ind ind) "形")
      "外型" "原形" "雛形" "外形" "形態"
      "形狀" "體形"
      ((shape-or-type-of abstract-type-of)
       "髮型" "户型" "型態"
       ((shape-or-type-of-ind ind) "型")))
     (address-of
      ((address-of-ind ind) "址")
      "住址" "地址" "會址" "場址" "現址" "網址" "遺址")
     (@ fallback-or-preparation
      ((fallback-or-preparation-ind ind) "備")
      "後備" "建備" "預備" "準備" "籌備")
     (posture
      ((posture-ind ind) "姿")
      "神態")
     (prestige
      ((prestige-ind ind) "威")
      "威信" "妊威" "威望" "雌威")
     (truth ;; or reality
      ((truth-ind ind) "實")
      "來龍去脈"
      "事實" "史實" "現實" "紀實" "真相")
     (basis
      ((basis-ind ind) "底")
      "功底")
     (shadows
      (shadow-video
       ((shadow-video-ind ind) "影"))
      "影子" "背影" "倒影" "刀光劍影"
      (tree-shade-like "蔭")
      (light-shadow "光影"))
     (trait-of
      ((trait-of-ind ind) "徵")
      "特徵")
     (twist-and-frustration
      "周折" "曲折" "曲曲折折" "挫折" "波折")
     (root-of ;; metaphorical root
      ((root-or-plant-root plant-part)
       ((root-or-plant-root-ind ind) "根"))
      "劣根")
     (checking-of ;; another sense of examination
      ((checking-of-ind ind) "檢")
      ;; many are short hands
      "健檢" "複檢" "體檢" "檢驗" "篩檢" "檢查"
      ;;
      )
     (@ past-record
      ((past-record-ind ind) "歷")
      (past-experience "經歷" "遭遇" "歷練" "經驗")
      "縣志" "方志" "地方志" "病志" "前科"
      "學歷" "履歷" "簡歷" "病歷" "學經歷")
     (ratio-of ;; or proportion
      ;; "X Y 比"
      ;; ratio usually consists of two things, so construct in grammar
      "百分比" "百份比"
      "比例" "本夢比" "本益比" "正比" "比重"
      ((ratio-of-ind ind) "比"))
     (temperature-of
      "氣溫"
      "攝氏" "華氏" "克耳文"
      "攝氏度" "華氏度" "克耳文度"
      ((temperature-of-ind ind) "温" "温度"))
     (version-of
      ((version-of-ind ind) "版"))
     (encoding-of
      ((encoding-of-ind ind) "碼")
      "代碼" "密碼" "程式碼")
     (@ foundation ;; the ind is elsewhere
      "基本" "基礎" "樞紐" "樞" "中樞")
     (@ essence-of
      ((essence-of-ind ind) "粹")
      "精華" "精粹" "要領" "綱要" "精髓"
      )
     (@ element-of ;; some constituent part of other things
      ;; 素 also means vegan food
      "元素" "葉綠素" "酵素" "萬古黴素" "青霉素")
     (@ cavity-of
        ((cavity-of-ind ind) "膛"))
     (transportation-of
      ((luck-or-transportation-of luck) "運")
      "春運" "專運" "專遞" "快遞" "速遞")
     (@ appearance
        "榜樣" "模樣" "花樣" "賣相" "外表" "包裝"
        "意象" "現象" "跡象" "外貌" "容貌" "天象"
        "造型" "外觀"
        (word-beauty "美")
        ;; 像 is in word-statue
        ((appearance-or-elephant animal)
         ;; 象 could mean the appearance of an abstract matter
         ;; or the elephant
         ((appearance-or-elephant-ind ind) "象"))
        (@ appearance-ind "容" "樣" "貌"))
     ;;
     (@ animate-attribute
        (birthday "生日")
        (life "生活")
        (fever "高燒")
        (dream
         "夢" "春秋大夢" "黃粱美夢"
         (nightmare "夢魘"))
        (idea-opinion
         ((idea-opinion-ind ind) "見")
         "意見" "看法" "想法" "政見" "創見" "己見" "成見"
         "見解" "異見" "動議" "提議" "建議" "主意"
         "反饋" "迴響" "倡議" "異議" "非議" "決議"
         "偏見"
         ((idea-opinion-or-benefit benefit) "回饋")
         ((opinion-or-discussion discussion-meeting)
          ((opinion-or-discussion-ind ind) "議")
          "复議" "爭議" "熱議")
         (guess "揣測" "推測" "猜測" "臆測" "估量"
                "估計" "預計" "預估" "預測" "預見"
                "評估" "猜想" "假設"
                ((prophecy spoken) ;; similar to spoken
                 "預言"))
         (recommendation "推薦")
         )
        (surname-of
         (surname-of-word-shi4 "氏")
         "姓" "姓氏")
        (hobby-interest
         ((hobby-interest-ind ind) "興")
         "興趣" "勃興"
         )
        (registered-status ;; such as nationality
         ((registered-status-or-book-like-record
           book-like-record)
          "籍")
         (word-nationality "國籍")
         "戶籍" "黨籍" "會籍" "省籍"
         )
        (accent
         ((accent-or-body-cavity body-cavity)
          ((accent-or-body-cavity-ind ind) "腔"))
         "口音" "哭腔" "唱腔" "南腔" "陳腔")
        ((burden-of money-related)
         ((burden-of-ind ind) "負")
         "負擔" "稅負" "費負" "負債" "欠款" "借款"
         "損失" "虧損")
        (wish
         ((wish-ind ind) "願" "志")
         (wish-vision "願景" "憧憬")
         ((wish-or-reputation reputation)
          "聲望"
          ((wish-or-reputation-ind ind) "望"))
         (desire "慾望" "慾" "口腹之慾" "七情六慾"
                 "利慾" "肉慾" "渴求")
         "期望"
         "願望" "宿願" "夙願" "心願" "志願"
         "大志" "鬥志" "職志" "遺志"
         "意願" "遺願" "盼望" "祝願" "祝福"
         "展望" "希望" "厚望" "指望" "期盼")
        ;;
        (word-mang2 "忙")
        (needs "需要" "需求")
        (request "請求" "要求" "辭呈" "申請" "訴求" "祈求")
        ((kindness-favour ethics)
         "恩" "德" "恩情" "惠" "恩惠")
        (feeling "心情" "感受" "感覺" "榮耀" "快感" "好感"
                 "預感" "憾" "遺憾" "所感" "痛楚" "迷狂" "興致"
                 "痛苦" "疼痛"
                 (word-ku3 "苦")
                 (consolation "慰藉" "安慰")
                 )
        (physical-sensation
         "知覺" "錯覺" "幻覺"
         (sleep-or-sensation
          ;; 覺 also means sense
          (word-sleep "覺"))
         "癢" "痛" "痛癢" "切身之痛" "陣痛")
        (attitude ;; or mentality
         "態度" "姿態" "心態")
        (twist ;; metaphorical, or mental
         ((mental-or-physical-twist physical-twist)
          ((mental-or-physical-twist-ind ind) "結"))
         "心結" "情結" "怨結" "鬱結"
         )
        (secret "秘密" "機密" "奧秘" "機秘" "難言之隱")
        (job
         (job-word-gong1-zuo4 "工作")
         "職業" "專業" "職位" "職" "兼職" "正職"
         "全職" "事業" "副業")
        (skill
         "技術" "技" "魔術" "伎倆" "技巧" 
         "招" "花招" "高招" "技能"
         ((secret-skill secret)
          ((secret-skill-or-hole hole) "竅")
          "絕活" "絕招" "訣竅" "訣" "要訣" "特長")
         )
        (support "支持")
        (age "年齡" "年紀" "壽" "年壽" "年歲")
        (social-status "地位")
        (@ religion-like
         ((religion-ind ind) "教" "宗")
         (@ religion-name
          (@ religion-or-human "穆斯林")
          "巴哈伊")
         "基督教" "東正教" "東政教" "新教" "天主教"
         "伊斯蘭教" "回教" "清真教" "回回教"
         "天方教" "大食法" "大食教度"
         "印度教" "濕婆教" "毗濕奴派" "沙克達教"
         "佛教" "道教"
         "錫克教" "梵天信仰"
         "猶太教" "挑筋教" "一賜樂業教"
         "耆那教" "大同教"
         "儒教" "孔教"
         "神道教"
         "天道教"
         "一貫道" "一貫先天大道" "孔孟聖道" "一聖道"
         "一門道" "儒教會"
         ;;
         "邪宗" "信仰"
         "密宗" "唯識宗" "三論宗" "天台宗"
         "華嚴宗" "禪宗" "淨土宗" "律宗"
         "宗教" "流派" "派別")
        (effort "努力")
        (power "權力" "公權力" "力量")
        "魅力" "魄力" "財力" "精力" "活力" "毅力" "智力"
        "武力" "法力" "無力" "記憶力" "實力" "生命力"
        "勞力" "功力"
        (strength
         (natural-power "風力" "水力" "火力")
         "餘力" "神力" "氣力" "暴力" "心力" "體力" "人力"
         "九牛二虎之力"
         ;; probably shorthands
         "全力" "己力"
         )
        (rights
         ((rights-ind ind) "權" "權利" "專利")
         "職權" "王權" "特權" "版權" "所有權"
         "軍政權" "產權" "主權" "業權" "行政權"
         "權柄" "權益"
         )
        (consideration "考量" "度量")
        (personal-qi4 ;; 氣 in the abstract sense
         "骨氣" "銳氣" "寒酸氣" "財氣" "英氣" "脾氣"
         "語氣" "胎氣" "膽氣" "福氣" "神氣" "火氣"
         "洋氣" "正氣" "晦氣" "人文氣" "手氣" "戾氣"
         "怨氣" "怒氣" "志氣" "底氣" "士氣" "名氣"
         "口氣" "勇氣" "力氣" "元氣" "俠氣" "牛仔氣"
         "人氣" "習氣" "義氣" "霉氣" "脂粉氣"
         )
        ((abstract-heart-attribute abstract-heart)
         "雄心" "野心" "身心" "芳心" "良心" "自心" "胸心"
         "耐心" "私心" "良苦用心" "用心" "歡心" "居心"
         "好奇心" "決心" "內心" "公心" "信心" "惻隱之心"
         "一己之心" "丹心" "警覺心"
         )
        ;;
        (thinking ;; or thoughts
         ((thinking-ind ind) "思" "念" "想" "慮")
         "思路" "思想" "信念" "心念" "意念" "理念" "心思"
         "意思" "奇想"
         "非分之想" "非份之想"
         "揣想" "浮想" "理想"
         "空想" "意料" "思潮" "感想" "思緒" "思慮"
         "一念之差" "省思" "反思" "追思" "遐想" "遐思"
         "想像" "企圖" "深思" "圖謀" "打算" "同感" "設想"
         "預謀" "意圖" "思維" "構想" "妄想" "思考")
        (conduct
         "為人" "私行" "操守"
         "一言一行" "言行" "品行" "暴行" "惡行"
         ((sin-conduct sin-or-crime) "罪行" "滔天大罪")
         "德行" "善行"
         ;; 行 could be something like a bank or shop, 
         ;; or a conduct (品行) of a person,
         ;; or a trip-like concept (行程),
         ;; or a profession or industry.
         ;; But we mainly prefer the bank or shop as default.
         ((conduct-ind ind) "行")
         )
        (account
         "户口" "户籍" "賬戶" "帳戶" "專戶")
        (talent "才" "才華"
                "天份" "天分"
                "才幹" "天賦")
        (spirit "魂" "靈魂" "魄" "魂魄")
        (responsibility
         (mission "任務")
         ((responsibility-ind ind) "責" "任")
         "職務" "職守"
         "大任" "重責大任" "重責" "己任" "責任" "重任")
        (addiction
         ((addiction-ind ind) "癖" "癖好" "癮")
         "癮頭"
         )
        (@ martial-arts
           "氣功" "内功" "武功" "香功" "功夫" "武"
           "刺拳" "勾拳" "太極")
        ((animate-appearance appearance)
         "儀容" "妝" "妝容" "姿容" "尊容" "笑容" "陣容"
         "面容" "顏容" "洋相" "長相" "形象" "相貌" "化妝"
         "打扮" "裝扮")
        (@ trace-of
         "踪" "跡" "踪跡" "踪影" "痕跡" "下落"
         "行踪" "痕"
         )
        (preference "嗜好" "喜好" "愛好" "偏好")
        ;; other animate-attribute
        "陰影" "行徑" "紀律" "心得"
        "所得" "心扉" "矜持" "脈搏" "天敵"
        "熱忱" "下懷" "情懷" "初戀" "戀愛"
        "戀" "作息" "出息" "氣息"
        "身材" "身段" "氣概" "舉止" "秋波" "福澤"
        "生涯" "肝火" "氣焰" "親疏" "新知" "精氣神"
        "眼神" "門第" "底細" "能耐" "生肖" "心胸"
        "本能" "人脈" "痛腳" "風範" "底蘊" "胸襟" "胸懷"
        "膽識" "熱誠" "赤誠" "誠" "抱負"
        "文采" "風采" "視野" "心防" "風韻" "風骨"
        "氣魄" "後塵" "掛礙"
        (temperament "氣質")
        (accomplishment
         "成就" "大成" "小成")
        (choice "選擇" "抉擇" "取捨" "首選")
        ;;
        (word-culture "文化")
        (word-country "國家") "王朝"
        "面子" "里子" 
        "性別" "歲數" "生平"
        "樣子" "觀點" "習慣"
        "年級" "聲調"
        (word-xin4-yong4 "信用")
        "夢話"  "觀念"
        "腦筋" "前途"
        "個子"
        "崗位" "家教" 
        "胃病" "身高" "體重"
        (spirit-or-mind "精神")
        ;;
        ((money-related-attribute money-related)
         "財產" "資產" "私產" "財富" "家底" "積蓄"
         "儲備" "捐獻" "儲蓄" "回報" "財"
         )
        (signature "連署" "署名" "簽名" "聯署" "落款" "下款")
        (observation "觀測" "觀察" "勘察")
        (misunderstanding "誤會" "誤解")
        (@ reply "答覆" "回覆" "回答" "回應" "應答")
        (consciousness
         "意識" "下意識" "潛意識" "覺悟" "自覺" "神志")
        ;;
        (word-qing1-bai2 "清白")
        (stress ;; could be physical or mental
         "壓力")
        ;;
        "強項" "學位" 
        "情感" "口味" "禮貌"
        "體格" 
        ;;
        "冗餘" "冗"
        "印象" "記憶"
        "靈感" "婚姻" "權威" "議論" 
        "不便" "孤獨"
        "苦心" "人情"
        "歉意" "信賴" "理智" "情意"  "境界"
        "外傷" "內傷" "傷" "瘀" "瘀傷" "傷患" "患" "病患"
        "燒傷" "重傷" "輕傷"
        "後患" "外患" "大患"
        "智慧" "智" "心智" "才智" "神智"
        "厄運" "性命" "命"
        "辛勞" "勞累" "一生" "平生" "寫作"
        "潛力" 
        "出身" "穿著" 
        "意志" "意志力" "人性"
        (goal "目標" "目的")
        (suspicion-or-question
         (suspicion-for-guilty
          ((suspicion-for-guilty-ind ind) "嫌")
          "嫌疑" "罪嫌")
         "困惑"
         "懷疑" "疑" "疑惑" "疑問" "置疑" "疑惑"
         "查詢" "質疑" "質詢" "詢問" "提問" "諮詢")
        (contact-and-communication
         "聯繫" "往來" "來往")
        (behavior
         ((behavior-word-ju3-ind ind) "舉")
         "表現"
         "舉動" "義舉" "創舉" "善舉" "壯舉" "逆舉"
         "行為" "所作所為" "所做所為" "所作" "所做" "所為")
        (sacrifice "犧牲品" "犧牲")
        (in-the-name-of "名義")
        (climax "高潮" "高峰")
        (stance "立場")
        (health "健康")
        (emergency-response "響應")
        ;; some combinations are commonly used
        "自由意志"
        ;;
        "步子" "命根" "架子" "底子"
        "威權" "胆量" "心理"
        "分量" "份量" 
        "心地" "境地" "遺產" "行頭" 
        "來頭" "手頭" "心頭" "準頭" "個頭"
        "動機" "一技之長" "專長" "智能" "日常"
        "齡" "風度" "氣度" "誠信" "情面" "人品" "筋道" "互信"
        "血統" "雙系血統"
        "能事" "本事" "年事" "来路" "精元"
        "惻隱" "个儿"
        ;; a few nouns ending with 商 are attributes
        "智商" "情商"
        ;;
        "尊嚴" "自尊"
        "心聲" "排場" "下場" "意向" "身價"
        "身手" "援手" "胃口" "眼界" "雄風" "創意"
        "誠意" "敬意" "心意" "用意" "己意" "資質"
        "身份" "身分"
        "表情" "神情" "性情" "交情" "做派" "真才實學"
        "真才" "實學" "氣節" "官位" "皇位" "席位" "大位"
        "品位" "品味" "風華" "韶華" "年華" "辭令" "眼光"
        "目光"
        "輩分" "本分" "名分" "養分"
        "輩份" "本份" "名份" "養份"
        "前程" "身世" "前景"
        "異義" "天倫之樂" "惡習" "習" "舊習"
        "家產" "步伐" "真傳" "形像"
        "修養" "涵養" "內涵" "素養" "素質" "親筆" "跟前"
        "安危" "談吐" "動向" "去向" "取向" "志向" "視聽"
        "天聽" "幻聽" "口吻" "忠告" "小報告" "血型"
        "得失" "過失" "閃失" "默契"
        "福" "案底" "輪廓" "配備" "威嚴" "不和"
        "冤屈" "冤" "委屈" "暗示" "顧忌" "真心"
        "教養" "指示" "警惕"
        "擔當" "担待" "擔戴" "承擔"
        "摯愛" "交待" "偽裝" "糾葛" "掌握"
        "尊敬" "尊重" "壓抑" "懷抱"
        "體現" "追求" "幸福" "決定" "自信" "夢想"
        "消遣" "不滿" "感觸" "回憶" "判斷"
        "發現" "傾向" "認知" "決策" "方便"
        "困擾" "寄托" "憑藉" "體驗" "作為"
        "把握" "自由"
        "高風" "亮節" "高風亮節"
        ;;
        )
     ;;
     (@ inanimate-attribute
        ((area-or-volume amount-of)
         (area-of "面積")
         (volume-of "體積" "容積" "容量")
         )
        (material-of
         "材料" "原料" "物料" "質料" "素材" "媒材"
         "題材" "原材料"
         ((material-or-word-material word-material)
          "教材"
          ((material-or-word-material-ind ind) "材"))
         ((material-or-information information)
          ((material-or-information-ind ind) "料")
          ))
        ((inanimate-appearance appearance)
         (color-or-appearance
          "顏色" "景色" "氣色" "起色" "神色" "天色"
          "色彩" "色澤"
          ((color-or-appearance-ind ind) "色")))
        (territory "版图")
        ;;
        (horse-power "馬力")
        (population-count "人口")
        (interface "界面")
        (life-expectancy "壽命")
        (protection
         "保護" "防護" "呵護" "偏护" "偏袒" "袒护")
        ;; other inanimate-attribute
        "熱力" "火力" "電力" "地力" "國力" "軍力" "兵力"
        "社会力" "當量" "分辨率"
        "嚼頭" "幅員" "用途" "樓面" "店面"
        "銷路" "紋路" "量体" "尺寸" "布幅" "長短"
        "半徑" "直徑" "規模" "水深"
        ;;
        "型號" "特點" "特色"
        "質量" "滋味" 
        "熱量" "品質" "畝產" "答案"
        "污跡" "漬" "污漬" "污"
        "貨色" "本色" "成色"
        "裝潢" "翻譯" "佈置" "配套" "配置" "顯示"
        "延伸" "應用" "音譯" "意譯" "治安"
        ;;
        )
     (obstacle "障礙" "礙" "阻礙" "隔礙" "阻攔")
     (mistake-right-wrong
      "是非" "對錯" "差錯" "過錯" "非" "錯")
     (analogy "類比" "比喻")
     (prohibition-like
      "忌諱" "大忌" "禁忌"
      ((prohibition-like-ind ind) "忌" "諱"))
     (experiment "實驗" "試驗")
     (separation "區隔" "阻隔" "隔閡" "分隔")
   ;;;
     ;; other abstract
     (life
      "人生" "來生" "餘生" "今生"
      (word-life "世" "生" ))
     (word-zhen3 "診")
     (word-tai2-tou2
      ;; part of a check (or similar documents), much like invoice title
      "抬頭")
     ((word-cluster compound) "集群")
     (suffering "苦厄" "厄" "苦難" "苦頭")
     (abstract-key "關鍵")
     (composition-of "成分" "成份")
     (economy
      ((economy-can-be-job can-be-job) "經濟")
      "經濟體")
     (design-of "設計")
     (word-pollution
      "空污" ;; shorthand for '空氣 污染'
      "污染")
     (weather-convergence-divergence "輻合" "輻散")
     ;; "統計" ;; use the verb form?
     "可能" "局限" "庫存" "架構"
     "平等" "打擊" "翻版" "傷害" "鏈接" "平衡"
     "定位" "輔助" "缺失" "娛樂" "比對" "編制"
     "來歷" "由來" "例外" "對比" "挑戰" "安排"
     "麻煩" "傷亡" "教訓" "預設" "突破"
     "講究" "了結" "提名" "供應" "約定" "異常"
     "覆蓋" "構成" "組成" "連結" "紕漏" "提示"
     "福證" "開端" "伊始" "景氣" "掣肘"
     "樣板" "马赛克" "環節" "情節"
     "苦海" "腦海" "政海" "官本位"
     "方位" "方向" "潮流"
     "起居" "傳奇" 
     ;;
     "磁譜" "光譜" "音譜"
     "上風" "下風" "口風" "耳旁風" "耳邊風"
     "尾聲" "情色" "磁" "磁場" "科教" "色情"
     "惡報" "好報" "善報" "載體" "軟體" "軟件" "軟硬體"
     "文體" "整體" "掩體" "總體" "形體" "實體"
     "大體" "國體" "混合體" "全體" "主體" "統一體"
     "一體" "對立體" "循環體"
     "检体" "查体" "毒手" "妙手" "先手"
     "豁口" "藉口" "缺口" "端口" "当口" "借口"
     "突破口"
     ;;
     "氣數" "招數" "定數" "天意" "涵意" "此行"
     "衣食住行" "命令行" "岗台" "对台" "後台"
     "福利" "孽" "手工" "豪門" "法門" "不二法門"
     "寒門" "宰客门" "命门" "柳暗花明" "自知之明"
     "歇斯底里" "衝動" "異動" "佯動" "悸動" "脈動"
     "氣派" "形而上" "形上" "幾何" "連理" "生理"
     "條理" "哨" "票房"
     ;;
     "途" "如是觀" "價值" "海市蜃樓"
     "资信" "民信" "徵信" "公信" "單元" "武俠"
     "包票" "特效" "根本" "範本" "版本" "樣本"
     "耳光" "点球" "缺陷" "資格"
     "規格" "真格" "位格" "字裡行間" "補丁" "名下" "旗下" "麾下"
     "美醜" "亂" "暴亂" "騷亂" "騷動" "大虧" "盈虧"
     "戰雲" "疑雲" "陷阱" "單產" "媒介" "簡介"
     "空優" "真偽" "盈餘" "餘暇"
     "暇" "天候" "火候" "端倪" "真假"
     "美聯儲" "意像" "炊" "恩典" 
     "洪水猛獸" "服務" "優劣" "回合" "異同" "天命" "宿命"
     "反響" "分嘵" "悲喜" "噴嚏" "氣氛" "氛圍" "方圓"
     "好壞" "乾坤" "音域" "下埸" "厘頭" "圈套"
     "異曲同工之妙" "奧妙" "前嫌" "時宜" "支付宝"
     "口實" "食宿" "貧富" "輻射" "紅塵" "風塵" "風尚"
     "家常" "内幕" "序幕" "煙幕" "主幹" "報應" "效應"
     "謎底" "謎面" "興廢" "附庸" "外延" "弊" "利弊"
     "時弊" "等式" "公式" "索引" "強弱" "依歸" "滿堂彩"
     "口徑" "一直之快" "意識形態"
     "當務之急" "燃眉之急" "緩急" "輕重" "善惡" "缺憾"
     "前戲" "拿手好戲" "把戲" "鑑戒" "重托" "重擔"
     "節拍" "海拔" "股指" "前提" "前題" "正題"
     "菩提" "性按摩" "分支" "未定之數" "未知之數"
     "片段" "片斷" "四舊" "旨" "宗旨" "要旨" "文明"
     "科普" "冷暖" "強弩之末" "始末" "結構" "因果"
     "把柄" "笑柄" "話柄" "風向" "坐標" "座標"
     "空檔" "歉" "農殘" "標段" "波段" "供求" "代溝"
     "先河" "漏洞" "深淺" "不測" "淡濃" "濃淡"
     "磁懸浮" "點點滴滴" "香火" "在天之靈" "馬後砲"
     "過眼雲煙" "視焦" "最愛" "筆劃" "瑕" "疵"
     "瑕疵" "弊病" "毛病" "黑白" "眾矢之的" "損益"
     "移民監" "買盤" "賣盤" "籌碼" "里程" "禪"
     "時空" "究竟" "書香門第" "標籤" "雲計算" "一團糟"
     "干系" "線索" "枷累" "負累" "經緯" "經度" "緯度"
     "東經" "西經" "南緯" "北緯" "粗細" "缺" "空缺"
     "命脈" "陣腳" "陣" "圖騰" "鋒芒" "千辛萬苦"
     "千辛" "萬苦" "着落" "瓜葛" "芥蒂" "興衰" "餘裕"
     "冰山一角" "情調" "格調" "民調" "萬象" "分貝"
     "勝負" "跤" "心跳" "前身" "軒輊" "轍" "覆轍"
     "關連" "關聯" "選" "待遇" "分野"
     "配" ;; short for 配對, a pairing
     "補釘" "補丁" "樞鈕"
     "險阻" "風險" "嫌隙" "迷障" "內需" "外需"
     "韻" "神韻" "溫飽" "誘餌" "餌"
     "不平之鳴" "烏龍" "天譴"
     
     ;; 
     (facility-like
      "設施" "建設" ;; TODO: building?
      )
     (release-of "排放")
     ;;
     ))
;; IMPORTANT!!!!
;; NOTE: need to update the also-verb-p flag with (mark-also-verbs *abstract-hash*)

;; end abstract-list
(def-word-class verb-action (abstract)
  ((verb
    ;; holds the verb that is turned into an abstract noun, representing the action
    :initarg :verb
    :initform nil
    )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-tagged-words *abstract-suffix-list* *abstract-suffix-hash*
  (@ abstract-suffix
     ;; TODO:
     ;; for many nouns and verbs
     ;;   noun + "性" -> noun; verb + "性" -> noun
     ;; but for quite a number of them, we have
     ;;   noun + "性" -> adj; verb + "性" -> adj
     ;; Worse, some can be both, depending on the usage.
     ;; For now, we assume they give nouns.
     ((s-xing4 ind) "性")
     ;; 觀 could also mean a temple like building, but it is usually
     ;; prefixed by a name, so we ignore it for now.
     ((s-guan1 ind) "觀")
     ((s-zhi1-bian4 ind) "之便")
     (@ belief
        ((s-lun4 ind) "論")
        ((s-shuo1 ind) "說"))
     ))

;; end abstract-suffix-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other words
(defparameter *exclaims*
  '("啊" "唉" "哦" "哎" "嗯" "哼" "噢" "嗬" "喂" "嗨" "咦"
    "喲" "呸" "哎呀" "哈" "呢" "呵" "呀" "嘛" "啦"
    "哪" "吶" "那" "呃" "矣" "哇" "唄" "嘍" "咯" "阿" "滴"
    "喔" "哉" "兮" "焉" "喏" "呦"
    "嘎" "王八个操的" "奶奶的" "呜呼" "妈的" "尼吗"
    "擦" "我C" "乖乖" "屁" "切" "嘻" "见鬼" "他妈地"
    "该死的" "呕" "啧" "哎咳" "咋" "我的天" "天呐" "好"
    "诶" "嘿咻" "嘘" "哈拉" "草泥马的" "靠" "天啊" "哟"
    "是的" "尼玛" "唔" "他妈的" "嘿" "天哪" "咳"))
(defparameter *exclaims-list* (strs-to-lists *exclaims*))

(defparameter *conns*
  '(
    ;; these can be used in clauses such as 'conn NN 有關'
    "和" "同" "跟" "與"
    ;;
    "及" "并" 
    "或" "或者" "並" "以及"))
(defparameter *conns-list* (strs-to-lists *conns*))


(defparameter *sounds-list*
  (strs-to-lists
   '("嘘" "嗯" "唉" "梟" "歪" "乎" "刺" "汪" "突" "蟈"
     "瑯" "蕭" "霍" "颼" "砰" "豁" "甕" "咦" "哦" "習"
     "淅" "瀝" "滴" "乒" "閣" "嘶" "呷" "呠" "的" "達"
     "喃" "篤" "溜" "唭" "硿" "嚨" "鈴" "唏" "霹" "朴"
     "嗶" "郎" "卡" "籠" "嘍" "低" "支" "晃" "骨" "魯"
     "塌" "澎" "幾" "此" "東" "忽" "登" "卜" "答" "當"
     "坷" "騰" "圪" "差" "黑" "擦" "初" "底" "丁" "零"
     "巴" "瑟" "沙" "颯" "呼" "拔" "拉" "潺" "通" "撲"
     "滋" "梆" "崩" "嚕" "呱" "轟" "隆" "哐" "啷" "咪"
     "乓" "里" "劈" "噗" "鏘" "鏜" "呀" "錚" "叭" "叮"
     "咤" "叱" "吆" "吼" "吽" "呃" "吧" "吥" "吱" "呦"
     "呴" "呵" "呶" "咄" "嗟" "咋" "咕" "噥" "呸" "咔"
     "咚" "咑" "呣" "咯" "咻" "咿" "啞" "哇" "哈" "咩"
     "哞" "咭" "咴" "哩" "哼" "哧" "唧" "哢" "唼" "啁"
     "唿" "啊" "啵" "唰" "啪" "唷" "啦" "啼" "啾" "喀"
     "喈" "喋" "喵" "喔" "喤" "喱" "喲" "嗞" "喳" "嗖"
     "嗚" "嗡" "嗤" "嗷" "嘎" "嘓" "嘣" "嘁" "嘡" "嘟"
     "嘰" "嘿" "噌" "噝" "噔" "嘩" "噙" "噁" "噢" "噹"
     "噼" "嗒" "噠" "嚓" "嚦" "嚶")))

(defparameter *preps*
  '("了" "的" "地" "得" "所" "被" "以" "之後" "時"
    "邊" "底" "旁" "些" "從" "向" "兒" "往" "之"))
(defparameter *preps-list* (strs-to-lists *preps*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
