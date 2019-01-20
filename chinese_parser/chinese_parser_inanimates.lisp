(in-package :chinese-parser)

(def-tagged-words *inanimate-list* *inanimate-hash*
  ;; nouns for inanimate physical objects that are not places
  (@ inanimate
     (ball-like ;; can have sub-classes elsewhere
      ;; mostly can be quantified with 粒
      "球團" ;; technical term for metal
      "塵" "埃" "泡沫" "泡" "烟球" "碎屑" "顆粒"
      "粒" "丸"
      ((ball-like-ind ind) "珠")
      (ball
       ((ball-sports sports)
        ((ball-ind ind) "球")
        ((ball-name name) "高爾夫" "保齡" "乒乓")
        "門球" "槌球" "排球" "步打球" "捶丸"
        "足球" "橄欖球" "棒球" "籃球" "木球" "撞球" "曲棍球"
        "澳式足球" "網球" "美式足球" "壘球" "班迪球" "高球"))
      ;;
      (particles ;; could be atomic
       "粒子" "中子" "原子" "電子" "質子")
      )
     ((stick-like long-thin)
      "鋼筋" "樑" "上樑" "下樑"  "棟樑"
      "香煙" "木頭" "柱" "煙囪" "管" "光管" "槓")
     (plate-like
      ;; TODO: 牌 as in license
      ((plate-word brand)
       ;; "名牌" could mean a famous brand, or a plate with a name on it
       "牌")
      ((plate-like-ind ind) "板")
      (video-or-plate
       ((video-or-plate-ind ind) "片")
       ((audio-disc music) "唱片"))
      (emblem
       ((emblem-ind ind) "徽")
       "會徽")
      "箔" "塊"
      "牌匾" "匾" "版塊" "板塊" "籌" "籤"
      "廣告牌" "招牌" "車牌" "碑" "石碑"
      "芯片" "晶板" "晶片" "天花板" "架板")
     (plant
      "植物"
      "一草一木"
      (tree
       ((tree-ind ind) "樹")
       ((tree-name name)
        "松" "柏" "栢" "槐" "柳" "檜" "檀香" "杉"
        "榕" "蘇鐵" "梅" "橡" "茄苳" "茄冬")
       "果樹" "楊樹" "牙香樹" "烏心石"
       "朴樹" "潺槁樹" "梭欏樹" "野漆樹"
       "灌木" "喬木" "叢" "菩提樹" "檀樹"
       "真檀" "白旃檀" "白檀" "柳杉" "油杉"
       "楊柳" "欒樹" "鐵樹" "鳳尾蕉" "鳳尾松"
       "避火樹" "荊" "棘" "棕櫚" "櫚"
       ;;
       "月橘" "七里香" "九里香" "十里香" "千里香"
       "萬里香" "滿山香" "九秋香" "九樹香"
       ;; "千隻眼" ;; too confusing?
       "千隻眼跌打"
       "千枝葉" "臭千隻眼" "小萬年青" "青木香"
       "四季香" "四時橘" "石柃" "石芬"
       )
      "盆景" "君子蘭" "芒" "罌粟" "薰衣草"
      "農作物" "穗" "作物" "莊稼" "糯" "薰"
      "吊蘭" "蘆葦" "秣" "蘺" "蕨" "藻"
      "八角楓" "裂斗錐粟" "羅浮柿" "蒺藜" "藜"
      "大頭茶" "紅楠" "白楸" "木臼" "八角"
      "木荷" "假蘋婆" "荼" "菊苣" "苦苣" "苦菜"
      "紫蘇" "大麻" "靈芝" "芝蘭"
      ((long-thin-plant long-thin)
       ;; "竹" elsewhere
       (word-plant-rice "稻")
       "草" "茅" "水稻"  "秧苗" "樹枝" "蔥"
       "蔗" "甘蔗" "秀苗" "種苗"
       )
      (flower
       ((flower-ind ind) "花" "卉" "蕾")
       "奇花異卉" "花卉" "花朵" "花蕾"
       "胸花" "腰花"
       ;;
       ((flower-name name)
        "玫瑰" "洋紫荊" "夾竹桃" "蘭" "百合" "木棉"
        "羊蹄甲" "宮粉羊蹄甲" "紅花羊蹄甲" "菊"
        "米仔蘭" "文殊蘭" "美人蕉" "珊瑚爵床"
        "紫薇" "鴨腳木" "龍吐珠" "杜鵑" "馬櫻丹"
        "五色梅" "七色梅" "五龍蘭" "臭草" "臭金鳳"
        "五色繡球" "五雷丹" "變色草" "蓮" "櫻"
        "茉莉" "洋甘菊" "甘菊" "荷"
        )
       "金銀花"  "杜鵑花" "炮仗花" "桂花" "長春花"
       "醉蝶花" "龍船花" "大紅花" "如意草" "五彩花"
       "綿鼻公花" "紫荊花")
      (plant-flakes
       "花瓣" "葉片" "瓣" "心皮"
       ((seed-like ball-like) "種子")))
     (liquid
      "液體" "液" "汽油" "漆" "糊" "漿糊"
      "油漆" "沫" "涕" "血清" "甲醇"
      (word-liquid-jiang1 "漿")
      (liquid-like-agent
       ((liquid-like-agent-ind ind) "劑")
       "成劑" "試劑" "清潔劑" "花納精" "沖劑" "誘發劑")
      (oil
       ((oil-ind ind) "油")
       "柴油" "石油" "精油" "酥油" "甘油" "貧油")
      (drink
       ((plant-or-drink tree)
        ((plant-or-drink-ind ind) "茶"))
       ((drink-ind ind) "酒" "醋" "汁" "釀" "飲")
       ;; "奶" and "乳" also in body-part as boobs
       "飲品" "飲料" "飲子"
       "雪碧" "可樂" "牛奶" "咖啡"
       ((drink-and-company-name company-name)
        "可口可樂" "COCA-COLA" "百事可樂" "百事"
        )
       "沱茶"
       "酒精" "乙醇"
       "敬酒" "罰酒" ;; also verb, so need to list
       "威士忌" "白酒" "汾酒" "杜康" "黃酒" "拉菲"
       "料酒" "花雕" "紹興酒" "狀元紅" "女兒紅"
       "米酒" "燒酒" "奶酒" "清酒" "梅酒" "冰酒"
       "琴酒" "伏特加" "氣態煙酒" "氣泡酒"
       "香檳酒" "蘭姆酒" "白葡萄酒" "高粱酒"
       "蜂蜜酒" "白蘭地" "干邑" "干邑白蘭地"
       "雅馬邑白蘭地" "雞尾酒" "力嬌酒" "利口酒"
       "龍舌蘭" "阿夸維特" "庫拉索酒"
       "葡萄酒" "杜松子酒" "红酒" "茅台"
       (beer
        ((beer-ind ind) "啤")
        "啤酒" "黑啤"
        "青啤" ;; shorthand for 青島啤酒
        )
       "陳醋"
       "豆汁" "果汁" "菜末汁"
       "醬油" "醬" "酪" "乳酪"
       "豆漿" "王漿" "蜂王漿" "春漿" "秋漿"
       "油污"
       (water
        ((water-ind ind) "水")
        "供水"
        "千山萬水" "露" "水體" "水花"
        "白开水" "开水" "茶水" "露水" "春水")))
     (@ body-part
        (head
         ;; not only physical head, but metaphorical head
         ;; not only 'head' of animate, but also inanimate
         "頭" "人頭")
        (word-body-part-ti3 "體")
        ((liquid-part liquid)
         (hormone "內啡" "內啡肽" "安多酚" "腦內啡" "腦內嗎啡"
                  "肽" "激素" "荷爾蒙" "賀爾蒙")
         ((tear ball-like) "眼淚" "淚")
         (urine "小便" "尿")
         ((sperm-or-liquid-agent liquid-like-agent) "精")
         "血" "口水" "唾" "精液" "汗" "痰"
         "冷汗" ;; also a verb
         )
        (face
         ;; TODO: whether to add one-way character substitution,
         ;; i.e. 面 could be used as 臉, but not the other way round?
         ((face-ind ind) "臉")
         ((face-word-mian4-ind ind) "面")
         "臉蛋" "薄面" "臉面" "容顏" "顏" "顏臉" "顏面"
         "臉目" "面目" "頰"
         "臉頰" "面頰" "臉孔" "面孔" "臉龐" "面龐" 
         "臉部" "面部" "臉膛" "面膛"
         (clown-face "鬼臉"))
        (forehead
         (word-forehead "額")
         "額頭")
        "角" "胎"
        (@ body-cavity
           ;; e.g. 口腔, 鼻腔, some are put at more specific place
           ;; 膛 could 
           "鼻腔" "耳腔" "胸腔" "腹腔" "胸膛")
        (mouth "口" "嘴" "嘴巴" "口腔"
               ((mouth-or-argument argument) "口角"))
        "五官" "唇" "嘴唇" "下巴"
        "耳" "耳朵" "耳跟" "鼻" "鼻頭" "鼻樑"
        "喉嚨" "喉" "喉頭" "咽" "咽喉"
        (word-hand "手") "左手" "右手" "雙手" "單手"
        "手腕" "腕" "手指" "手指頭" "指" "指頭" "爪"
        "拇指" "食指" "中指" "無名指" "尾指"
        "掌" "手掌" "巴掌" "股掌" "肢" "抗體"
        "拳" "拳頭" "肘" "靈道" "穴位" "部位"
        "腳心" "腳指頭" "腳趾" "趾" "趾頭" "腳丫" "足"
        "膝蓋" "膝頭" "膝"
        "人體" "身軀" "軀" "軀體" "身體" "懷" "軀殼"
        "軀幹" "骨盆" "腮" "膊" "胎記" "腦白質" "蛋白質"
        "角質" "腳跟" "跟" "周身" "血肉之軀"
        "骸" "骸骨" "骨骼" "骼" "骷" "髏" "骷髏"
        (word-body "身")
        "上身" "下身" "上半身" "下半身"
        "嗓" "嗓門" "肚" "胸" "腹" "臀" "肺"
        "器官" "皮肉""皮膚" "膚" "眉頭" "背"
        "肩" "肩頭" "全身" "骨架" "肩胛" "胛"
        "乳頭" "奶頭"
        (eye "眼睛" "眼" "睛" "鳳眼" "淚眼" "秋水" "明眸" "眸"
             "招子" "眼球" "眼框" "眼眶"
             "法眼" "慧眼"
             (word-eye-mu4 "目"))
        ((eye-or-trace-of trace-of) "眉目")
        ((teeth ball-like)
         "牙" "蛀牙" "牙齒" "牙冠" "齒"
         "門牙" "臼齒" "犬齒")
        (word-body-part-ti2 "蹄" "蹄子")
        "喙" "子宫"
        ((scar beam-like)
         "疤" "疤痕" "傷疤" "瘡疤" "傷痕")
        ((long-thin-body-part long-thin) ;; can be quantified with 條
         (leg "腿" "腳" "腿腳" "腳根")
         "肌肉" "膀" "脖" "脖子" "腰" "腰杆"
         "骨髓" "腸" "肌" "尾" "尾巴" "陰道" "陰部" "呼吸道"
         "染色體" "基因" "基因體" "筋" "髓"
         "脊" "脊柱" "脊椎" "脊樑" "椎"
         "頸" "頸椎" "鎖骨"
         ((neck-or-aspect aspect)
          ((neck-or-aspect-ind ind) "項"))
         "韌帶" "聲帶" "前列腺" "腺" "舌" "舌頭" "屌"
         "骨幹" "肋" "臂" "手臂" "臂彎" "胳" "胳膊"
         "脈絡" "絡" "經絡" "經脈"
         "鹿茸"
         "脈" ;; "脈" also has metaphorical meaning
         (nerves "神經" "神經線")
         (feces "大便" "糞便" "屎" "糞"))
        "胃" "肝" "膀胱" "肝臟" "胃臟"
        (fat
         ;; 肥 could mean fat, or fertilizer, so elsewhere
         ((fat-or-plant-part plant-part)
          ;; 脂 could also mean resin from trees
          "脂")
         "脂肪" "肥膏" "甘油三脂")
        ((plate-like-body-part plate-like)
         "膜" ;; 膜 could generally refer to a thin membrane, which we put into plate-like
         "視網膜" "橫膈膜" "膈膜"
         "鱗" "表皮" "眼瞼" "眼皮" "視盤" "椎間盤" "椎肩盤")
        ((ball-like-body-part ball-like)
         "腦" "腦袋" "心臟" "膽" "腎" "腎臟" "內臟" "臟器" "卵"
         "屁股" "睾" "睾丸" "顱" "頭顱" "光頭" "酒窩" "臟"
         "繭" "髻" "髮髻" "細胞"
         (boobs
          ((milk-or-boobs drink)
           "奶粉" ;; milk power, but also used as if drink
           ((milk-or-boobs-ind ind) "乳" "奶"))
          "乳房")
         ((heart abstract-heart)
          ((heart-ind ind) "心")))
        (corpse "屍" "屍體" "木乃伊" "遺體" "屍首")
        ((hair-like long-thin)
         (word-mao2 "毛")
         "鬚根" "髮根" "鴻毛" "毫" "茸" "鬍鬚" "鬚"
         "假髮" "華髮" "絨" "鬢" "眉" "鬍" "翅" "鬃"
         "頭髮" "髮" "辮" "辮子" "鬍子" "羽"
         "羽毛" "翅膀" "眉毛" "翼" "鬈"))
     (medicine ;; also drugs, good or bad
      "鴉片" "尼古丁" "可卡因" "海洛因" "藥引"
      "麻黄"
      (medicine-injection
       "疫苗" "嗎啡" "補血針" "強心針" "點滴"))
     (@ edible
      (food-additive
       "蘇丹紅" "蛋白精" "三聚氰胺" "密胺" "蜜胺")
      ((vegan-food-or-element-of element-of)
       ((vegan-food-or-element-of-ind ind) "素"))
      "布丁" "奶凍" "布甸" "蛋塔" "蛋撻" "撻" "葡撻"
      "食物" "碳水化合物" "泔水" "油串"
      "漢堡" "火鍋" "便飯" "粥"
      "點心" "糧" "糧食" "便當" "零食"
      ((year-end-meal meal-like-event)
       "年夜飯")
      "麥" "主食" "食" "伙食" "衣食"
      "餡" "鹽" "鹽巴" "調味品" "山珍海味" "味精"
      "飼料" "香料" "食材" "雜碎" "蒜香骨" "海鮮"
      "白斬雞" "白切雞"
      (bread-like ;; "包" also in container as bag
       )
      "甜食" "速食" "山珍" "海味"
      "小吃" "素食" "食品" "肉食" 
      "湯料"
      "麵粉" "面粉" "白麵" ;; 白面??
      "滷味" "滷"
      "美食" "補品" "乳制品" "奶制品"
      (vitamin
       "維生素" "維他命"
       "維他命A" "維他命B" "維他命C" "維他命D" "維他命E" "維他命K"
       "維生素A" "維生素B" "維生素C" "維生素D" "維生素E" "維生素K"
       "維A" "維B" "維C" "維D" "維E" "維K")
      (canned-food "罐頭")
      ((plate-like-food plate-like)
       "蛋糕" "糕" "糕點" "月餅" "餅"  "酥餅" "餡餅" "豆腐"
       "乾酪" "披薩" "餅乾" "餡兒餅" "陷兒餅")
      ((liquid-like-food liquid)
       "湯" "冰" "羹" "冰淇淋" "雪糕" "冰激凌" "蜜" "蜂蜜")
      ((long-thin-food long-thin)
       ;; can be quantified with 條
       "麵包" "面包" 
       "熱狗" "油條" "薯條" "魚翅"
       (noodle-like
        ((noodle-word-mian4-ind ind)
         "面" ;; idiot simplified chinese has this character for noodle, this character is also for 'face' and 'aspect'
         )
        ((noodle-like-ind ind) "麵")
        ;; also to explicit list out the substitution of 面 and 麵
        "壽面" "壽麵" "長壽面" "長壽麵" "泡面" "泡麵" "卷面" "卷麵"
        "方便面" "方便麵" "面線" "麵線" "面條" "麵條"
        ;;
        "粉絲" "米線" "米粉" "板條" "粄條" "河粉"))
      ((ball-like-food ball-like)
       ;; sufficiently like a ball, can be quantified with 粒, 顆
       ((meal-or-rice meal-like-event) "飯")
       "巧克力" "朱古力"
       "米飯" "餃" "餃子" "山楂" "糖" "糖果" "喜糖" "粽"
       "圓子" "饅頭" "粉團" "貢丸" "果仁"
       "麵團" "面團")
      (meal
       "餐" "餐點" "菜餚" "餚" "酒菜" "酒席" "膳"
       "膳食" "套餐" "料理"
       ((meal-event meal-like-event)
        "中餐" "西餐" "早餐" "午餐" "晚餐"
        "早飯" "午飯" "晚飯" "早點" "午點" "晚點"
        "茶點" "宵夜" "夜宵" "消夜" "夜消"))
      ((edible-part body-part) ;; animal parts
       "烤鴨" "涮羊肉" "烤全羊"  "炒雞丁"
       "肉" "白肉" "蛋白" "蛋黃" "肉鬆"
       "乳鐵蛋白" "臘肉" "五花肉" "面肉" "火腿"
       "魚肉" "烤肉" ;; need special mentioning as a noun, because it is also a verb
       ((edible-long-thin-body-part long-thin)
        "肉絲" "紅燒魚" "香腸")
       ((edible-ball-like-body-part ball-like)
        "蛋" "雞蛋"))
      ((food-plant plant)
       "桂皮" "大蒜末"
       ((long-thin-food-plant long-thin)
        ;; can be quantified with 條
        "魚腥草" "辣椒" "花椒" "姜" "參" "人參" "高麗參"
        "椒" "藕" "蓮藕" "銀耳" "木耳" "雪耳" "雲耳"
        "豆角"
        (mushroom
         ((mushroom-ind ind) "菇")
         "菇頭" "香菇頭" "蘑菇")
        (vegetable
         ((vegetable-ind ind) "蔬" "菜" "瓜" "薯" "茄" "苗")
         "甜菜" "菾菜" "菾"
         "筍" "雨後春筍" "萵" "苣" "萵苣" "菠菜"
         "青菜" "酸菜" "白菜" "蔬菜" "細菜" "素菜"
         "竹筍" "蘿蔔" "白薯" "番薯" "地瓜" "苦瓜"
         "芥菜" "菜頭" "胡蘿蔔" "木薯" "匏" "匏瓜"))
       ((ball-like-food-plant ball-like)
        ;; sufficiently like a ball, can be quantified with 粒, 顆
        "米" "大米" "小米" "糙米"
        "芋頭" "核桃仁" "穀物" "花生" "玉米" "穀粒"
        "檳榔" "種籽" "大蒜" "蒜" "棗" "稻米" "長生果"
        "橡子" "粟" "粟米" "栗" "杏" "糯米" "芋" "山芋"
        "葫蘆" "蒲瓜" "扁蒲" "瓠瓜" "瓠子" "蒲仔" "匏仔"
        "瓠" "西葫蘆" "葛" "粉葛" "洋蔥" "蓬" "蓮蓬"
        "馬鈴薯" "豆" "芝麻" "香米"
        (word-grain "穀"))
       (fruit
        ((fruit-or-resulting-effect resulting-effect) "果")
        "水果" "生果" "果實"
        "佛手柑" "佛手" "五指柑" "福壽柑" "密羅柑"
        ((banana long-thin) "香蕉" "蕉")
        ((ball-like-fruit ball-like-food-plant)
         ;; sufficiently like a ball, can be quantified with 粒, 顆
         "橄欖" "柑" "桔" "柑桔" "話梅" "酸梅" "檸"
         "蘋果" "葡萄" "西瓜" "獼猴桃" "橘" "葡提" "提子"
         "草莓" "士多啤梨" "藍莓" "柿" "柿子" "奇異果" "枇杷"
         "紅桑子" "橙" "檸檬" "青檸" "西柚" "柚" "柑橘" "蜜瓜"
         "哈密瓜" "杏桃" "車厘子" "桃駁李" "水蜜桃" "布冧" "牛油果"
         "椰子" "火龍果" "榴槤" "番石榴" "石榴" "龍眼" "荔枝" "山竹" "木瓜"
         "蕃荔枝" "西梅" "榴蓮" "楊桃" "大樹菠蘿" "芒果" "菠蘿" "李子"
         "蓮霧" "楊山梨" "鴨梨" "白梨" "鴨嘴梨" "京白梨"
         "萊陽茌梨" "碭山酥梨" "雪花梨"
         
         ((ball-like-fruit-ind ind) "桃" "莓" "梨")
         )))
      ((medicine-oral medicine)
       ((medicine-oral-ind ind) "藥" "丹" "膏")
       ((medicine-name name)
        "银翘" "银翘散"
        )
       "草藥" "丸散膏丹" "大補帖" "抗生素" "阿膠"
       "藥片" "避孕片" "成藥" "靈丹" "妙藥" "靈丹妙藥"
       "中藥" "西藥" "膏藥" "藥膏" "醫藥"))
     ;; other belongings or objects
     "塗料" "苯"
     (@ material
        "陶" "瓷" "陶瓷" "玉" "釉" "蠟" "橡皮"
        ((long-thin-material long-thin)
         "化纖" ;; shorthand for 化学纤维
         "维尼龙" "尼龙" "纖維")
        ;;
        (element-name ;; names of chemical element, no better place to put them
         "鉝" "鏌" "鈇" "鎶" "錀" "鐽" "䥑" "鑪" "鍩"
         "鍆" "鐨" "鎄" "鑀" "鐦" "鉲" "錇" "鉳" "鋦"
         "鎇" "鋂" "鈈" "鈽" "鎿" "錼" "鈾" "鏷" "釷"
         "錒" "鐳" "鈁" "鍅" "砹" "砈" "鉍" "鉈" "鎦"
         "鐿" "銩" "鉺" "鈥" "鏑" "鋱" "釓" "銪" "釤"
         "鉕" "釹" "鐠" "鈰" "鑭" "鋇" "銫" "碘" "碲"
         "銻" "銦" "鍀" "鍶" "銣" "溴" "硒" "砷" "鍺"
         "鎵" "硫" "磷" "矽" "硅" "硼" "鈹")
        ;;
        "貧鈾" "貧化鈾"
        ;;
        (@ metal
           "金屬" "五金" "不鏽鋼" "鋼鐵"
           ((metal-element-name element-name)
            ((gold-or-cost cost-or-budget)
             ((gold-or-cost-ind ind) "金"))
            "銀" "鈧" "鈦" "釩" "鉻" "錳" "鐵" "鈷" "鎳" "錫"
            "銅" "鋅" "釔" "鋯" "鈮" "鉬" "鎝" "釕" "銠" "鈀"
            "銀" "鎘" "鑥" "鉿" "鉭" "鎢" "錸" "鋨" "銥" "鉑"
            "汞" "鐒" "鋼" "釙" "鉛" "鈉" "鋰" "鈣" "鉀" "鎂"
            "鋁"))
        (cloth "布" "錦緞" "布料" "棉" "麻" "紗" "絲織" "絲"
               "紗布" "布匹" "錦" "絹" "帛" "絹帛" "綢緞"
               "天絲棉" "天絲" "紡" "綿" "刺繡"
               ((fig-leaf-like animate-attribute) "遮羞布" "羞布"))
        (stone
         ((stone-ind ind) "石")
         "礁" "石英"
         "角闪石" "礫" "石灰" "基石" "他山之石"
         "石頭" "化石" "雨花石" "岩" "砂" "砂岩")
        (@ soil
         ((soil-ind ind) "泥")
         "混凝土" "塵土" "墩" "橋墩" "淤" "沙" "沙丘"
         "泥濘" "泥土" "土壤" "土坷拉" "土丘" "泥丘")
        ((material-wood plant) "木" "木頭")
        "塑膠" "膠" "水泥" "磚" "磚頭" "瓦"
        "磚瓦" "玻璃" "琉璃" "塑料" "塑" "磁砖"
        "搪瓷" "琺瑯" "佛菻" "佛郎" "拂郎"
        ;;"紙" also elsewhere
        ((long-thin-plant-material long-thin-plant)
         "竹" "藤")
        "皮革"
        ((body-part-material body-part) "皮" "骨" "骨頭")
        )
     (seat-like
      (stair
       ((stair-ind ind) "梯")
       (word-down-stairs
        ;; has metaphorical meaning besides literal stairs
        "下台階")
       "階梯" "石階" "樓梯" "雲梯" "梯級" "樓梯級" "台階"))
     (has-roof-like ;; can be quantified with 頂
      ;; some sort of cover
      ((jiao seat-like) "轎子" "轎" "花轎")
      "飛行傘" "帳棚" "篷" "棚" "傘" "拖曳傘"
      "降落傘" "華蓋" "頂拱" "拱頂")
     (small-plate-like ;; small enough that can be quantified with 枚
      ((coins metal-currency) "錢幣" "硬幣")
      "CD" "VCD" "DVD" "光盤" "光碟" "磁碟" "磁盤"
      "CD-R" "CD-RW")
     (@ valuable-treasure
      ((valuable-treasure-ind ind) "寶" "珍")
      ;; 藏 could be shorthand for 西藏, or collected valuables, so elsewhere
      "典藏"
      "寶藏" "油藏" "館藏" "寶貝" "收藏" "珍藏"
      "法寶" "珍寶" "瑰寶" "古玩" "古董" "珍玩")
     (wearable
      ((jewelry valuable-treasure)
       "珠寶" "寶石" "鑽石" "珍珠" "瑰" "明珠"
       "瑪瑙")
      (shoe-like
       (stilts "高蹺" "蹺")
       "鞋" "旅游鞋" "靴" "拖鞋" "高跟鞋")
      "眼鏡" "墨鏡" "服飾"
      "耳環" "飾" "首飾"
      ((ring small-plate-like)
       ((ring-ind ind) "戒")
       "戒指" "鑽戒" "指環")
      ((necklace long-thin) "項鏈")
      (clothing
       ((clothing-ind ind) "服" "巾" "衫" "衣" "袍" "裝" "裳")
       "絲綢" "西服" "羽絨服" "長衫" "睡衣"
       "服裝" "大衣" "馬褂" "棉袍"  "將軍服"
       "衣服" "旗袍" "襯衫" "風衣" "披風"
       "夹克" "比基尼" "時裝" "衣冠" "外套"
       "古裝" "便服" "制服" "燕尾服" "裝束"
       "衣着" "婚紗" "面紗" "披肩" "便衣"
       "襯衣" "襖" "便裝" "西裝" "衣裳"
       "襟" "內衣" "上衣"
       ((package-or-set-cloth abstract) "套裝")
       ((long-thin-clothing long-thin) ;; can be quantified with 條
        "圍巾" "毛巾" "裙擺" "圍脖" "褲衩" "衩" "褲襠" "襠"
        (word-ling3 "領")
        ((long-thin-clothing-ind ind) "褲" "裙" "襪" "袖"))
       ((hat has-roof-like)
        ((hat-ind ind) "帽" "冠" "冕")
        "冠冕" "王冠" "箍" "沿帽"
        )))
     (@ furniture
        ;; 座 is a stand-like thing, but also has multiple other meanings, so put elsewhere
        ((furniture-ind ind) "櫃" "架" "桌" "具")
        (furniture-tai2 "枱")
        "家具" "神壇" "祭壇" "底座"
        "書架" "書櫃" "抽屜" "藏櫃"
        "裝飾" "擺設"
        (seat-like-furniture
         "凳" "椅" "板凳" "床" "臥" "臥鋪" "沙發"
         (clay-bed "炕")))
     (bedding
      "寢具"
      "床罩" "枕頭" "床單" "枕"
      (blanket-like
       "被子" "褥" "毛毯" "被褥" "涼被" "棉被" "被單"
       "草蓆" "蓆" "蓆子" "毯" "氈" "被窩")
      )
     (coffin "棺" "棺材" "棺木" "靈柩" "柩")
     (@ paper-like ;; can be quantified by 張
      (memo "備註" "備忘錄")
      (form
       "表格"
       ((form-ind ind) "表"))
      ((diaper wearable) "尿片")
      ;;"紙" also elsewhere
      "名片" "信用卡" "明信片" 
      (list-or-brochure
       (name-list "名單" "黑名單" "白名單")
       ((brochure-ind ind) "單")
       (order-request "訂單")
       "傳單" "清單" "菜單" "單張")
      ((paper-like-evidence evidence legal-document)
       "牒" "戒牒" "通牒" "度牒" "尺牒"
       "白條" "執照" "駕照" "牌照"
       "产证" "文憑" "單據" "字據" "收據" "票據"
       "借書證" "結婚證" "護照" "身份證" "憑證"
       "簽證" "認證"
       ((paper-like-evidence-ind ind) "證"))
      ((paper-like-ind ind)
       "卷" "券" "卡" "帖" "箋")
      "說帖" "紙張" "獎狀"
      "考卷" "試卷" "獎券" "彩券"
      (stock
       ((stock-ind ind) "股")
       "Ｎ股" "Ｈ股" "Ａ股" "Ｂ股" "藍籌" "紅籌"
       "股票" "證券" "股份"
       "股條" ;; only similar to stock
       "期貨" "持股"
       )
      (bond-financial
       "債券"
       "公債" "社債" "公社債" ;; some Japanese nouns, some sort of bonds
       )
      ((paper-cash currency)
       "錢" "金錢" "銀票" "鈔票" "支票")
      ((picture creative-work)
       "圖" "畫" "圖案" "幻燈片" "圖像" "圖象"
       "畫圖" "畫像"
       (photo ;; can be used with "幀"
        ((photo-or-appearance appearance) "相")
        "圖片" "照片" "相片" "样片" "自拍"
        "寫照" "寫真" "照" "沙龍" "合影" "合照")
       ((plan-picture plan)
        "藍圖" "宏圖" "草圖")
       "肖像" "腦電圖" "心電圖" 
       "地圖" "水彩" "字畫" "中國畫" "油畫"
       "山水畫" "海報" "畫報" "條幅" "横幅")
      (letter
       ;; 信 could also mean trust, but treated separately
       ;; nowadays 信 could also mean letter-like short message
       ((letter-ind ind) "信" "函" "札")
       ((letter-or-can-be-fang4 can-be-fang4)
        ;; * + 電 is something like a letter
        "來電"
        )
       ((letter-reply reply) "回信" "回函")
       "傳真" "回條"
       "魚雁" "來函" "來信" "留言" "通告"
       "來鴻" "函電" "信箋" "電郵" "電子郵件"
       "信函" "文函" "黑函" "手札" "字條"
       "信件" "名信片" "情书" "短信" "挂件"
       "郵件" "通知單" "遺書" "照会")
      "信封"
      (ticket-or-vote
       "站票"
       ((ticket-or-vote-ind ind) "票"))
      "門票" "糧票" "彩票" "發票" "傳票"
      ((stamp small-plate-like) "郵票" "郵")
      ((paper-like-sign sign-signal)
       ((paper-like-sign-ind ind) "符")
       )
      (@ book-like
       (@ book-like-record
        ((book-like-record-ind ind) "錄" "記" "譜" "傳")
        (registration "登記")
        "語錄" "記錄" "傳記" "史記" "後記" "日記"
        "筆記" "樂譜" "菜譜" "食譜" "傳記" "警世錄"
        "名錄" "藥方" "處方" "筆錄" "紀錄" "手記"
        "扎記"
        )
       ((book-legal-doc-character-style legal-document character-style)
        ;; 書 is quite overloaded
        ((book-character-style-ind ind) "書"))
       ((book-ind ind) "書本" "經" "册" "刊" "書目" "著")
       ((cost-or-book-like cost-or-budget)
        ;; 本 could also mean cost as in 成本, or the origin, fundamental as in 根本
        ((cost-or-book-ind ind) "本"))
       "教科書" "讀本" "副刊"
       "指南" "日曆" "月曆" "年曆" "辭庫" "詞庫" "文獻"
       "字典" "法典" "經典" "辭典" "手册" "簡冊" "報刊"
       "書籍" "刊物" "圖書" "課本" "雜誌" "詞典"
       "古書" "漫畫" "藏書" "金剛經" "佛經" "百科全書"
       "集子" "佚書" "中书" "譯本" "周刊" "期刊"
       "情報誌" "典籍" "古籍" "本草" "本草經" "名著")
      )
     (document "文件" "檔案" "公文" "批件" "要件" "訴件"
               "附件" "卷宗" "札記" "文檔" "資料檔"
               "簡報")
     (machine
      ((machine-ind ind) "機" "器" "儀")
      "機器" "機器人" "總機" "單機" "主機"
      "ATM機" "儀器" "怪手" "雷達" "械" "機械"
      "雲霄飛車" "器材" "渾像儀" "渾天儀"
      "地動儀" "機關" "引擎" "泵" "機濾" "馬達"
      "摩打" "摩達" "錠" "錠子" "顯微鏡" "潛望鏡"
      "渾象儀" "設備" "裝置"
      (clock-like
       ((watch wearable small-plate-like) "錶" "手錶")
       "鐘" "鐘錶" "喪鐘" "鬧鐘" "上課鐘")
      (can-fly
       ;;"箭"
       ;; also add some names
       ((rocket-name nr-name) "和平号" "神州号")
       )
      (transportation
       "交通工具" "橇" "雪橇"
       ((flying-vehicle can-fly)
        "火箭" "戰鬥機" "戰機" "軍機")
       (scheduled
        ((plane can-fly)
         "飛機" "班機" "航機" "民機" "包機"
         "民航機" "航班"))
       (wheeled
        ((wheeled-ind ind) "車" "汽車" "車輛")
        "輪椅" "出租車" "警車" "卡車" "轎車"
        "三輪車" "貨車" "箱行车" "舳车" "油罐车"
        "纜車" "計程車" "私車" "愛車" "样车"
        "拖板车" "野机车" "吉普車" "推車" "彩车"
        "座车" "私家車" "包車" "賓士" "座駕"
        (bike-like
         "自行車" "機車" "腳踏車" "摩托" "摩托車")
        (horse-powered-wheel "馬車")
        ((scheduled-wheeled scheduled)
         "公共汽車" "班车" "大巴" "大巴車" "中巴"
         "中巴車" "公车" "公交车" "公交" "巴士"
         (train-like
          "火車" "地鐵" "地下鐵" "列車" "高鐵"
          "捷運" "专列"
          ((train-like-name nr-name)
           "复兴号" "自强号"))))
       (ship
        ((ship-ind ind) "舟" "艦" "艇" "筏" "舶" "舫")
        "舳艫" "艨艟" "舢舨"
        "漁輪" "渡輪"
        "輪駁" "頂推駁" "駁船" "駁艇" "駁輪"
        (word-cargo-ship "貨輪")
        "艦支" "船支" "艇支" "舟支" "航母" "航空母艦"
        "輪機" "輪船" "潛艇"
        ((ship-name nr-name)
         "科尔号" "库尔斯克号" "阿玛斯号" "库勒号")
        "基德艦" "戎克船"
        ((scheduled-ship scheduled)
         ((scheduled-ship-ind ind) "船")
         ((scheduled-ship-name nr-name)
          "铁达尼号")
         "飛船" "班輪" "油輪")))
      (electric-appliance
       "電器" "路由器" "家電" "白電" "機頂盒"
       "軟盤" "硬盤" "鍵盤" "硬碟" "軟碟"
       "快譯通" "遙控" "機電"
       (little-electronics
        "二極體" "二極管" "記憶體" "半導體" "IC"
        "DRAM" "RAM" "D-RAM" "GPS" "CPU" "内存"
        "電池" "U盤" "開關"
        )
       (tele-communication
        ((teletype info-report letter) "電報")
        "電話" "話機" "耳機" "麥克風" "隨身聽" "大哥大" "免提")
       (computer-like
        (word-mobile-phone "手機")
        "電腦" "微机" "處理器" "伺服器")
       (tv "TV" "彩電"
           ((tv-can-be-job can-be-job) "電視")
           (monitor-like "顯示屏" "熒屏" "熒光幕" "熒幕" "屏幕"
                         "顯像管"))
       "電梯"
       "電冰箱" "空調" "電飯鍋" "電熨斗" "熨斗" "照相機" "冰箱"
       "清新機" "掌機")
      (bulb-like
       ((bulb-like-ind ind) "燈")
       "走馬燈" "燈泡"
       ((light-bulb electric-appliance stick-like)
        "電燈" "日光燈" "霓虹燈"))
      )
     (@ beam-like ;; can be quantified with 道
        ((long-thin-beam long-thin) "傷口" "條紋")
        (light
         ((only-light can-be-fang4) "光" "光線")
         "虹"
         "激光" "熒光" "光芒" "霞" "霞光" "霞帔" "彩虹"
         "燈光" "陽光" "月光" "埃克斯光" "X光" "Ｘ光"))
     ;;
     (medal
      "勳章" "獎章" "肩章")
     (@ tool
        "工具" "用具"
        ;; TODO: ?? these are both noun for tool, and verb
        "鋸" "錘" "銼" "鏟" "釘" "扣" "墊"  "刷"
        "鋤" "鍘" "犁" "鉤" "磨"
        (ring-like-tool "圈")
        ((stick-like-tool stick-like)
         ((stick-like-tool-unit ind) "棍" "杆" "棒")
         "匙" ;; could mean 'key' or 'spoon'
         (key "鑰匙" "鑰")
         )
        ((wearable-tool wearable)
         (mask-like
          ((mask-like-ind ind) "罩")
          "口罩" "臉罩" "面罩" "胸罩" "乳罩"
          )
         )
        ;;
        "鋤頭" "榔頭" "天平" "螺絲刀" "螺絲批" "剪刀"
        "鐮" "簧" "彈簧" "枷" "枷鎖" "舵" "桎" "梏"
        "桎梏" "按鈕" "鈕" "手銬" "鐐" "銬"
        "鐐銬"
        "鋒" ;; the sharp part of a knife, not usually used alone
        ;; *+計 could mean a measuring tool, 計 also means a strategy
        ;;
        (wheel-like
         ((wheel-or-ship ship)
          ;; 輪 could be short for 輪船
          ((wheel-or-ship-ind ind) "輪"))
         "輪胎" "車胎" "齒輪")
        (tool-stamp
         "印章" "帥印" "璽" "公章" "閒章"
         ((chapter-or-law-or-stamp-or-medal chapter-or-law medal) "章")
         (footprint-or-stamp
          ;; 印 could mean a physical thing for stamping
          ;; or something like a footprint or fingerprint
          ((footprint-or-stamp-ind ind) "印")
          "酒印"))
        ((tool-plate-like plate-like)
         "豐碑")
        ((tool-ball-like ball-like)
         "螺絲" "塞" "瓶塞" "栓" "螺栓" "啞鈴")
        ((tool-long-thin long-thin)
         ((rope long-thin)
          "繩" "索" "䌫" "橡皮筋" "繩索" "索䌫" "繫繩")
         "簡" "牘" "簡牘" ;; for writing, somehow like paper
         ((needle-or-medicine-injection medicine-injection ind)
          "針")
         "槓鈴" "引信"
         "樁" "竿" "光纖" "指南針" "指針"
         (word-chain "鏈")
         "軸" "軸心" "握把" "秤" "杖"
         "拐杖" "粘搭條" "魔術粘" "魔鬼沾"
         "魔鬼氈" "魔術貼" "黏扣帶" "電焊條" "柄")
        (lock "鎖" "鎖頭")
        (luggage-like "行李")
        (@ container
         ((container-ind ind)
          "桶" "槽" "皿" "器皿" "筐" "筒" "篝"
          "籃" "罐" "箱" "簍" "盒" "瓶" "匣" "籠" "壺")
         (@ large-container
          ;; has large opening, can be quantified with 口
          "缸" "甕" "溫泉池" "魚塭" "池"
          (well-like "井" "水井" "泉井" "泉"))
         ((bottle-or-furniture furniture)
          ((bottle-or-furniture-ind ind) "壇"))
         (bag
          ((bag-or-bread bread-like)
           ((bag-or-bread-ind ind) "包"))
          ((bag-ind ind) "囊" "袋" "袱")
          (folder-like
           ((folder-like-ind ind) "夾")
           "公文包")
          "包包"
          "背包" "書包" "行囊" "包袱" "袱子" "口袋")
         "荷包" "信箱" "夜壺" "紋壺" "尿鱉" "容器" "漏斗")
        ;;
        "網球拍" "球拍" "螺旋槳" "槳"
        "器用" "大用" "算盤" "砝碼" "面罩"
        ;;
        (cooking-utensil
         ((cooking-container container) "鍋")
         "廚具" "煮食用具"
         "爐" "刀俎" "俎" "灶")
        (eating-utensil
         "餐具"
         "筷" "刀叉" "叉" "箸"
         ((container-utensil container)
          ((cup-or-competition competition) "杯")
          "瓢" "飯碗" "碟" "觥" "罌" "缽"
          "盆" "盤" "碗" "酒杯" "馬克杯"))
        (stationery
         "文具"
         ((piece-of-paper paper-like material)
          "紙" "紙頭")
         "筆" "墨" "毛筆" "尺" "粉筆" "粉筆頭" "標尺"
         "硯" "鉛筆")
        (armor
         ((plate-like-armor plate-like) "盾")
         ((hat-armor hat) "盔" "頭盔")
         "甲" "盔甲" "鎧" "鎧甲"
         )
        (weapon
         "武器" "兵器" "核武" "弓" "弩"
         "彈藥" "炸藥" "火藥" "軍火"
         ((car-like-weapon wheeled)
          "坦克")
         ((long-thing-weapon long-thin)
          "鞭" "教鞭" "鐧")
         (blade-like
          ((knife eating-utensil)
           ((knife-ind ind) "刀" "刃")
           "兩面三刀" "菜刀" "刀刃")
          ((blade-like-ind ind) "劍")
          "太極劍" "斧" "矛" "匕首"
          )
         ((bullet-like can-fly)
          ((bullet-like-ind ind) "箭" "彈")
          "炸彈"
          "箭頭" "彈頭" "鏢" "炮雨" "彈雨" "水雷"
          "砲" "核彈" "榴彈" "母彈" "霰彈" "魚雷" "地雷"
          "子彈" "砲彈" "雷射" "羽箭" "飛彈" "手榴彈")
         (canon
          ((canon-ind ind) "炮")
          "火炮" "迫擊炮")
         ((gun stick-like) ;; can be quantified with 杆
          ((gun-ind ind) "槍")
          "火藥槍" "機關槍" "槍支" "突火槍" "槍林"))
        ((for-smoking stick-like) "煙槍")
        "煙頭"
        (mirror "鏡" "鏡子")
        ((ring-maybe-unit ind) "環")
        "梳" "聽診器" "扇" "磨扇" "石磨")
     ((musical-instrument audible)
      "樂器"
      "琴"
      (drum ;; can be 敲, 打
       ((drum-ind ind) "鼓")
       "鈴" ;; *+鈴 could also mean gym equipment such as dumbells
       "鐺"
       "打擊樂器" "敲擊樂器"
       "花鼓" "博浪鼓" "撥浪鼓" "不浪鼓"
       "鑼" "定音鼓" "非洲鼓" "管鐘" "馬林巴" "鈸"
       "小鼓" "大鼓" "爵士鼓" "編鐘")
      ((keyboard machine) ;; can be 彈
       "鍵盤樂器"
       "鋼琴" "風琴" "管風琴" "古鋼琴" "羽管鍵琴"
       "手風琴" "口風琴")
      (pluck-string ;; can be 彈
       "撥弦樂器"
       "結他" "古典結他" "民謠結他" "電結他" "豎琴" "低音結他"
       "烏克麗麗" "贝吉" "贝斯" "吉他"
       ;; chinese style
       "古琴" "古瑟" "瑟" "箏" "箜篌" "阮咸" "阮" "月琴"
       "琵琶" "柳琴" "三弦")
      (woodwind-brass ;; can be 吹
       "木管樂器"
       "單簧管" "雙簧管" "英國管" "長笛" "短笛" "口琴" "巴松管"
       "薩克斯風" "鼻笛"
       "銅管樂器" "小號" "短號" "法國號" "長號" "次中音號"
       "上低音號" "低音號" "號角"
       ;; chinese style
       ((xiao1-or-sound sound) ;; 籟 is also a type of 簫
        "籟")
       "排簫" "洞簫" "笙" "竽" "塤" "箎" "簫" "笛" "笳")
      (bow-string ;; can be 拉
       "弓弦樂器" "提琴"
       "小提琴" "中提琴" "大提琴" "低音提琴" "低音大提琴"
       ;; chinese style
       "京胡" "高胡" "板胡" "二胡" "中胡" "大胡" "革胡"
       "墜胡" "擂琴" "二弦" "四胡" "椰胡" "南胡"))
     ;;
     (wall
      "牆" "壁" "牆壁" "垣" "殘桓斷壁" "圍牆"
      (fence
       ((fence-or-monitor monitor-like) "屏" "幕" "障")
       "柵欄" "柵" "屏風" "籬" "屏藩" "屏障"
       "藩籬" "籬笆" "檻" "路障" "笆" "隔"
       ((fence-or-field-or-column field-or-column)
        "欄")
       ((fence-or-layout inanimate-attribute)
        "間隔")
       )
      )
     (specimen "標本")
     (@ fluid-like ;; can be quantified with 股
        "氣流" "烽" "烽煙"
        ((stick-like-fluid-like for-smoking)
         ((smoke-plant plant) "煙" "菸")
         "卷煙" "雪茄")
        (@ smell
           (word-smell-xiang1 "香") ;; TODO: 香 also refers to something to burn
           "氣味" "花香" "香味" "銅臭" "芳菲"
           "芳馥" "馥" "芳馨" "馨" "芬芳" "菲"))
     (door-like
      (word-door-like-hu4 "户")
      (word-door-like-guan1 "關")
      ((door-like-or-abstract-threshold attribute)
       "門檻")
      "門" "關卡" "路卡" "閘" "窗" "捲門"
      "前门" "閨" "門戶" "窗户" "要塞" "隘"
      "關隘" "扉" "閥")
     (fire "火" "火炬" "炬" "營火" "焰" "火焰" "火舌" "火苗"
           "火花" "火種")
     ((natural-phenomena disaster)
      ((natural-phenomena-ind ind)
       "災難" "災害" "災" "難" "害" "禍" "荒" "殃")
      "自然現象" "自然災難" "天然災難"
      "山呼海嘯"
      "天然災害" "天災" "天禍" "人禍" "天患" "災荒"
      "地震" "震" "泥石流" "山崩" "山泥傾瀉"
      ((rain liquid)
       ((rain-ind ind) "雨" "霖")
       "甘霖"
       "雨點" "暴雨" "陣雨" "風風雨雨"
       "雨災" "對流雨" "鋒面雨" "風雨")
      ((flood liquid)
       "大水" "水患" "水災" "洪災" "洪水" "海嘯" "汛" "洪")
      (snow-like
       ;; *+霜 could be cream-like makeup-product
       "雪" "雪花"
       "大風雪" "暴風雪" "雪崩" "雹暴" "冰雹")
      "雷擊" "雷暴" "閃電" "雷" "雷電"
      ((wind fluid-like)
       ((wind-ind ind) "風")
       "風暴" "沙暴" "疾風" "陣風"
       "陰風" "旋風" "氣旋" "暴風" "微風" "勁風" "狂風"
       "風災" "颶風" "熱帶氣旋" "龍捲風" "颱風" "沙塵暴")
      "全球暖化" "熱浪" "旱災"
      (disease
       ((disease-ind ind)
        "病" "疫" "症" "症候群" "綜合徵" "炎" "疾"
        "疾病")
       ((disease-body-part body-part)
        "癌" "青光眼"
        ((ball-like-disease-body-part ball-like)
         ((ball-like-disease-body-part-or-essence
           essence-of)
          "癥結")
         "囊腫"
         "瘡" "癰瘡" "癰" "疽" "癰疽" "皰疹" "皰" "疹"
         "心疱" "瘤" "肌瘤" "腫瘤" "疙瘩" "疙" "瘩"))
       ((disease-name name)
        "肌無力" "瘟" "愛滋" "骨質疏鬆" "情绪紊乱"
        "气包卵" "疝氣" "小腸氣" "心肌梗塞" "腦血栓"
        "栓塞" "黃疸" "瘧疾" "帕金森" "艾滋" "天花"
        "痲瘋"
        )
       ;; TODO: whether to add a noun-mod for "noun adj" as a pred?
       "巴金森氏症" "不治之症" "黑死病" "出血" "溢血"
       "傳染病" "SARS" "伊波拉病毒" "流行病" "瘟疫"
       "流感" "流行性感冒" "感冒" "傷風"
       "近視" "遠視")
      ((fire-disaster fire)
       "火警"
       "火災" "森林大火" "大火" "山火" "火山災害" "火山爆發")
      "飢荒" )
     (@ wave
        ;; "潮" could also mean trend, so also elsewhere
        "波" "浪" "浪頭" "濤" "漩" "渦" "漩渦" "漣" "漪" "漣漪"
        "浪潮" "黑潮" "瀾" "波瀾")
     (flower-like
      "雲" "雲朵"
      ((flower-like-wave wave) "浪花"))
     ((wire long-thin) "高壓電線")
     ((rail long-thin) "軌")
     (@ star-like
      (sun-word "日")
      (moon-word "月")
      ((star-like-stone stone) "隕石")
      "星斗" "落日" "衛星" "彗" "旭" "旭日"
      "人造衛星" "月亮" "流星" "彗星" 
      "烈日" "昭昭天日" "天日" "天體")
     (flag
      ((flag-or-competition competition) "錦標")
      "旗" "飛虎旗" "旗幟" "幡" "幟")
     ((chess-like-game event)
      ((chess-like-game-ind ind) "棋")
      "圍棋" "陸軍棋" "中國跳棋" "跳棋" "雙陸棋" "鬥獸棋" "區字棋"
      "五子棋" "拼六棋" "連珠" "西洋跳棋" "五子飛棋" "暗棋" "暗獸棋"
      "象棋" "中國象棋" "朝鮮象棋" "將棋" "日本將棋" "西洋象棋"
      "國際象棋" "播棋" "黑白棋" "同化棋" "古西亞連子棋" "拱橋棋"
      "企鵝棋" "蛙棋" "蘋果棋"
      "橋牌" "扑克"
      (mahjong "麻將"))
     (statue
      ((word-statue video)
       ((word-statue-or-appearance appearance) "像")
       "俑" "偶")
      "布偶" "木偶" "玩偶" "模型"
      "如意" ;; not exactly, but similar
      "秦俑" "兵馬俑"
      "塑像" "雕像" "銅像" "佛像" "神像")
     (lid "蓋")
     ((poison can-be-fang4) "毒" "劇毒")
     (thing "東西" "有的没的") ;; a general reference
     ;;
     (@ road
      (@ street
       ((street-ind ind) "隧")
       ;; "道" is combined with theory-word-dao4
       "十字路" "街道" "胡同" "死胡同" "埂"
       "高速公路" "公路" "馬路" "隧道" "道路" "鐵道"
       "平郊道" "水道" "幹道" "坑道" "便道" "通道"
       "走道"))
     (@ net-like ;; physical net
        "網子")
     (sky
      "秋水長天" "楚天"
      ((physical-sky place)
       ((physical-sky-ind ind) "空")
       "空中" "天空" "夜空")
      ((sky-or-god-like god-like)
       "老天" "老天爺" "上天"
       ((sky-or-god-like-or-place physical-sky)
        ((sky-or-god-like-or-place-ind ind) "天"))))
     (sculpture
      ((sculpture-ind ind) "刻")
      ((sculpture-or-bird bird)
       ((sculpture-or-bird-ind ind) "雕"))
      "雕塑" "雕刻" "浮雕")
     (explosive-like
      "鞭炮" "爆竹" "爆竿" "炮仗" "煙花"
      )

     ;; not common, and easily confused with functional word
     ;; "的" ;; 箭靶的中心,

     ;; "inanimate" to be finely categorized
     "靶" "鍵" 
     "稻草人"
     ;;
     "包裹" 
     "搖籃" 
     "鼻煙壺"
     "壓歲錢"
     "玩具"   
     "衣架"     "喇叭"
     "窗簾"     
     "手帕"  "瑣屑"
     "掃帚" "絨毛"
     ;;
     "油彩"
     "窗花" "把手"
     (word-tan1 "攤")
     "晚報"
     "灰塵" "灰"
     (word-burnt-coke "焦")
     "帆"
     "框" "話筒" 
     "烤箱" "胚" "章子" "模" "套" "分子" "漁獲"
     (fertilizer
      ((fertilizer-or-fat fat)
       ((fertilizer-or-fat-ind ind) "肥"))
      "化肥" "肥料")
     (@ plant-part
        ;; 根 also has metaphorical meaning, in root-of
        ((long-thin-plant-part long-thin)
         "枝" "枝頭" "樹幹" "枝條" "株" "梢"
         "蔥白" "芽" "秧" "蒂" "蔓")
        "樹脂" "環氧樹脂" "葉" "竹葉" "花粉")
     (fuel
      "燃料" "火渣"
      "炭" "煤炭"
      ((fuel-element-name element-name) "碳")
      ((long-thin-fuel long-thin)
       "燭" "蠟燭" "香燭")
      ((plant-part-fuel plant)
       ((long-thin-plant-fuel long-thin)
        "柴" "火柴" "柴薪"))
      )
     (gas
      ((gas-ind ind) "氣" "氣體" "汽")
      ((gas-name name)
       ;; normally in gas form in room temperature and atmospheric perssure
       ((gas-element-name element-name)
        "氘" "氨"
        "氫" "氦" "氮" "氧" "氟" "氖" "氯" "氬"
        "氪" "氙" "氡")
       "一氧化碳" "乙硼烷" "丙烯"
       "乙烯" "乙烷" "甲烷" "磷化氢" "氯乙烷" "丁烷"
       "二氧化碳" "六氟乙烷" "六氟化硫" "一氧化二氮"
       "三氟化氮" "三氟溴甲烷" "二氟一氯一溴甲烷"
       "全氟三丁胺" "四氟化碳" "鹵代甲烷"
       )
      (pollution-gas
       "霧霾" "霾" "陰霾" "濁氣" "毒氣" "廢氣"
       (pollution-gas-PM
        (pollutant-particles
         "顆粒物" "微粒" "顆粒"
         )
        ;; these may be confused with time, e.g. "PM 10 o'clock"
        "PM2.5" "PM10")
       )
      "霧" "霧氣" "蒸汽" "蒸氣" "濕氣"
      "水氣" "汗氣" "暮氣"
      "尾氣" "寒氣" "大氣" "涼氣" "冷氣"
      "沆瀣" "屁" "暖氣" "空氣" "雲層" "大氣層"
      ;; fuels
      ((fuel-gas fuel)
       "燃气" ;; shorthand for 气体燃料!???!!!!!
       "煤氣" "瓦斯" "天燃氣" "天然氣" "油氣" "煤層氣"
       "液化氣")
      )
     (little-thing ;; 細碎的東西
      ((little-thing-ind ind) "星子"))
     (ore
      "硝" "瀝青" "硫磺" "礦物" "礦藏"
      ((fuel-like-ore fuel)
       "煤" "燃煤")
      ((ore-ind ind) "礦"))
     ((physical-line long-thin)
      ((physical-line-ind ind) "纜")
      "終點線" "綾線" "標線" "天線" "紫外線" "子午線"
      "下劃線" 
      ((line abstract-line)
       ((line-ind ind) "線")
       (railway-line "大兴线")
       "網線" "沿線" "正線" "幹線" "支線" "幹支線" "主線" "中線"
       "外線" "複線" "前線" "一線" "二線" "三線" "四線" "五線")
      )
     (goods
      ((goods-ind ind)
       ((goods-word-ind ind) "品")
       "物品" "物" "制品" "事物" "成品" "制成品")
      ((goods-product product)
       "產品" "副產品" "畜產品" "名產" "名產品"
       "特產" "特產品" "工藝品"
       "水產" "海產""水產品" "海產品")
      "萬物" "文物" "物資"
      "風物" "遺物" "贓" "證物" "禮物" "百物"
      "精品" "珍品" "樣品" "極品" "拍品" "展品"
      "獎品" "商品"
      (cargo "貨" "貨件" "貨物" "水貨" "百貨" "散貨")
      )
     (strip-like
      ;; 带 could mean a physical strip, or a place or region
      "水容带"
      ((strip-or-region-like region)
       "紐帶"
       ((strip-or-region-like-ind ind) "帶")
       ))
     (enzyme
      ((enzyme-ind ind) "酶")
      "蛋白酶"
      )
     ;;
     "垃圾" "風箏" "斑" "斑點" "空席" "零頭"
     "鏡頭" "磁頭" "瓦斯头" "插頭" "監控頭" "接頭"
     "攝像頭" "探頭" "物業" "裝甲" 
     "土石方" "脂粉" "核電"
     ;;
     "核" "顏料"
     "線材" "晶圓" "彎" "重器" "禮器"
     (parts
      ((parts-ind ind) "件")
      "原件" "元件" "元器件" "料件" "物件" "部件" "配件"
      "修理件" "硬件" "组件" "零组件" "零部件" "零件"
      "零配件"
      )
     (@ hole
        ((hole-ind ind) "孔")
        ((gap-like long-thin)
         ((gap-like-ind ind) "縫" "隙")
         "空隙" "縫隙")
        "彈孔" "毛細孔" "毛孔" "鑽孔" "孔洞")
     ((physical-foundation foundation)
      ((foundation-ind ind) "基")
      "地基" "根基" "橋基" "築基" "路基")
     (@ layer-like
        "塗層"
        (shell-or-casing
         ((shell-or-casing-ind ind) "殼")
         "外殼" "底殼" "彈殼" "玻殼" "空殼"))
     ((preparation-material fallback-or-preparation)
      "軍備" "戰備" "裝備" "武裝")
     (pointy-tip
      ((pointy-tip-ind ind) "尖" "刺")
      )
     (makeup-product ;; or skin care products
      ((stick-like-makeup-product stick-like) "口紅")
      ((plate-like-makeup-product plate-like) "面膜")
      ((makeup-cream-or-frost snow-like) "霜")
      "美乳霜" "面霜"
      )
     (@ tent-like ;; 帳 is elsewhere
        "蚊帳" "帷幕" "帳幕" "帷帳" "帷" "帷幄"
        "幄"
        )
     (crystal
      ((crystal-ind ind) "晶")
      "晶體" "水晶"
      ((crystal-or-abstract abstract) "結晶"))
     "硬體" "油门" "快門" "簾"
     "汽配" "粉" "皂" "肥皂" "鹼"
     "公仔" "道具" "面具" "乾冰" "冰凌" "凌"
     "鞦韆" "韆鞦" "秋千" "垢" "渣" "渣滓" "滓"
     "碴" "介質" "酯" "酸" "核酸" "醇"
     "膽固醇" "甲醛" "醛" "棱"
     (@ physical-twist
        "中國結" "繩結")
     (lever-like
      "排檔"
      ((lever-or-document document)
       ((lever-or-document-ind ind) "檔")
       ))
     (@ word-material
        ;; not different types of material, but the concept of physical
        ;; material itself
        "地材" "基材" "建材" "耗材")
     ;;
     ))

;; end inanimate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
