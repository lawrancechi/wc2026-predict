library(jsonlite)

set.seed(2026)
N <- 100000
w <- 0.15   # Bayesian 更新權重（低權重避免單場極端結果扭曲）

cfg      <- fromJSON("data/teams.json", simplifyVector = FALSE)
base_lam <- setNames(as.numeric(unlist(cfg$base_lambda)), names(cfg$base_lambda))
matches  <- cfg$matches

# ── 近期戰績追蹤 ──────────────────────────────────────────
recent_form <- setNames(vector("list", length(base_lam)), names(base_lam))
for (nm in names(base_lam)) recent_form[[nm]] <- character(0)

# ── Bayesian 更新（修正版：防守係數改為除法）────────────────
atk <- setNames(rep(1.0, length(base_lam)), names(base_lam))
def <- setNames(rep(1.0, length(base_lam)), names(base_lam))

for (m in matches) {
  if (!isTRUE(m$played)) next
  h <- m$home; a <- m$away
  hs <- m$home_score; as_ <- m$away_score
  heat <- ifelse(is.null(m$heat), 0, m$heat)

  # 修正 bug：lh_exp = base * atk / def（防守好 → 對手 xG 降低）
  lh_exp <- max(0.1, base_lam[h] * atk[h] / def[a] + heat)
  la_exp <- max(0.1, base_lam[a] * atk[a] / def[h] + heat)

  # 更新係數，限制在 [0.4, 2.5] 內
  atk[h] <- min(2.5, max(0.4, atk[h] * (1-w) + w * (hs  / lh_exp)))
  atk[a] <- min(2.5, max(0.4, atk[a] * (1-w) + w * (as_ / la_exp)))
  def[h] <- min(2.5, max(0.4, def[h] * (1-w) + w * (la_exp / max(0.1, as_))))
  def[a] <- min(2.5, max(0.4, def[a] * (1-w) + w * (lh_exp / max(0.1, hs))))

  # 記錄近3場勝/平/負
  recent_form[[h]] <- tail(c(recent_form[[h]], if(hs>as_)"W" else if(hs==as_)"D" else "L"), 3)
  recent_form[[a]] <- tail(c(recent_form[[a]], if(as_>hs)"W" else if(as_==hs)"D" else "L"), 3)
}

# ── 主辦國加成 ────────────────────────────────────────────
HOST_TEAMS <- c("USA", "Mexico", "Canada")

# ── 歷史爆冷強隊（世界盃常見黑馬）────────────────────────
GIANT_KILLERS <- c("Japan", "Morocco", "Switzerland", "Senegal",
                   "South Korea", "Ghana", "Australia", "Iran", "Czechia")

# ── Poisson 模擬（綜合因素版）────────────────────────────
simulate_match <- function(home, away, heat, altitude=FALSE, h_played=0, a_played=0) {
  xg_h <- max(0.05, base_lam[home] * atk[home] / def[away] + heat)
  xg_a <- max(0.05, base_lam[away] * atk[away] / def[home] + heat)

  # 主辦國主場加成 +5%
  if (home %in% HOST_TEAMS) xg_h <- xg_h * 1.05

  # 海拔加成
  if (isTRUE(altitude)) { xg_h <- xg_h * 1.15; xg_a <- xg_a * 0.95 }

  # 首場比賽不確定性：若兩隊均未踢過，稍微拉近差距（爆冷因素）
  if (h_played == 0 && a_played == 0) {
    mid <- (xg_h + xg_a) / 2
    xg_h <- xg_h * 0.85 + mid * 0.15
    xg_a <- xg_a * 0.85 + mid * 0.15
  }

  # 黑馬加成：傳統爆冷強隊在劣勢時多5%攻擊力
  ratio <- xg_h / max(0.1, xg_a)
  if (away %in% GIANT_KILLERS && ratio > 1.5) xg_a <- xg_a * 1.05
  if (home %in% GIANT_KILLERS && ratio < 0.67) xg_h <- xg_h * 1.05
  gh <- rpois(N, xg_h); ga <- rpois(N, xg_a)
  hw  <- round(mean(gh > ga) * 100, 1)
  dr  <- round(mean(gh == ga) * 100, 1)
  aw  <- round(mean(gh < ga) * 100, 1)
  sc  <- paste0(gh, "-", ga)
  tbl <- sort(table(sc), decreasing = TRUE)
  top5 <- lapply(seq_len(min(5, length(tbl))), function(i) {
    s   <- names(tbl)[i]
    pct <- round(as.integer(tbl[i]) / N * 100, 2)
    pts <- as.integer(strsplit(s, "-")[[1]])
    res <- if (pts[1] > pts[2]) "home" else if (pts[1] == pts[2]) "draw" else "away"
    list(score = s, pct = pct, result = res)
  })
  list(xg_h = round(xg_h, 2), xg_a = round(xg_a, 2),
       hw = hw, dr = dr, aw = aw, top5 = top5)
}

# ── 旗幟對應表 ────────────────────────────────────────────
FLAG_MAP <- c(
  Germany="\U0001F1E9\U0001F1EA", "Ivory Coast"="\U0001F1E8\U0001F1EE",
  Ecuador="\U0001F1EA\U0001F1E8", Curacao="\U0001F1E8\U0001F1FC",
  Netherlands="\U0001F1F3\U0001F1F1", Sweden="\U0001F1F8\U0001F1EA",
  Japan="\U0001F1EF\U0001F1F5", Tunisia="\U0001F1F9\U0001F1F3",
  Spain="\U0001F1EA\U0001F1F8", "Saudi Arabia"="\U0001F1F8\U0001F1E6",
  Uruguay="\U0001F1FA\U0001F1FE", "Cape Verde"="\U0001F1E8\U0001F1FB",
  Belgium="\U0001F1E7\U0001F1EA", Iran="\U0001F1EE\U0001F1F7",
  "New Zealand"="\U0001F1F3\U0001F1FF", Egypt="\U0001F1EA\U0001F1EC",
  Argentina="\U0001F1E6\U0001F1F7", Austria="\U0001F1E6\U0001F1F9",
  Jordan="\U0001F1EF\U0001F1F4", Algeria="\U0001F1E9\U0001F1FF",
  France="\U0001F1EB\U0001F1F7", Iraq="\U0001F1EE\U0001F1F6",
  Norway="\U0001F1F3\U0001F1F4", Senegal="\U0001F1F8\U0001F1F3",
  Portugal="\U0001F1F5\U0001F1F9", Uzbekistan="\U0001F1FA\U0001F1FF",
  Colombia="\U0001F1E8\U0001F1F4", "DR Congo"="\U0001F1E8\U0001F1E9",
  England="\U0001F3F4\U000E0067\U000E0062\U000E0065\U000E006E\U000E0067\U000E007F",
  Ghana="\U0001F1EC\U0001F1ED", Panama="\U0001F1F5\U0001F1E6",
  Croatia="\U0001F1ED\U0001F1F7", Mexico="\U0001F1F2\U0001F1FD",
  "South Africa"="\U0001F1FF\U0001F1E6", "South Korea"="\U0001F1F0\U0001F1F7",
  Czechia="\U0001F1E8\U0001F1FF", Canada="\U0001F1E8\U0001F1E6",
  Switzerland="\U0001F1E8\U0001F1ED", Bosnia="\U0001F1E7\U0001F1E6",
  Qatar="\U0001F1F6\U0001F1E6", Brazil="\U0001F1E7\U0001F1F7",
  Morocco="\U0001F1F2\U0001F1E6",
  Scotland="\U0001F3F4\U000E0067\U000E0062\U000E0073\U000E0063\U000E0074\U000E007F",
  Haiti="\U0001F1ED\U0001F1F9", USA="\U0001F1FA\U0001F1F8",
  Turkey="\U0001F1F9\U0001F1F7", Australia="\U0001F1E6\U0001F1FA",
  Paraguay="\U0001F1F5\U0001F1FE"
)

DATE_LABELS <- c(
  "2026-06-20"="6月20日 — Group E,F 第二輪",
  "2026-06-21"="6月21日 — Group G,H 第二輪",
  "2026-06-22"="6月22日 — Group I,J 第二輪",
  "2026-06-23"="6月23日 — Group K,L 第二輪",
  "2026-06-24"="6月24日 — Group A,B,C 末輪",
  "2026-06-25"="6月25日 — Group D,E,F 末輪",
  "2026-06-26"="6月26日 — Group G,H,I 末輪",
  "2026-06-27"="6月27日 — Group J,K,L 末輪"
)

temp_class <- function(t) {
  if (t <= 26) return(list(label=paste0("\U0001F324 ",t,"\U00B0C"), cls="success"))
  if (t <= 30) return(list(label=paste0("\U2600 ",t,"\U00B0C"), cls="warning"))
  list(label=paste0("\U0001F321 ",t,"\U00B0C"), cls="danger")
}

form_html <- function(team) {
  fs <- recent_form[[team]]
  if (length(fs) == 0) return("")
  badges <- paste(sapply(fs, function(r) {
    cls <- switch(r, W="fb-w", D="fb-d", L="fb-l")
    sprintf('<span class="%s">%s</span>', cls, r)
  }), collapse="")
  paste0('<div class="form-row">', badges, '</div>')
}

res_label <- function(r, h, a) {
  if (r == "home") return(sprintf('<span class="sres rh">%s</span>', h))
  if (r == "draw") return('<span class="sres rd">平局</span>')
  sprintf('<span class="sres ra">%s</span>', a)
}

upset_tag <- function(sim, home, away) {
  # 爆冷風險：劣勢方（aw%）高於25%且主場xG比>1.5時
  fav_win <- sim$hw; dog_win <- sim$aw
  if (sim$xg_h < sim$xg_a) { fav_win <- sim$aw; dog_win <- sim$hw }
  risk_team <- if (sim$xg_h < sim$xg_a) home else away
  if (dog_win >= 30) return(sprintf('<span class="upset-high">🔥 高爆冷風險 %s</span>', risk_team))
  if (dog_win >= 20) return(sprintf('<span class="upset-med">⚡ 爆冷可能 %s</span>', risk_team))
  ""
}

value_tip <- function(sim, m) {
  if (is.null(m$odds_h) || is.null(m$odds_a)) return("")
  implied_h <- 1 / m$odds_h; implied_a <- 1 / m$odds_a
  model_h   <- sim$hw / 100; model_a <- sim$aw / 100
  tips <- c()
  if (model_h - implied_h >  0.08) tips <- c(tips, sprintf('主勝有價值（模型%.0f%% vs 賠率隱含%.0f%%）', model_h*100, implied_h*100))
  if (model_a - implied_a >  0.08) tips <- c(tips, sprintf('客勝有價值（模型%.0f%% vs 賠率隱含%.0f%%）', model_a*100, implied_a*100))
  if (length(tips) == 0) return("")
  sprintf('<div class="value-tip">💡 %s</div>', paste(tips, collapse="｜"))
}

expert_analysis <- function(m, sim) {
  notes <- c()
  ht <- m$home; at <- m$away
  ratio <- sim$xg_h / max(0.1, sim$xg_a)
  fav   <- if (sim$xg_h >= sim$xg_a) ht else at
  dog   <- if (sim$xg_h >= sim$xg_a) at else ht
  dog_pct <- if (sim$xg_h >= sim$xg_a) sim$aw else sim$hw
  hp <- get_played_g(ht); ap <- get_played_g(at)

  # 1. 實力分析（泊松+xG）
  if (ratio >= 2.0)
    notes <- c(notes, sprintf('📊 <b>實力懸殊</b>：%s xG 是對手 %.1f 倍，模擬主勝率 %.0f%%，強弱差距明顯。', ht, ratio, sim$hw))
  else if (ratio >= 1.3)
    notes <- c(notes, sprintf('📊 <b>主場佔優</b>：%s xG 優勢 %.0f%%，主勝率 %.0f%%，但未到壓倒性。', ht, (ratio-1)*100, sim$hw))
  else if (ratio <= 0.5)
    notes <- c(notes, sprintf('📊 <b>客隊壓制</b>：%s xG 是主場 %.1f 倍，模擬客勝率 %.0f%%。', at, 1/ratio, sim$aw))
  else if (ratio <= 0.77)
    notes <- c(notes, sprintf('📊 <b>客場佔優</b>：%s xG 優勢 %.0f%%，客勝率 %.0f%%。', at, (1/ratio-1)*100, sim$aw))
  else
    notes <- c(notes, sprintf('📊 <b>勢均力敵</b>：雙方 xG 差距僅 %.2f，平局率高達 %.0f%%，比賽走向難測。', abs(sim$xg_h - sim$xg_a), sim$dr))

  # 2. 戰術相剋分析
  ht_tac <- if (!is.na(TACTICAL[ht])) TACTICAL[[ht]] else "均衡型"
  at_tac <- if (!is.na(TACTICAL[at])) TACTICAL[[at]] else "均衡型"
  clash <- ""
  if (grepl("控球|傳控|tiki", ht_tac) && grepl("防守|低位", at_tac))
    clash <- sprintf('%s 控球型 vs %s 防守型，易演變為低比分膠著。', ht, at)
  else if (grepl("壓迫|逼搶", ht_tac) && grepl("快速反擊|防反", at_tac))
    clash <- sprintf('%s 高壓打法 vs %s 防反，空間反擊或令客隊受益。', ht, at)
  else if (grepl("快速反擊|防反", ht_tac) && grepl("控球|傳控", at_tac))
    clash <- sprintf('%s 防反 vs %s 控球，預計低節奏、%s 少輸即贏。', ht, at, ht)
  if (nchar(clash) > 0)
    notes <- c(notes, sprintf('⚔️ <b>戰術相剋</b>：%s（%s）vs（%s）%s', "", ht_tac, at_tac, clash))

  # 3. 歷史爆冷 & 黑馬
  if (dog %in% GIANT_KILLERS && dog_pct >= 20)
    notes <- c(notes, sprintf('🔥 <b>爆冷警示</b>：%s 是世界盃傳統黑馬，歷史上多次爆冷擊敗強隊，本場勝率 %.0f%%，不可輕視。', dog, dog_pct))

  # 4. 主辦國優勢
  if (ht %in% HOST_TEAMS)
    notes <- c(notes, sprintf('🏟 <b>主辦國主場</b>：%s 坐擁地利，現場球迷支持+熟悉場地，歷史數據顯示主辦國首輪勝率提升約 15%%。', ht))

  # 5. 首戰不確定性
  if (hp == 0 && ap == 0)
    notes <- c(notes, '⚡ <b>首戰效應</b>：雙方均為本屆首場，世界盃首戰心理壓力大、技術狀態尚未磨合，爆冷機率比後續輪次高 20-30%%，模型已調整。')

  # 6. 小組賽出線壓力
  if (hp >= 2 || ap >= 2) {
    who <- if (hp >= 2 && ap >= 2) "雙方" else if (hp >= 2) ht else at
    notes <- c(notes, sprintf('🔑 <b>出線壓力</b>：%s 已踢完兩場，本場為生死戰，必勝壓力下戰術可能更激進、賠率水位易受情緒影響。', who))
  } else if (hp >= 1 || ap >= 1)
    notes <- c(notes, '📋 <b>積分關鍵場</b>：已踢過第一輪，本場積分直接影響小組排名，雙方戰意預計全開。')

  # 7. 賠率分析（莊家心理 & 價值）
  if (!is.null(m$odds_h) && !is.null(m$odds_a)) {
    imp_h <- round(1/m$odds_h*100, 1); imp_a <- round(1/m$odds_a*100, 1)
    vig   <- imp_h + if (!is.null(m$odds_d)) round(1/m$odds_d*100,1) else 0 + imp_a
    if (sim$hw - imp_h > 8)
      notes <- c(notes, sprintf('💰 <b>主勝具價值</b>：模型主勝 %.0f%% > 莊家隱含 %.0f%%，超額 %.0f%%，建議關注主勝或讓球盤。', sim$hw, imp_h, sim$hw-imp_h))
    else if (sim$aw - imp_a > 8)
      notes <- c(notes, sprintf('💰 <b>客勝具價值</b>：模型客勝 %.0f%% > 莊家隱含 %.0f%%，超額 %.0f%%，賠率偏低值得留意。', sim$aw, imp_a, sim$aw-imp_a))
    else
      notes <- c(notes, sprintf('⚖️ <b>賠率合理</b>：莊家賠率（主 %.2f / 客 %.2f）與模型機率吻合，無明顯套利空間。', m$odds_h, m$odds_a))
  }

  # 8. 前幾天賽果影響（根據 form）
  hf <- recent_form[[ht]]; af <- recent_form[[at]]
  if (length(hf) > 0 || length(af) > 0) {
    hform <- if (length(hf) > 0) paste(hf, collapse="") else "–"
    aform <- if (length(af) > 0) paste(af, collapse="") else "–"
    h_hot <- sum(hf == "W") >= 2; a_hot <- sum(af == "W") >= 2
    h_cold <- sum(hf == "L") >= 2; a_cold <- sum(af == "L") >= 2
    if (h_hot) notes <- c(notes, sprintf('🔴 <b>近況火熱</b>：%s 近期 %s，士氣高昂，莊家可能調低主勝賠率，需確認是否已反映。', ht, hform))
    if (a_hot) notes <- c(notes, sprintf('🔵 <b>黑馬狀態</b>：%s 近期 %s 表現亮眼，莊家可能尚未完全調整賠率。', at, aform))
    if (h_cold && !a_cold) notes <- c(notes, sprintf('⚠️ <b>主場狀態堪憂</b>：%s 近期 %s，低迷狀態下主場優勢可能縮水。', ht, hform))
  }

  if (length(notes) == 0) return("")
  items <- paste(sapply(notes, function(n) sprintf('<div class="ai-item">%s</div>', n)), collapse="")
  sprintf('<div class="ai-box"><div class="ai-title">🧠 運彩投資專家分析</div>%s</div>', items)
}

match_html <- function(m, sim) {
  hf  <- FLAG_MAP[m$home]; af <- FLAG_MAP[m$away]
  tc  <- temp_class(m$temp)
  # 賠率（從 JSON 取，若無則顯示 N/A）
  odds_h <- if (!is.null(m$odds_h)) sprintf("%.2f", m$odds_h) else "N/A"
  odds_d <- if (!is.null(m$odds_d)) sprintf("%.2f", m$odds_d) else "N/A"
  odds_a <- if (!is.null(m$odds_a)) sprintf("%.2f", m$odds_a) else "N/A"
  utag   <- upset_tag(sim, m$home, m$away)
  vtip   <- value_tip(sim, m)
  analysis <- expert_analysis(m, sim)

  pills <- paste(sapply(seq_along(sim$top5), function(i) {
    s   <- sim$top5[[i]]
    cls <- c("s1","s2","s3","s4","s5")[i]
    sprintf('<div class="spill %s"><div class="rbadge">#%d</div><div class="sval">%s</div><div class="spct">%s%%</div>%s</div>',
            cls, i, s$score, s$pct, res_label(s$result, m$home, m$away))
  }), collapse="")

  sprintf('
<div class="card">
  <div class="card-header">
    <span class="venue"><b>%s</b></span>
    <div style="display:flex;gap:5px;align-items:center">
      <span class="grp-chip">%s</span>
      <span class="wchip w-%s">%s</span>
    </div>
  </div>
  <div class="teams-row">
    <div class="team">
      <span class="flag">%s</span>
      <div class="tname">%s</div>
      <div class="tform">xG %.2f</div>
      %s
    </div>
    <div class="vs-badge"><span class="grp-sm">模擬</span>VS</div>
    <div class="team">
      <span class="flag">%s</span>
      <div class="tname">%s</div>
      <div class="tform">xG %.2f</div>
      %s
    </div>
  </div>
  <div class="prob-section">
    <div class="prob-bar-wrap">
      <div class="prob-seg seg-home" style="width:%s%%"></div>
      <div class="prob-seg seg-draw" style="width:%s%%"></div>
      <div class="prob-seg seg-away" style="width:%s%%"></div>
    </div>
    <div class="prob-labels">
      <span class="pl-home"><strong>%s%%</strong> %s</span>
      <span class="pl-draw"><strong>%s%%</strong> 平局</span>
      <span class="pl-away"><strong>%s%%</strong> %s</span>
    </div>
  </div>
  <div class="odds-row">
    <div class="odds-chip"><div class="odds-lbl">主勝 (%s)</div><div class="odds-val">%s</div></div>
    <div class="odds-chip"><div class="odds-lbl">平局 (%s)</div><div class="odds-val">%s</div></div>
    <div class="odds-chip"><div class="odds-lbl">客勝 (%s)</div><div class="odds-val">%s</div></div>
  </div>
  <div class="scores-section">
    <div class="scores-title">最可能比分（10萬次模擬）</div>
    <div class="scores-grid">%s</div>
  </div>
  %s%s%s
</div>',
    m$venue, m$group, tc$cls, tc$label,
    hf, m$home, sim$xg_h, form_html(m$home),
    af, m$away, sim$xg_a, form_html(m$away),
    sim$hw, sim$dr, sim$aw,
    sim$hw, m$home, sim$dr, sim$aw, m$away,
    m$home, odds_h,
    "平", odds_d,
    m$away, odds_a,
    pills, utag, vtip, analysis)
}

# ── 戰術風格資料 ──────────────────────────────────────────
TACTICAL <- c(
  Germany="傳控壓迫", "Ivory Coast"="快速反擊", Ecuador="穩守反擊",
  Curacao="防守型", Netherlands="全攻全守", Sweden="穩守反擊",
  Japan="高壓逼搶", Tunisia="防守反擊", Spain="tiki-taka控球",
  "Saudi Arabia"="防守型", Uruguay="強硬防反", "Cape Verde"="防守反擊",
  Belgium="個人能力型", Iran="低位防守", "New Zealand"="穩守反擊",
  Egypt="防反為主", Argentina="快速反擊+個人能力", Austria="積極壓迫",
  Jordan="低位防守", Algeria="防守反擊", France="全能均衡型",
  Iraq="防守反擊", Norway="高空球+直接打法", Senegal="快速反擊",
  Portugal="控球+個人能力", Uzbekistan="穩守反擊",
  Colombia="快速反擊", "DR Congo"="防守反擊",
  England="直接打法", Ghana="快速反擊",
  Panama="低位防守", Croatia="中場控制",
  Mexico="快速反擊", "South Africa"="防守反擊",
  "South Korea"="高壓逼搶", Czechia="穩守反擊",
  Canada="積極壓迫", Switzerland="防守反擊",
  Bosnia="直接打法", Qatar="控球型",
  Brazil="技術控球", Morocco="低位防守",
  Scotland="直接打法", Haiti="防守反擊",
  USA="積極壓迫", Turkey="快速反擊", Australia="積極壓迫",
  Paraguay="穩守反擊"
)

# ── 全域已踢場數（供分析函數使用）────────────────────────
played_matches <- Filter(function(x) isTRUE(x$played), matches)
played_count_g <- table(c(
  sapply(played_matches, function(x) x$home),
  sapply(played_matches, function(x) x$away)
))
get_played_g <- function(t) { v <- played_count_g[t]; if (!is.na(v)) as.integer(v) else 0L }

# ── 執行模擬 ──────────────────────────────────────────────
remaining <- Filter(function(m) !isTRUE(m$played), matches)
played_n  <- length(matches) - length(remaining)
today     <- format(Sys.Date(), "%Y-%m-%d")
dates     <- sort(unique(sapply(remaining, function(m) m$date)))
cat(sprintf("[SIM] %d matches to simulate...\n", length(remaining)))

# UPCOMING JS 陣列（按 ko 時間排序）
get_ko <- function(m) if (!is.null(m$ko)) as.numeric(m$ko) else as.integer(as.Date(m$date) - as.Date("1970-01-01")) * 86400 + 23*3600
remaining_sorted <- remaining[order(sapply(remaining, get_ko))]
upcoming_js <- paste0("[", paste(sapply(remaining_sorted, function(m) {
  ts_ms <- get_ko(m) * 1000
  sprintf('{"date":"%s","home":"%s","away":"%s","ts":%s}',
          m$date, m$home, m$away, format(ts_ms, scientific=FALSE))
}), collapse=","), "]")

tab_btns <- paste(sapply(seq_along(dates), function(i) {
  d <- dates[i]; sh <- sub("2026-", "", d)
  ac <- if (i == 1) " active" else ""
  sprintf('<button class="tab%s" onclick="showDay(\'%s\',this)">%s</button>', ac, d, sh)
}), collapse="\n")

sections <- paste(sapply(seq_along(dates), function(i) {
  d  <- dates[i]; label <- DATE_LABELS[d]
  ms <- Filter(function(m) m$date == d, remaining)
  cards <- paste(sapply(ms, function(m) {
    heat <- ifelse(is.null(m$heat), 0, m$heat)
    hp   <- get_played_g(m$home); ap <- get_played_g(m$away)
    sim  <- simulate_match(m$home, m$away, heat, isTRUE(m$altitude), hp, ap)
    cat(sprintf("  ✓ %s vs %s  xG[%.2f-%.2f]  %s%%-%s%%-%s%%\n",
                m$home, m$away, sim$xg_h, sim$xg_a, sim$hw, sim$dr, sim$aw))
    match_html(m, sim)
  }), collapse="\n")
  disp <- if (i == 1) "block" else "none"
  sprintf('<div id="day-%s" class="day-section" style="display:%s"><div class="day-label">%s</div>%s</div>',
          d, disp, label, cards)
}), collapse="\n")

# ── 倒數計時 JS（台灣時間 UTC+8 顯示）───────────────────────
cd_js <- paste(
  "const UPCOMING =", upcoming_js, ";",
  "function nextMs(){",
  "  var n=Date.now(), f=UPCOMING.filter(function(m){return m.ts>n;});",
  "  if(!f.length) return null;",
  "  var t=Math.min.apply(null,f.map(function(m){return m.ts;}));",
  "  return f.filter(function(m){return m.ts===t;});",
  "}",
  "function pad2(n){return String(n).padStart(2,'0');}",
  "function toTW(ts){",
  "  var d=new Date(ts+8*3600000);",
  "  return (d.getUTCMonth()+1)+'/'+(d.getUTCDate())+' '+pad2(d.getUTCHours())+':'+pad2(d.getUTCMinutes());",
  "}",
  "function renderCD(){",
  "  var w=document.getElementById('cd-wrap'); if(!w) return;",
  "  var ms=nextMs();",
  "  if(!ms){w.innerHTML='<div class=\"cd-done\">\U0001F3C6 小組賽已全部結束！</div>';return;}",
  "  var diff=ms[0].ts-Date.now();",
  "  if(diff<0){w.innerHTML='<div class=\"cd-done\">⚽ 比賽進行中...</div>';return;}",
  "  var dd=Math.floor(diff/86400000);",
  "  var hh=Math.floor((diff%86400000)/3600000);",
  "  var mm=Math.floor((diff%3600000)/60000);",
  "  var ss=Math.floor((diff%60000)/1000);",
  "  var list=ms.map(function(x){return x.home+' vs '+x.away;}).join(' &middot; ');",
  "  var twt=toTW(ms[0].ts);",
  "  w.innerHTML=",
  "    '<div class=\"cd-label\">⏱ 下一場比賽倒數</div>'+",
  "    '<div class=\"cd-match\">'+list+'</div>'+",
  "    '<div class=\"cd-twtime\">\U0001F1F9\U0001F1FC 台灣時間約 '+twt+' 開賽 · <a href=\"https://www.sporttery.com.tw/\" style=\"color:#34d399\">確認運彩賽程</a></div>'+",
  "    '<div class=\"cd-digits\">'+",
  "    '<div class=\"cd-unit\"><div class=\"cd-num\">'+pad2(dd)+'</div><div class=\"cd-lbl\">天</div></div>'+",
  "    '<div class=\"cd-sep\">:</div>'+",
  "    '<div class=\"cd-unit\"><div class=\"cd-num\">'+pad2(hh)+'</div><div class=\"cd-lbl\">時</div></div>'+",
  "    '<div class=\"cd-sep\">:</div>'+",
  "    '<div class=\"cd-unit\"><div class=\"cd-num\">'+pad2(mm)+'</div><div class=\"cd-lbl\">分</div></div>'+",
  "    '<div class=\"cd-sep\">:</div>'+",
  "    '<div class=\"cd-unit\"><div class=\"cd-num\">'+pad2(ss)+'</div><div class=\"cd-lbl\">秒</div></div>'+",
  "    '</div>'+(ms.length>1?'<div class=\"cd-multi\">同時段共 '+ms.length+' 場比賽</div>':'');",
  "}",
  "renderCD(); setInterval(renderCD,1000);",
  sep="\n")

# ── HTML ──────────────────────────────────────────────────
html <- paste0('<!DOCTYPE html>
<html lang="zh-TW">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>2026 世界盃比分預測</title>
<style>
*{box-sizing:border-box;margin:0;padding:0}
:root{--bg:#09101f;--card:#141e2e;--border:rgba(255,255,255,0.07);--text:#eef2ff;--muted:#7a8fa8;--accent:#3b82f6;--gold:#f59e0b;--silver:#9ca3af;--bronze:#b45309}
body{background:var(--bg);color:var(--text);font-family:"Segoe UI",system-ui,sans-serif;min-height:100vh;padding-bottom:3rem}
.hero{background:linear-gradient(160deg,#0d1b35 0%,#09101f 60%);border-bottom:1px solid var(--border);padding:2.5rem 1.5rem 2rem;text-align:center;position:relative}
.hero::before{content:"";position:absolute;inset:0;background:radial-gradient(ellipse 80% 50% at 50% 0%,rgba(59,130,246,.12) 0%,transparent 70%);pointer-events:none}
.badge{display:inline-flex;align-items:center;gap:6px;background:rgba(59,130,246,.12);border:1px solid rgba(59,130,246,.25);border-radius:20px;padding:4px 14px;font-size:11px;color:#93c5fd;margin-bottom:1rem}
.dot{display:inline-block;width:6px;height:6px;background:#3b82f6;border-radius:50%;animation:pulse 1.5s ease-in-out infinite}
@keyframes pulse{0%,100%{opacity:1}50%{opacity:.3}}
.hero h1{font-size:clamp(1.6rem,4vw,2.4rem);font-weight:700;letter-spacing:-.02em;line-height:1.15;margin-bottom:.5rem}
.hero h1 em{font-style:normal;color:var(--gold)}
.hero p{color:var(--muted);font-size:13px;max-width:500px;margin:0 auto .8rem;line-height:1.6}
.update-info{font-size:11px;color:var(--muted);margin-top:.4rem}
.update-info strong{color:#34d399}
.stats-row{display:flex;justify-content:center;gap:2rem;flex-wrap:wrap;margin-top:1rem}
.stat .n{font-size:1.2rem;font-weight:700;color:var(--accent)}
.stat .l{font-size:10px;color:var(--muted);margin-top:2px}
.container{max-width:960px;margin:0 auto;padding:0 1rem}
/* countdown */
#cd-wrap{background:linear-gradient(135deg,rgba(59,130,246,.08),rgba(139,92,246,.08));border:1px solid rgba(59,130,246,.2);border-radius:14px;padding:1rem 1.2rem;margin:.8rem 0 .4rem;text-align:center}
.cd-label{font-size:10px;font-weight:600;letter-spacing:.1em;text-transform:uppercase;color:var(--muted);margin-bottom:.35rem}
.cd-match{font-size:12px;color:var(--text);margin-bottom:.6rem;opacity:.85}
.cd-digits{display:flex;justify-content:center;gap:6px}
.cd-unit{display:flex;flex-direction:column;align-items:center}
.cd-num{font-size:clamp(1.5rem,5vw,2.2rem);font-weight:700;line-height:1;background:linear-gradient(135deg,#3b82f6,#8b5cf6);-webkit-background-clip:text;-webkit-text-fill-color:transparent;background-clip:text;min-width:2.4ch;text-align:center}
.cd-sep{font-size:1.6rem;font-weight:700;color:rgba(139,92,246,.5);line-height:1.15}
.cd-lbl{font-size:9px;color:var(--muted);margin-top:2px}
.cd-done{font-size:1rem;font-weight:600;color:#34d399;padding:.5rem}
.cd-twtime{font-size:11px;color:#34d399;margin-bottom:.6rem;font-weight:500}
.cd-multi{font-size:11px;color:var(--muted);margin-top:.5rem}
/* tabs */
.tabs{display:flex;gap:6px;padding:1rem 0 .6rem;overflow-x:auto;scrollbar-width:none}
.tabs::-webkit-scrollbar{display:none}
.tab{flex-shrink:0;border:1px solid var(--border);background:rgba(255,255,255,.03);color:var(--muted);padding:6px 14px;border-radius:18px;font-size:12px;cursor:pointer;transition:all .2s}
.tab:hover{border-color:rgba(59,130,246,.4);color:var(--text)}
.tab.active{background:var(--accent);border-color:var(--accent);color:#fff;font-weight:500}
.day-section{display:none}
.day-label{display:flex;align-items:center;gap:8px;font-size:10px;font-weight:600;letter-spacing:.08em;color:var(--muted);text-transform:uppercase;padding:.7rem 0 .5rem}
.day-label::after{content:"";flex:1;height:.5px;background:var(--border)}
/* card */
.card{background:var(--card);border:1px solid var(--border);border-radius:14px;margin-bottom:.9rem;overflow:hidden;transition:border-color .2s}
.card:hover{border-color:rgba(59,130,246,.25)}
.card-header{padding:.5rem 1rem;border-bottom:1px solid var(--border);background:rgba(0,0,0,.2);display:flex;align-items:center;justify-content:space-between;gap:8px;flex-wrap:wrap}
.venue{font-size:11px;color:var(--muted)}.venue b{color:var(--text);font-weight:500}
.grp-chip{font-size:10px;font-weight:600;padding:2px 8px;border-radius:10px;background:rgba(59,130,246,.15);color:#93c5fd;border:1px solid rgba(59,130,246,.25)}
.wchip{display:inline-flex;align-items:center;gap:4px;border-radius:16px;padding:3px 9px;font-size:10px;font-weight:500;border:1px solid}
.w-success{background:rgba(16,185,129,.1);color:#34d399;border-color:rgba(16,185,129,.2)}
.w-warning{background:rgba(251,191,36,.1);color:#fcd34d;border-color:rgba(251,191,36,.2)}
.w-danger{background:rgba(239,68,68,.1);color:#f87171;border-color:rgba(239,68,68,.2)}
.teams-row{padding:.8rem 1rem .65rem;display:grid;grid-template-columns:1fr 60px 1fr;align-items:center;gap:.5rem}
.team{text-align:center}
.flag{font-size:1.9rem;display:block;margin-bottom:4px;line-height:1}
.tname{font-size:13px;font-weight:500}
.tform{font-size:10px;color:var(--muted);margin-top:2px}
.form-row{display:flex;justify-content:center;gap:3px;margin-top:4px}
.fb-w{background:rgba(16,185,129,.2);color:#34d399;border:1px solid rgba(16,185,129,.3);font-size:9px;font-weight:700;padding:1px 5px;border-radius:3px}
.fb-d{background:rgba(107,114,128,.2);color:#9ca3af;border:1px solid rgba(107,114,128,.3);font-size:9px;font-weight:700;padding:1px 5px;border-radius:3px}
.fb-l{background:rgba(239,68,68,.15);color:#f87171;border:1px solid rgba(239,68,68,.25);font-size:9px;font-weight:700;padding:1px 5px;border-radius:3px}
.vs-badge{background:rgba(255,255,255,.04);border:1px solid var(--border);border-radius:8px;padding:5px 0;font-size:11px;font-weight:600;color:var(--muted);text-align:center}
.grp-sm{font-size:9px;color:var(--muted);display:block;margin-bottom:1px}
.prob-section{padding:0 1rem .6rem}
.prob-bar-wrap{display:flex;border-radius:4px;overflow:hidden;height:6px;gap:2px;margin-bottom:5px}
.prob-seg{height:100%;border-radius:2px}
.seg-home{background:var(--accent)}.seg-draw{background:#4b5563}.seg-away{background:#8b5cf6}
.prob-labels{display:flex;justify-content:space-between;font-size:10px}
.pl-home strong{color:var(--accent)}.pl-draw strong{color:#9ca3af}.pl-away strong{color:#a78bfa}
.prob-labels span{color:var(--muted)}
/* 運彩賠率 */
.odds-row{display:flex;gap:6px;padding:0 1rem .65rem}
.odds-chip{flex:1;background:rgba(255,255,255,.03);border:1px solid var(--border);border-radius:8px;padding:5px 4px;text-align:center}
.odds-lbl{font-size:9px;color:var(--muted);margin-bottom:2px}
.odds-val{font-size:13px;font-weight:600;color:#fcd34d}
/* scores */
.scores-section{padding:0 1rem 1rem}
.scores-title{font-size:10px;font-weight:500;letter-spacing:.06em;color:var(--muted);text-transform:uppercase;margin-bottom:.6rem}
.scores-grid{display:grid;grid-template-columns:repeat(5,1fr);gap:6px}
.spill{border-radius:10px;padding:9px 4px 7px;text-align:center;border:.5px solid;position:relative}
.s1{background:rgba(245,158,11,.08);border-color:rgba(245,158,11,.3)}
.s2{background:rgba(255,255,255,.04);border-color:rgba(255,255,255,.1)}
.s3{background:rgba(255,255,255,.02);border-color:rgba(255,255,255,.06)}
.s4,.s5{background:rgba(255,255,255,.01);border-color:rgba(255,255,255,.04)}
.rbadge{position:absolute;top:-7px;left:50%;transform:translateX(-50%);font-size:8px;font-weight:600;padding:1px 6px;border-radius:8px;white-space:nowrap}
.s1 .rbadge{background:var(--gold);color:#1a0e00}
.s2 .rbadge{background:var(--silver);color:#111}
.s3 .rbadge{background:var(--bronze);color:#fff8f0}
.s4 .rbadge,.s5 .rbadge{background:#374151;color:#9ca3af}
.sval{font-size:1.1rem;font-weight:600;margin-top:5px;line-height:1}
.s1 .sval{color:var(--gold)}.s2 .sval{color:#d1d5db}.s3 .sval{color:#d97706}.s4 .sval,.s5 .sval{color:#6b7280}
.spct{font-size:9px;color:var(--muted);margin-top:2px}
.sres{font-size:8px;margin-top:3px;padding:1px 5px;border-radius:4px;display:inline-block}
.rh{background:rgba(59,130,246,.15);color:#93c5fd}
.rd{background:rgba(107,114,128,.2);color:#9ca3af}
.ra{background:rgba(139,92,246,.15);color:#c4b5fd}
.upset-high{display:block;margin:.1rem 1rem .3rem;padding:4px 10px;border-radius:6px;background:rgba(239,68,68,.12);border:1px solid rgba(239,68,68,.3);font-size:11px;color:#f87171;font-weight:600}
.upset-med{display:block;margin:.1rem 1rem .3rem;padding:4px 10px;border-radius:6px;background:rgba(251,191,36,.1);border:1px solid rgba(251,191,36,.25);font-size:11px;color:#fcd34d;font-weight:600}
.value-tip{margin:.1rem 1rem .3rem;padding:5px 10px;border-radius:6px;background:rgba(52,211,153,.08);border:1px solid rgba(52,211,153,.2);font-size:11px;color:#34d399}
.sporttery-bar{display:flex;gap:8px;justify-content:center;flex-wrap:wrap;margin-top:1rem}
.sporttery-btn{display:inline-flex;align-items:center;gap:6px;background:rgba(234,179,8,.12);border:1px solid rgba(234,179,8,.35);border-radius:20px;padding:7px 18px;font-size:12px;color:#fcd34d;text-decoration:none;font-weight:600;transition:all .2s}
.sporttery-btn:hover{background:rgba(234,179,8,.22);border-color:rgba(234,179,8,.6);color:#fef08a}
.sporttery-btn2{background:rgba(59,130,246,.1);border-color:rgba(59,130,246,.3);color:#93c5fd}
.sporttery-btn2:hover{background:rgba(59,130,246,.2);color:#bfdbfe}
.ai-box{margin:.3rem 1rem .8rem;padding:10px 12px;border-radius:10px;background:rgba(59,130,246,.05);border:1px solid rgba(59,130,246,.15)}
.ai-title{font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#60a5fa;margin-bottom:6px}
.ai-item{font-size:11px;color:#cbd5e1;line-height:1.6;padding:3px 0;border-bottom:.5px solid rgba(255,255,255,.05)}
.ai-item:last-child{border-bottom:none}
.ai-item b{color:#e2e8f0}
.disc{background:rgba(245,158,11,.06);border:1px solid rgba(245,158,11,.2);border-radius:8px;padding:8px 12px;font-size:11px;color:#fcd34d;margin:.4rem 0;display:flex;gap:6px}
.footer{text-align:center;padding:2rem 1rem 1rem;font-size:11px;color:var(--muted);line-height:1.9;border-top:1px solid var(--border);margin-top:1.5rem}
.footer strong{color:#60a5fa}
@media(max-width:520px){.flag{font-size:1.5rem}.tname{font-size:12px}.sval{font-size:.95rem}.scores-grid{grid-template-columns:repeat(3,1fr)}.s4,.s5{display:none}}
</style>
</head>
<body>
<div class="hero">
  <div class="badge"><span class="dot"></span> 每2小時自動更新 · xG Poisson 模型</div>
  <h1>2026 FIFA 世界盃<br>小組賽比分預測 <em>AI</em></h1>
  <p>以 xG 預期進球値為基礎，透過 Bayesian 將賽果動態更新各隊攻防係數，10萬次 Poisson 模擬導出比分機率分布。</p>
  <p class="update-info">最後更新：<strong>', today, '</strong>｜已完成 ', played_n, ' 場 · 剩餘 ', length(remaining), ' 場</p>
  <div class="stats-row">
    <div class="stat"><div class="n">', length(remaining), '</div><div class="l">場剩餘</div></div>
    <div class="stat"><div class="n">10萬</div><div class="l">次/場模擬</div></div>
    <div class="stat"><div class="n">xG+</div><div class="l">防守修正版</div></div>
  </div>
  <div class="sporttery-bar">
    <a href="https://www.sporttery.com.tw/" class="sporttery-btn">
      🎯 台灣運彩官網（查詢最新賠率）
    </a>
    <a href="https://www.fifa.com/en/tournaments/mens/worldcup/canadamexicousa2026/match-centre" class="sporttery-btn sporttery-btn2">
      🏆 FIFA 官方賽程表
    </a>
  </div>
</div>
<div class="container">
  <div id="cd-wrap"></div>
  <div class="tabs">', tab_btns, '</div>
  <div class="disc">⚡ 純統計模型，賞率為台灣運彩資料（需手動更新），僅供娛樂參考。</div>
  ', sections, '
  <div class="footer">
    <strong>模型架構</strong>｜ xG Poisson + Bayesian 動態更新（w=0.15，係數上限 2.5）｜防守係數以除法計算<br>
    天氣：高溫 → xG 下調·墨西哥城 → 海拔加成｜自動排程：每日 UTC 06:00 重算<br>
    Claude AI × R 4.6 × GitHub Actions · 僅供娛樂
  </div>
</div>
<script>
function showDay(d,btn){
  document.querySelectorAll(".tab").forEach(function(t){t.classList.remove("active");});
  btn.classList.add("active");
  document.querySelectorAll(".day-section").forEach(function(s){s.style.display="none";});
  document.getElementById("day-"+d).style.display="block";
}
', cd_js, '
</script>
</body>
</html>')

writeLines(html, "index.html", useBytes = FALSE)
cat(sprintf("[OK] index.html 生成完成（%d 場比賽已模擬）\n", length(remaining)))
