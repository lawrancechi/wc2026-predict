library(jsonlite)

set.seed(2026)
w <- 0.10   # Bayesian 更新權重

# ══════════════════════════════════════════════════════════════
# 埃羅評分系統（Elo Rating）
# 基於 FIFA 官方積分換算（2026 世界盃賽前）
# ══════════════════════════════════════════════════════════════
ELO <- c(
  Argentina=1891, Spain=1858, France=1851, England=1824,
  Brazil=1820, Belgium=1816, Portugal=1804, Netherlands=1797,
  Germany=1767, Uruguay=1742, Colombia=1740, Japan=1736,
  Morocco=1730, USA=1720, Senegal=1710, Mexico=1705,
  Turkey=1700, Croatia=1698, Switzerland=1682, "South Korea"=1676,
  Norway=1665, Austria=1660, Australia=1640, Ecuador=1620,
  Egypt=1615, Canada=1610, Algeria=1580, "Ivory Coast"=1575,
  Ghana=1550, Iran=1545, Sweden=1530, Czechia=1525,
  Scotland=1510, "New Zealand"=1505, Paraguay=1490,
  "South Africa"=1480, Tunisia=1470, "Saudi Arabia"=1465,
  Bosnia=1450, "Cape Verde"=1430, "DR Congo"=1410,
  Uzbekistan=1390, Jordan=1360, Haiti=1340, Iraq=1320,
  Panama=1310, Curacao=1280, Qatar=1250
)

# Elo 預測：傳回 list(hw, dr, aw) 機率（%）
elo_predict <- function(home, away) {
  eh <- if (!is.na(ELO[home])) ELO[[home]] else 1500
  ea <- if (!is.na(ELO[away])) ELO[[away]] else 1500
  diff <- (eh - ea + 100) / 400   # +100 = 主場加成
  e_h  <- 1 / (1 + 10^(-diff))   # 期望勝率（含平局）
  # 將期望得分轉換成 win/draw/lose
  p_hw <- e_h * 0.85
  p_dr <- 0.26 - abs(e_h - 0.5) * 0.32
  p_dr <- max(0.10, min(0.32, p_dr))
  p_aw <- max(0, 1 - p_hw - p_dr)
  list(hw=round(p_hw*100,1), dr=round(p_dr*100,1), aw=round(p_aw*100,1))
}

# ── 教練資料（姓名 + 能力評分 1.0=平均，>1.0=強帥）─────────
COACH <- list(
  Argentina    = list(name="Lionel Scaloni",      rating=1.08),
  France       = list(name="Didier Deschamps",    rating=1.07),
  Spain        = list(name="Luis de la Fuente",   rating=1.06),
  Germany      = list(name="Julian Nagelsmann",   rating=1.06),
  England      = list(name="Thomas Tuchel",       rating=1.05),
  Portugal     = list(name="Roberto Martínez",    rating=1.04),
  Brazil       = list(name="Dorival Júnior",      rating=1.03),
  Netherlands  = list(name="Ronald Koeman",       rating=1.03),
  Morocco      = list(name="Walid Regragui",      rating=1.04),
  Japan        = list(name="Hajime Moriyasu",     rating=1.03),
  USA          = list(name="Mauricio Pochettino", rating=1.03),
  Croatia      = list(name="Zlatko Dalić",        rating=1.03),
  Switzerland  = list(name="Murat Yakin",         rating=1.02),
  Colombia     = list(name="Néstor Lorenzo",      rating=1.02),
  Mexico       = list(name="Javier Aguirre",      rating=1.01),
  Uruguay      = list(name="Marcelo Bielsa",      rating=1.04),
  Senegal      = list(name="Aliou Cissé",         rating=1.02),
  Australia    = list(name="Tony Popovic",        rating=1.01),
  Canada       = list(name="Jesse Marsch",        rating=1.02),
  "South Korea"= list(name="Hong Myung-bo",       rating=1.00),
  Turkey       = list(name="Vincenzo Montella",   rating=1.01),
  Austria      = list(name="Ralf Rangnick",       rating=1.03),
  Norway       = list(name="Ståle Solbakken",     rating=1.01),
  Sweden       = list(name="Jon Dahl Tomasson",   rating=1.00),
  Belgium      = list(name="Domenico Tedesco",    rating=1.01),
  Ecuador      = list(name="Sebastián Beccacece", rating=1.00),
  Iran         = list(name="Amir Ghalenoei",      rating=0.98),
  "Saudi Arabia"=list(name="Hervé Renard",        rating=1.00),
  "Ivory Coast"= list(name="Emerse Faé",          rating=0.99),
  Algeria      = list(name="Vladimir Petković",   rating=1.00),
  Ghana        = list(name="Otto Addo",           rating=0.99),
  Tunisia      = list(name="Faouzi Benzarti",     rating=0.98),
  Egypt        = list(name="Hossam El-Badry",     rating=0.98),
  "DR Congo"   = list(name="Sébastien Desabre",   rating=0.99),
  "Cape Verde" = list(name="Bubista",             rating=0.98),
  Jordan       = list(name="Hussein Ammouta",     rating=0.97),
  Paraguay     = list(name="Gustavo Alfaro",      rating=0.99),
  Bosnia       = list(name="Sergej Barbarez",     rating=0.98),
  Uzbekistan   = list(name="Srecko Katanec",      rating=0.98),
  Curacao      = list(name="Remko Bicentini",     rating=0.97),
  Panama       = list(name="Thomas Christiansen", rating=0.98),
  Qatar        = list(name="Marquez López",       rating=0.97),
  Scotland     = list(name="Steve Clarke",        rating=0.99),
  Haiti        = list(name="Marc Collat",         rating=0.97),
  "South Africa"=list(name="Hugo Broos",          rating=0.98),
  "New Zealand"= list(name="Darren Bazeley",      rating=0.97),
  Iraq         = list(name="Jesús Casas",         rating=0.98),
  Czechia      = list(name="Ivan Hašek",          rating=0.99)
)
get_coach <- function(t) if (!is.null(COACH[[t]])) COACH[[t]] else list(name="(未知)", rating=1.0)

# ── 場地天氣資料（6-7月實際氣候）───────────────────────────
WEATHER <- list(
  "Dallas"        = list(emoji="🌡️", desc="高溫悶熱 36°C 晴", impact=-0.04),
  "Arlington"     = list(emoji="🌡️", desc="高溫 35°C 部分多雲", impact=-0.04),
  "Houston"       = list(emoji="☀️", desc="高溫潮濕 35°C", impact=-0.05),
  "Miami"         = list(emoji="🌧️", desc="高溫潮濕 33°C 雷陣雨", impact=-0.06),
  "Atlanta"       = list(emoji="⛅", desc="炎熱 32°C 多雲", impact=-0.03),
  "Kansas City"   = list(emoji="⛈️", desc="悶熱 30°C 雷雨風險", impact=-0.03),
  "New York"      = list(emoji="⛅", desc="溫暖 29°C 舒適", impact=0.00),
  "Boston"        = list(emoji="🌤️", desc="涼爽 26°C 適宜", impact=0.01),
  "Philadelphia"  = list(emoji="⛅", desc="溫熱 30°C 多雲", impact=-0.01),
  "Toronto"       = list(emoji="🌤️", desc="涼爽 24°C 晴", impact=0.01),
  "Vancouver"     = list(emoji="🌦️", desc="涼爽 21°C 小雨", impact=0.01),
  "Los Angeles"   = list(emoji="☀️", desc="溫暖乾燥 28°C", impact=0.00),
  "San Francisco" = list(emoji="🌁", desc="涼爽 19°C 霧", impact=0.01),
  "Seattle"       = list(emoji="🌦️", desc="溫涼 22°C 陰雨", impact=0.01),
  "Monterrey"     = list(emoji="🔥", desc="極熱潮濕 39°C", impact=-0.07),
  "Guadalupe"     = list(emoji="🔥", desc="極熱 38°C", impact=-0.07),
  "Mexico City"   = list(emoji="⛅", desc="高海拔 2240m 涼爽 19°C", impact=0.00),
  "Guadalajara"   = list(emoji="⛅", desc="溫熱 29°C 高海拔 1560m", impact=-0.01)
)
get_weather <- function(venue) {
  if (!is.null(WEATHER[[venue]])) WEATHER[[venue]]
  else list(emoji="🌤️", desc="氣候適中", impact=0.00)
}

cfg          <- fromJSON("data/teams.json", simplifyVector = FALSE)
base_lam     <- setNames(as.numeric(unlist(cfg$base_lambda)), names(cfg$base_lambda))
matches      <- cfg$matches
lineup_factor <- if (!is.null(cfg$lineup_factor)) cfg$lineup_factor else list()

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
  def[h] <- min(2.5, max(0.4, def[h] * (1-w) + w * (la_exp / max(0.5, as_))))
  def[a] <- min(2.5, max(0.4, def[a] * (1-w) + w * (lh_exp / max(0.5, hs))))

  # 記錄近3場勝/平/負
  recent_form[[h]] <- tail(c(recent_form[[h]], if(hs>as_)"W" else if(hs==as_)"D" else "L"), 3)
  recent_form[[a]] <- tail(c(recent_form[[a]], if(as_>hs)"W" else if(as_==hs)"D" else "L"), 3)
}

# ── 主辦國加成 ────────────────────────────────────────────
HOST_TEAMS <- c("USA", "Mexico", "Canada")

# ── 主力缺陣名單（影響 xG 係數）────────────────────────────
MISSING_PLAYERS <- list(
  Netherlands = list(
    list(name="Jurriën Timber",  pos="CB",  note="ACL傷缺全程"),
    list(name="Xavi Simons",     pos="MF",  note="ACL傷缺全程")
  ),
  Japan = list(
    list(name="三笘薰 (Mitoma)", pos="LW",  note="腿筋傷缺全程"),
    list(name="南野拓實 (Minamino)", pos="FW", note="ACL傷缺全程")
  ),
  Ghana = list(
    list(name="Mohammed Kudus",  pos="FW",  note="大腿/腿筋傷缺全程，最強進攻核心"),
    list(name="Abdul Salisu",    pos="CB",  note="傷缺全程")
  ),
  Brazil = list(
    list(name="Rodrygo",         pos="FW",  note="ACL傷缺全程"),
    list(name="Éder Militão",    pos="CB",  note="腿筋傷缺全程")
  ),
  Germany = list(
    list(name="Serge Gnabry",    pos="RW",  note="大腿內收肌傷缺全程")
  ),
  Spain = list(
    list(name="Fermín López",    pos="MF",  note="蹠骨骨折傷缺全程")
  ),
  Canada = list(
    list(name="Alphonso Davies", pos="LB",  note="傷缺全程，主力左翼衛")
  ),
  France = list(
    list(name="Hugo Ekitike",    pos="FW",  note="備役前鋒未入選")
  )
)

get_missing <- function(t) if (!is.null(MISSING_PLAYERS[[t]])) MISSING_PLAYERS[[t]] else list()

# ── 歷史爆冷強隊（世界盃常見黑馬）────────────────────────
GIANT_KILLERS <- c("Japan", "Morocco", "Switzerland", "Senegal",
                   "South Korea", "Ghana", "Australia", "Iran", "Czechia")

# ── 積分榜計算（供晉級壓力分析）────────────────────────
standings <- list()
for (m in matches) {
  if (!isTRUE(m$played)) next
  h <- m$home; a <- m$away
  hs <- m$home_score; as_ <- m$away_score
  if (is.null(standings[[h]])) standings[[h]] <- list(pts=0L, gp=0L, gf=0L, ga_=0L)
  if (is.null(standings[[a]])) standings[[a]] <- list(pts=0L, gp=0L, gf=0L, ga_=0L)
  standings[[h]]$gp <- standings[[h]]$gp + 1L
  standings[[a]]$gp <- standings[[a]]$gp + 1L
  standings[[h]]$gf <- standings[[h]]$gf + hs
  standings[[h]]$ga_ <- standings[[h]]$ga_ + as_
  standings[[a]]$gf <- standings[[a]]$gf + as_
  standings[[a]]$ga_ <- standings[[a]]$ga_ + hs
  if (hs > as_)       { standings[[h]]$pts <- standings[[h]]$pts + 3L }
  else if (hs == as_) { standings[[h]]$pts <- standings[[h]]$pts + 1L
                        standings[[a]]$pts <- standings[[a]]$pts + 1L }
  else                { standings[[a]]$pts <- standings[[a]]$pts + 3L }
}
get_standing <- function(t) {
  s <- standings[[t]]
  if (is.null(s)) list(pts=NA, gp=0L, status="首場出賽", pressure=0.0)
  else if (s$gp == 0L) list(pts=0, gp=0L, status="首場出賽", pressure=0.0)
  else {
    pts <- s$pts; gp <- s$gp
    status <- if (pts >= 6) "幾乎晉級 ✅"
              else if (pts == 4) "積分領先"
              else if (pts == 3) "積分尚可"
              else if (pts == 1) "急需積分 ⚠️"
              else               "背水一戰 🔴"
    pressure <- if (pts == 0 && gp >= 1) 0.06
                else if (pts <= 1 && gp >= 2) 0.10
                else if (pts >= 6) -0.03
                else 0.0
    list(pts=pts, gp=gp, status=status, pressure=pressure)
  }
}

# ── 解析式 Poisson 分析（取代10萬次模擬）────────────────
simulate_match <- function(home, away, heat, altitude=FALSE, h_played=0, a_played=0, venue="") {
  xg_h <- max(0.05, base_lam[home] * atk[home] / def[away] + heat)
  xg_a <- max(0.05, base_lam[away] * atk[away] / def[home] + heat)

  # 教練能力加成
  ch <- get_coach(home)$rating; ca <- get_coach(away)$rating
  xg_h <- xg_h * ch; xg_a <- xg_a * ca

  # 天氣影響
  wi <- get_weather(venue)$impact
  xg_h <- xg_h * (1 + wi); xg_a <- xg_a * (1 + wi)

  # 主辦國主場加成
  if (home %in% HOST_TEAMS) xg_h <- xg_h * 1.05

  # 海拔加成
  if (isTRUE(altitude)) { xg_h <- xg_h * 1.15; xg_a <- xg_a * 0.95 }

  # 晉級壓力修正
  xg_h <- xg_h * (1 + get_standing(home)$pressure)
  xg_a <- xg_a * (1 + get_standing(away)$pressure)

  # 首戰不確定性（拉近差距）
  if (h_played == 0 && a_played == 0) {
    mid <- (xg_h + xg_a) / 2
    xg_h <- xg_h * 0.85 + mid * 0.15
    xg_a <- xg_a * 0.85 + mid * 0.15
  }

  # 主力缺陣調整（lineup_factor）
  lf_h <- if (!is.null(lineup_factor[[home]])) lineup_factor[[home]] else 1.0
  lf_a <- if (!is.null(lineup_factor[[away]])) lineup_factor[[away]] else 1.0
  xg_h <- xg_h * lf_h; xg_a <- xg_a * lf_a

  # 黑馬加成
  ratio <- xg_h / max(0.1, xg_a)
  if (away %in% GIANT_KILLERS && ratio > 1.5) xg_a <- xg_a * 1.05
  if (home %in% GIANT_KILLERS && ratio < 0.67) xg_h <- xg_h * 1.05

  # ── 解析式 Poisson 比分矩陣（精確機率，無隨機性）──────
  max_g <- 8L
  ph <- dpois(0:max_g, xg_h)
  pa <- dpois(0:max_g, xg_a)
  mat <- outer(ph, pa)          # mat[i,j] = P(home=i-1, away=j-1)
  idx_h <- row(mat) > col(mat)  # home 進球 > away
  idx_d <- row(mat) == col(mat)
  hw <- round(sum(mat[idx_h]) * 100, 1)
  dr <- round(sum(mat[idx_d]) * 100, 1)
  aw <- round(100 - hw - dr, 1)

  # 前5高機率比分
  df <- data.frame(h=rep(0:max_g, times=max_g+1L),
                   a=rep(0:max_g, each=max_g+1L),
                   p=as.vector(mat), stringsAsFactors=FALSE)
  df <- df[order(-df$p), ]
  top5 <- lapply(seq_len(5L), function(i) {
    s <- df[i, ]
    list(score  = paste0(s$h, "-", s$a),
         pct    = round(s$p * 100, 2),
         result = if (s$h > s$a) "home" else if (s$h == s$a) "draw" else "away")
  })

  # ── Elo 預測 ──────────────────────────────────────────────
  elo  <- elo_predict(home, away)

  # ── 集成（Ensemble）：Poisson 50% + Elo 50% ────────────────
  W_POISSON <- 0.55; W_ELO <- 0.45
  ens_hw <- round(hw * W_POISSON + elo$hw * W_ELO, 1)
  ens_dr <- round(dr * W_POISSON + elo$dr * W_ELO, 1)
  ens_aw <- round(100 - ens_hw - ens_dr, 1)

  list(xg_h=round(xg_h,2), xg_a=round(xg_a,2),
       hw=ens_hw, dr=ens_dr, aw=ens_aw,
       poi_hw=hw, poi_dr=dr, poi_aw=aw,
       elo_hw=elo$hw, elo_dr=elo$dr, elo_aw=elo$aw,
       top5=top5)
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
  if (is.null(m$odds_h) || is.null(m$odds_d) || is.null(m$odds_a)) return("")

  # 莊家隱含機率（含抽水）
  raw_h <- 1/m$odds_h; raw_d <- 1/m$odds_d; raw_a <- 1/m$odds_a
  overround <- raw_h + raw_d + raw_a          # 抽水率（>1 = 莊家優勢）
  vig_pct <- round((overround - 1) * 100, 1)  # 抽水 %

  # 去除抽水後的公允隱含機率
  fair_h <- raw_h / overround
  fair_d <- raw_d / overround
  fair_a <- raw_a / overround

  # 模型機率
  mod_h <- sim$hw / 100; mod_d <- sim$dr / 100; mod_a <- sim$aw / 100

  # 期望值 EV = model_p * odds - 1（正 = 正期望值）
  ev_h <- round((mod_h * m$odds_h - 1) * 100, 1)
  ev_d <- round((mod_d * m$odds_d - 1) * 100, 1)
  ev_a <- round((mod_a * m$odds_a - 1) * 100, 1)

  # 優勢差（模型 - 公允隱含）
  edge_h <- round((mod_h - fair_h) * 100, 1)
  edge_d <- round((mod_d - fair_d) * 100, 1)
  edge_a <- round((mod_a - fair_a) * 100, 1)

  ev_row <- function(label, odds, model_p, fair_p, ev, edge) {
    ev_cls  <- if (ev > 5) "ev-pos" else if (ev > 0) "ev-slight" else "ev-neg"
    ev_sign <- if (ev > 0) "+" else ""
    edge_cls <- if (edge > 5) "edge-pos" else if (edge > 0) "edge-slight" else "edge-neg"
    sprintf('<tr><td class="ev-label">%s</td><td>%.2f</td><td>%.0f%%</td><td class="%s">%.0f%%</td><td class="%s">%s%.1f%%</td><td class="%s">%s%.1f%%</td></tr>',
            label, odds, model_p*100, edge_cls, fair_p*100, ev_cls, ev_sign, ev, ev_cls, ev_sign, edge)
  }

  best_ev <- max(ev_h, ev_d, ev_a)
  verdict <- if (best_ev > 8) {
    best_label <- c("主勝","平局","客勝")[which.max(c(ev_h,ev_d,ev_a))]
    sprintf('<div class="ev-verdict ev-good">✅ <b>%s</b> 有正期望值（EV %+.1f%%），模型認為賠率低估此結果</div>', best_label, best_ev)
  } else if (best_ev > 0) {
    sprintf('<div class="ev-verdict ev-neutral">⚠️ 正期望值偏低（最高 EV %+.1f%%），台彩抽水 %.1f%% 侵蝕獲利空間</div>', best_ev, vig_pct)
  } else {
    sprintf('<div class="ev-verdict ev-bad">❌ 三種結果均為負期望值，台彩抽水 %.1f%% 過高，不建議下注</div>', vig_pct)
  }

  sprintf('<div class="ev-box">
<div class="ev-title">📈 期望值分析（EV Analysis）</div>
<div class="ev-vig">台彩抽水率：<b>%.1f%%</b>（下注 100 元平均損失 %.1f 元）</div>
<table class="ev-table">
<thead><tr><th></th><th>賠率</th><th>模型機率</th><th>公允隱含</th><th>EV</th><th>優勢差</th></tr></thead>
<tbody>%s%s%s</tbody>
</table>%s</div>',
    vig_pct, vig_pct,
    ev_row("主勝", m$odds_h, mod_h, fair_h, ev_h, edge_h),
    ev_row("平局", m$odds_d, mod_d, fair_d, ev_d, edge_d),
    ev_row("客勝", m$odds_a, mod_a, fair_a, ev_a, edge_a),
    verdict)
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

  # 1.5 主力缺陣分析
  miss_h <- get_missing(ht); miss_a <- get_missing(at)
  lf_h <- if (!is.null(lineup_factor[[ht]])) lineup_factor[[ht]] else 1.0
  lf_a <- if (!is.null(lineup_factor[[at]])) lineup_factor[[at]] else 1.0
  if (length(miss_h) > 0) {
    plist <- paste(sapply(miss_h, function(p) sprintf('<b>%s</b>（%s，%s）', p$name, p$pos, p$note)), collapse="、")
    pct   <- round((1 - lf_h) * 100)
    notes <- c(notes, sprintf('🚑 <b>%s 主力傷缺</b>：%s。攻擊力預估下降 %d%%，模型已調整。', ht, plist, pct))
  }
  if (length(miss_a) > 0) {
    plist <- paste(sapply(miss_a, function(p) sprintf('<b>%s</b>（%s，%s）', p$name, p$pos, p$note)), collapse="、")
    pct   <- round((1 - lf_a) * 100)
    notes <- c(notes, sprintf('🚑 <b>%s 主力傷缺</b>：%s。攻擊力預估下降 %d%%，模型已調整。', at, plist, pct))
  }

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

  # 6. 晉級壓力（從積分榜計算）
  sh <- get_standing(ht); sa <- get_standing(at)
  if (!is.na(sh$pts) || !is.na(sa$pts)) {
    hpts <- if (is.na(sh$pts)) "–" else sh$pts
    apts <- if (is.na(sa$pts)) "–" else sa$pts
    if (sh$pressure >= 0.08 || sa$pressure >= 0.08) {
      desperate <- if (sh$pressure >= sa$pressure) ht else at
      notes <- c(notes, sprintf('🔑 <b>生死壓力</b>：%s（%s）已到背水一戰，激進打法可能帶來更多空間，比賽節奏預計激烈，也可能造成戰術失當。', desperate, if(sh$pressure>=sa$pressure) sh$status else sa$status))
    } else if (sh$gp > 0 || sa$gp > 0) {
      notes <- c(notes, sprintf('📋 <b>積分態勢</b>：%s 現有 %s 分（%s），%s 現有 %s 分（%s），本場積分直接影響小組排名。', ht, hpts, sh$status, at, apts, sa$status))
    }
  }
  if (hp == 0 && ap == 0)
    notes <- c(notes, '⚡ <b>首戰效應</b>：雙方均為本屆首場，世界盃首戰心理壓力大、技術狀態尚未磨合，爆冷機率比後續輪次高 20-30%，模型已調整。')

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

  # 8. 近期賽況（本屆已踢場次）
  hf <- recent_form[[ht]]; af <- recent_form[[at]]
  if (length(hf) > 0 || length(af) > 0) {
    hform <- if (length(hf) > 0) paste(hf, collapse="") else "–"
    aform <- if (length(af) > 0) paste(af, collapse="") else "–"
    h_hot <- sum(hf == "W") >= 2; a_hot <- sum(af == "W") >= 2
    h_cold <- sum(hf == "L") >= 2; a_cold <- sum(af == "L") >= 2
    if (h_hot) notes <- c(notes, sprintf('🔴 <b>主隊近況火熱</b>：%s 本屆戰績 %s，士氣高昂，莊家可能已調低主勝賠率，需確認是否仍有價值。', ht, hform))
    if (a_hot) notes <- c(notes, sprintf('🔵 <b>客隊狀態亮眼</b>：%s 本屆戰績 %s 表現突出，莊家可能尚未完全反映在盤口，留意客勝賠率。', at, aform))
    if (h_cold && !a_cold) notes <- c(notes, sprintf('⚠️ <b>主隊低迷</b>：%s 本屆 %s，低迷狀態下主場優勢縮水，建議審慎看待主勝盤。', ht, hform))
  }

  # 9. 教練分析
  ch_h <- get_coach(ht); ch_a <- get_coach(at)
  if (ch_h$rating >= 1.05 || ch_a$rating >= 1.05) {
    top_coach <- if (ch_h$rating >= ch_a$rating) sprintf('%s（%s，頂級名帥）', ch_h$name, ht) else sprintf('%s（%s，頂級名帥）', ch_a$name, at)
    notes <- c(notes, sprintf('👔 <b>教練優勢</b>：%s 執教評分顯著偏高，世界盃大賽經驗豐富，臨場調整能力強，面對膠著局面更能扭轉戰局。', top_coach))
  } else if (abs(ch_h$rating - ch_a$rating) >= 0.04) {
    better <- if (ch_h$rating > ch_a$rating) sprintf('%s（%s）', ch_h$name, ht) else sprintf('%s（%s）', ch_a$name, at)
    notes <- c(notes, sprintf('👔 <b>教練差距</b>：%s 執教能力略勝一籌，策略部署上佔有微幅優勢。', better))
  } else {
    notes <- c(notes, sprintf('👔 <b>教練</b>：%s（%s）vs %s（%s），執教能力相當，戰術佈局成勝負關鍵。', ch_h$name, ht, ch_a$name, at))
  }

  # 10. 天氣環境
  wt <- get_weather(if (!is.null(m$venue)) m$venue else "")
  if (wt$impact <= -0.05)
    notes <- c(notes, sprintf('🌡️ <b>極端天氣警示</b>：%s 場地 %s，高溫潮濕嚴重消耗體力，體能較差或陣容深度不足的球隊後半場失球風險大幅提升，比賽節奏預計放慢、下半場進球偏多。', m$venue, wt$desc))
  else if (wt$impact <= -0.02)
    notes <- c(notes, sprintf('☀️ <b>天氣影響</b>：%s（%s），中等程度高溫，對身體對抗型球隊較為不利，建議留意 60-90 分鐘的體能表現。', m$venue, wt$desc))
  else if (wt$impact >= 0.01)
    notes <- c(notes, sprintf('🌤️ <b>天氣有利</b>：%s（%s），氣候涼爽適宜，有利技術足球發揮，場面預計較為開放。', m$venue, wt$desc))

  if (length(notes) == 0) return("")
  items <- paste(sapply(notes, function(n) sprintf('<div class="ai-item">%s</div>', n)), collapse="")
  sprintf('<div class="ai-box"><div class="ai-title">🧠 運彩投資專家綜合分析</div>%s</div>', items)
}

match_html <- function(m, sim) {
  hf  <- FLAG_MAP[m$home]; af <- FLAG_MAP[m$away]
  tc  <- temp_class(m$temp)
  odds_h <- if (!is.null(m$odds_h)) sprintf("%.2f", m$odds_h) else "N/A"
  odds_d <- if (!is.null(m$odds_d)) sprintf("%.2f", m$odds_d) else "N/A"
  odds_a <- if (!is.null(m$odds_a)) sprintf("%.2f", m$odds_a) else "N/A"
  utag   <- upset_tag(sim, m$home, m$away)
  vtip   <- value_tip(sim, m)
  analysis <- expert_analysis(m, sim)
  # 教練資訊
  ch_h <- get_coach(m$home); ch_a <- get_coach(m$away)
  coach_row <- sprintf('<div class="coach-row"><span>👔 %s</span><span style="color:#94a3b8">vs</span><span>%s 👔</span></div>', ch_h$name, ch_a$name)
  # 天氣資訊
  wt <- get_weather(if (!is.null(m$venue)) m$venue else "")
  weather_row <- sprintf('<div class="weather-row">%s %s</div>', wt$emoji, wt$desc)

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
  %s
  %s
  <div class="model-breakdown">
    <div class="mb-row"><span class="mb-label">📐 泊松分布</span><span class="mb-vals"><b>%s%%</b> / %s%% / <b>%s%%</b></span></div>
    <div class="mb-row"><span class="mb-label">📊 埃羅評分</span><span class="mb-vals"><b>%s%%</b> / %s%% / <b>%s%%</b></span></div>
    <div class="mb-row mb-ens"><span class="mb-label">🤖 集成預測</span><span class="mb-vals"><b>%s%%</b> / %s%% / <b>%s%%</b></span></div>
  </div>
  <div class="scores-section">
    <div class="scores-title">Poisson 機率分析 — 最可能比分</div>
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
    coach_row, weather_row,
    sim$poi_hw, sim$poi_dr, sim$poi_aw,
    sim$elo_hw, sim$elo_dr, sim$elo_aw,
    sim$hw, sim$dr, sim$aw,
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

# ── 已完賽比賽卡片 ──────────────────────────────────────────
played_match_html <- function(m) {
  hf  <- FLAG_MAP[m$home]; af <- FLAG_MAP[m$away]
  hs  <- m$home_score;     as_ <- m$away_score
  res <- if (hs > as_) "home" else if (hs == as_) "draw" else "away"
  res_cls  <- list(home="res-home", draw="res-draw", away="res-away")[[res]]
  res_text <- list(home=sprintf("%s 勝", m$home),
                   draw="平局",
                   away=sprintf("%s 勝", m$away))[[res]]
  grp <- if (!is.null(m$group)) m$group else ""
  venue <- if (!is.null(m$venue)) m$venue else ""
  date_label <- sub("2026-", "", m$date)
  sprintf('
<div class="card played-card">
  <div class="card-header">
    <span class="venue"><b>%s</b> · %s</span>
    <span class="grp-chip">%s</span>
  </div>
  <div class="played-scoreline">
    <div class="played-team">
      <span class="flag">%s</span>
      <div class="tname">%s</div>
    </div>
    <div class="played-score %s">
      <span class="score-num">%d</span>
      <span class="score-sep">–</span>
      <span class="score-num">%d</span>
    </div>
    <div class="played-team">
      <span class="flag">%s</span>
      <div class="tname">%s</div>
    </div>
  </div>
  <div class="played-result-label %s">%s</div>
</div>',
  venue, date_label, grp,
  hf, m$home,
  res_cls, hs, as_,
  af, m$away,
  res_cls, res_text)
}

# 按日期（倒序）分組已完賽場次
played_dates <- sort(unique(sapply(played_matches, function(m) m$date)), decreasing=TRUE)
played_section <- paste(sapply(played_dates, function(d) {
  ms    <- Filter(function(m) m$date == d, played_matches)
  label <- if (!is.na(DATE_LABELS[d])) DATE_LABELS[[d]] else d
  cards <- paste(sapply(ms, played_match_html), collapse="\n")
  sprintf('<div class="day-label">%s</div>%s', label, cards)
}), collapse="\n")

played_html <- sprintf('<div id="played-section" class="day-section" style="display:none">%s</div>', played_section)

# ── 即將出賽分頁按鈕 ─────────────────────────────────────
tab_btns <- paste(sapply(seq_along(dates), function(i) {
  d <- dates[i]; sh <- sub("2026-", "", d)
  ac <- if (i == 1) " active" else ""
  sprintf('<button class="tab%s" onclick="showDay(\'%s\',this)">%s</button>', ac, d, sh)
}), collapse="\n")

# 在最前面加「已完賽」分頁按鈕
tab_btns <- paste0(
  sprintf('<button class="tab tab-played" onclick="showPlayed(this)">✅ 已完賽（%d）</button>\n', played_n),
  tab_btns
)

sections <- paste(sapply(seq_along(dates), function(i) {
  d  <- dates[i]; label <- DATE_LABELS[d]
  ms <- Filter(function(m) m$date == d, remaining)
  cards <- paste(sapply(ms, function(m) {
    heat <- ifelse(is.null(m$heat), 0, m$heat)
    hp   <- get_played_g(m$home); ap <- get_played_g(m$away)
    sim  <- simulate_match(m$home, m$away, heat, isTRUE(m$altitude), hp, ap, venue=if(!is.null(m$venue)) m$venue else "")
    cat(sprintf("  ✓ %s vs %s  xG[%.2f-%.2f]  %s%%-%s%%-%s%%\n",
                m$home, m$away, sim$xg_h, sim$xg_a, sim$hw, sim$dr, sim$aw))
    match_html(m, sim)
  }), collapse="\n")
  disp <- if (i == 1) "block" else "none"
  sprintf('<div id="day-%s" class="day-section" style="display:%s"><div class="day-label">%s</div>%s</div>',
          d, disp, label, cards)
}), collapse="\n")

sections <- paste0(played_html, "\n", sections)

# ── 淘汰賽賽程 ───────────────────────────────────────────────
ko_data <- tryCatch(fromJSON("data/knockouts.json", simplifyVector=FALSE), error=function(e) NULL)

ko_match_card <- function(m) {
  h_known <- nchar(m$home) > 0
  a_known <- nchar(m$away) > 0
  both    <- h_known && a_known
  hflag <- if (h_known && !is.na(FLAG_MAP[m$home])) FLAG_MAP[[m$home]] else "🏳️"
  aflag <- if (a_known && !is.na(FLAG_MAP[m$away])) FLAG_MAP[[m$away]] else "🏳️"
  hname <- if (h_known) m$home else m$label_h
  aname <- if (a_known) m$away else m$label_a
  date_tw <- {
    ko <- if (!is.null(m$ko)) as.numeric(m$ko) else NA
    if (!is.na(ko)) {
      tw <- as.POSIXct(ko + 8*3600, origin="1970-01-01", tz="UTC")
      format(tw, "%m/%d %H:%M 台灣")
    } else m$date
  }
  probs_html <- if (both && h_known && a_known) {
    sim <- tryCatch(
      simulate_match(m$home, m$away, heat=0, altitude=FALSE,
                     h_played=get_played_g(m$home), a_played=get_played_g(m$away),
                     venue=if(!is.null(m$venue)) m$venue else ""),
      error=function(e) NULL)
    if (!is.null(sim))
      sprintf('<div class="ko-probs"><span class="kph">主%s%%</span><span class="kpd">平%s%%</span><span class="kpa">客%s%%</span></div>
               <div class="ko-xg">xG %s – %s</div>', sim$hw, sim$dr, sim$aw, sim$xg_h, sim$xg_a)
    else ""
  } else '<div class="ko-tbd-tag">待定</div>'
  coach_h <- if (h_known) get_coach(m$home)$name else "–"
  coach_a <- if (a_known) get_coach(m$away)$name else "–"
  coach_line <- if (both) sprintf('<div class="ko-coach">👔 %s  vs  %s</div>', coach_h, coach_a) else ""
  cls <- if (both) "ko-card known" else "ko-card tbd"
  sprintf('<div class="%s">
  <div class="ko-meta">%s · %s</div>
  <div class="ko-teams">
    <div class="ko-team%s"><span class="ko-flag">%s</span><span class="ko-nm">%s</span></div>
    <span class="ko-vs">VS</span>
    <div class="ko-team%s"><span class="ko-nm">%s</span><span class="ko-flag">%s</span></div>
  </div>
  %s%s
</div>', cls, date_tw, m$round,
    if(h_known) "" else " tbd-team", hflag, hname,
    if(a_known) "" else " tbd-team", aname, aflag,
    probs_html, coach_line)
}

knockout_html <- if (!is.null(ko_data)) {
  round_order <- c("r32","r16","qf","sf","third","final")
  round_labels <- c(r32="⚔️ 32強",r16="🔥 16強",qf="💥 8強（準決賽）",
                    sf="🌟 4強（半決賽）",third="🥉 季軍賽",final="🏆 決賽")
  paste(sapply(round_order, function(rnd) {
    ms <- ko_data[[rnd]]
    if (is.null(ms) || length(ms)==0) return("")
    cards <- paste(sapply(ms, ko_match_card), collapse="\n")
    sprintf('<div class="ko-round"><div class="ko-round-title">%s</div><div class="ko-grid">%s</div></div>',
            round_labels[rnd], cards)
  }), collapse="\n")
} else '<div class="ko-empty">淘汰賽賽程載入中...</div>'

# 加入淘汰賽 tab 按鈕
tab_btns <- paste0(tab_btns, '\n<button class="tab" onclick="showKO(this)">🏆 淘汰賽</button>')

# ══════════════════════════════════════════════════════════════
# 蒙特卡洛全賽事模擬（奪冠機率）
# ══════════════════════════════════════════════════════════════
mc_sim_match <- function(h, a) {
  eh <- if (!is.na(ELO[h])) ELO[[h]] else 1500
  ea <- if (!is.na(ELO[a])) ELO[[a]] else 1500
  lf_h <- if (!is.null(lineup_factor[[h]])) lineup_factor[[h]] else 1.0
  lf_a <- if (!is.null(lineup_factor[[a]])) lineup_factor[[a]] else 1.0
  xh <- max(0.4, base_lam[h] * atk[h] / def[a]) * lf_h
  xa <- max(0.4, base_lam[a] * atk[a] / def[h]) * lf_a
  diff <- (eh - ea + 100) / 400
  ep <- 1 / (1 + 10^(-diff))
  poi_hw <- sum(outer(dpois(0:8,xh), dpois(0:8,xa))[row(matrix(0,9,9))>col(matrix(0,9,9))])
  poi_dr <- sum(diag(outer(dpois(0:8,xh), dpois(0:8,xa))))
  poi_aw <- 1 - poi_hw - poi_dr
  p_hw <- poi_hw * 0.55 + ep * 0.85 * 0.45
  p_dr <- poi_dr * 0.55 + max(0.10, 0.26 - abs(ep-0.5)*0.32) * 0.45
  p_aw <- max(0, 1 - p_hw - p_dr)
  r <- runif(1)
  if (r < p_hw) h else if (r < p_hw + p_dr) "draw" else a
}

mc_champion <- function(n_sim=5000L) {
  # 取得當前小組排名（用已踢結果）
  grp_teams <- list()
  for (m in matches) {
    grp <- m$group
    if (is.null(grp) || grp == "") next
    for (t in c(m$home, m$away))
      if (is.null(grp_teams[[grp]][[t]]))
        grp_teams[[grp]][[t]] <- list(pts=0L,gd=0L,gf=0L,team=t)
    if (!isTRUE(m$played)) next
    h <- m$home; a <- m$away
    hs <- m$home_score; as_ <- m$away_score
    grp_teams[[grp]][[h]]$gf  <- grp_teams[[grp]][[h]]$gf + hs
    grp_teams[[grp]][[h]]$gd  <- grp_teams[[grp]][[h]]$gd + hs-as_
    grp_teams[[grp]][[a]]$gf  <- grp_teams[[grp]][[a]]$gf + as_
    grp_teams[[grp]][[a]]$gd  <- grp_teams[[grp]][[a]]$gd + as_-hs
    if (hs>as_)       grp_teams[[grp]][[h]]$pts <- grp_teams[[grp]][[h]]$pts+3L
    else if(hs==as_){ grp_teams[[grp]][[h]]$pts <- grp_teams[[grp]][[h]]$pts+1L
                      grp_teams[[grp]][[a]]$pts <- grp_teams[[grp]][[a]]$pts+1L }
    else              grp_teams[[grp]][[a]]$pts <- grp_teams[[grp]][[a]]$pts+3L
  }
  rank_g <- function(gp) {
    tl <- lapply(names(gp), function(t) gp[[t]])
    tl[order(-sapply(tl,`[[`,"pts"), -sapply(tl,`[[`,"gd"), -sapply(tl,`[[`,"gf"))]
  }
  wins <- setNames(integer(length(base_lam)), names(base_lam))

  for (sim_i in seq_len(n_sim)) {
    # 先模擬剩餘小組賽
    cur <- grp_teams
    for (m in matches) {
      if (isTRUE(m$played)) next
      grp <- m$group; if(is.null(grp)||grp=="") next
      h <- m$home; a <- m$away
      res <- mc_sim_match(h, a)
      if (res==h)      { cur[[grp]][[h]]$pts<-cur[[grp]][[h]]$pts+3L; cur[[grp]][[h]]$gd<-cur[[grp]][[h]]$gd+1L; cur[[grp]][[h]]$gf<-cur[[grp]][[h]]$gf+1L }
      else if(res=="draw"){ cur[[grp]][[h]]$pts<-cur[[grp]][[h]]$pts+1L; cur[[grp]][[a]]$pts<-cur[[grp]][[a]]$pts+1L }
      else             { cur[[grp]][[a]]$pts<-cur[[grp]][[a]]$pts+3L; cur[[grp]][[a]]$gd<-cur[[grp]][[a]]$gd+1L; cur[[grp]][[a]]$gf<-cur[[grp]][[a]]$gf+1L }
    }
    # 各組前2名 + 最佳4個第3名 進入32強（簡化：取各組前2名 = 16 × 2 = 32隊，只模擬前16名）
    advancers <- c()
    for (grp in names(cur)) {
      rk <- rank_g(cur[[grp]])
      advancers <- c(advancers, rk[[1]]$team, rk[[2]]$team)
    }
    # 淘汰賽隨機配對（簡化輪空模擬）
    bracket <- sample(advancers)
    while (length(bracket) > 1) {
      next_rd <- c()
      for (k in seq(1, length(bracket), by=2)) {
        if (k+1 > length(bracket)) { next_rd <- c(next_rd, bracket[k]); next }
        res <- mc_sim_match(bracket[k], bracket[k+1])
        winner <- if (res=="draw") { if(runif(1)>0.5) bracket[k] else bracket[k+1] } else res
        next_rd <- c(next_rd, winner)
      }
      bracket <- next_rd
    }
    if (length(bracket)==1 && bracket[1] %in% names(wins))
      wins[[bracket[1]]] <- wins[[bracket[1]]] + 1L
  }
  wins_pct <- sort(wins/n_sim*100, decreasing=TRUE)
  wins_pct[wins_pct >= 0.5]
}

cat("[MC] 執行蒙特卡洛奪冠模擬（5000次）...\n")
mc_odds <- tryCatch(mc_champion(5000L), error=function(e){ cat("[MC] 錯誤:", conditionMessage(e),"\n"); numeric(0) })
cat(sprintf("[MC] 完成，前3: %s\n", paste(names(mc_odds)[1:min(3,length(mc_odds))],
    sprintf("%.1f%%", mc_odds[1:min(3,length(mc_odds))]), sep="=", collapse=" / ")))

mc_html <- if (length(mc_odds) > 0) {
  top_n <- min(12L, length(mc_odds))
  rows <- paste(sapply(seq_len(top_n), function(i) {
    t <- names(mc_odds)[i]; p <- mc_odds[i]
    fl <- if (!is.na(FLAG_MAP[t])) FLAG_MAP[[t]] else "🏳️"
    bar_w <- round(p / mc_odds[1] * 100)
    sprintf('<div class="mc-row"><span class="mc-rank">%d</span><span class="mc-flag">%s</span><span class="mc-team">%s</span><div class="mc-bar-wrap"><div class="mc-bar" style="width:%d%%"></div></div><span class="mc-pct">%.1f%%</span></div>', i, fl, t, bar_w, p)
  }), collapse="")
  sprintf('<div id="mc-section" class="day-section" style="display:none">
<div class="day-label">🎲 蒙特卡洛奪冠機率（5,000次模擬）</div>
<div class="mc-box">
<div class="mc-subtitle">根據 Poisson+Elo 集成模型模擬完整賽程，涵蓋小組賽剩餘場次 + 淘汰賽</div>
%s</div></div>', rows)
} else ""

if (nchar(mc_html) > 0)
  tab_btns <- paste0('<button class="tab" onclick="showMC(this)">🎲 奪冠預測</button>\n', tab_btns)

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
  "    '<div class=\"cd-twtime\">\U0001F1F9\U0001F1FC 台灣時間約 '+twt+' 開賽 · <a href=\"https://article.sportslottery.com.tw/\" style=\"color:#34d399\">確認運彩賽程</a></div>'+",
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
.played-card{opacity:.92}.played-card:hover{border-color:rgba(16,185,129,.3)}
.played-scoreline{display:grid;grid-template-columns:1fr auto 1fr;align-items:center;gap:.5rem;padding:1rem 1.2rem .6rem}
.played-team{text-align:center}.played-team .flag{font-size:1.9rem;display:block;margin-bottom:4px}
.played-score{display:flex;align-items:center;gap:6px;padding:6px 14px;border-radius:10px;font-size:1.6rem;font-weight:700;border:2px solid}
.played-score.res-home{background:rgba(16,185,129,.1);color:#34d399;border-color:rgba(16,185,129,.3)}
.played-score.res-draw{background:rgba(251,191,36,.08);color:#fcd34d;border-color:rgba(251,191,36,.25)}
.played-score.res-away{background:rgba(99,102,241,.1);color:#a5b4fc;border-color:rgba(99,102,241,.3)}
.score-num{min-width:24px;text-align:center}.score-sep{color:var(--muted);font-weight:300}
.played-result-label{text-align:center;font-size:11px;font-weight:600;padding:0 1rem .7rem;letter-spacing:.3px}
.played-result-label.res-home{color:#34d399}.played-result-label.res-draw{color:#fcd34d}.played-result-label.res-away{color:#a5b4fc}
.tab-played{border-color:rgba(16,185,129,.3)!important;color:#34d399!important}
.tab-played.active{background:rgba(16,185,129,.15)!important;border-color:#34d399!important}
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
.model-breakdown{margin:.4rem 1rem .5rem;border:1px solid rgba(59,130,246,.15);border-radius:8px;overflow:hidden;font-size:11px}
.mb-row{display:flex;justify-content:space-between;align-items:center;padding:4px 10px;border-bottom:1px solid rgba(255,255,255,.04)}
.mb-row:last-child{border-bottom:none}
.mb-ens{background:rgba(59,130,246,.08)}
.mb-label{color:var(--muted);white-space:nowrap}
.mb-vals{color:var(--text);letter-spacing:.3px}
.mb-vals b{color:#93c5fd}
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
/* EV Analysis */
.ev-box{margin:.4rem 1rem .6rem;border:1px solid rgba(139,92,246,.2);border-radius:10px;overflow:hidden;font-size:11px}
.ev-title{background:rgba(139,92,246,.1);padding:5px 10px;font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#c4b5fd}
.ev-vig{padding:4px 10px;font-size:11px;color:var(--muted);border-bottom:1px solid rgba(255,255,255,.05)}
.ev-vig b{color:#f87171}
.ev-table{width:100%;border-collapse:collapse;font-size:11px}
.ev-table thead tr{background:rgba(0,0,0,.2)}
.ev-table th{padding:4px 6px;color:var(--muted);font-weight:500;text-align:right;font-size:10px}
.ev-table th:first-child{text-align:left}
.ev-table td{padding:4px 6px;text-align:right;border-top:.5px solid rgba(255,255,255,.04);color:var(--text)}
.ev-table td.ev-label{text-align:left;color:var(--muted);font-weight:500}
.ev-pos{color:#34d399;font-weight:700}
.ev-slight{color:#fcd34d;font-weight:600}
.ev-neg{color:#f87171}
.edge-pos{color:#34d399;font-weight:600}
.edge-slight{color:#fcd34d}
.edge-neg{color:#6b7280}
.ev-verdict{padding:6px 10px;font-size:11px;line-height:1.5}
.ev-good{background:rgba(16,185,129,.08);color:#34d399}
.ev-neutral{background:rgba(251,191,36,.07);color:#fcd34d}
.ev-bad{background:rgba(239,68,68,.07);color:#f87171}
.coach-row{display:flex;justify-content:space-between;align-items:center;margin:.2rem 1rem .1rem;padding:5px 10px;border-radius:6px;background:rgba(139,92,246,.07);border:1px solid rgba(139,92,246,.2);font-size:11px;color:#c4b5fd}
.weather-row{margin:.1rem 1rem .3rem;padding:5px 10px;border-radius:6px;background:rgba(56,189,248,.06);border:1px solid rgba(56,189,248,.18);font-size:11px;color:#7dd3fc;text-align:center}
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
/* knockout */
.ko-note{font-size:11px;color:#fcd34d;background:rgba(245,158,11,.07);border:1px solid rgba(245,158,11,.2);border-radius:8px;padding:8px 12px;margin:.5rem 0 .8rem;text-align:center}
.ko-round{margin-bottom:1.5rem}
.ko-round-title{font-size:13px;font-weight:700;color:#93c5fd;margin:.8rem 0 .5rem;padding-bottom:.3rem;border-bottom:1px solid rgba(59,130,246,.2)}
.ko-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(260px,1fr));gap:.6rem}
.ko-card{background:var(--card);border:1px solid var(--border);border-radius:10px;padding:.7rem .9rem;transition:border-color .2s}
.ko-card.known{border-color:rgba(59,130,246,.25)}
.ko-card.tbd{opacity:.7}
.ko-meta{font-size:10px;color:var(--muted);margin-bottom:.4rem}
.ko-teams{display:flex;align-items:center;justify-content:space-between;gap:.4rem;margin-bottom:.4rem}
.ko-team{display:flex;align-items:center;gap:5px;flex:1}
.ko-team:last-child{flex-direction:row-reverse;text-align:right}
.ko-flag{font-size:1.2rem}
.ko-nm{font-size:12px;font-weight:600;color:var(--text)}
.tbd-team .ko-nm{color:var(--muted);font-style:italic;font-weight:400}
.ko-vs{font-size:11px;font-weight:700;color:var(--muted);flex-shrink:0}
.ko-probs{display:flex;gap:6px;font-size:11px;margin:.2rem 0}
.kph{color:#34d399;font-weight:600}.kpd{color:#94a3b8}.kpa{color:#f87171;font-weight:600}
.ko-xg{font-size:10px;color:var(--muted)}
.ko-tbd-tag{font-size:10px;color:var(--muted);font-style:italic;margin-top:.2rem}
.ko-coach{font-size:10px;color:#c4b5fd;margin-top:.3rem}
.ko-empty{text-align:center;color:var(--muted);padding:2rem;font-size:13px}
/* monte carlo */
.mc-box{background:var(--card);border:1px solid var(--border);border-radius:12px;padding:1rem;margin-top:.5rem}
.mc-subtitle{font-size:11px;color:var(--muted);margin-bottom:.8rem;text-align:center;line-height:1.5}
.mc-row{display:flex;align-items:center;gap:8px;padding:5px 0;border-bottom:.5px solid rgba(255,255,255,.05)}
.mc-row:last-child{border-bottom:none}
.mc-rank{font-size:11px;font-weight:700;color:var(--muted);min-width:16px;text-align:right}
.mc-flag{font-size:1.1rem;min-width:22px}
.mc-team{font-size:12px;font-weight:500;min-width:100px;color:var(--text)}
.mc-bar-wrap{flex:1;background:rgba(255,255,255,.05);border-radius:4px;height:6px;overflow:hidden}
.mc-bar{height:100%;background:linear-gradient(90deg,#3b82f6,#8b5cf6);border-radius:4px;transition:width .4s}
.mc-pct{font-size:12px;font-weight:600;color:#93c5fd;min-width:40px;text-align:right}
.footer{text-align:center;padding:2rem 1rem 1rem;font-size:11px;color:var(--muted);line-height:1.9;border-top:1px solid var(--border);margin-top:1.5rem}
.footer strong{color:#60a5fa}
@media(max-width:520px){.flag{font-size:1.5rem}.tname{font-size:12px}.sval{font-size:.95rem}.scores-grid{grid-template-columns:repeat(3,1fr)}.s4,.s5{display:none}}
</style>
</head>
<body>
<div class="hero">
  <div class="badge"><span class="dot"></span> 每2小時自動更新 · xG Poisson 模型</div>
  <h1>2026 FIFA 世界盃<br>比分預測 <em>AI</em></h1>
  <p>📐 泊松分布 · 📊 埃羅評分 · 🎲 蒙特卡洛 · 🧠 貝葉斯更新 · 🤖 集成預測</p>
  <p class="update-info">最後更新：<strong>', today, '</strong>｜已完成 ', played_n, ' 場 · 剩餘 ', length(remaining), ' 場</p>
  <div class="stats-row">
    <div class="stat"><div class="n">', length(remaining), '</div><div class="l">場小組賽剩餘</div></div>
    <div class="stat"><div class="n">32強</div><div class="l">淘汰賽賽程</div></div>
    <div class="stat"><div class="n">10+</div><div class="l">分析維度</div></div>
  </div>
  <div class="sporttery-bar">
    <a href="https://article.sportslottery.com.tw/" class="sporttery-btn">
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
  <div class="disc">⚡ 純統計模型，賠率為台灣運彩資料（需手動更新），僅供娛樂參考。</div>
  ', sections, '
  ', mc_html, '
  <div id="ko-section" style="display:none">
    <div class="ko-note">📌 淘汰賽賽程依小組賽結果自動更新 · 確認晉級後顯示預測機率</div>
    ', knockout_html, '
  </div>
  <div class="footer">
    <strong>模型架構</strong>｜ xG Poisson + Bayesian 動態更新（w=0.10，係數上限 2.5）｜防守係數以除法計算<br>
    天氣：高溫 → xG 下調·墨西哥城 → 海拔加成｜自動排程：每日 UTC 06:00 重算<br>
    Claude AI × R 4.6 × GitHub Actions · 僅供娛樂
  </div>
</div>
<script>
function showDay(d,btn){
  document.querySelectorAll(".tab").forEach(function(t){t.classList.remove("active");});
  btn.classList.add("active");
  document.querySelectorAll(".day-section").forEach(function(s){s.style.display="none";});
  document.getElementById("ko-section").style.display="none";
  var mc=document.getElementById("mc-section"); if(mc) mc.style.display="none";
  document.getElementById("day-"+d).style.display="block";
}
function showKO(btn){
  document.querySelectorAll(".tab").forEach(function(t){t.classList.remove("active");});
  btn.classList.add("active");
  document.querySelectorAll(".day-section").forEach(function(s){s.style.display="none";});
  var mc=document.getElementById("mc-section"); if(mc) mc.style.display="none";
  document.getElementById("ko-section").style.display="block";
}
function showMC(btn){
  document.querySelectorAll(".tab").forEach(function(t){t.classList.remove("active");});
  btn.classList.add("active");
  document.querySelectorAll(".day-section").forEach(function(s){s.style.display="none";});
  document.getElementById("ko-section").style.display="none";
  var mc=document.getElementById("mc-section");
  if(mc) mc.style.display="block";
}
function showPlayed(btn){
  document.querySelectorAll(".tab").forEach(function(t){t.classList.remove("active");});
  btn.classList.add("active");
  document.querySelectorAll(".day-section").forEach(function(s){s.style.display="none";});
  document.getElementById("ko-section").style.display="none";
  var mc=document.getElementById("mc-section"); if(mc) mc.style.display="none";
  document.getElementById("played-section").style.display="block";
}
', cd_js, '
</script>
</body>
</html>')

writeLines(html, "index.html", useBytes = FALSE)
cat(sprintf("[OK] index.html 生成完成（%d 場比賽已模擬）\n", length(remaining)))
