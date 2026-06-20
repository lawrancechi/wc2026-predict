library(jsonlite)

set.seed(2026)
N <- 1000000

cfg     <- fromJSON("data/teams.json", simplifyVector = FALSE)
base_lam <- setNames(as.numeric(unlist(cfg$base_lambda)), names(cfg$base_lambda))
matches  <- cfg$matches

# ── 依已完成比賽更新各隊攻防係數 ────────────────────────────
atk <- setNames(rep(1.0, length(base_lam)), names(base_lam))
def <- setNames(rep(1.0, length(base_lam)), names(base_lam))

for (m in matches) {
  if (!isTRUE(m$played)) next
  h <- m$home; a <- m$away
  hs <- m$home_score; as_ <- m$away_score
  heat <- ifelse(is.null(m$heat), 0, m$heat)
  lh_exp <- max(0.1, base_lam[h] * atk[h] * def[a] + heat)
  la_exp <- max(0.1, base_lam[a] * atk[a] * def[h] + heat)
  w <- 0.4
  atk[h] <- atk[h] * (1-w) + w * (hs  / lh_exp)
  atk[a] <- atk[a] * (1-w) + w * (as_ / la_exp)
  def[h] <- def[h] * (1-w) + w * (la_exp / max(0.1, as_))
  def[a] <- def[a] * (1-w) + w * (lh_exp / max(0.1, hs))
}

# ── Poisson 模擬 ─────────────────────────────────────────
simulate_match <- function(home, away, heat, altitude = FALSE) {
  lh <- max(0.05, base_lam[home] * atk[home] * def[away] + heat)
  la <- max(0.05, base_lam[away] * atk[away] * def[home] + heat)
  if (isTRUE(altitude)) { lh <- lh * 1.15; la <- la * 0.95 }
  gh <- rpois(N, lh); ga <- rpois(N, la)
  hw  <- round(mean(gh > ga) * 100, 1)
  dr  <- round(mean(gh == ga) * 100, 1)
  aw  <- round(mean(gh < ga) * 100, 1)
  sc  <- paste0(gh, "-", ga)
  tbl <- sort(table(sc), decreasing = TRUE)
  top3 <- lapply(seq_len(min(3, length(tbl))), function(i) {
    s   <- names(tbl)[i]
    pct <- round(as.integer(tbl[i]) / N * 100, 2)
    pts <- as.integer(strsplit(s, "-")[[1]])
    res <- if (pts[1] > pts[2]) "home" else if (pts[1] == pts[2]) "draw" else "away"
    list(score = s, pct = pct, result = res)
  })
  list(lh = round(lh, 2), la = round(la, 2), hw = hw, dr = dr, aw = aw, top3 = top3)
}

# ── 輸出待預測比賽 ────────────────────────────────────────
FLAG_MAP <- c(
  Germany="🇩🇪", "Ivory Coast"="🇨🇮", Ecuador="🇪🇨", Curacao="🇨🇼",
  Netherlands="🇳🇱", Sweden="🇸🇪", Japan="🇯🇵", Tunisia="🇹🇳",
  Spain="🇪🇸", "Saudi Arabia"="🇸🇦", Uruguay="🇺🇾", "Cape Verde"="🇨🇻",
  Belgium="🇧🇪", Iran="🇮🇷", "New Zealand"="🇳🇿", Egypt="🇪🇬",
  Argentina="🇦🇷", Austria="🇦🇹", Jordan="🇯🇴", Algeria="🇩🇿",
  France="🇫🇷", Iraq="🇮🇶", Norway="🇳🇴", Senegal="🇸🇳",
  Portugal="🇵🇹", Uzbekistan="🇺🇿", Colombia="🇨🇴", "DR Congo"="🇨🇩",
  England="🏴󠁧󠁢󠁥󠁮󠁧󠁿", Ghana="🇬🇭", Panama="🇵🇦", Croatia="🇭🇷",
  Mexico="🇲🇽", "South Africa"="🇿🇦", "South Korea"="🇰🇷", Czechia="🇨🇿",
  Canada="🇨🇦", Switzerland="🇨🇭", Bosnia="🇧🇦", Qatar="🇶🇦",
  Brazil="🇧🇷", Morocco="🇲🇦", Scotland="🏴󠁧󠁢󠁳󠁣󠁴󠁿", Haiti="🇭🇹",
  USA="🇺🇸", Turkey="🇹🇷", Australia="🇦🇺", Paraguay="🇵🇾"
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
  if (t <= 26) return(list(label=paste0("🌤 ",t,"°C"), cls="success"))
  if (t <= 30) return(list(label=paste0("☀ ",t,"°C"),  cls="warning"))
  if (t <= 33) return(list(label=paste0("🌡 ",t,"°C"), cls="danger"))
  list(label=paste0("🔥 ",t,"°C"), cls="danger")
}

res_label <- function(r, h, a) {
  if (r == "home") return(sprintf('<span class="sres rh">%s</span>', h))
  if (r == "draw") return('<span class="sres rd">平局</span>')
  sprintf('<span class="sres ra">%s</span>', a)
}

match_html <- function(m, sim) {
  hf   <- FLAG_MAP[m$home]; af <- FLAG_MAP[m$away]
  tc   <- temp_class(m$temp)
  pills <- paste(sapply(seq_along(sim$top3), function(i) {
    s    <- sim$top3[[i]]
    cls  <- c("s1","s2","s3")[i]
    sprintf('<div class="spill %s"><div class="rbadge">#%d</div><div class="sval">%s</div><div class="spct">%s%%</div>%s</div>',
            cls, i, s$score, s$pct, res_label(s$result, m$home, m$away))
  }), collapse="")
  sprintf('
<div class="card">
  <div class="card-header">
    <span class="venue"><b>%s</b></span>
    <div style="display:flex;gap:5px;align-items:center">
      <span class="grp-chip">組別 %s</span>
      <span class="wchip w-%s">%s</span>
    </div>
  </div>
  <div class="teams-row">
    <div class="team"><span class="flag">%s</span><div class="tname">%s</div><div class="tform">λ=%.2f</div></div>
    <div class="vs-badge"><span class="grp-sm">模擬</span>VS</div>
    <div class="team"><span class="flag">%s</span><div class="tname">%s</div><div class="tform">λ=%.2f</div></div>
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
  <div class="scores-section">
    <div class="scores-title">最可能比分（100萬次模擬）</div>
    <div class="scores-grid">%s</div>
  </div>
</div>',
    m$venue, m$group, tc$cls, tc$label,
    hf, m$home, sim$lh,
    af, m$away, sim$la,
    sim$hw, sim$dr, sim$aw,
    sim$hw, m$home, sim$dr, sim$aw, m$away,
    pills)
}

# ── 執行模擬並建 HTML ─────────────────────────────────────
remaining <- Filter(function(m) !isTRUE(m$played), matches)
played_n  <- length(matches) - length(remaining)
today     <- format(Sys.Date(), "%Y-%m-%d")
dates     <- unique(sapply(remaining, function(m) m$date))
dates     <- sort(dates)

cat(sprintf("[SIM] %d matches to simulate...\n", length(remaining)))

tab_btns <- paste(sapply(seq_along(dates), function(i) {
  d <- dates[i]
  sh <- sub("2026-", "", d)
  ac <- if (i == 1) " active" else ""
  sprintf('<button class="tab%s" onclick="showDay(\'%s\',this)">%s</button>', ac, d, sh)
}), collapse="\n")

sections <- paste(sapply(seq_along(dates), function(i) {
  d     <- dates[i]
  label <- DATE_LABELS[d]
  ms    <- Filter(function(m) m$date == d, remaining)
  cards <- paste(sapply(ms, function(m) {
    heat <- ifelse(is.null(m$heat), 0, m$heat)
    alt  <- isTRUE(m$altitude)
    sim  <- simulate_match(m$home, m$away, heat, alt)
    cat(sprintf("  ✓ %s vs %s  [%.2f-%.2f]  %s%%-%s%%-%s%%\n",
                m$home, m$away, sim$lh, sim$la, sim$hw, sim$dr, sim$aw))
    match_html(m, sim)
  }), collapse="\n")
  disp <- if (i == 1) "block" else "none"
  sprintf('<div id="day-%s" class="day-section" style="display:%s"><div class="day-label">%s</div>%s</div>',
          d, disp, label, cards)
}), collapse="\n")

# ── HTML 骨架 ─────────────────────────────────────────────
html <- sprintf('<!DOCTYPE html>
<html lang="zh-TW">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>2026 世界盃比分預測</title>
<style>
*{box-sizing:border-box;margin:0;padding:0}
:root{--bg:#09101f;--card:#141e2e;--border:rgba(255,255,255,0.07);--text:#eef2ff;--muted:#7a8fa8;--accent:#3b82f6;--gold:#f59e0b;--silver:#9ca3af;--bronze:#b45309}
body{background:var(--bg);color:var(--text);font-family:"Segoe UI",system-ui,sans-serif;min-height:100vh;padding-bottom:3rem}
.hero{background:linear-gradient(160deg,#0d1b35 0%%,#09101f 60%%);border-bottom:1px solid var(--border);padding:2.5rem 1.5rem 2rem;text-align:center;position:relative}
.hero::before{content:"";position:absolute;inset:0;background:radial-gradient(ellipse 80%% 50%% at 50%% 0%%,rgba(59,130,246,.12) 0%%,transparent 70%%);pointer-events:none}
.badge{display:inline-flex;align-items:center;gap:6px;background:rgba(59,130,246,.12);border:1px solid rgba(59,130,246,.25);border-radius:20px;padding:4px 14px;font-size:11px;color:#93c5fd;margin-bottom:1rem}
.dot{display:inline-block;width:6px;height:6px;background:#3b82f6;border-radius:50%%;animation:pulse 1.5s ease-in-out infinite}
@keyframes pulse{0%%,100%%{opacity:1}50%%{opacity:.3}}
.hero h1{font-size:clamp(1.6rem,4vw,2.4rem);font-weight:700;letter-spacing:-.02em;line-height:1.15;margin-bottom:.5rem}
.hero h1 em{font-style:normal;color:var(--gold)}
.hero p{color:var(--muted);font-size:13px;max-width:500px;margin:0 auto .8rem;line-height:1.6}
.update-info{font-size:11px;color:var(--muted);margin-top:.4rem}
.update-info strong{color:#34d399}
.stats-row{display:flex;justify-content:center;gap:2rem;flex-wrap:wrap;margin-top:1rem}
.stat .n{font-size:1.2rem;font-weight:700;color:var(--accent)}
.stat .l{font-size:10px;color:var(--muted);margin-top:2px}
.container{max-width:960px;margin:0 auto;padding:0 1rem}
.tabs{display:flex;gap:6px;padding:1.2rem 0 .8rem;overflow-x:auto;scrollbar-width:none}
.tabs::-webkit-scrollbar{display:none}
.tab{flex-shrink:0;border:1px solid var(--border);background:rgba(255,255,255,.03);color:var(--muted);padding:6px 14px;border-radius:18px;font-size:12px;cursor:pointer;transition:all .2s}
.tab:hover{border-color:rgba(59,130,246,.4);color:var(--text)}
.tab.active{background:var(--accent);border-color:var(--accent);color:#fff;font-weight:500}
.day-section{display:none}
.day-label{display:flex;align-items:center;gap:8px;font-size:10px;font-weight:600;letter-spacing:.08em;color:var(--muted);text-transform:uppercase;padding:.8rem 0 .6rem}
.day-label::after{content:"";flex:1;height:.5px;background:var(--border)}
.card{background:var(--card);border:1px solid var(--border);border-radius:14px;margin-bottom:.9rem;overflow:hidden;transition:border-color .2s}
.card:hover{border-color:rgba(59,130,246,.25)}
.card-header{padding:.55rem 1rem;border-bottom:1px solid var(--border);background:rgba(0,0,0,.2);display:flex;align-items:center;justify-content:space-between;gap:8px;flex-wrap:wrap}
.venue{font-size:11px;color:var(--muted)}.venue b{color:var(--text);font-weight:500}
.grp-chip{font-size:10px;font-weight:600;padding:2px 8px;border-radius:10px;background:rgba(59,130,246,.15);color:#93c5fd;border:1px solid rgba(59,130,246,.25)}
.wchip{display:inline-flex;align-items:center;gap:4px;border-radius:16px;padding:3px 9px;font-size:10px;font-weight:500;border:1px solid}
.w-success{background:rgba(16,185,129,.1);color:#34d399;border-color:rgba(16,185,129,.2)}
.w-warning{background:rgba(251,191,36,.1);color:#fcd34d;border-color:rgba(251,191,36,.2)}
.w-danger{background:rgba(239,68,68,.1);color:#f87171;border-color:rgba(239,68,68,.2)}
.teams-row{padding:.85rem 1rem .75rem;display:grid;grid-template-columns:1fr 60px 1fr;align-items:center;gap:.5rem}
.team{text-align:center}
.flag{font-size:1.9rem;display:block;margin-bottom:4px;line-height:1}
.tname{font-size:13px;font-weight:500}.tform{font-size:10px;color:var(--muted);margin-top:3px}
.vs-badge{background:rgba(255,255,255,.04);border:1px solid var(--border);border-radius:8px;padding:5px 0;font-size:11px;font-weight:600;color:var(--muted);text-align:center}
.grp-sm{font-size:9px;color:var(--muted);display:block;margin-bottom:1px}
.prob-section{padding:0 1rem .75rem}
.prob-bar-wrap{display:flex;border-radius:4px;overflow:hidden;height:6px;gap:2px;margin-bottom:5px}
.prob-seg{height:100%%;border-radius:2px}
.seg-home{background:var(--accent)}.seg-draw{background:#4b5563}.seg-away{background:#8b5cf6}
.prob-labels{display:flex;justify-content:space-between;font-size:10px}
.pl-home strong{color:var(--accent)}.pl-draw strong{color:#9ca3af}.pl-away strong{color:#a78bfa}
.prob-labels span{color:var(--muted)}
.scores-section{padding:0 1rem 1rem}
.scores-title{font-size:10px;font-weight:500;letter-spacing:.06em;color:var(--muted);text-transform:uppercase;margin-bottom:.6rem}
.scores-grid{display:grid;grid-template-columns:repeat(3,1fr);gap:8px}
.spill{border-radius:10px;padding:10px 6px 8px;text-align:center;border:.5px solid;position:relative}
.s1{background:rgba(245,158,11,.08);border-color:rgba(245,158,11,.3)}
.s2{background:rgba(255,255,255,.03);border-color:rgba(255,255,255,.08)}
.s3{background:rgba(255,255,255,.02);border-color:rgba(255,255,255,.06)}
.rbadge{position:absolute;top:-7px;left:50%%;transform:translateX(-50%%);font-size:8px;font-weight:600;padding:1px 7px;border-radius:8px;white-space:nowrap}
.s1 .rbadge{background:var(--gold);color:#1a0e00}
.s2 .rbadge{background:var(--silver);color:#111}
.s3 .rbadge{background:var(--bronze);color:#fff8f0}
.sval{font-size:1.35rem;font-weight:600;margin-top:6px;line-height:1}
.s1 .sval{color:var(--gold)}.s2 .sval{color:#d1d5db}.s3 .sval{color:#d97706}
.spct{font-size:10px;color:var(--muted);margin-top:3px}
.sres{font-size:9px;margin-top:3px;padding:2px 6px;border-radius:4px;display:inline-block}
.rh{background:rgba(59,130,246,.15);color:#93c5fd}
.rd{background:rgba(107,114,128,.2);color:#9ca3af}
.ra{background:rgba(139,92,246,.15);color:#c4b5fd}
.disc{background:rgba(245,158,11,.06);border:1px solid rgba(245,158,11,.2);border-radius:8px;padding:8px 12px;font-size:11px;color:#fcd34d;margin:.6rem 0;display:flex;gap:6px}
.footer{text-align:center;padding:2rem 1rem 1rem;font-size:11px;color:var(--muted);line-height:1.9;border-top:1px solid var(--border);margin-top:1.5rem}
.footer strong{color:#60a5fa}
@media(max-width:480px){.flag{font-size:1.5rem}.tname{font-size:12px}.sval{font-size:1.1rem}}
</style>
</head>
<body>
<div class="hero">
  <div class="badge"><span class="dot"></span> 每日自動更新 · Poisson 模擬</div>
  <h1>2026 FIFA 世界盃<br>小組賽比分預測 <em>AI</em></h1>
  <p>依每日賽況動態調整各隊 λ，以 Bayesian 更新 + 100萬次 Poisson 模擬推算最可能比分。</p>
  <p class="update-info">最後更新：<strong>%s</strong>｜已完成 %d 場 · 剩餘 %d 場</p>
  <div class="stats-row">
    <div class="stat"><div class="n">%d</div><div class="l">場剩餘</div></div>
    <div class="stat"><div class="n">100萬</div><div class="l">次/場模擬</div></div>
    <div class="stat"><div class="n">動態λ</div><div class="l">Bayesian 更新</div></div>
  </div>
</div>
<div class="container">
  <div class="tabs">%s</div>
  <div class="disc">⚡ 純統計模型，λ 依本屆賽果動態調整，僅供娛樂參考。</div>
  %s
  <div class="footer">
    <strong>模型架構</strong>｜Poisson 分布 × Bayesian 動態更新：每場賽後以 40%% 權重調整各隊攻防係數<br>
    天氣：高溫 → λ 下調 · 墨西哥城 → 海拔加成｜自動排程：每日 UTC 06:00 重算並部署<br>
    Claude AI × R 4.6 × GitHub Actions · 僅供娛樂
  </div>
</div>
<script>
function showDay(d,btn){
  document.querySelectorAll(".tab").forEach(t=>t.classList.remove("active"));
  btn.classList.add("active");
  document.querySelectorAll(".day-section").forEach(s=>s.style.display="none");
  document.getElementById("day-"+d).style.display="block";
}
</script>
</body>
</html>',
  today, played_n, length(remaining), length(remaining),
  tab_btns, sections)

writeLines(html, "index.html", useBytes = FALSE)
cat(sprintf("[OK] index.html generated  (%d matches simulated)\n", length(remaining)))
