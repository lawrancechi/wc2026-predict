"""
2026 World Cup Predictor
Runs 1M Poisson simulations per match, updates based on actual results.
"""
import json, math, random, os
from collections import Counter
from datetime import date, datetime

random.seed(2026)

# ── Load config ────────────────────────────────────────────
with open("data/teams.json", encoding="utf-8") as f:
    config = json.load(f)

base_lam = config["base_lambda"]
matches  = config["matches"]

# ── Build dynamic team form from played matches ────────────
# attack_mod / defense_mod: ratio of actual vs expected performance
attack_mod  = {t: 1.0 for t in base_lam}
defense_mod = {t: 1.0 for t in base_lam}

for m in matches:
    if not m.get("played"):
        continue
    h, a = m["home"], m["away"]
    hs, as_ = m["home_score"], m["away_score"]
    lh_exp = max(0.1, base_lam.get(h, 1.0) + m.get("heat", 0))
    la_exp = max(0.1, base_lam.get(a, 1.0) + m.get("heat", 0))

    # Bayesian-style update (blend actual with prior, weight=0.4)
    w = 0.4
    attack_mod[h]  = attack_mod[h]  * (1-w) + w * (hs  / lh_exp)
    attack_mod[a]  = attack_mod[a]  * (1-w) + w * (as_ / la_exp)
    defense_mod[h] = defense_mod[h] * (1-w) + w * (la_exp / max(0.1, as_))
    defense_mod[a] = defense_mod[a] * (1-w) + w * (lh_exp / max(0.1, hs))

# ── Poisson simulation ─────────────────────────────────────
N = 1_000_000

def poisson_sample(lam, n):
    """Pure-Python Poisson sampler using multiplicative method."""
    out = []
    L = math.exp(-lam)
    for _ in range(n):
        k, p = 0, 1.0
        while p > L:
            p *= random.random()
            k += 1
        out.append(k - 1)
    return out

def simulate_match(home, away, heat, altitude=False):
    lh = max(0.05, base_lam.get(home, 1.0) * attack_mod.get(home, 1.0)
             * defense_mod.get(away, 1.0) + heat)
    la = max(0.05, base_lam.get(away, 1.0) * attack_mod.get(away, 1.0)
             * defense_mod.get(home, 1.0) + heat)
    if altitude:          # Mexico City home advantage
        lh *= 1.15
        la *= 0.95
    gh = poisson_sample(lh, N)
    ga = poisson_sample(la, N)
    counts = Counter(zip(gh, ga))
    total  = N
    hw = sum(v for (h,a),v in counts.items() if h > a) / total * 100
    dr = sum(v for (h,a),v in counts.items() if h == a) / total * 100
    aw = sum(v for (h,a),v in counts.items() if h < a) / total * 100
    top3 = sorted(counts.items(), key=lambda x: -x[1])[:3]
    top3_out = []
    for (sh, sa), cnt in top3:
        res = "home" if sh > sa else ("draw" if sh == sa else "away")
        top3_out.append({"score": f"{sh}-{sa}", "pct": round(cnt/total*100, 2), "result": res})
    return round(lh, 2), round(la, 2), round(hw, 1), round(dr, 1), round(aw, 1), top3_out

# ── Build output data ──────────────────────────────────────
today = date.today().isoformat()
output = []

for m in matches:
    if m.get("played"):
        continue
    home, away = m["home"], m["away"]
    lh, la, hw, dr, aw, top3 = simulate_match(
        home, away, m.get("heat", 0), m.get("altitude", False)
    )
    output.append({
        "date":  m["date"],
        "group": m["group"],
        "home":  home,
        "away":  away,
        "venue": m["venue"],
        "temp":  m["temp"],
        "lh": lh, "la": la,
        "hw": hw, "dr": dr, "aw": aw,
        "top3":  top3
    })

# ── Generate HTML ──────────────────────────────────────────
FLAG_MAP = {
    "Germany":"🇩🇪","Ivory Coast":"🇨🇮","Ecuador":"🇪🇨","Curacao":"🇨🇼",
    "Netherlands":"🇳🇱","Sweden":"🇸🇪","Japan":"🇯🇵","Tunisia":"🇹🇳",
    "Spain":"🇪🇸","Saudi Arabia":"🇸🇦","Uruguay":"🇺🇾","Cape Verde":"🇨🇻",
    "Belgium":"🇧🇪","Iran":"🇮🇷","New Zealand":"🇳🇿","Egypt":"🇪🇬",
    "Argentina":"🇦🇷","Austria":"🇦🇹","Jordan":"🇯🇴","Algeria":"🇩🇿",
    "France":"🇫🇷","Iraq":"🇮🇶","Norway":"🇳🇴","Senegal":"🇸🇳",
    "Portugal":"🇵🇹","Uzbekistan":"🇺🇿","Colombia":"🇨🇴","DR Congo":"🇨🇩",
    "England":"🏴󠁧󠁢󠁥󠁮󠁧󠁿","Ghana":"🇬🇭","Panama":"🇵🇦","Croatia":"🇭🇷",
    "Mexico":"🇲🇽","South Africa":"🇿🇦","South Korea":"🇰🇷","Czechia":"🇨🇿",
    "Canada":"🇨🇦","Switzerland":"🇨🇭","Bosnia":"🇧🇦","Qatar":"🇶🇦",
    "Brazil":"🇧🇷","Morocco":"🇲🇦","Scotland":"🏴󠁧󠁢󠁳󠁣󠁴󠁿","Haiti":"🇭🇹",
    "USA":"🇺🇸","Turkey":"🇹🇷","Australia":"🇦🇺","Paraguay":"🇵🇾",
}

DATE_LABELS = {
    "2026-06-20":"6月20日 — Group E,F 第二輪",
    "2026-06-21":"6月21日 — Group G,H 第二輪",
    "2026-06-22":"6月22日 — Group I,J 第二輪",
    "2026-06-23":"6月23日 — Group K,L 第二輪",
    "2026-06-24":"6月24日 — Group A,B,C 末輪",
    "2026-06-25":"6月25日 — Group D,E,F 末輪",
    "2026-06-26":"6月26日 — Group G,H,I 末輪",
    "2026-06-27":"6月27日 — Group J,K,L 末輪",
}

dates = sorted(set(m["date"] for m in output))

def temp_label(t):
    if t <= 22: return f"❄ {t}°C 涼爽", "success"
    if t <= 26: return f"🌤 {t}°C 溫和", "success"
    if t <= 30: return f"☀ {t}°C 溫熱", "warning"
    if t <= 33: return f"🌡 {t}°C 悶熱", "danger"
    return f"🔥 {t}°C 酷熱", "danger"

def res_label(r, h, a):
    if r == "home": return f'<span class="sres rh">{h}</span>'
    if r == "draw": return '<span class="sres rd">平局</span>'
    return f'<span class="sres ra">{a}</span>'

def match_html(m):
    hf = FLAG_MAP.get(m["home"], "🏳")
    af = FLAG_MAP.get(m["away"], "🏳")
    tlabel, tclass = temp_label(m["temp"])
    scores_html = ""
    for i, s in enumerate(m["top3"]):
        rank_cls = f"s{i+1}"
        scores_html += f'''<div class="spill {rank_cls}">
          <div class="rbadge">#{i+1}</div>
          <div class="sval">{s["score"]}</div>
          <div class="spct">{s["pct"]}%</div>
          {res_label(s["result"], m["home"], m["away"])}
        </div>'''
    return f'''<div class="card">
  <div class="card-header">
    <span class="venue"><b>{m["venue"]}</b></span>
    <div style="display:flex;gap:5px;align-items:center">
      <span class="grp-chip">組別 {m["group"]}</span>
      <span class="wchip w-{tclass}">{tlabel}</span>
    </div>
  </div>
  <div class="teams-row">
    <div class="team"><span class="flag">{hf}</span><div class="tname">{m["home"]}</div><div class="tform">λ={m["lh"]}</div></div>
    <div class="vs-badge"><span class="grp-sm">模擬</span>VS</div>
    <div class="team"><span class="flag">{af}</span><div class="tname">{m["away"]}</div><div class="tform">λ={m["la"]}</div></div>
  </div>
  <div class="prob-section">
    <div class="prob-bar-wrap">
      <div class="prob-seg seg-home" style="width:{m["hw"]}%"></div>
      <div class="prob-seg seg-draw" style="width:{m["dr"]}%"></div>
      <div class="prob-seg seg-away" style="width:{m["aw"]}%"></div>
    </div>
    <div class="prob-labels">
      <span class="pl-home"><strong>{m["hw"]}%</strong> {m["home"]}</span>
      <span class="pl-draw"><strong>{m["dr"]}%</strong> 平局</span>
      <span class="pl-away"><strong>{m["aw"]}%</strong> {m["away"]}</span>
    </div>
  </div>
  <div class="scores-section">
    <div class="scores-title">最可能比分（100萬次模擬）</div>
    <div class="scores-grid">{scores_html}</div>
  </div>
</div>'''

# ── Build tabs & sections ──────────────────────────────────
tab_btns = ""
sections_html = ""
for i, d in enumerate(dates):
    active = "active" if i == 0 else ""
    short  = d[5:].replace("-", "/")
    label  = DATE_LABELS.get(d, d)
    tab_btns += f'<button class="tab {active}" onclick="showDay(\'{d}\',this)">{short}</button>\n'
    cards    = "\n".join(match_html(m) for m in output if m["date"] == d)
    display  = "block" if i == 0 else "none"
    sections_html += f'<div id="day-{d}" class="day-section" style="display:{display}"><div class="day-label">{label}</div>{cards}</div>\n'

played_count = sum(1 for m in matches if m.get("played"))
remaining    = len(output)

html = f'''<!DOCTYPE html>
<html lang="zh-TW">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>2026 世界盃比分預測</title>
<style>
*{{box-sizing:border-box;margin:0;padding:0}}
:root{{--bg:#09101f;--bg2:#111827;--card:#141e2e;--border:rgba(255,255,255,0.07);--text:#eef2ff;--muted:#7a8fa8;--accent:#3b82f6;--gold:#f59e0b;--silver:#9ca3af;--bronze:#b45309}}
body{{background:var(--bg);color:var(--text);font-family:'Segoe UI',system-ui,sans-serif;min-height:100vh;padding-bottom:3rem}}
.hero{{background:linear-gradient(160deg,#0d1b35 0%,#09101f 60%);border-bottom:1px solid var(--border);padding:2.5rem 1.5rem 2rem;text-align:center;position:relative}}
.hero::before{{content:'';position:absolute;inset:0;background:radial-gradient(ellipse 80% 50% at 50% 0%,rgba(59,130,246,.12) 0%,transparent 70%);pointer-events:none}}
.badge{{display:inline-flex;align-items:center;gap:6px;background:rgba(59,130,246,.12);border:1px solid rgba(59,130,246,.25);border-radius:20px;padding:4px 14px;font-size:11px;color:#93c5fd;margin-bottom:1rem}}
.dot{{display:inline-block;width:6px;height:6px;background:#3b82f6;border-radius:50%;animation:pulse 1.5s ease-in-out infinite}}
@keyframes pulse{{0%,100%{{opacity:1}}50%{{opacity:.3}}}}
.hero h1{{font-size:clamp(1.6rem,4vw,2.4rem);font-weight:700;letter-spacing:-.02em;line-height:1.15;margin-bottom:.5rem}}
.hero h1 em{{font-style:normal;color:var(--gold)}}
.hero p{{color:var(--muted);font-size:13px;max-width:500px;margin:0 auto 1.2rem;line-height:1.6}}
.update-time{{font-size:11px;color:var(--muted);margin-top:.5rem}}
.update-time strong{{color:#34d399}}
.stats-row{{display:flex;justify-content:center;gap:2rem;flex-wrap:wrap}}
.stat .n{{font-size:1.2rem;font-weight:700;color:var(--accent)}}
.stat .l{{font-size:10px;color:var(--muted);margin-top:2px}}
.container{{max-width:960px;margin:0 auto;padding:0 1rem}}
.tabs{{display:flex;gap:6px;padding:1.2rem 0 .8rem;overflow-x:auto;scrollbar-width:none}}
.tabs::-webkit-scrollbar{{display:none}}
.tab{{flex-shrink:0;border:1px solid var(--border);background:rgba(255,255,255,.03);color:var(--muted);padding:6px 14px;border-radius:18px;font-size:12px;cursor:pointer;transition:all .2s}}
.tab:hover{{border-color:rgba(59,130,246,.4);color:var(--text)}}
.tab.active{{background:var(--accent);border-color:var(--accent);color:#fff;font-weight:500}}
.day-section{{display:none}}
.day-label{{display:flex;align-items:center;gap:8px;font-size:10px;font-weight:600;letter-spacing:.08em;color:var(--muted);text-transform:uppercase;padding:.8rem 0 .6rem}}
.day-label::after{{content:'';flex:1;height:.5px;background:var(--border)}}
.card{{background:var(--card);border:1px solid var(--border);border-radius:14px;margin-bottom:.9rem;overflow:hidden;transition:border-color .2s}}
.card:hover{{border-color:rgba(59,130,246,.25)}}
.card-header{{padding:.55rem 1rem;border-bottom:1px solid var(--border);background:rgba(0,0,0,.2);display:flex;align-items:center;justify-content:space-between;gap:8px;flex-wrap:wrap}}
.venue{{font-size:11px;color:var(--muted)}}
.venue b{{color:var(--text);font-weight:500}}
.grp-chip{{font-size:10px;font-weight:600;padding:2px 8px;border-radius:10px;background:rgba(59,130,246,.15);color:#93c5fd;border:1px solid rgba(59,130,246,.25)}}
.wchip{{display:inline-flex;align-items:center;gap:4px;border-radius:16px;padding:3px 9px;font-size:10px;font-weight:500;border:1px solid}}
.w-success{{background:rgba(16,185,129,.1);color:#34d399;border-color:rgba(16,185,129,.2)}}
.w-warning{{background:rgba(251,191,36,.1);color:#fcd34d;border-color:rgba(251,191,36,.2)}}
.w-danger{{background:rgba(239,68,68,.1);color:#f87171;border-color:rgba(239,68,68,.2)}}
.teams-row{{padding:.85rem 1rem .75rem;display:grid;grid-template-columns:1fr 60px 1fr;align-items:center;gap:.5rem}}
.team{{text-align:center}}
.flag{{font-size:1.9rem;display:block;margin-bottom:4px;line-height:1}}
.tname{{font-size:13px;font-weight:500}}
.tform{{font-size:10px;color:var(--muted);margin-top:3px}}
.vs-badge{{background:rgba(255,255,255,.04);border:1px solid var(--border);border-radius:8px;padding:5px 0;font-size:11px;font-weight:600;color:var(--muted);text-align:center}}
.grp-sm{{font-size:9px;color:var(--muted);display:block;margin-bottom:1px}}
.prob-section{{padding:0 1rem .75rem}}
.prob-bar-wrap{{display:flex;border-radius:4px;overflow:hidden;height:6px;gap:2px;margin-bottom:5px}}
.prob-seg{{height:100%;border-radius:2px}}
.seg-home{{background:var(--accent)}}
.seg-draw{{background:#4b5563}}
.seg-away{{background:#8b5cf6}}
.prob-labels{{display:flex;justify-content:space-between;font-size:10px}}
.pl-home strong{{color:var(--accent)}}
.pl-draw strong{{color:#9ca3af}}
.pl-away strong{{color:#a78bfa}}
.prob-labels span{{color:var(--muted)}}
.scores-section{{padding:0 1rem 1rem}}
.scores-title{{font-size:10px;font-weight:500;letter-spacing:.06em;color:var(--muted);text-transform:uppercase;margin-bottom:.6rem}}
.scores-grid{{display:grid;grid-template-columns:repeat(3,1fr);gap:8px}}
.spill{{border-radius:10px;padding:10px 6px 8px;text-align:center;border:.5px solid;position:relative}}
.s1{{background:rgba(245,158,11,.08);border-color:rgba(245,158,11,.3)}}
.s2{{background:rgba(255,255,255,.03);border-color:rgba(255,255,255,.08)}}
.s3{{background:rgba(255,255,255,.02);border-color:rgba(255,255,255,.06)}}
.rbadge{{position:absolute;top:-7px;left:50%;transform:translateX(-50%);font-size:8px;font-weight:600;padding:1px 7px;border-radius:8px;white-space:nowrap}}
.s1 .rbadge{{background:var(--gold);color:#1a0e00}}
.s2 .rbadge{{background:var(--silver);color:#111}}
.s3 .rbadge{{background:var(--bronze);color:#fff8f0}}
.sval{{font-size:1.35rem;font-weight:600;margin-top:6px;line-height:1}}
.s1 .sval{{color:var(--gold)}}
.s2 .sval{{color:#d1d5db}}
.s3 .sval{{color:#d97706}}
.spct{{font-size:10px;color:var(--muted);margin-top:3px}}
.sres{{font-size:9px;margin-top:3px;padding:2px 6px;border-radius:4px;display:inline-block}}
.rh{{background:rgba(59,130,246,.15);color:#93c5fd}}
.rd{{background:rgba(107,114,128,.2);color:#9ca3af}}
.ra{{background:rgba(139,92,246,.15);color:#c4b5fd}}
.disc{{background:rgba(245,158,11,.06);border:1px solid rgba(245,158,11,.2);border-radius:8px;padding:8px 12px;font-size:11px;color:#fcd34d;margin:.6rem 0;display:flex;gap:6px;align-items:flex-start}}
.footer{{text-align:center;padding:2rem 1rem 1rem;font-size:11px;color:var(--muted);line-height:1.9;border-top:1px solid var(--border);margin-top:1.5rem}}
.footer strong{{color:#60a5fa}}
@media(max-width:480px){{.flag{{font-size:1.5rem}}.tname{{font-size:12px}}.sval{{font-size:1.1rem}}}}
</style>
</head>
<body>
<div class="hero">
  <div class="badge"><span class="dot"></span> 每日自動更新 · Poisson 模擬</div>
  <h1>2026 FIFA 世界盃<br>小組賽比分預測 <em>AI</em></h1>
  <p>依每日賽況動態調整各隊期望進球數 λ，以 100萬次 Poisson 模擬推算最可能比分。</p>
  <p class="update-time">最後更新：<strong>{today}</strong>｜已完成 {played_count} 場 · 剩餘 {remaining} 場待預測</p>
  <div class="stats-row">
    <div class="stat"><div class="n">{remaining}</div><div class="l">場待預測</div></div>
    <div class="stat"><div class="n">100萬</div><div class="l">次/場模擬</div></div>
    <div class="stat"><div class="n">動態λ</div><div class="l">依賽況更新</div></div>
  </div>
</div>
<div class="container">
  <div class="tabs">{tab_btns}</div>
  <div class="disc">⚡ 純統計模型預測，λ 值依本屆實際賽果動態調整，僅供娛樂參考。</div>
  {sections_html}
  <div class="footer">
    <strong>模型架構</strong>｜Poisson 分布 + Bayesian 動態更新：每場比賽結束後，以 40% 權重調整各隊進攻/防守修正係數<br>
    天氣調整：高溫 → λ 下調 · 墨西哥城 → 海拔主場優勢加成<br>
    每日 UTC 06:00 自動抓取賽果 → 重算 → 部署｜Claude AI × Python × GitHub Actions
  </div>
</div>
<script>
function showDay(date, btn) {{
  document.querySelectorAll('.tab').forEach(t => t.classList.remove('active'));
  btn.classList.add('active');
  document.querySelectorAll('.day-section').forEach(s => s.style.display = 'none');
  document.getElementById('day-' + date).style.display = 'block';
}}
</script>
</body>
</html>'''

with open("index.html", "w", encoding="utf-8") as f:
    f.write(html)

print(f"[OK] index.html generated — {remaining} upcoming matches predicted")
print(f"     Played: {played_count} | Remaining: {remaining}")
