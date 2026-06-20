"""
每天比賽打完後執行這個腳本來更新賽果。
用法：
  python update_results.py "Germany" "Ivory Coast" 2 1
"""
import json, sys

if len(sys.argv) != 5:
    print("用法: python update_results.py <home> <away> <home_score> <away_score>")
    sys.exit(1)

home, away = sys.argv[1], sys.argv[2]
hs, as_ = int(sys.argv[3]), int(sys.argv[4])

with open("data/teams.json", encoding="utf-8") as f:
    config = json.load(f)

found = False
for m in config["matches"]:
    if m["home"] == home and m["away"] == away and not m.get("played"):
        m["played"]     = True
        m["home_score"] = hs
        m["away_score"] = as_
        found = True
        print(f"✅ 已更新: {home} {hs}-{as_} {away}")
        break

if not found:
    print(f"❌ 找不到比賽: {home} vs {away}（可能已標記為已完成）")
    sys.exit(1)

with open("data/teams.json", "w", encoding="utf-8") as f:
    json.dump(config, f, ensure_ascii=False, indent=2)

print("💾 data/teams.json 已儲存，請重新執行 simulate.py 或等候 GitHub Actions 自動更新。")
