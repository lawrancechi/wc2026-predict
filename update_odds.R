library(jsonlite)
library(httr)

API_KEY <- Sys.getenv("ODDS_API_KEY")
if (nchar(API_KEY) == 0) stop("ODDS_API_KEY not set")

url <- paste0(
  "https://api.the-odds-api.com/v4/sports/soccer_fifa_world_cup/odds/",
  "?apiKey=", API_KEY,
  "&regions=eu&markets=h2h&oddsFormat=decimal"
)

resp <- GET(url)
cat(sprintf("[Odds] HTTP %d  remaining=%s\n",
            status_code(resp),
            headers(resp)[["x-requests-remaining"]]))

if (status_code(resp) != 200) {
  cat("[Odds] API error, skipping odds update\n")
  quit(status = 0)
}

raw <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)

# 隊名對照（Odds API → 我們的 teams.json 名稱）
NAME_MAP <- c(
  "Cote d'Ivoire"          = "Ivory Coast",
  "Côte d'Ivoire"          = "Ivory Coast",
  "Korea Republic"         = "South Korea",
  "Republic of Korea"      = "South Korea",
  "United States"          = "USA",
  "Cape Verde Islands"     = "Cape Verde",
  "Cape Verde"             = "Cape Verde",
  "Bosnia and Herzegovina" = "Bosnia",
  "Czech Republic"         = "Czechia",
  "Congo DR"               = "DR Congo",
  "Democratic Republic of Congo" = "DR Congo",
  "New Zealand"            = "New Zealand",
  "Saudi Arabia"           = "Saudi Arabia"
)

normalize <- function(n) {
  if (!is.na(NAME_MAP[n])) NAME_MAP[[n]] else n
}

# 解析每場比賽的 h2h 賠率（取歐賠均值）
odds_lookup <- list()
for (game in raw) {
  teams <- game$home_team
  ht <- normalize(game$home_team)
  at <- normalize(game$away_team)
  key <- paste(sort(c(ht, at)), collapse="|")

  h2h_markets <- Filter(function(b) {
    any(sapply(b$markets, function(mk) mk$key == "h2h"))
  }, game$bookmakers)

  if (length(h2h_markets) == 0) next

  # 收集所有莊家的賠率
  all_odds <- lapply(h2h_markets, function(b) {
    mk <- Filter(function(mk) mk$key == "h2h", b$markets)[[1]]
    out <- mk$outcomes
    oh <- Filter(function(o) normalize(o$name) == ht, out)
    oa <- Filter(function(o) normalize(o$name) == at, out)
    od <- Filter(function(o) !normalize(o$name) %in% c(ht, at), out)
    if (length(oh) && length(oa))
      list(h = oh[[1]]$price, a = oa[[1]]$price,
           d = if (length(od)) od[[1]]$price else NA)
    else NULL
  })
  all_odds <- Filter(Negate(is.null), all_odds)
  if (length(all_odds) == 0) next

  avg_h <- mean(sapply(all_odds, `[[`, "h"), na.rm=TRUE)
  avg_a <- mean(sapply(all_odds, `[[`, "a"), na.rm=TRUE)
  avg_d <- mean(sapply(all_odds, function(x) x$d), na.rm=TRUE)

  odds_lookup[[key]] <- list(h=round(avg_h,2), d=round(avg_d,2), a=round(avg_a,2))
  cat(sprintf("  [Odds] %s vs %s  %.2f / %.2f / %.2f\n",
              ht, at, avg_h, avg_d, avg_a))
}

# 寫入 teams.json
cfg <- fromJSON("data/teams.json", simplifyVector = FALSE)
updated <- 0
for (i in seq_along(cfg$matches)) {
  m  <- cfg$matches[[i]]
  ht <- m$home; at <- m$away
  key <- paste(sort(c(ht, at)), collapse="|")
  if (!is.null(odds_lookup[[key]])) {
    ol <- odds_lookup[[key]]
    # 依照 home/away 順序對齊
    api_h <- normalize(m$home); api_a <- normalize(m$away)
    key2  <- paste(sort(c(api_h, api_a)), collapse="|")
    ol2   <- odds_lookup[[key2]]
    if (!is.null(ol2)) {
      # 確認哪邊是 home
      raw_home <- Filter(function(g) {
        normalize(g$home_team) == ht && normalize(g$away_team) == at
      }, raw)
      if (length(raw_home) > 0) {
        cfg$matches[[i]]$odds_h <- ol2$h
        cfg$matches[[i]]$odds_d <- ol2$d
        cfg$matches[[i]]$odds_a <- ol2$a
      } else {
        cfg$matches[[i]]$odds_h <- ol2$a
        cfg$matches[[i]]$odds_d <- ol2$d
        cfg$matches[[i]]$odds_a <- ol2$h
      }
      updated <- updated + 1
    }
  }
}

write(toJSON(cfg, auto_unbox=TRUE, pretty=TRUE), "data/teams.json")
cat(sprintf("[Odds] 完成，共更新 %d 場賠率\n", updated))
