library(jsonlite)
cfg <- fromJSON("data/teams.json", simplifyVector = FALSE)

# 各賽場時區（落後UTC的小時數）
tz_off <- c(
  Houston=5, Dallas=5, "Kansas City"=5, Arlington=5,
  Toronto=4, Miami=4, Atlanta=4, "New York"=4, Boston=4, Philadelphia=4,
  "Los Angeles"=7, "San Francisco"=7, Seattle=7, Vancouver=7,
  Monterrey=6, "Mexico City"=6, Guadalajara=6, Guadalupe=6
)

# 各日期0:00 UTC的Unix秒數
base_ts <- c(
  "2026-06-20"=1781913600L, "2026-06-21"=1782000000L,
  "2026-06-22"=1782086400L, "2026-06-23"=1782172800L,
  "2026-06-24"=1782259200L, "2026-06-25"=1782345600L,
  "2026-06-26"=1782432000L, "2026-06-27"=1782518400L
)

for (i in seq_along(cfg$matches)) {
  m <- cfg$matches[[i]]
  if (isTRUE(m$played)) next
  venue <- m$venue
  tz <- if (!is.na(tz_off[venue])) as.integer(tz_off[venue]) else 5L
  base  <- as.integer(base_ts[m$date])
  # 晚間場：當地 19:00 → UTC = 19 + tz 小時後
  cfg$matches[[i]]$ko <- base + (19L + tz) * 3600L
}

write(toJSON(cfg, auto_unbox=TRUE, pretty=TRUE), "data/teams.json")
cat("ko 已寫入 teams.json\n")
