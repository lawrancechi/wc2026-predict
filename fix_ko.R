library(jsonlite)
cfg <- fromJSON("data/teams.json", simplifyVector=FALSE)

# All correct UTC ko values based on official Wikipedia schedule
# Base midnight UTC: Jun20=1781913600 Jun21=1782000000 Jun22=1782086400
#                    Jun23=1782172800 Jun24=1782259200 Jun25=1782345600
#                    Jun26=1782432000 Jun27=1782518400
ko_map <- list(
  # June 20
  "Netherlands|Sweden"        = 1781913600L + 17L*3600L,  # 12pm CDT -> Taiwan 6/21 01:00
  "Germany|Ivory Coast"       = 1781913600L + 20L*3600L,  # 4pm EDT  -> Taiwan 6/21 04:00
  "Ecuador|Curacao"           = 1782000000L,               # 7pm CDT  -> Taiwan 6/21 08:00
  "Japan|Tunisia"             = 1782000000L + 4L*3600L,   # 10pm CST -> Taiwan 6/21 12:00
  # June 21
  "Spain|Saudi Arabia"        = 1782000000L + 16L*3600L,  # 12pm EDT -> Taiwan 6/22 00:00
  "Belgium|Iran"              = 1782000000L + 19L*3600L,  # 12pm PDT -> Taiwan 6/22 03:00
  "Uruguay|Cape Verde"        = 1782000000L + 22L*3600L,  # 6pm EDT  -> Taiwan 6/22 06:00
  "New Zealand|Egypt"         = 1782000000L + 25L*3600L,  # 6pm PDT  -> Taiwan 6/22 09:00
  # June 22
  "Argentina|Austria"         = 1782086400L + 17L*3600L,  # 12pm CDT -> Taiwan 6/23 01:00
  "France|Iraq"               = 1782086400L + 21L*3600L,  # 5pm EDT  -> Taiwan 6/23 05:00
  "Norway|Senegal"            = 1782086400L + 24L*3600L,  # 8pm EDT  -> Taiwan 6/23 08:00
  "Jordan|Algeria"            = 1782086400L + 27L*3600L,  # 8pm PDT  -> Taiwan 6/23 11:00
  # June 23
  "Portugal|Uzbekistan"       = 1782172800L + 17L*3600L,  # 12pm CDT -> Taiwan 6/24 01:00
  "England|Ghana"             = 1782172800L + 20L*3600L,  # 4pm EDT  -> Taiwan 6/24 04:00
  "Panama|Croatia"            = 1782172800L + 23L*3600L,  # 7pm EDT  -> Taiwan 6/24 07:00
  "Colombia|DR Congo"         = 1782172800L + 26L*3600L,  # 8pm CST  -> Taiwan 6/24 10:00
  # June 24
  "Switzerland|Canada"        = 1782259200L + 19L*3600L,  # 12pm PDT -> Taiwan 6/25 03:00
  "Bosnia|Qatar"              = 1782259200L + 19L*3600L,  # 12pm PDT -> Taiwan 6/25 03:00
  "Scotland|Brazil"           = 1782259200L + 22L*3600L,  # 6pm EDT  -> Taiwan 6/25 06:00
  "Morocco|Haiti"             = 1782259200L + 22L*3600L,  # 6pm EDT  -> Taiwan 6/25 06:00
  "Czechia|Mexico"            = 1782259200L + 25L*3600L,  # 7pm CST  -> Taiwan 6/25 09:00
  "South Africa|South Korea"  = 1782259200L + 25L*3600L,  # 7pm CST  -> Taiwan 6/25 09:00
  # June 25
  "Ecuador|Germany"           = 1782345600L + 20L*3600L,  # 4pm EDT  -> Taiwan 6/26 04:00
  "Curacao|Ivory Coast"       = 1782345600L + 20L*3600L,  # 4pm EDT  -> Taiwan 6/26 04:00
  "Japan|Sweden"              = 1782345600L + 23L*3600L,  # 6pm CDT  -> Taiwan 6/26 07:00
  "Tunisia|Netherlands"       = 1782345600L + 23L*3600L,  # 6pm CDT  -> Taiwan 6/26 07:00
  "Turkey|USA"                = 1782345600L + 26L*3600L,  # 7pm PDT  -> Taiwan 6/26 10:00
  "Paraguay|Australia"        = 1782345600L + 26L*3600L,  # 7pm PDT  -> Taiwan 6/26 10:00
  # June 26
  "Norway|France"             = 1782432000L + 19L*3600L,  # 3pm EDT  -> Taiwan 6/27 03:00
  "Senegal|Iraq"              = 1782432000L + 19L*3600L,  # 3pm EDT  -> Taiwan 6/27 03:00
  "Cape Verde|Saudi Arabia"   = 1782432000L + 24L*3600L,  # 7pm CDT  -> Taiwan 6/27 08:00
  "Uruguay|Spain"             = 1782432000L + 24L*3600L,  # 6pm CST  -> Taiwan 6/27 08:00
  "Egypt|Iran"                = 1782432000L + 27L*3600L,  # 8pm PDT  -> Taiwan 6/27 11:00
  "New Zealand|Belgium"       = 1782432000L + 27L*3600L,  # 8pm PDT  -> Taiwan 6/27 11:00
  # June 27
  "Panama|England"            = 1782518400L + 21L*3600L,  # 5pm EDT  -> Taiwan 6/28 05:00
  "Croatia|Ghana"             = 1782518400L + 21L*3600L,  # 5pm EDT  -> Taiwan 6/28 05:00
  "Colombia|Portugal"         = 1782518400L + 83700L,     # 7:30pm EDT-> Taiwan 6/28 07:30
  "DR Congo|Uzbekistan"       = 1782518400L + 83700L,     # 7:30pm EDT-> Taiwan 6/28 07:30
  "Algeria|Austria"           = 1782518400L + 26L*3600L,  # 9pm CDT  -> Taiwan 6/28 10:00
  "Jordan|Argentina"          = 1782518400L + 26L*3600L   # 9pm CDT  -> Taiwan 6/28 10:00
)

updated <- 0
for (i in seq_along(cfg$matches)) {
  m <- cfg$matches[[i]]
  if (isTRUE(m$played)) next
  key <- paste(m$home, m$away, sep="|")
  if (!is.null(ko_map[[key]])) {
    cfg$matches[[i]]$ko <- ko_map[[key]]
    tw <- format(as.POSIXct(ko_map[[key]] + 8*3600, origin="1970-01-01", tz="UTC"), "%m/%d %H:%M")
    cat(sprintf("OK: %-25s -> 台灣 %s\n", key, tw))
    updated <- updated + 1
  }
}
write(toJSON(cfg, auto_unbox=TRUE, pretty=TRUE), "data/teams.json")
cat(sprintf("\n共更新 %d 場 ko 時間\n", updated))
