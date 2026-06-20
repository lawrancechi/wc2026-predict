library(jsonlite)
cfg <- fromJSON("data/teams.json", simplifyVector=FALSE)

ko_map <- list(
  "Netherlands|Sweden"   = 1781913600L + 17L*3600L,
  "Japan|Tunisia"        = 1781913600L + 17L*3600L,
  "Germany|Ivory Coast"  = 1781913600L + 20L*3600L,
  "Ecuador|Curacao"      = 1781913600L + 20L*3600L
)

for (i in seq_along(cfg$matches)) {
  m <- cfg$matches[[i]]
  key <- paste(m$home, m$away, sep="|")
  if (!is.null(ko_map[[key]])) {
    cfg$matches[[i]]$ko <- ko_map[[key]]
    tw <- format(as.POSIXct(ko_map[[key]] + 8*3600, origin="1970-01-01", tz="UTC"), "%m/%d %H:%M")
    cat(sprintf("Updated: %s vs %s -> Taiwan %s\n", m$home, m$away, tw))
  }
}
write(toJSON(cfg, auto_unbox=TRUE, pretty=TRUE), "data/teams.json")
cat("Done\n")
