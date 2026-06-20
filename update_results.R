library(jsonlite)

# 用法: Rscript update_results.R "Germany" "Ivory Coast" 2 1
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4) {
  cat("用法: Rscript update_results.R <home> <away> <home_score> <away_score>\n")
  quit(status = 1)
}

home <- args[1]; away <- args[2]
hs   <- as.integer(args[3]); as_ <- as.integer(args[4])

cfg <- fromJSON("data/teams.json", simplifyVector = FALSE)
found <- FALSE
for (i in seq_along(cfg$matches)) {
  m <- cfg$matches[[i]]
  if (m$home == home && m$away == away && !isTRUE(m$played)) {
    cfg$matches[[i]]$played     <- TRUE
    cfg$matches[[i]]$home_score <- hs
    cfg$matches[[i]]$away_score <- as_
    found <- TRUE
    cat(sprintf("已更新: %s %d-%d %s\n", home, hs, as_, away))
    break
  }
}

if (!found) {
  cat(sprintf("找不到比賽: %s vs %s\n", home, away))
  quit(status = 1)
}

write(toJSON(cfg, auto_unbox = TRUE, pretty = TRUE), "data/teams.json")
cat("data/teams.json 已儲存，請執行 Rscript simulate.R 更新預測。\n")
