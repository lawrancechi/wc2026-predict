library(jsonlite)

cfg     <- fromJSON("data/teams.json",   simplifyVector = FALSE)
ko_cfg  <- fromJSON("data/knockouts.json", simplifyVector = FALSE)
matches <- cfg$matches

# ── 計算各組積分榜 ────────────────────────────────────────
groups <- list()
for (m in matches) {
  grp <- m$group
  if (is.null(grp) || grp == "") next
  for (t in c(m$home, m$away)) {
    if (is.null(groups[[grp]][[t]]))
      groups[[grp]][[t]] <- list(team=t, pts=0L, gp=0L, gf=0L, ga=0L, gd=0L)
  }
  if (!isTRUE(m$played)) next
  h <- m$home; a <- m$away
  hs <- m$home_score; as_ <- m$away_score
  for (t in c(h,a)) {
    groups[[grp]][[t]]$gp <- groups[[grp]][[t]]$gp + 1L
  }
  groups[[grp]][[h]]$gf  <- groups[[grp]][[h]]$gf  + hs
  groups[[grp]][[h]]$ga  <- groups[[grp]][[h]]$ga  + as_
  groups[[grp]][[h]]$gd  <- groups[[grp]][[h]]$gd  + hs - as_
  groups[[grp]][[a]]$gf  <- groups[[grp]][[a]]$gf  + as_
  groups[[grp]][[a]]$ga  <- groups[[grp]][[a]]$ga  + hs
  groups[[grp]][[a]]$gd  <- groups[[grp]][[a]]$gd  + as_ - hs
  if (hs > as_)       groups[[grp]][[h]]$pts <- groups[[grp]][[h]]$pts + 3L
  else if (hs == as_) { groups[[grp]][[h]]$pts <- groups[[grp]][[h]]$pts + 1L
                        groups[[grp]][[a]]$pts <- groups[[grp]][[a]]$pts + 1L }
  else                  groups[[grp]][[a]]$pts <- groups[[grp]][[a]]$pts + 3L
}

# ── 判斷各組完賽、排名 ───────────────────────────────────
total_per_group <- table(sapply(Filter(function(m) !is.null(m$group) && m$group != "", matches), `[[`, "group"))
played_per_group<- {
  pm <- Filter(function(m) !is.null(m$group) && m$group != "" && isTRUE(m$played), matches)
  if (length(pm)) table(sapply(pm, `[[`, "group")) else integer(0)
}

rank_group <- function(grp_teams) {
  tl <- lapply(names(grp_teams), function(t) grp_teams[[t]])
  tl[order(-sapply(tl,`[[`,"pts"), -sapply(tl,`[[`,"gd"), -sapply(tl,`[[`,"gf"))]
}

qmap   <- list()   # "A組第1" -> team
thirds <- list()   # for best-3rd ranking

for (grp in names(groups)) {
  total  <- if (!is.na(total_per_group[grp])) as.integer(total_per_group[grp]) else 0L
  played <- if (!is.na(played_per_group[grp])) as.integer(played_per_group[grp]) else 0L
  ranked <- rank_group(groups[[grp]])
  # Partial update (even incomplete groups show likely standings)
  for (i in seq_along(ranked)) {
    key <- sprintf("%s組第%d", grp, i)
    qmap[[key]] <- ranked[[i]]$team
  }
  if (played == total && total > 0) {
    cat(sprintf("[KO] %s組完賽: 1=%s(%dpts) 2=%s(%dpts) 3=%s\n",
                grp, ranked[[1]]$team, ranked[[1]]$pts,
                ranked[[2]]$team, ranked[[2]]$pts,
                ranked[[3]]$team))
    thirds[[ranked[[3]]$team]] <- list(pts=ranked[[3]]$pts, gd=ranked[[3]]$gd, gf=ranked[[3]]$gf)
  }
}

# ── 最佳第3名排序 ─────────────────────────────────────────
third_ranked <- if (length(thirds) > 0) {
  thirds[order(-sapply(thirds,`[[`,"pts"),
               -sapply(thirds,`[[`,"gd"),
               -sapply(thirds,`[[`,"gf"))]
} else { list() }
third_teams  <- names(third_ranked)
for (i in seq_along(third_teams)) {
  symbols <- c("①","②","③","④","⑤","⑥","⑦","⑧","⑨","⑩","⑪","⑫")
  qmap[[paste0("最佳第3-",symbols[i])]] <- third_teams[i]
}

# ── 填入淘汰賽名單 ────────────────────────────────────────
fill_slot <- function(label) {
  v <- qmap[[label]]
  if (!is.null(v) && nchar(v) > 0) v else ""
}

rounds <- c("r32","r16","qf","sf","third","final")
updated <- 0L
for (rnd in rounds) {
  if (is.null(ko_cfg[[rnd]])) next
  for (i in seq_along(ko_cfg[[rnd]])) {
    m <- ko_cfg[[rnd]][[i]]
    if (nchar(m$home) == 0) {
      nh <- fill_slot(m$label_h)
      if (nchar(nh) > 0) { ko_cfg[[rnd]][[i]]$home <- nh; updated <- updated + 1L }
    }
    if (nchar(m$away) == 0) {
      na_ <- fill_slot(m$label_a)
      if (nchar(na_) > 0) { ko_cfg[[rnd]][[i]]$away <- na_; updated <- updated + 1L }
    }
  }
}

write(toJSON(ko_cfg, auto_unbox=TRUE, pretty=TRUE), "data/knockouts.json")
cat(sprintf("[KO] knockouts.json 更新完成（填入 %d 個隊伍名稱）\n", updated))
