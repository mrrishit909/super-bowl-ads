# Steps 2-4: audit the five files, then the analysis. Every number on the page and in the README comes from the CSVs this script writes.
#
#   Rscript analyze.R   -> results/{audit,games,trends,rating_trend,summary,halftime,ads_tags,brands,tag_views,models}.csv
#
# A. The audience and the price (Wikipedia + FRED), one row per Super Bowl.
#    - parse the ratings table of the pinned Wikipedia revision; drop the Nickelodeon second feed of LVIII; keep Super Bowl I (two networks, no single figure) out of the trend;
#    - prices in 2025 dollars with the annual mean of CPI-U (FRED CPIAUCNS; 2025 is the mean of 11 months, October 2025 was never published); cost per thousand viewers = price / (viewers in millions x 1000);
#    - trends: ordinary least squares of log(series) on year, so the slope is a steady growth rate (%/year); 95% CI (normal) from Newey-West standard errors (Bartlett, 3 lags),
#      because neighbouring games are not independent. Games II-LVIII (1968-2024): the 57 games with one audience figure and one price. 2025 and 2026 prices are quoted as ranges
#      and the 2026 CPI year is incomplete, so they are shown but not fitted. The household rating (% of TV homes) gets a linear trend in points per decade.
# B. Halftime: the listed halftime audience against the game's, with the headliners from the second Wikipedia page.
# C. The ads (FiveThirtyEight tags + TidyTuesday YouTube counts), one row per ad after removing the repeated rows.
#    - prevalence of each tag overall and 2000-09 against 2010-20 (Fisher exact test; and Mantel-Haenszel stratified by brand, because the brand mix changed between the periods);
#    - ads per brand;
#    - views against tags: OLS of log(views) on the 7 tags (1) alone, (2) + log days since the video was uploaded (views accumulate with age),
#      (3) + brand fixed effects (brands repeat; some brands just draw more views), (4) + Super Bowl year fixed effects. Coefficients are shown as the % difference in views;
#      95% CI = estimate +/- 1.96 x HC3 robust standard error. Same three-step idea for the like share = likes/(likes+dislikes) among the ads with >= 100 votes.
set.seed(1)
suppressMessages({ library(rvest); library(jsonlite); library(sandwich) })
dir.create("results", showWarnings = FALSE)
Z <- qnorm(0.975); REF <- 2025; SCRAPE <- as.Date("2021-03-02"); NW_LAG <- 3
out <- function(df, name) write.csv(df, file.path("results", paste0(name, ".csv")), row.names = FALSE, na = "")
clean <- function(s) trimws(gsub("\\[[^]]*\\]", "", s))
num <- function(s) suppressWarnings(as.numeric(gsub("[,$]", "", clean(s))))

# ---------------------------------------------------------------- A. games
pg <- read_html(fromJSON("data/wiki_ratings.json")$parse$text)
raw <- as.data.frame(html_table(html_elements(pg, "table.wikitable")[[1]], header = FALSE)[-(1:2), ])
names(raw) <- c("sb", "date", "network", "viewers", "half", "hh_rating", "hh_share", "a_rating", "a_share", "cost", "wiki_real")
wiki_rows <- nrow(raw)
raw$game <- match(raw$sb, unique(raw$sb)); raw$year <- as.integer(format(as.Date(raw$date, "%B %d, %Y"), "%Y"))
stopifnot(all(raw$game == raw$year - 1966))
g <- raw[raw$network != "Nickelodeon", ]
rng <- regmatches(clean(g$cost), gregexpr("\\$[0-9,]+", clean(g$cost)))
g$cost_low <- sapply(rng, function(v) if (length(v)) min(num(v)) else NA); g$cost_high <- sapply(rng, function(v) if (length(v)) max(num(v)) else NA)
cpi_m <- read.csv("data/cpiaucns.csv"); cpi_m$yr <- as.integer(substr(cpi_m$observation_date, 1, 4))
cpi_y <- aggregate(CPIAUCNS ~ yr, cpi_m, function(v) { v <- v[!is.na(v)]; if (length(v) >= 11) mean(v) else NA }, na.action = na.pass)   # 2025 has 11 months: BLS never published October 2025 (federal shutdown)
cpi_ref <- cpi_y$CPIAUCNS[cpi_y$yr == REF]; cpi_2025_months <- sum(!is.na(cpi_m$CPIAUCNS[cpi_m$yr == REF]))
g$cpi <- cpi_y$CPIAUCNS[match(g$year, cpi_y$yr)]
g$real_low <- g$cost_low * cpi_ref / g$cpi; g$real_high <- g$cost_high * cpi_ref / g$cpi
g$viewers_m <- num(g$viewers); g$halftime_m <- num(g$half); g$hh_rating <- num(g$hh_rating); g$wiki_real <- num(g$wiki_real)
single <- !is.na(g$cost_low) & g$cost_low == g$cost_high & !is.na(g$viewers_m)
g$cpm_nominal <- ifelse(single, g$cost_low / (g$viewers_m * 1000), NA); g$cpm_real <- ifelse(single, g$real_low / (g$viewers_m * 1000), NA)
g$in_trend <- as.integer(g$game >= 2 & g$year <= 2024 & single)
games <- g[, c("game", "sb", "year", "network", "viewers_m", "halftime_m", "hh_rating", "cost_low", "cost_high", "cpi", "real_low", "real_high", "cpm_nominal", "cpm_real", "wiki_real", "in_trend")]
games$sb <- gsub("^50$", "L", games$sb)
out(games, "games")
tr <- g[g$in_trend == 1, ]; stopifnot(nrow(tr) == 57)

fit <- function(y, x, log = TRUE) {
  m <- lm((if (log) base::log(y) else y) ~ x); V <- NeweyWest(m, lag = NW_LAG, prewhite = FALSE, adjust = FALSE)
  b <- coef(m)[[2]]; se <- sqrt(V[2, 2]); c(b = b, se = se, r2 = summary(m)$r.squared)
}
series <- list(viewers = tr$viewers_m, price_nominal = tr$cost_low, price_real = tr$real_low, cpm_real = tr$cpm_real)
periods <- list(c(1968, 2024), c(1968, 1999), c(2000, 2024), c(2010, 2024), c(1968, 2019))
rows <- list()
for (s in names(series)) for (p in periods) {
  k <- tr$year >= p[1] & tr$year <= p[2]; f <- fit(series[[s]][k], tr$year[k])
  rows[[length(rows) + 1]] <- data.frame(series = s, from = p[1], to = p[2], n = sum(k), growth_pct_per_year = 100 * (exp(f[["b"]]) - 1),
                                         lo = 100 * (exp(f[["b"]] - Z * f[["se"]]) - 1), hi = 100 * (exp(f[["b"]] + Z * f[["se"]]) - 1), doubling_years = log(2) / f[["b"]], r2 = f[["r2"]])
}
out(do.call(rbind, rows), "trends")
rt <- list()
for (p in periods[c(1, 5)]) {
  k <- tr$year >= p[1] & tr$year <= p[2] & !is.na(tr$hh_rating); f <- fit(tr$hh_rating[k], tr$year[k], log = FALSE)
  rt[[length(rt) + 1]] <- data.frame(from = p[1], to = p[2], n = sum(k), pts_per_decade = 10 * f[["b"]], lo = 10 * (f[["b"]] - Z * f[["se"]]), hi = 10 * (f[["b"]] + Z * f[["se"]]), r2 = f[["r2"]])
}
out(do.call(rbind, rt), "rating_trend")

at <- function(yr, col) tr[[col]][tr$year == yr]
rated <- tr[!is.na(tr$hh_rating), ]
hv <- g[!is.na(g$halftime_m) & !is.na(g$viewers_m), ]
S <- list(
  viewers_1968 = at(1968, "viewers_m"), viewers_2024 = at(2024, "viewers_m"), viewers_ratio = at(2024, "viewers_m") / at(1968, "viewers_m"),
  price_nominal_1968 = at(1968, "cost_low"), price_nominal_2024 = at(2024, "cost_low"), price_nominal_ratio = at(2024, "cost_low") / at(1968, "cost_low"),
  price_real_1968 = at(1968, "real_low"), price_real_2024 = at(2024, "real_low"), price_real_ratio = at(2024, "real_low") / at(1968, "real_low"),
  cpm_real_1968 = at(1968, "cpm_real"), cpm_real_2024 = at(2024, "cpm_real"), cpm_real_ratio = at(2024, "cpm_real") / at(1968, "cpm_real"),
  cpm_nominal_1968 = at(1968, "cpm_nominal"), cpm_nominal_2024 = at(2024, "cpm_nominal"),
  viewers_2025 = g$viewers_m[g$year == 2025], viewers_2026 = g$viewers_m[g$year == 2026],
  price_2025_low = g$cost_low[g$year == 2025], price_2025_high = g$cost_high[g$year == 2025], price_2026_low = g$cost_low[g$year == 2026], price_2026_high = g$cost_high[g$year == 2026],
  viewers_2011 = at(2011, "viewers_m"), viewers_2011_2024_ratio = at(2024, "viewers_m") / at(2011, "viewers_m"), price_real_2011 = at(2011, "real_low"), price_real_2011_2024_ratio = at(2024, "real_low") / at(2011, "real_low"),
  peak_viewers_year = tr$year[which.max(tr$viewers_m)], peak_viewers = max(tr$viewers_m), games_in_trend = nrow(tr),
  peak_rating = max(rated$hh_rating), peak_rating_year = rated$year[which.max(rated$hh_rating)], low_rating = min(rated$hh_rating), low_rating_year = rated$year[which.min(rated$hh_rating)],
  low_rating_since_1970 = min(rated$hh_rating[rated$year >= 1970]), low_rating_since_1970_year = rated$year[rated$year >= 1970][which.min(rated$hh_rating[rated$year >= 1970])], rating_1968 = at(1968, "hh_rating"), rating_2024 = at(2024, "hh_rating"),
  max_abs_diff_vs_wikipedia_real_pct = max(abs(tr$real_low / tr$wiki_real - 1)) * 100,
  halftime_games = nrow(hv), halftime_above_game = sum(hv$halftime_m > hv$viewers_m),
  halftime_peak_m = max(hv$halftime_m), halftime_peak_year = hv$year[which.max(hv$halftime_m)], halftime_peak_ratio = max(hv$halftime_m / hv$viewers_m), halftime_peak_ratio_year = hv$year[which.max(hv$halftime_m / hv$viewers_m)],
  halftime_median_ratio_2011_2026 = median((hv$halftime_m / hv$viewers_m)[hv$year >= 2011]),
  halftime_2025_m = hv$halftime_m[hv$year == 2025], halftime_1993_m = hv$halftime_m[hv$year == 1993], game_1993_m = hv$viewers_m[hv$year == 1993])

# ---------------------------------------------------------------- B. halftime
th <- html_table(html_elements(read_html(fromJSON("data/wiki_halftime.json")$parse$text), "table.wikitable"), header = FALSE)
roman <- function(s) as.integer(as.roman(gsub("\\(show\\)", "", gsub("\\s+", "", clean(s)))))
heads <- list()
for (i in 1:7) { x <- as.data.frame(th[[i]]); k <- grep("Perform|Headliner", unlist(x[1, ]))[1]
  heads[[i]] <- data.frame(game = sapply(x[-1, 1], roman), headliners = gsub("\n", "; ", clean(x[-1, k]))) }
heads <- do.call(rbind, heads); stopifnot(!anyDuplicated(heads$game))
ht <- hv[order(hv$game), c("game", "year", "halftime_m", "viewers_m")]
ht$headliners <- heads$headliners[match(ht$game, heads$game)]; stopifnot(!anyNA(ht$headliners))
ht$ratio <- ht$halftime_m / ht$viewers_m
out(ht[, c("game", "year", "headliners", "halftime_m", "viewers_m", "ratio")], "halftime")

# ---------------------------------------------------------------- C. ads
tags <- c("funny", "show_product_quickly", "patriotic", "celebrity", "danger", "animals", "use_sex")
yt <- read.csv("data/youtube.csv", stringsAsFactors = FALSE); fb <- read.csv("data/superbowl-ads.csv", stringsAsFactors = FALSE)
fb$youtube_url[fb$youtube_url == ""] <- NA                                                  # the FiveThirtyEight file leaves it empty, the TidyTuesday one writes NA
key <- function(d) paste(d$year, d$brand, d$superbowl_ads_dot_com_url)
ads <- yt[!duplicated(key(yt)), ]; stopifnot(nrow(ads) == nrow(fb), !anyDuplicated(key(fb)))
m <- match(key(ads), key(fb)); stopifnot(!anyNA(m))
tags_differ <- sum(sapply(tags, function(t) sum(ads[[t]] != (fb[[t]][m] == "True")))) + sum(ads$youtube_url != fb$youtube_url[m] | xor(is.na(ads$youtube_url), is.na(fb$youtube_url[m])), na.rm = TRUE)
ads[tags] <- lapply(ads[tags], as.integer)
ads$brand <- ifelse(ads$brand == "Hynudai", "Hyundai", ads$brand)                       # the source spells it Hynudai
ads$age_days <- as.numeric(SCRAPE - as.Date(substr(ads$published_at, 1, 10)))
ads$up_year <- as.integer(substr(ads$published_at, 1, 4))
ads$votes <- ads$like_count + ads$dislike_count; ads$share <- ads$like_count / ads$votes
v <- ads[!is.na(ads$view_count), ]
audit <- data.frame(key = c("wikipedia_ratings_rows", "wikipedia_rows_dropped_nickelodeon_feed", "super_bowl_I_rows_two_networks", "games_in_trend", "games_with_price_range_only", "games_with_halftime_audience",
                            "youtube_csv_rows", "superbowl_ads_csv_rows", "rows_repeating_one_ad", "unique_ads", "ads_where_the_two_files_disagree", "ads_without_youtube_link", "ads_link_but_no_counts",
                            "ads_with_views", "ads_with_likes", "ads_with_100_votes", "ads_relabelled_Hynudai", "uploaded_year_after_airing_year", "uploaded_year_before_airing_year", "median_video_age_years",
                            "youngest_video_age_days", "oldest_video_age_days", "brands", "super_bowls_covered", "ads_min_year", "ads_max_year", "cpi_2025_months_available"),
                    value = c(wiki_rows, sum(raw$network == "Nickelodeon"), sum(raw$game == 1), nrow(tr), sum(!is.na(g$cost_low) & g$cost_low != g$cost_high), nrow(hv),
                              nrow(yt), nrow(fb), nrow(yt) - nrow(ads), nrow(ads), tags_differ, sum(is.na(ads$youtube_url)), sum(!is.na(ads$youtube_url) & is.na(ads$view_count)),
                              nrow(v), sum(!is.na(v$like_count)), sum(!is.na(v$votes) & v$votes >= 100), sum(ads$brand == "Hyundai"), sum(v$up_year > v$year), sum(v$up_year < v$year), median(v$age_days) / 365.25,
                              min(v$age_days), max(v$age_days), length(unique(ads$brand)), length(unique(ads$year)), min(ads$year), max(ads$year), cpi_2025_months))
out(audit, "audit")

# tags: overall and by period
early <- ads$year <= 2009
tg <- do.call(rbind, lapply(tags, function(t) {
  a <- sum(ads[[t]][early]); b <- sum(early) - a; c <- sum(ads[[t]][!early]); d <- sum(!early) - c
  # the same comparison inside each brand (Mantel-Haenszel over the 10 brands): is the shift only a change in which brands advertised?
  arr <- table(period = factor(ifelse(early, "early", "late"), levels = c("late", "early")), yes = factor(ads[[t]], levels = 1:0), brand = ads$brand); mh <- mantelhaen.test(arr, correct = FALSE)
  data.frame(tag = t, n_yes = sum(ads[[t]]), pct_all = 100 * mean(ads[[t]]), n_2000_09 = a, pct_2000_09 = 100 * a / sum(early), n_2010_20 = c, pct_2010_20 = 100 * c / sum(!early),
             diff_pts = 100 * (c / sum(!early) - a / sum(early)), fisher_p = fisher.test(matrix(c(a, b, c, d), 2))$p.value, mh_or_late_vs_early = unname(mh$estimate), mh_p = mh$p.value) }))
out(tg, "ads_tags")
S$ads_total <- nrow(ads); S$ads_2000_09 <- sum(early); S$ads_2010_20 <- sum(!early)
S$tags_per_ad_mean <- mean(rowSums(ads[tags])); S$ads_no_tag_except_product <- sum(rowSums(ads[setdiff(tags, "show_product_quickly")]) == 0)

# brands
br <- do.call(rbind, lapply(sort(unique(ads$brand)), function(b) { a <- ads[ads$brand == b, ]; w <- a[!is.na(a$view_count), ]
  data.frame(brand = b, ads = nrow(a), pct_of_ads = 100 * nrow(a) / nrow(ads), super_bowls = length(unique(a$year)), first_year = min(a$year), last_year = max(a$year), max_in_one_game = max(table(a$year)),
             with_views = nrow(w), median_views = median(w$view_count), pct_funny = 100 * mean(a$funny)) }))
br <- br[order(-br$ads, br$brand), ]; out(br, "brands")
S$bud_ads <- sum(ads$brand %in% c("Bud Light", "Budweiser")); S$bud_pct <- 100 * S$bud_ads / nrow(ads)
S$max_ads_one_brand_one_game <- max(table(ads$brand, ads$year)); S$spearman_views_upload_age <- cor(log(v$view_count), log(v$age_days), method = "spearman"); S$spearman_views_airing_year <- cor(v$view_count, v$year, method = "spearman")
S$median_views_all <- median(v$view_count); S$views_top10_share_pct <- 100 * sum(sort(v$view_count, decreasing = TRUE)[1:10]) / sum(v$view_count)

# raw medians by tag
tv <- do.call(rbind, lapply(tags, function(t) { y1 <- v$view_count[v[[t]] == 1]; y0 <- v$view_count[v[[t]] == 0]
  data.frame(tag = t, n_yes = length(y1), n_no = length(y0), median_yes = median(y1), median_no = median(y0), ratio = median(y1) / median(y0)) }))
out(tv, "tag_views")

# models
fitm <- function(d, outcome, rhs) {
  d$la <- log(d$age_days) - mean(log(d$age_days)); d$y <- if (outcome == "log_views") log(d$view_count) else d$share
  m <- lm(as.formula(paste("y ~", paste(c(tags, rhs), collapse = " + "))), data = d); V <- vcovHC(m, type = "HC3")
  b <- coef(m)[tags]; se <- sqrt(diag(V))[tags]
  list(b = b, se = se, n = nobs(m))
}
specs <- list(c(), "la", c("la", "factor(brand)"), c("la", "factor(brand)", "factor(year)"))
mrows <- list()
for (oc in c("log_views", "like_share")) {
  d <- if (oc == "log_views") v else v[!is.na(v$votes) & v$votes >= 100, ]
  for (si in seq_along(specs)) { if (oc == "like_share" && si == 4) next
    f <- fitm(d, oc, specs[[si]])
    for (t in tags) { b <- f$b[[t]]; se <- f$se[[t]]
      eff <- function(x) if (oc == "log_views") 100 * (exp(x) - 1) else 100 * x
      mrows[[length(mrows) + 1]] <- data.frame(outcome = oc, spec = si, tag = t, n = f$n, b = b, se_hc3 = se, effect = eff(b), lo = eff(b - Z * se), hi = eff(b + Z * se)) } }
}
out(do.call(rbind, mrows), "models")
S$like_share_median_pct <- 100 * median(v$share[!is.na(v$votes) & v$votes >= 100])

out(data.frame(key = names(S), value = unlist(S)), "summary")
cat("analyze.R done\n"); print(read.csv("results/summary.csv"), row.names = FALSE); print(read.csv("results/audit.csv"), row.names = FALSE)
