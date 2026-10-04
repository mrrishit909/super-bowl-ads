# Step 6: charts, with ggplot2.   Rscript charts.R -> charts/00_cover.png, 01_audience.png, 02_price.png, 03_cpm.png, 04_halftime.png, 05_ads.png, 06_views.png
suppressPackageStartupMessages({ library(ggplot2); library(dplyr); library(readr); library(tidyr); library(scales); library(ragg); library(patchwork) })
INK <- "#f2f2f0"; DIM <- "#8a8a87"; GRID <- "#1d1d1d"; BG <- "#0b0b0b"; BLUE <- "#3987e5"; ORANGE <- "#d95926"; GRAY <- "#9a9a96"; GREEN <- "#3aa876"
theme_house <- function(base = 11) theme_minimal(base_size = base, base_family = "Helvetica Neue") + theme(
  plot.background = element_rect(fill = BG, color = NA), panel.background = element_rect(fill = BG, color = NA), panel.grid.major = element_line(color = GRID, linewidth = 0.5), panel.grid.minor = element_blank(),
  text = element_text(color = INK), axis.text = element_text(color = DIM), axis.title = element_text(color = DIM), axis.line = element_line(color = "#3a3a3a", linewidth = 0.4), axis.ticks = element_blank(),
  plot.title = element_text(color = INK, size = base + 2, hjust = 0, margin = margin(b = 10)), plot.title.position = "plot", legend.text = element_text(color = INK, size = base - 1), legend.title = element_blank(),
  legend.key = element_rect(fill = BG, color = NA), legend.background = element_rect(fill = BG, color = NA), plot.margin = margin(10, 14, 8, 8))
save <- function(p, name, w = 10, h = 4, dpi = 150) ggsave(file.path("charts", name), p, width = w, height = h, dpi = dpi, device = ragg::agg_png, bg = BG)
dir.create("charts", showWarnings = FALSE)

S <- read_csv("results/summary.csv", show_col_types = FALSE); sm <- setNames(S$value, S$key)
gm <- read_csv("results/games.csv", show_col_types = FALSE) |> filter(network != "Nickelodeon")
tr <- gm |> filter(in_trend == 1)
tg <- read_csv("results/ads_tags.csv", show_col_types = FALSE); br <- read_csv("results/brands.csv", show_col_types = FALSE); md <- read_csv("results/models.csv", show_col_types = FALSE)
tr_t <- read_csv("results/trends.csv", show_col_types = FALSE); ht <- read_csv("results/halftime.csv", show_col_types = FALSE)
lab <- c(funny = "funny", show_product_quickly = "product in first 10 s", patriotic = "patriotic", celebrity = "celebrity", danger = "danger", animals = "animals", use_sex = "sex appeal")
M <- function(x) sprintf("$%.1fM", x / 1e6)

# index (1968 = 100), log scale
idx <- tr |> transmute(year, Viewers = 100 * viewers_m / first(viewers_m), `Price, 2025 dollars` = 100 * real_low / first(real_low), `Price, as quoted` = 100 * cost_low / first(cost_low)) |>
  pivot_longer(-year, names_to = "series", values_to = "index")
cols <- c("Viewers" = BLUE, "Price, 2025 dollars" = ORANGE, "Price, as quoted" = GRAY)
ends <- idx |> filter(year == 2024) |> mutate(label = sprintf("%.1fx", index / 100))
pidx <- function(base = 11, lw = 1.1) ggplot(idx, aes(year, index, color = series)) + geom_line(linewidth = lw) +
  geom_text(data = ends, aes(label = label), hjust = -0.15, size = base * 0.3, show.legend = FALSE) + scale_color_manual(values = cols) +
  scale_y_log10(breaks = c(100, 300, 1000, 3000, 10000, 30000), labels = label_comma()) + scale_x_continuous(breaks = seq(1970, 2020, 10), limits = c(1968, 2031)) +
  labs(x = NULL, y = "index, 1968 = 100 (log scale)") + theme_house(base) + theme(legend.position = "top", legend.justification = "left")
pcpm <- function(base = 11) ggplot(tr, aes(year, cpm_real)) + geom_line(color = ORANGE, linewidth = 1.1) + geom_point(color = ORANGE, size = 1.6) +
  annotate("text", x = 1968, y = tr$cpm_real[1] + 5, label = sprintf("$%.0f in 1968", sm[["cpm_real_1968"]]), hjust = 0, color = INK, size = base * 0.3) +
  annotate("text", x = 2024.5, y = sm[["cpm_real_2024"]] - 12, label = sprintf("$%.0f in 2024", sm[["cpm_real_2024"]]), hjust = 1, color = INK, size = base * 0.3) +
  scale_y_continuous(limits = c(0, 75), labels = label_dollar()) + scale_x_continuous(breaks = seq(1970, 2020, 10)) + labs(x = NULL, y = "price per 1,000 viewers, 2025 dollars") + theme_house(base)

# cover
c1 <- pidx(13, 1.4) + labs(title = "Audience and price of a 30-second ad, indexed to 1968")
c2 <- pcpm(13) + labs(title = "What one thousand viewers cost an advertiser")
cover <- (c1 | c2) + plot_annotation(title = sprintf("Since 1968 the Super Bowl audience grew %.1fx; a 30-second ad costs %.1fx more in real dollars", sm[["viewers_ratio"]], sm[["price_real_ratio"]]),
  theme = theme(plot.background = element_rect(fill = BG, color = NA), plot.title = element_text(color = INK, size = 19, hjust = 0, margin = margin(t = 6, b = 10))))
save(cover, "00_cover.png", 14, 8, 100)

# 01 audience: viewers and household rating
v1 <- gm |> filter(!is.na(viewers_m), game >= 2)
p1 <- ggplot(v1, aes(year, viewers_m)) + geom_vline(xintercept = 2020.5, color = "#4a4a48", linetype = "dashed", linewidth = 0.5) +
  geom_line(color = BLUE, linewidth = 1.1) + geom_point(color = BLUE, size = 1.5) +
  annotate("text", x = 2020, y = 12, label = "from 2021: counts revised,\nstreaming reported too", hjust = 1, color = DIM, size = 3.0, lineheight = 0.95) +
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 120, 30)) + scale_x_continuous(breaks = seq(1970, 2020, 10)) +
  labs(x = NULL, y = "average viewers, millions", title = "Viewers (millions)") + theme_house(11)
p2 <- ggplot(tr |> filter(!is.na(hh_rating)), aes(year, hh_rating)) + geom_line(color = GREEN, linewidth = 1.1) + geom_point(color = GREEN, size = 1.5) +
  scale_y_continuous(limits = c(0, 55), breaks = seq(0, 50, 10)) + scale_x_continuous(breaks = seq(1970, 2020, 10)) + labs(x = NULL, y = "% of TV households", title = "Household rating") + theme_house(11)
save(p1 | p2, "01_audience.png", 10, 3.9)

# 02 price: as quoted and in 2025 dollars; 2025-26 quoted as ranges
pr <- tr |> transmute(year, `As quoted` = cost_low / 1e6, `In 2025 dollars` = real_low / 1e6) |> pivot_longer(-year, names_to = "series", values_to = "m")
rg <- gm |> filter(year >= 2025) |> transmute(year, lo = cost_low / 1e6, hi = cost_high / 1e6)
p3 <- ggplot(pr, aes(year, m, color = series)) + geom_line(linewidth = 1.1) +
  geom_linerange(data = rg, aes(x = year, ymin = lo, ymax = hi), inherit.aes = FALSE, color = GRAY, linewidth = 2.2) +
  annotate("text", x = 2023.5, y = rg$hi[2] + 0.2, label = "2025 and 2026:\nquoted as ranges", hjust = 1, color = DIM, size = 3.0, lineheight = 0.95) +
  scale_color_manual(values = c("As quoted" = GRAY, "In 2025 dollars" = ORANGE)) + scale_y_continuous(limits = c(0, 10.5), breaks = seq(0, 10, 2), labels = function(x) paste0("$", x, "M")) +
  scale_x_continuous(breaks = seq(1970, 2025, 10)) + labs(x = NULL, y = "average price of a 30-second ad", title = "What a 30-second ad cost, as quoted and in 2025 dollars") + theme_house(11) + theme(legend.position = "top", legend.justification = "left")
save(p3, "02_price.png", 10, 3.9)

# 03 cost per thousand viewers
g3 <- tr_t |> filter(series == "cpm_real", from == 1968 | from == 2000, to != 2019, to != 2000)
p4 <- pcpm(11) + labs(title = "Real price per 1,000 viewers of a 30-second ad") +
  annotate("text", x = 1970, y = 70, hjust = 0, color = DIM, size = 3.1, label = sprintf("trend: %.1f%% a year 1968-99, %.1f%% a year 2000-24", tr_t$growth_pct_per_year[tr_t$series == "cpm_real" & tr_t$from == 1968 & tr_t$to == 1999], tr_t$growth_pct_per_year[tr_t$series == "cpm_real" & tr_t$from == 2000]))
save(p4, "03_cpm.png", 10, 3.8)

# 04 halftime
h4 <- ht |> mutate(kind = ifelse(year %in% c(1993, 2006), "listed for 1993 and 2006 only", "halftime"))
lbl <- h4 |> filter(year %in% c(2015, 2023, 2025)) |> mutate(who = sub(";.*", "", headliners))
p5 <- ggplot(h4) + geom_segment(aes(x = year, xend = year, y = viewers_m, yend = halftime_m), color = "#4a4a48", linewidth = 0.9) +
  geom_point(aes(year, viewers_m, color = "game average"), size = 2.4) + geom_point(aes(year, halftime_m, color = "halftime show"), size = 2.4) +
  geom_text(data = lbl, aes(year, halftime_m + 7, label = who), color = INK, size = 3.0) +
  annotate("text", x = 1996, y = 120, label = "1993 and 2006: one listed figure each,\nnot comparable with 2011 on", hjust = 0, color = DIM, size = 3.0, lineheight = 0.95) +
  scale_color_manual(values = c("game average" = BLUE, "halftime show" = ORANGE)) + scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, 30)) + scale_x_continuous(breaks = c(1993, 2006, 2011, 2015, 2019, 2023, 2026)) +
  labs(x = NULL, y = "viewers, millions", title = "Halftime show audience against the game's average (the 18 games that list both)") + theme_house(11) + theme(legend.position = "top", legend.justification = "left")
save(p5, "04_halftime.png", 10, 3.9)

# 05 ads: ads per brand, tag shares by period
b5 <- br |> mutate(brand = factor(brand, levels = rev(brand)))
p6 <- ggplot(b5, aes(ads, brand)) + geom_col(fill = BLUE, width = 0.7) + geom_text(aes(label = ads), hjust = -0.25, color = INK, size = 3.1) +
  scale_x_continuous(limits = c(0, 70), breaks = seq(0, 60, 20)) + labs(x = "ads in the data, 2000-2020", y = NULL, title = "Ads per brand") + theme_house(11) + theme(panel.grid.major.y = element_blank())
t5 <- tg |> arrange(pct_all) |> mutate(tag = factor(lab[tag], levels = lab[tag])) |> select(tag, `2000-09` = pct_2000_09, `2010-20` = pct_2010_20) |> pivot_longer(-tag, names_to = "period", values_to = "pct")
p7 <- ggplot(t5, aes(pct, tag, fill = period)) + geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = sprintf("%.0f%%", pct)), position = position_dodge(width = 0.75), hjust = -0.15, color = INK, size = 2.9) +
  scale_fill_manual(values = c("2000-09" = GRAY, "2010-20" = BLUE)) + scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  labs(x = "% of ads with the tag", y = NULL, title = "What the ads contained") + theme_house(11) + theme(panel.grid.major.y = element_blank(), legend.position = "top", legend.justification = "left")
save(p6 | p7, "05_ads.png", 10, 3.9)

# 06 views: ratio of median-style effect, raw and adjusted
f6 <- md |> filter(outcome == "log_views", spec %in% c(1, 3)) |> mutate(r = 1 + effect / 100, rlo = 1 + lo / 100, rhi = 1 + hi / 100, tag = factor(lab[tag], levels = rev(lab)),
  model = factor(ifelse(spec == 1, "tags only", "plus video age and brand"), levels = c("tags only", "plus video age and brand")))
p8 <- ggplot(f6, aes(r, tag, color = model)) + geom_vline(xintercept = 1, color = "#6a6a68", linewidth = 0.6) +
  geom_pointrange(aes(xmin = rlo, xmax = rhi), position = position_dodge(width = 0.6), linewidth = 0.55, size = 0.4) +
  scale_color_manual(values = c("tags only" = GRAY, "plus video age and brand" = ORANGE)) + scale_x_log10(breaks = c(0.25, 0.5, 1, 2, 4, 8), labels = c("0.25x", "0.5x", "same", "2x", "4x", "8x")) +
  labs(x = "views of ads with the tag, relative to ads without it (log scale; bars: 95% CI)", y = NULL, title = "No tag reliably goes with more YouTube views") + theme_house(11) +
  theme(panel.grid.major.y = element_blank(), legend.position = "top", legend.justification = "left")
save(p8, "06_views.png", 10, 4.2)
cat("charts done\n")
