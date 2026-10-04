# Step 1: download the five raw files.
#
#   Rscript download.R   -> data/{youtube.csv, superbowl-ads.csv, wiki_ratings.json, wiki_halftime.json, cpiaucns.csv}, results/pull.json
#
# Sources and licences (nothing in data/ is committed; the page and README quote only aggregates computed from it):
#  1. superbowl-ads.csv  FiveThirtyEight, github.com/fivethirtyeight/superbowl-ads: 244 ads from the 10 brands with the most Super Bowl spots 2000-2020 (superbowl-ads.com),
#     each tagged yes/no on seven features by FiveThirtyEight staff. The repo has no licence file of its own; FiveThirtyEight's main data repo says its data sets are CC BY 4.0
#     unless otherwise noted, so it is credited on that basis. Pinned to commit b65e233.
#  2. youtube.csv       TidyTuesday 2021-03-02 (github.com/rfordatascience/tidytuesday, repo licence CC0-1.0): the same ads matched to YouTube view/like/dislike counts, pulled
#     by the TidyTuesday curator from the YouTube Data API in early 2021 (committed 2021-03-02). YouTube's terms apply to the counts, hence aggregates only. Pinned to commit fa1a9c3.
#  3. wiki_ratings.json  English Wikipedia, "Super Bowl television ratings" (CC BY-SA 4.0), through the MediaWiki API (action=parse) pinned to one revision: viewers, halftime
#     viewers, household rating and the average cost of a 30-second ad for every game. Descriptive User-Agent, one request per page, no crawling.
#  4. wiki_halftime.json English Wikipedia, "List of Super Bowl halftime shows" (CC BY-SA 4.0), pinned revision: the headliners of each halftime show.
#  5. cpiaucns.csv      FRED, series CPIAUCNS (CPI-U, all items, not seasonally adjusted; U.S. Bureau of Labor Statistics, public domain), used to put prices in 2025 dollars.
# Cite: FiveThirtyEight, "According To Super Bowl Ads, Americans Love America, Animals And Sex" (2021); Wikipedia contributors, as above; BLS CPI-U via FRED.
#
# The Wikipedia pages are fetched at fixed revision ids so a rerun gets the same text; change REV_* to move to a newer revision.
REV_RATINGS <- 1371496892; REV_HALFTIME <- 1373962383
UA <- "research-portfolio (Rishit Raj Mathur; mathur1@usf.edu) R-download.file"
options(HTTPUserAgent = UA, timeout = 120)
dir.create("data", showWarnings = FALSE); dir.create("results", showWarnings = FALSE)
wiki <- function(rev) sprintf("https://en.wikipedia.org/w/api.php?action=parse&oldid=%d&prop=text%%7Crevid&format=json&formatversion=2", rev)
files <- list(
  "superbowl-ads.csv" = "https://raw.githubusercontent.com/fivethirtyeight/superbowl-ads/b65e23360cb165676806865ebc84207c0a5f5b63/superbowl-ads.csv",
  "youtube.csv" = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/fa1a9c3e6c3bcf7a30fd5823c59c7aeb7f44a973/data/2021/2021-03-02/youtube.csv",
  "wiki_ratings.json" = wiki(REV_RATINGS),
  "wiki_halftime.json" = wiki(REV_HALFTIME),
  "cpiaucns.csv" = "https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIAUCNS")
info <- list(pulled = as.character(Sys.Date()), wikipedia_revisions = list(ratings = REV_RATINGS, halftime = REV_HALFTIME), files = list())
for (nm in names(files)) {
  dest <- file.path("data", nm)
  if (nm == "cpiaucns.csv") options(HTTPUserAgent = NULL)       # FRED stalls on custom user agents
  ok <- FALSE
  for (try in 1:4) { ok <- tryCatch({ download.file(files[[nm]], dest, mode = "wb", quiet = TRUE); TRUE }, error = function(e) FALSE, warning = function(w) FALSE); if (ok) break; Sys.sleep(3 * try) }
  stopifnot(ok)
  if (nm == "cpiaucns.csv") options(HTTPUserAgent = UA)
  sha <- sub(" .*", "", system2("shasum", c("-a", "256", dest), stdout = TRUE))
  info$files[[nm]] <- list(url = files[[nm]], bytes = file.size(dest), sha256 = sha)
}
writeLines(jsonlite::toJSON(info, auto_unbox = TRUE, pretty = TRUE), "results/pull.json")
print(info)
