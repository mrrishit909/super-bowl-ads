# Did Super Bowl ad prices outrun the audience?

The Super Bowl is the one television broadcast advertisers pay a fortune to be seen in. Wikipedia's tables give the audience and the average price of a 30-second ad for every game since 1967; FiveThirtyEight and TidyTuesday give the ads of 2000 to 2020 with their YouTube numbers. Five questions, answered in R:
- has the audience grown as fast as the price of a 30-second ad, in real (2025) dollars, and what does one thousand viewers cost now against 1968;
- has the audience grown at all since 2010, and what share of TV homes tunes in;
- how big is the halftime show's audience next to the game's;
- what the ads of 2000 to 2020 were like (funny, patriotic, celebrities, animals, danger, sex), how that changed, and how many ads each brand aired;
- whether any of those features goes with more YouTube views or likes, once the age of the video and the brand are accounted for.

The task follows DataCamp's "Analyzing Super Bowl Viewership and Advertising" (R, beginner: games, ads, halftime shows), rebuilt on public tables with an independent recount. The page with the results and the charts is the build log:

Build log: https://mrrishit909.github.io/projects/super-bowl-ads/

## Data

Five files, fetched by `download.R` and not committed (`data/` is ignored; `results/pull.json` has the URLs, sizes and SHA-256 checksums, about 1.3 MB in all):

1. **Ad tags**: FiveThirtyEight, `superbowl-ads.csv` (https://github.com/fivethirtyeight/superbowl-ads, commit b65e233): 244 ads from the 10 brands with the most Super Bowl spots in 2000 to 2020 according to superbowl-ads.com, each marked yes or no by FiveThirtyEight staff on seven features. The repository has no licence file, and the ad folder is not part of FiveThirtyEight's main data repository, which says its data sets are CC BY 4.0 unless otherwise noted; the file is credited on that basis and only aggregates are published here.
2. **YouTube counts**: TidyTuesday, 2021-03-02, `youtube.csv` (https://github.com/rfordatascience/tidytuesday, repository licence CC0-1.0, commit fa1a9c3): the same ads matched to YouTube views, likes, dislikes and upload dates, collected by the TidyTuesday curator from the YouTube Data API in early 2021 (the file was committed on 2021-03-02). The counts are YouTube's, so only aggregates are published here.
3. **Audience and price**: English Wikipedia, "Super Bowl television ratings" (revision 1371496892, CC BY-SA 4.0), through the MediaWiki API with a descriptive User-Agent: average viewers, halftime viewers, household rating and the average cost of a 30-second ad for every game. Wikipedia's tables cite Nielsen and trade press.
4. **Halftime headliners**: English Wikipedia, "List of Super Bowl halftime shows" (revision 1373962383, CC BY-SA 4.0).
5. **Prices in 2025 dollars**: FRED series CPIAUCNS, the U.S. Bureau of Labor Statistics' CPI-U, all items, not seasonally adjusted (public domain).

`results/games.csv` and `results/halftime.csv` are transcriptions and calculations on the Wikipedia tables, so they are shared under CC BY-SA 4.0 with attribution to Wikipedia contributors; the other results are the author's aggregates. Cite: FiveThirtyEight, "According To Super Bowl Ads, Americans Love America, Animals And Sex" (2021); Wikipedia contributors, as above; U.S. Bureau of Labor Statistics via FRED.

```
Rscript download.R    # data/*            (R with jsonlite)
Rscript analyze.R     # results/*         (R with rvest, jsonlite, sandwich; a few seconds)
python3 check.py      # plain Python, no packages
Rscript charts.R      # charts/*          (R with ggplot2, dplyr, tidyr, readr, scales, patchwork, ragg)
```

## Steps

1. **Download** the five files at pinned commits and revisions.
2. **Audit** the files: the Wikipedia table has two rows for Super Bowl I (two networks, no single audience figure) and a second, Nickelodeon feed of LVIII; the prices for 2025 and 2026 are ranges; October 2025 is missing from the CPI series; the YouTube file repeats one ad (Bud Light, 2005) in 4 rows; the brand "Hyundai" is spelled "Hynudai" in both ad files.
3. **Audience against price**, games II to LVIII (1968 to 2024, 57 games with one audience figure and one price): price in 2025 dollars = price x CPI of 2025 / CPI of the game's year (annual mean); price per thousand viewers = price / (average viewers x 1,000); a least-squares line through the logarithm of each series against the year gives a steady growth rate, with a 95% interval from Newey-West standard errors (3 lags) because neighbouring games are not independent. The household rating gets a straight line in points per decade.
4. **Halftime**: the 18 games with a listed halftime audience, compared with the game's average.
5. **The ads**: how often each feature appears overall and in 2000 to 2009 against 2010 to 2020 (Fisher's exact test, and a Mantel-Haenszel test within brands, because the brand mix changed); ads per brand.
6. **Views and likes against features**: the logarithm of views on the seven features (a) alone, (b) plus the logarithm of the days since upload (views accumulate with age), (c) plus a fixed effect for each brand (brands repeat, and some draw more views), (d) plus a fixed effect for the Super Bowl year; and the like share (likes / likes + dislikes) among the 126 ads with at least 100 votes, the same way without (d). 95% intervals are the estimate plus or minus 1.96 HC3 robust standard errors.
7. **Check**: `check.py` recomputes every number in `results/` in plain Python: it parses the Wikipedia HTML with `html.parser` (expanding row and column spans by hand), solves every regression by Gauss-Jordan elimination, writes out the Newey-West and HC3 formulas, and sums Fisher's exact test and the Mantel-Haenszel statistic by hand. 1,667 numbers agree with R's to a relative 1e-8; counts match exactly.
8. **Charts**, in ggplot2.

## Results

All numbers below are R's (`results/`), and `check.py` recomputed each of them.

- **Prices outran the audience.** Between 1968 and 2024 the average audience grew 3.1 times (39.1 to 120.3 million viewers), the price of a 30-second ad 128 times as quoted ($0.05M to $7.0M) and 14.2 times in 2025 dollars ($0.50M to $7.2M). One thousand viewers cost an advertiser $13 in 1968 and $60 in 2024 (2025 dollars), 4.6 times as much. As steady growth rates (95% interval): audience 1.4% a year (1.0% to 1.9%), real price 5.2% (4.9% to 5.6%), price per thousand viewers 3.8% (3.3% to 4.2%). The real price doubles every 14 years, the audience every 49.
- **The audience has not grown since 2010; the price has.** From 2010 to 2024 the audience trend is -0.3% a year (-1.2% to 0.6%: flat), the real price 4.0% (2.8% to 5.2%) and the price per thousand viewers 4.3% (2.6% to 6.0%). Growth was fastest before 2000 (audience / real price / per thousand, % a year: 1968-99 2.4% / 6.0% / 3.5%; 2000-24 1.1% / 3.3% / 2.2%). Ending in 2019, before the measurement changes below, gives the same picture: audience 1.6% a year, real price 5.5%. The 2025 game drew 127.7 million (the highest figure in the table; price quoted as $7.5M to $8.0M) and 2026 124.9 million ($8M to $10M); these two are shown but not fitted.
- **The share of TV homes watching is flat.** The household rating peaked at 49.1 in 1982, was 36.8 in 1968 and 42.1 in 2024, and fell to 36.9 in 2022, its lowest since 1969. Its straight-line trend is −0.03 points a decade (−1.0 to +0.9), so the growth in millions is not a rise in the share of TV homes tuning in (population and the number of TV homes were not examined).
- **Halftime.** 18 games list a halftime audience; in 15 of them it is above the game's average. From 2011 on the median halftime audience is 3% above the game's average; the highest is 133.5 million in 2025 (Kendrick Lamar). Halftime falls in the middle of the broadcast, when the audience is near its peak, so this does not show that the shows draw viewers. The 1993 figure (133.4 million against 91.0) is a single listed number and is not comparable.
- **The ads.** Of 244 ads: funny 69%, product shown in the first 10 seconds 68%, animals 36%, danger 31%, celebrity 28%, sex appeal 26%, patriotic 17%. Funny fell from 84% of the ads of 2000 to 2009 to 56% in 2010 to 2020 (Fisher p <0.001; within brands, Mantel-Haenszel odds ratio 0.24, p <0.001), and patriotic rose from 10% to 23% (within brands, odds ratio 3.3, p 0.004). Sex appeal (33% to 20%) is not distinguishable from chance within brands (p 0.12); with seven tags tested, p under 0.007 is the Bonferroni line.
- **Brands.** Bud Light aired 60 of the ads and Budweiser 43: 103 ads, 42% of the data, and up to 7 spots in one game from one brand. The ten brands were chosen for having the most spots, so this describes them, not Super Bowl advertising.
- **No feature reliably goes with views or likes.** 228 ads have view counts (median 41,635 views); the ten most viewed videos hold 89% of all views. Views rise with the age of the upload (rank correlation 0.30) and not with the airing year (0.01). Unadjusted, ads that are patriotic have a median 1.8x the views of those that are not (danger 1.6x, funny 1.3x), but with the upload age and the brand taken into account every feature's 95% interval includes no difference, in all four specifications, and so does every like-share interval (all within plus or minus 9 points; the median like share is 95.7%). The point estimates move a lot between specifications (patriotic: +67% unadjusted, +65% with age and brand, +8% with the year as well), which is what intervals this wide mean.

## Not done, and caveats

- **The audience figures are not like-for-like.** The table's own notes say Nielsen revised 2021 from 91.63 to 95.2 million (out-of-home viewers had been undercounted) and 2023 from 112.17 to 114.21 (an error), and the page's text gives 2022 as 99.18 million on TV with about 11 million more streaming. The table does not say which definition each row uses, so 2021 onward may not be comparable with earlier years; the trends ending in 2019 are given above for that reason. Audience is the average over the game, not the audience of any one ad.
- **The prices are one number per game** (the average cost of a 30-second ad) from sources the table cites, not what any advertiser paid; the 2023 and 2024 prices ($7,000,000) carry no citation in the table; 2025 and 2026 are quoted as ranges. Real prices use the annual mean of CPI-U; 2025 has 11 months because the Bureau of Labor Statistics never published October 2025. They match Wikipedia's own inflation-adjusted column to within 0.04%.
- **Price per viewer is not worth.** Nothing here measures sales, attention, who the viewers are, or what the 30 seconds do for a brand; and an ad's audience is not the game's average.
- **Super Bowl I is left out** of the trends (two networks, two audience figures, two prices), and so is the Nickelodeon feed of LVIII. The confidence intervals on the trends allow for neighbouring games being alike (Newey-West, 3 lags) but are still approximate with 57 games.
- **The ad data are 10 brands, not all advertisers**: the brands with the most spots in 2000 to 2020 according to superbowl-ads.com, so "ads per brand" and the share of Anheuser-Busch say nothing about the rest of the broadcast. The seven tags are FiveThirtyEight staff's judgement (for example, "funny" means trying to be funny), not measurements. Shifts between decades may reflect which brands were in the sample; the Mantel-Haenszel tests are one way of checking that, not a cure.
- **YouTube counts are as of early 2021** and belong to whichever channel uploaded the video: 87 of the 228 videos were uploaded in a later year than the game and 5 in an earlier one, so many are re-uploads by third parties and the uploader is a confounder that is not in the data. Video age is measured to 2021-03-02, the date the file was committed (the scrape date is not recorded; the newest upload is from 27 January 2021). 11 ads have no video, 5 more have no counts (removed or private) and 6 hide likes. Dislike counts are present for every ad that shows likes: the scrape predates YouTube hiding public dislike counts (announced November 2021), so it cannot be repeated.
- **Associations, not causes**, from observational data with 10 brands: the HC3 standard errors treat ads as independent given the brand fixed effects, and with ten brands cluster-robust errors would be unreliable, so the intervals are probably still too narrow. Seven tags times four specifications were looked at; none was significant. No test of whether ad quality, not these tags, drives views was done, and ads of 2021 on, the 2021 to 2026 games' ad tags, and video or text analysis were not examined.
- Wikipedia tables can change; the revisions used are pinned in `download.R`, and the YouTube file and the ad file are pinned to commits.
