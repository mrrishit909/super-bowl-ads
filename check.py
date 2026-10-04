"""Step 5: recompute every number in results/ with different code and compare.

    python3 check.py     (plain Python: csv, json, math, datetime, html.parser; no pandas/numpy/scipy)

R parsed the Wikipedia tables with rvest and fitted with lm / sandwich / fisher.test. Here the HTML is parsed with html.parser (rowspans and colspans expanded by hand),
the regressions are solved with a hand-written Gauss-Jordan inverse, the Newey-West and HC3 standard errors are written out from their formulas,
and Fisher's exact test is the hypergeometric sum. Counts must match exactly, everything else to a relative 1e-8.
"""
import csv
import datetime
import json
import math
import re
from html.parser import HTMLParser
from pathlib import Path

ROOT = Path(__file__).parent
D, R = ROOT / "data", ROOT / "results"
TAGS = ["funny", "show_product_quickly", "patriotic", "celebrity", "danger", "animals", "use_sex"]
Z = 1.959963984540054          # qnorm(0.975)
SCRAPE = datetime.date(2021, 3, 2)
REF = 2025
N = 0


def agree(name, got, want, tol=1e-8):
    global N
    if want in ("", None):
        assert got is None or (isinstance(got, float) and math.isnan(got)), f"{name}: expected NA, got {got}"
    else:
        want = float(want)
        assert got is not None, f"{name}: got NA, expected {want}"
        ok = got == want if tol == 0 else abs(got - want) <= tol * max(abs(want), abs(got)) + 1e-300
        assert ok, f"{name}: recomputed {got!r} but results has {want!r}"
    N += 1


def rd(name):
    with open(R / f"{name}.csv", newline="") as f:
        return list(csv.DictReader(f))


# ------------------------------------------------------------------ small linear algebra
def inv(A):
    n = len(A); M = [row[:] + [1.0 if i == j else 0.0 for j in range(n)] for i, row in enumerate(A)]
    for c in range(n):
        p = max(range(c, n), key=lambda r: abs(M[r][c])); M[c], M[p] = M[p], M[c]
        piv = M[c][c]; assert abs(piv) > 1e-12, "singular"
        M[c] = [v / piv for v in M[c]]
        for r in range(n):
            if r != c and M[r][c] != 0.0:
                f = M[r][c]; M[r] = [a - f * b for a, b in zip(M[r], M[c])]
    return [row[n:] for row in M]


def ols(X, y):
    n, k = len(X), len(X[0])
    A = inv([[sum(X[i][a] * X[i][b] for i in range(n)) for b in range(k)] for a in range(k)])
    xy = [sum(X[i][a] * y[i] for i in range(n)) for a in range(k)]
    beta = [sum(A[a][b] * xy[b] for b in range(k)) for a in range(k)]
    u = [y[i] - sum(X[i][a] * beta[a] for a in range(k)) for i in range(n)]
    return beta, u, A


def trend(x, y, lag=3):
    """slope, Newey-West se (Bartlett, `lag` lags, no small-sample factor), R^2 of y on [1, x]."""
    n = len(x); X = [[1.0, v] for v in x]
    beta, u, _ = ols(X, y)
    my = sum(y) / n; r2 = 1 - sum(e * e for e in u) / sum((v - my) ** 2 for v in y)
    S = [[0.0, 0.0], [0.0, 0.0]]
    for l in range(lag + 1):
        w = 1.0 if l == 0 else 1 - l / (lag + 1)
        for t in range(l, n):
            c = u[t] * u[t - l]
            for a in range(2):
                for b in range(2):
                    v = w * c * X[t][a] * X[t - l][b]
                    S[a][b] += v
                    if l:
                        S[b][a] += v
    XtXi = inv([[sum(X[i][a] * X[i][b] for i in range(n)) for b in range(2)] for a in range(2)])
    V = [[sum(XtXi[a][c] * S[c][d] * XtXi[d][b] for c in range(2) for d in range(2)) for b in range(2)] for a in range(2)]
    return beta[1], math.sqrt(V[1][1]), r2


def hc3_tag_effects(X, y, ntag=7):
    n, k = len(X), len(X[0])
    beta, u, A = ols(X, y)
    h = [sum(X[i][a] * A[a][b] * X[i][b] for a in range(k) for b in range(k)) for i in range(n)]
    out = []
    for j in range(ntag):
        v = sum((sum(A[j][b] * X[i][b] for b in range(k)) * u[i] / (1 - h[i])) ** 2 for i in range(n))
        out.append((beta[j], math.sqrt(v)))
    return out


def median(v):
    s = sorted(v); n = len(s)
    return s[n // 2] if n % 2 else (s[n // 2 - 1] + s[n // 2]) / 2


def fisher_p(a, b, c, d):
    r1, r2, c1 = a + b, c + d, a + c; tot = r1 + r2
    prob = lambda x: math.comb(r1, x) * math.comb(r2, c1 - x) / math.comb(tot, c1)
    lo, hi = max(0, c1 - r2), min(r1, c1)
    p0 = prob(a)
    return sum(p for p in (prob(x) for x in range(lo, hi + 1)) if p <= p0 * (1 + 1e-7))


def ranks(v):
    order = sorted(range(len(v)), key=lambda i: v[i]); r = [0.0] * len(v); i = 0
    while i < len(v):
        j = i
        while j + 1 < len(v) and v[order[j + 1]] == v[order[i]]:
            j += 1
        for t in range(i, j + 1):
            r[order[t]] = (i + j) / 2 + 1
        i = j + 1
    return r


def pearson(a, b):
    ma, mb = sum(a) / len(a), sum(b) / len(b)
    return sum((x - ma) * (y - mb) for x, y in zip(a, b)) / math.sqrt(sum((x - ma) ** 2 for x in a) * sum((y - mb) ** 2 for y in b))


# ------------------------------------------------------------------ HTML tables
class Tables(HTMLParser):
    def __init__(self):
        super().__init__(); self.tables = []; self.cur = None; self.row = None; self.cell = None; self.depth = 0; self.skip = 0

    def handle_starttag(self, tag, attrs):
        a = dict(attrs)
        if tag in ("style", "script"):
            self.skip += 1
        if tag == "table":
            self.depth += 1
            if self.depth == 1:
                self.cur = {"cls": a.get("class", ""), "rows": []}
        elif self.cur is not None and self.depth == 1:
            if tag == "tr":
                self.row = []
            elif tag in ("td", "th") and self.row is not None:
                self.cell = {"t": [], "rs": int(a.get("rowspan", 1)), "cs": int(a.get("colspan", 1))}
            elif tag == "br" and self.cell is not None:
                self.cell["t"].append("\n")

    def handle_endtag(self, tag):
        if tag in ("style", "script"):
            self.skip -= 1
        if tag == "table":
            if self.depth == 1 and self.cur is not None:
                self.tables.append(self.cur); self.cur = None
            self.depth -= 1
        elif tag in ("td", "th") and self.cell is not None and self.row is not None:
            self.row.append(self.cell); self.cell = None
        elif tag == "tr" and self.row is not None and self.cur is not None:
            self.cur["rows"].append(self.row); self.row = None

    def handle_data(self, data):
        if self.cell is not None and not self.skip:
            self.cell["t"].append(data)


def grid(table):
    """expand rowspan/colspan into a rectangular list of lists of cell texts (lines joined by \\n)."""
    g = {}; out = []
    for r, row in enumerate(table["rows"]):
        c = 0
        for cell in row:
            while (r, c) in g:
                c += 1
            txt = "".join(cell["t"]).replace("\xa0", " ")
            txt = "\n".join(" ".join(line.split()) for line in txt.split("\n")).strip()
            for dr in range(cell["rs"]):
                for dc in range(cell["cs"]):
                    g[(r + dr, c + dc)] = txt
            c += cell["cs"]
    nr = len(table["rows"]); nc = max(c for (_, c) in g) + 1
    return [[g.get((r, c), "") for c in range(nc)] for r in range(nr)]


def wikitables(name):
    p = Tables(); p.feed(json.load(open(D / name))["parse"]["text"])
    return [grid(t) for t in p.tables if "wikitable" in t["cls"]]


def strip_notes(s):
    return re.sub(r"\[[^\]]*\]", "", s).strip()


def num(s):
    s = strip_notes(s).replace(",", "").replace("$", "")
    try:
        return float(s)
    except ValueError:
        return None


def roman(s):
    s = re.sub(r"\s+", "", strip_notes(s)).replace("(show)", "")
    if s.isdigit():
        return int(s)
    val = {"I": 1, "V": 5, "X": 10, "L": 50, "C": 100}; t = 0
    for i, ch in enumerate(s):
        t += -val[ch] if i + 1 < len(s) and val[ch] < val[s[i + 1]] else val[ch]
    return t


MONTHS = {m: i + 1 for i, m in enumerate(["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"])}


def main():
    # ---------------- A. games
    rows = wikitables("wiki_ratings.json")[0][2:]
    wiki_rows = len(rows)
    labels = []
    for r in rows:
        if r[0] not in labels:
            labels.append(r[0])
    cpi_m = {}
    for rec in csv.DictReader(open(D / "cpiaucns.csv")):
        if rec["CPIAUCNS"] != "":
            cpi_m.setdefault(int(rec["observation_date"][:4]), []).append(float(rec["CPIAUCNS"]))
    cpi = {y: sum(v) / len(v) for y, v in cpi_m.items() if len(v) >= 11}
    cpi_ref = cpi[REF]
    games = []
    for r in rows:
        if r[2] == "Nickelodeon":
            continue
        year = int(re.search(r"(\d{4})$", r[1]).group(1)); game = labels.index(r[0]) + 1
        assert game == year - 1966
        amounts = [num(v) for v in re.findall(r"\$[0-9,]+", strip_notes(r[9]))]
        lo, hi = (min(amounts), max(amounts)) if amounts else (None, None)
        c = cpi.get(year)
        g = dict(game=game, sb="L" if r[0] == "50" else r[0], year=year, network=r[2], viewers_m=num(r[3]), halftime_m=num(r[4]), hh_rating=num(r[5]), cost_low=lo, cost_high=hi, cpi=c,
                 real_low=lo * cpi_ref / c if lo is not None and c else None, real_high=hi * cpi_ref / c if hi is not None and c else None, wiki_real=num(r[10]))
        single = lo is not None and lo == hi and g["viewers_m"] is not None
        g["cpm_nominal"] = lo / (g["viewers_m"] * 1000) if single else None
        g["cpm_real"] = g["real_low"] / (g["viewers_m"] * 1000) if single else None
        g["in_trend"] = int(game >= 2 and year <= 2024 and single)
        games.append(g)
    res = rd("games"); assert len(res) == len(games), (len(res), len(games))
    for g, rr in zip(games, res):
        assert g["sb"] == rr["sb"] and g["network"] == rr["network"], (g["sb"], rr["sb"])
        for k in ["game", "year", "viewers_m", "halftime_m", "hh_rating", "cost_low", "cost_high", "cpi", "real_low", "real_high", "cpm_nominal", "cpm_real", "wiki_real", "in_trend"]:
            agree(f"games {g['sb']} {g['network']} {k}", g[k], rr[k], 0 if k in ("game", "year", "in_trend") else 1e-8)
    tr = [g for g in games if g["in_trend"]]; assert len(tr) == 57
    at = lambda yr, k: [g[k] for g in tr if g["year"] == yr][0]

    series = {"viewers": [g["viewers_m"] for g in tr], "price_nominal": [g["cost_low"] for g in tr], "price_real": [g["real_low"] for g in tr], "cpm_real": [g["cpm_real"] for g in tr]}
    years = [g["year"] for g in tr]
    for rr in rd("trends"):
        a, b = int(rr["from"]), int(rr["to"]); idx = [i for i, y in enumerate(years) if a <= y <= b]
        slope, se, r2 = trend([float(years[i]) for i in idx], [math.log(series[rr["series"]][i]) for i in idx])
        t = f"trend {rr['series']} {a}-{b}"
        agree(t + " n", len(idx), rr["n"], 0)
        agree(t + " growth", 100 * (math.exp(slope) - 1), rr["growth_pct_per_year"]); agree(t + " lo", 100 * (math.exp(slope - Z * se) - 1), rr["lo"]); agree(t + " hi", 100 * (math.exp(slope + Z * se) - 1), rr["hi"])
        agree(t + " doubling", math.log(2) / slope, rr["doubling_years"]); agree(t + " r2", r2, rr["r2"])
    for rr in rd("rating_trend"):
        a, b = int(rr["from"]), int(rr["to"]); idx = [i for i, y in enumerate(years) if a <= y <= b and tr[i]["hh_rating"] is not None]
        slope, se, r2 = trend([float(years[i]) for i in idx], [tr[i]["hh_rating"] for i in idx], )
        t = f"rating trend {a}-{b}"
        agree(t + " n", len(idx), rr["n"], 0); agree(t + " pts", 10 * slope, rr["pts_per_decade"]); agree(t + " lo", 10 * (slope - Z * se), rr["lo"]); agree(t + " hi", 10 * (slope + Z * se), rr["hi"]); agree(t + " r2", r2, rr["r2"])

    # ---------------- B. halftime
    heads = {}
    for tb in wikitables("wiki_halftime.json")[:7]:
        k = next(i for i, h in enumerate(tb[0]) if re.search("Perform|Headliner", h))
        for r in tb[1:]:
            heads[roman(r[0])] = r[k].replace("\n", "; ")
    norm = lambda s: re.sub(r"[^a-z0-9]", "", s.lower())
    hv = sorted([g for g in games if g["halftime_m"] is not None and g["viewers_m"] is not None], key=lambda g: g["game"])
    res = rd("halftime"); assert len(res) == len(hv)
    for g, rr in zip(hv, res):
        agree(f"halftime {g['year']} game", g["game"], rr["game"], 0); agree(f"halftime {g['year']} year", g["year"], rr["year"], 0)
        assert norm(heads[g["game"]]) == norm(rr["headliners"]), (heads[g["game"]], rr["headliners"]); agree(f"halftime {g['year']} headliners", 1.0, 1.0, 0)
        agree(f"halftime {g['year']} half", g["halftime_m"], rr["halftime_m"]); agree(f"halftime {g['year']} game_m", g["viewers_m"], rr["viewers_m"])
        agree(f"halftime {g['year']} ratio", g["halftime_m"] / g["viewers_m"], rr["ratio"])

    # ---------------- C. ads
    fb = list(csv.DictReader(open(D / "superbowl-ads.csv", newline="")))
    yt = list(csv.DictReader(open(D / "youtube.csv", newline="")))
    key = lambda r: (r["year"], r["brand"], r["superbowl_ads_dot_com_url"])
    seen, ads = set(), []
    for r in yt:
        if key(r) not in seen:
            seen.add(key(r)); ads.append(r)
    fbk = {key(r): r for r in fb}; assert len(fbk) == len(fb) == len(ads)
    differ = 0
    for a in ads:
        f = fbk[key(a)]
        ua = None if a["youtube_url"] in ("", "NA") else a["youtube_url"]; uf = f["youtube_url"] or None
        differ += sum((a[t] == "TRUE") != (f[t] == "True") for t in TAGS) + (ua != uf)
    for a in ads:
        a["year"] = int(a["year"]); a["brand"] = "Hyundai" if a["brand"] == "Hynudai" else a["brand"]
        for t in TAGS:
            a[t] = 1 if a[t] == "TRUE" else 0
        num_ = lambda s: None if s in ("", "NA") else float(s)
        a["views"] = num_(a["view_count"]); a["likes"] = num_(a["like_count"]); a["dislikes"] = num_(a["dislike_count"])
        a["pub"] = None if a["published_at"] in ("", "NA") else datetime.date.fromisoformat(a["published_at"][:10])
        a["age"] = (SCRAPE - a["pub"]).days if a["pub"] else None
        a["votes"] = a["likes"] + a["dislikes"] if a["likes"] is not None and a["dislikes"] is not None else None
        a["share"] = a["likes"] / a["votes"] if a["votes"] else None
    V = [a for a in ads if a["views"] is not None]
    hv100 = [a for a in V if a["votes"] is not None and a["votes"] >= 100]
    ok_url = lambda a: a["youtube_url"] not in ("", "NA")
    A = {
        "wikipedia_ratings_rows": wiki_rows, "wikipedia_rows_dropped_nickelodeon_feed": sum(r[2] == "Nickelodeon" for r in rows), "super_bowl_I_rows_two_networks": sum(r[0] == "I" for r in rows),
        "games_in_trend": len(tr), "games_with_price_range_only": sum(g["cost_low"] is not None and g["cost_low"] != g["cost_high"] for g in games), "games_with_halftime_audience": len(hv),
        "youtube_csv_rows": len(yt), "superbowl_ads_csv_rows": len(fb), "rows_repeating_one_ad": len(yt) - len(ads), "unique_ads": len(ads), "ads_where_the_two_files_disagree": differ,
        "ads_without_youtube_link": sum(not ok_url(a) for a in ads), "ads_link_but_no_counts": sum(ok_url(a) and a["views"] is None for a in ads), "ads_with_views": len(V),
        "ads_with_likes": sum(a["likes"] is not None for a in V), "ads_with_100_votes": len(hv100), "ads_relabelled_Hynudai": sum(a["brand"] == "Hyundai" for a in ads),
        "uploaded_year_after_airing_year": sum(a["pub"].year > a["year"] for a in V), "uploaded_year_before_airing_year": sum(a["pub"].year < a["year"] for a in V),
        "median_video_age_years": median([a["age"] for a in V]) / 365.25, "youngest_video_age_days": min(a["age"] for a in V), "oldest_video_age_days": max(a["age"] for a in V),
        "brands": len({a["brand"] for a in ads}), "super_bowls_covered": len({a["year"] for a in ads}), "ads_min_year": min(a["year"] for a in ads), "ads_max_year": max(a["year"] for a in ads),
        "cpi_2025_months_available": len(cpi_m[REF]),
    }
    for r in rd("audit"):
        agree("audit " + r["key"], A[r["key"]], r["value"], 0 if r["key"] != "median_video_age_years" else 1e-8)
    assert len(rd("audit")) == len(A)

    early = [a for a in ads if a["year"] <= 2009]; late = [a for a in ads if a["year"] > 2009]
    for r in rd("ads_tags"):
        t = r["tag"]; a, c = sum(x[t] for x in early), sum(x[t] for x in late)
        agree(f"tag {t} n_yes", a + c, r["n_yes"], 0); agree(f"tag {t} pct_all", 100 * (a + c) / len(ads), r["pct_all"])
        agree(f"tag {t} n_early", a, r["n_2000_09"], 0); agree(f"tag {t} pct_early", 100 * a / len(early), r["pct_2000_09"])
        agree(f"tag {t} n_late", c, r["n_2010_20"], 0); agree(f"tag {t} pct_late", 100 * c / len(late), r["pct_2010_20"])
        agree(f"tag {t} diff", 100 * (c / len(late) - a / len(early)), r["diff_pts"])
        agree(f"tag {t} fisher", fisher_p(a, len(early) - a, c, len(late) - c), r["fisher_p"], 1e-6)

    brands = sorted({a["brand"] for a in ads})
    br = rd("brands"); assert [r["brand"] for r in br] == sorted(brands, key=lambda b: (-sum(a["brand"] == b for a in ads), b))
    for r in br:
        s = [a for a in ads if a["brand"] == r["brand"]]; w = [a for a in s if a["views"] is not None]
        t = f"brand {r['brand']}"
        agree(t + " ads", len(s), r["ads"], 0); agree(t + " pct", 100 * len(s) / len(ads), r["pct_of_ads"]); agree(t + " games", len({a["year"] for a in s}), r["super_bowls"], 0)
        agree(t + " first", min(a["year"] for a in s), r["first_year"], 0); agree(t + " last", max(a["year"] for a in s), r["last_year"], 0)
        agree(t + " max", max(sum(1 for a in s if a["year"] == y) for y in {a["year"] for a in s}), r["max_in_one_game"], 0)
        agree(t + " with_views", len(w), r["with_views"], 0); agree(t + " median_views", median([a["views"] for a in w]), r["median_views"]); agree(t + " funny", 100 * sum(a["funny"] for a in s) / len(s), r["pct_funny"])

    for r in rd("tag_views"):
        t = r["tag"]; y1 = [a["views"] for a in V if a[t]]; y0 = [a["views"] for a in V if not a[t]]
        agree(f"tagviews {t} n_yes", len(y1), r["n_yes"], 0); agree(f"tagviews {t} n_no", len(y0), r["n_no"], 0)
        agree(f"tagviews {t} med_yes", median(y1), r["median_yes"]); agree(f"tagviews {t} med_no", median(y0), r["median_no"]); agree(f"tagviews {t} ratio", median(y1) / median(y0), r["ratio"])

    # models: tags + (log age, centred) + brand dummies + year dummies
    def design(d, spec, la):
        cols = []
        for a in d:
            row = [float(a[t]) for t in TAGS] + [1.0]
            if spec >= 2:
                row.append(math.log(a["age"]) - la)
            if spec >= 3:
                row += [1.0 if a["brand"] == b else 0.0 for b in brands[1:]]
            if spec >= 4:
                row += [1.0 if a["year"] == y else 0.0 for y in sorted({x["year"] for x in d})[1:]]
            cols.append(row)
        return cols
    for oc, d, specs in [("log_views", V, [1, 2, 3, 4]), ("like_share", hv100, [1, 2, 3])]:
        la = sum(math.log(a["age"]) for a in d) / len(d)
        y = [math.log(a["views"]) for a in d] if oc == "log_views" else [a["share"] for a in d]
        for sp in specs:
            eff = hc3_tag_effects(design(d, sp, la), y)
            rows_ = [r for r in rd("models") if r["outcome"] == oc and int(r["spec"]) == sp]; assert len(rows_) == 7
            for (b, se), r in zip(eff, rows_):
                t = f"model {oc} spec{sp} {r['tag']}"
                assert r["tag"] == TAGS[rows_.index(r)]
                f = (lambda x: 100 * (math.exp(x) - 1)) if oc == "log_views" else (lambda x: 100 * x)
                agree(t + " n", len(d), r["n"], 0); agree(t + " b", b, r["b"]); agree(t + " se", se, r["se_hc3"]); agree(t + " effect", f(b), r["effect"])
                agree(t + " lo", f(b - Z * se), r["lo"]); agree(t + " hi", f(b + Z * se), r["hi"])
    assert len(rd("models")) == 7 * 7

    # summary
    rated = [g for g in tr if g["hh_rating"] is not None]
    r70 = [g for g in rated if g["year"] >= 1970]
    pk = max(tr, key=lambda g: g["viewers_m"]); gr = max(rated, key=lambda g: g["hh_rating"]); lw = min(rated, key=lambda g: g["hh_rating"]); l70 = min(r70, key=lambda g: g["hh_rating"])
    byyear = {g["year"]: g for g in games}
    ratio_h = lambda g: g["halftime_m"] / g["viewers_m"]
    hpk = max(hv, key=lambda g: g["halftime_m"]); hrk = max(hv, key=ratio_h)
    sorted_views = sorted((a["views"] for a in V), reverse=True)
    S = {
        "viewers_1968": at(1968, "viewers_m"), "viewers_2024": at(2024, "viewers_m"), "viewers_ratio": at(2024, "viewers_m") / at(1968, "viewers_m"),
        "price_nominal_1968": at(1968, "cost_low"), "price_nominal_2024": at(2024, "cost_low"), "price_nominal_ratio": at(2024, "cost_low") / at(1968, "cost_low"),
        "price_real_1968": at(1968, "real_low"), "price_real_2024": at(2024, "real_low"), "price_real_ratio": at(2024, "real_low") / at(1968, "real_low"),
        "cpm_real_1968": at(1968, "cpm_real"), "cpm_real_2024": at(2024, "cpm_real"), "cpm_real_ratio": at(2024, "cpm_real") / at(1968, "cpm_real"),
        "cpm_nominal_1968": at(1968, "cpm_nominal"), "cpm_nominal_2024": at(2024, "cpm_nominal"),
        "viewers_2025": byyear[2025]["viewers_m"], "viewers_2026": byyear[2026]["viewers_m"],
        "price_2025_low": byyear[2025]["cost_low"], "price_2025_high": byyear[2025]["cost_high"], "price_2026_low": byyear[2026]["cost_low"], "price_2026_high": byyear[2026]["cost_high"],
        "viewers_2011": at(2011, "viewers_m"), "viewers_2011_2024_ratio": at(2024, "viewers_m") / at(2011, "viewers_m"), "price_real_2011": at(2011, "real_low"), "price_real_2011_2024_ratio": at(2024, "real_low") / at(2011, "real_low"),
        "peak_viewers_year": pk["year"], "peak_viewers": pk["viewers_m"], "games_in_trend": len(tr),
        "peak_rating": gr["hh_rating"], "peak_rating_year": gr["year"], "low_rating": lw["hh_rating"], "low_rating_year": lw["year"],
        "low_rating_since_1970": l70["hh_rating"], "low_rating_since_1970_year": l70["year"], "rating_1968": at(1968, "hh_rating"), "rating_2024": at(2024, "hh_rating"),
        "max_abs_diff_vs_wikipedia_real_pct": max(abs(g["real_low"] / g["wiki_real"] - 1) for g in tr) * 100,
        "halftime_games": len(hv), "halftime_above_game": sum(g["halftime_m"] > g["viewers_m"] for g in hv),
        "halftime_peak_m": hpk["halftime_m"], "halftime_peak_year": hpk["year"], "halftime_peak_ratio": ratio_h(hrk), "halftime_peak_ratio_year": hrk["year"],
        "halftime_median_ratio_2011_2026": median([ratio_h(g) for g in hv if g["year"] >= 2011]),
        "halftime_2025_m": byyear[2025]["halftime_m"], "halftime_1993_m": byyear[1993]["halftime_m"], "game_1993_m": byyear[1993]["viewers_m"],
        "ads_total": len(ads), "ads_2000_09": len(early), "ads_2010_20": len(late),
        "tags_per_ad_mean": sum(sum(a[t] for t in TAGS) for a in ads) / len(ads), "ads_no_tag_except_product": sum(sum(a[t] for t in TAGS if t != "show_product_quickly") == 0 for a in ads),
        "bud_ads": sum(a["brand"] in ("Bud Light", "Budweiser") for a in ads), "bud_pct": 100 * sum(a["brand"] in ("Bud Light", "Budweiser") for a in ads) / len(ads),
        "max_ads_one_brand_one_game": max(sum(1 for a in ads if a["brand"] == b and a["year"] == y) for b in brands for y in range(2000, 2021)),
        "spearman_views_upload_age": pearson(ranks([math.log(a["views"]) for a in V]), ranks([math.log(a["age"]) for a in V])),
        "spearman_views_airing_year": pearson(ranks([a["views"] for a in V]), ranks([float(a["year"]) for a in V])),
        "median_views_all": median([a["views"] for a in V]), "views_top10_share_pct": 100 * sum(sorted_views[:10]) / sum(sorted_views),
        "like_share_median_pct": 100 * median([a["share"] for a in hv100]),
    }
    res = rd("summary"); assert [r["key"] for r in res] == list(S), set(S) ^ {r["key"] for r in res}
    for r in res:
        agree("summary " + r["key"], S[r["key"]], r["value"], 1e-8)
    print(f"check.py: {N} recomputed numbers agree with results/")


main()
