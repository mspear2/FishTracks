# Passage events across locks, dams and the dispersal barrier

*Feature idea for FishTracks Real Time. Drafted 2026-10-22. Nothing implemented yet.*

## Goal

Flag when a tagged fish crosses a structure (the Electric Dispersal Barrier, Brandon Rd. L&D or Dresden Isl. L&D), using receivers placed on either side. Upstream passages of invasive carp at the barrier and at Brandon Road matter most.

## Approach

Treat a crossing as a derived event: detections → **visits** (one stay at one station) → **transitions** between visits → classify each transition against a reference table of structures.

## 1. Reference table: structures and the stations on each side

`dbo.station` has `plot_order` but no notion of which stations flank a structure or which way is upstream. Add a small table:

| structure | type | station_down | station_up |
|---|---|---|---|
| Electric Dispersal Barrier | barrier | 05536995 (Below) | 05536890 (Above) |
| Brandon Rd. L&D | lock & dam | 05538020 (Below) | 05538010 (Above) |
| Dresden Isl. L&D | lock & dam | ? | 05541498 (Above) |

- Add `river_mile` to `dbo.station`, so direction and skipped receivers follow from arithmetic rather than hand-coded pairs.
- **Open question:** is Dresden Generation Stn. (`412341088161001`) below Dresden Isl. L&D? It is about 400 m from "Above Dresden".

## 2. Visits: collapse detections into stays at one station

Per fish (`coalesce(animal_id, TagID)`), sorted by time. A new visit starts when the station changes or after a long gap at the same station (starting value: more than 2 h).

```r
visits <- det %>%
  arrange(fish_id, TimeStamp) %>%
  group_by(fish_id) %>%
  mutate(
    new_visit = station_id != lag(station_id, default = '') |
      difftime(TimeStamp, lag(TimeStamp), units = 'hours') > 2,
    visit_id = cumsum(coalesce(new_visit, TRUE))
  ) %>%
  group_by(fish_id, common_name_e, visit_id, station_id) %>%
  summarise(start = min(TimeStamp), end = max(TimeStamp), n_det = n(), .groups = 'drop')
```

## 3. Transitions: classify each move between visits

Use `lead()` on the visits to pair each visit with the fish's next visit at a different station, then join to the structure table in both directions. Each candidate crossing records:

- **direction:** upstream or downstream
- **timing:** last detection on the old side, first detection on the new side, and the transit time between them

## 4. Confidence tiers

The receiver pairs are very close (about 0.8 km at Brandon Road, about 0.4 km at Dresden), so tailwater noise or overlapping detection ranges can fake a crossing.

| tier | rule | counted as a passage? |
|---|---|---|
| Confirmed | at least *n* detections on the new side over at least *m* minutes, and no return to the old side within *t* hours | yes |
| Tentative | detected on the new side, but too briefly to meet the confirmed bar | report separately |
| Range overlap | rapid A–B–A–B switching within minutes | no (kept for QA) |
| Inferred | the fish skipped a receiver (e.g. Below Barrier → Above Brandon), so it passed a structure without being detected next to it; needs `river_mile` | yes, flagged as inferred |

Set the thresholds (*n*, *m*, *t* and the visit gap) from real sequences. A good first QA task is to pull every transition at Brandon Road and look at the A/B patterns.

**Timestamp caveat:** `TagIDTimeStamp_n` still carries the time-zone offset bug (see `follow up items/timestamp_issue_report.md`). Until that is fixed and backfilled, build on the record-level `TimeStamp`. That limits timing to the logger's reporting interval: fine for flagging passages, too coarse for precise transit times.

## 5. Where to compute it

Querying `event_animal` over full history on every app load is too slow; its 30-slot unpivot is already the slowest path. Instead, have the append job (`append_fishtracks.qmd`) incrementally update two persisted tables, `dbo.visit` and `dbo.passage`. This lines up with the "persisted detection table" fix already in the session notes, and it makes passages cheap for the app to read.

## 6. Ways to surface passages

**In the app**

- **Plot markers:** passages as markers (e.g. `geom_vline` or labelled points) in the facets of the two stations involved.
- **Metric card:** "Passages in window", split upstream/downstream, with barrier and Brandon Road upstream counts called out.
- **Events table:** fish, species, structure, direction, transit time and confidence tier, linked to the Tag lookup page.

**Alerts**

- A Connect-scheduled email whenever a confirmed upstream invasive-carp passage appears at the barrier or Brandon Road.

**Analysis**

- **Denominator:** count passages per fish that *approached* the structure (detected on the downstream side), not per fish tagged.
- **Time to passage:** model the time from arrival below the structure to passage with `survival::coxph`. Covariates could include discharge, temperature and lockage counts.
- **Movement between pools:** a multistate model treating each pool as a state (e.g. with `msm`) describes movement across the whole array.
- **Lock vs. spillway:** these receivers can't tell the route. Joining USACE lockage records could make it a probabilistic covariate: a crossing whose window overlaps a lockage is more likely to have gone through the lock.

## Suggested order

1. Reference table and `river_mile` on `dbo.station`, after confirming which side of the dam Dresden Generation Stn. is on.
2. Visit and transition logic as a standalone R script.
3. QA against real sequences at Brandon Road; set the thresholds.
4. Persist `dbo.visit` / `dbo.passage` from the append job.
5. Wire into the app: markers, metric card, events table, then alerts.
6. Analysis: time-to-passage and multistate models.
