# FishTracks: detection timestamp offsets and the 05538010 receiver event

**Prepared:** 2026-09-28
**Author:** Mike Spear (with Claude), Illinois River Biological Station
**Status:** Shelved for the app (no user-facing impact); open for data integrity
**Scope:** `dbo.event` in the `Fish_Tracks_Real_Time` SQL Server database, the ingest step in `append_fishtracks.qmd` / `code/helper_functions.R`, and the VR2C receiver at USGS station 05538010

---

## 1. Summary

Every row in `dbo.event` carries two families of timestamps that come from two different clocks. `TimeStamp` is written by the Campbell datalogger when it polls the receiver; the `TagIDTimeStamp_1`–`_30` columns are written by the VR2C receiver when it hears a tag. In the source CSVs the logger reports local **standard** time (no daylight saving) and the receiver reports **UTC**. The ingest function applies the station's standard-time zone to *both* families, which is correct for the logger column and wrong for the receiver columns. The result is that stored `TimeStamp` values are true UTC, while stored `TagIDTimeStamp` values are true UTC **plus** the station's offset: +6 h at the Illinois stations and +5 h at the Ohio stations.

Separately, on the afternoon of 2026-09-23 the datalogger at station 05538010 restarted, about four hours of records were never delivered, and from that point on the receiver's clock reads one hour behind UTC. The logger's clock was unaffected.

Neither issue affects what the Shiny app currently shows. The app bins, filters, and plots on `TimeStamp`, and the `event_animal` view now attributes detections to fish using `TimeStamp` as well. The receiver-clock column is presently unused by anything in the pipeline. The issues matter for anyone who later queries `TagIDTimeStamp` directly, for the long-term integrity of the table, and for the array's field maintenance.

## 2. Evidence

### 2.1 Stored `TimeStamp` is UTC

```
MAX(TimeStamp)        2026-09-28 17:10:00
MAX(DateRetrieved)    2026-09-28 18:18:32
SYSUTCDATETIME()      2026-09-28 18:23:43
SYSDATETIME()         2026-09-28 13:23:43   (server local, CDT)
```

The latest record is 68–83 minutes behind the retrieval time at every station, consistent with the usual USGS real-time delay. If `TimeStamp` were stored in any Central zone it would read *later* than the local clock, which is impossible. So `TimeStamp` is UTC and the app's lookback window and x-axis are correct.

### 2.2 Stored `TagIDTimeStamp` is offset by the station's standard-time zone

Seven-day mean of `DATEDIFF(second, TagIDTimeStamp, TimeStamp)` per station (negative means the receiver stamp is later than the logger stamp):

| Station | Min (s) | Mean (s) | Max (s) | Interpretation |
|---|---|---|---|---|
| 05536890 | −21376 | −21293 | −21210 | −6 h (CST) |
| 05536995 | −22255 | −21581 | −21476 | −6 h; ~11 min receiver drift at min |
| 05538010 | −21474 | −17828 | −17685 | mixed −6 h / −5 h (see §3) |
| 05538020 | −21543 | −21358 | −21246 | −6 h |
| 05541498 | −21555 | −21359 | −21255 | −6 h |
| 411133091003401 | −21994 | −21632 | −21515 | −6 h |
| 411333091051000 | −21669 | −21448 | −21363 | −6 h |
| 411955088280601 | −21555 | −21377 | −21256 | −6 h |
| 412341088161001 | −21505 | −21373 | −21291 | −6 h |
| 412618083021701 | −17969 | −17795 | −17682 | −5 h (EST; Sandusky River, Ohio) |
| 412652509111316 | −21532 | −21156 | −21058 | −6 h |

Central Standard Time is UTC−6 and Eastern Standard Time is UTC−5. June records (deep in daylight time) show the same −6 h, so the loggers run on standard time year-round, which is standard USGS practice. Once the whole-hour offset is removed, the residual is the polling lag (0–5 min) plus a few minutes of genuine receiver clock drift at some stations.

Station 412109083063800 had no detections in the seven-day window and its offset has not been measured yet.

### 2.3 Mechanism

In `get_fishtracks()` (`code/helper_functions.R`):

```r
mutate(across(where(is.POSIXct), ~ force_tz(.x, tzone = tz)))
```

`force_tz()` reinterprets a wall-clock value as being in zone `tz`. Applied to the logger's `11:10` with `tz = Etc/GMT+6`, that yields the correct 17:10 UTC. Applied to the receiver's `17:10`, which is already UTC, it yields 23:10 UTC. Same line of code, right answer for one column family and a six-hour error for the other. The odbc write then stores both as UTC wall-clock values in `datetime2`, which carries no zone information, so nothing in the database can detect the mismatch.

## 3. The 05538010 event on 2026-09-23

Per-record offset for station 05538010, showing only rows where the whole-hour offset changed:

```
2026-06-18 18:05:00   offset −6 h   (start of record)
2026-09-23 20:10:00   offset −5 h   (previous −6 h)
```

Records around the seam, with the logger's own record counter:

```
2026-09-23 19:55  Record 45   (first record after a gap; nothing 16:00–19:55)
2026-09-23 20:00  Record 46   gap 5 min, step 1
2026-09-23 20:05  Record 47   gap 5 min, step 1
...                           regular 5-minute cadence continues
```

Three facts fall out of this.

1. **The logger was restarted.** A Campbell record counter only resets when the program is recompiled or the table is reset. Records 1–44 (roughly 16:10–19:50 UTC, 11:10–14:50 CDT) never reached the database: either the logger was offline or the USGS feed didn't carry them. About four hours of data are missing at this station.
2. **The logger clock did not change.** Its latest record lags retrieval by 78 minutes, the same as its neighbours. If the logger had been set to daylight time it would sit an hour ahead of the pack.
3. **The receiver clock was set back one hour.** From 20:10 UTC (15:10 CDT) the receiver stamp is UTC−1 h rather than UTC. The timing, a workday afternoon coinciding with a logger restart, points to a site visit at which the VR2C clock was set from a device on local time or with the wrong zone.

Practical consequence: with the ingest still adding +6 h, this station's stored `TagIDTimeStamp` values since the 23rd are UTC+5 h rather than UTC+6 h, and once the ingest is fixed they will be UTC−1 h until the receiver clock is corrected in the field.

## 4. Why it has no visible effect today, and where it would

The Shiny app bins on `TimeStamp` to match the official five-minute reporting interval, so the plot, the lookback window, the metric cards, and the tooltips are unaffected. The `event_animal` view was changed on 2026-09-28 to join `deployment_intervals` on `e.TimeStamp` instead of `v.TagIDTimeStamp`, so fish attribution no longer depends on the receiver clock either. Before that change, any detection within six hours before a tag's redeployment on a new fish would have been attributed to the new fish; the number of affected rows is probably zero, but it is worth a one-time check (§6, item 5).

Where it *would* bite: any colleague querying `TagIDTimeStamp` for residency, transit, or hydrology joins; any export to a partner; and any future decision to bin on detection time rather than record time. The error is silent, differs by an hour between Illinois and Ohio stations, and differs again at 05538010 across the September 23 seam.

## 5. Topics to raise with the USGS / receiver maintainers

- **Confirm the time references.** Ask them to confirm that the datalogger `TimeStamp` in the Fish Tracks CSVs is local standard time, held year-round, and that `TagIDTimeStamp` is UTC as reported by the VR2C. The data are unambiguous on this, but a documented answer belongs in the project README.
- **The September 23 visit at 05538010.** Was the site visited? What was done to the logger (the record counter reset around 16:10 UTC / 11:10 CDT), and was the VR2C clock touched? Which device was used to set it, and to what zone?
- **Receiver clock at 05538010 is now one hour slow.** Request that it be reset to UTC at the next visit (or remotely, if the unit allows), so it matches the rest of the array.
- **The four-hour data hole at 05538010** (approximately 11:10–14:50 CDT on the 23rd). Were those records lost at the logger, or are they retrievable from the logger's internal memory or a USGS archive?
- **Receiver clock drift generally.** Station 05536995 shows detections stamped up to 11 minutes *after* the logger record that carried them, which can only be clock error. Ask whether the loggers resync the VR2C clocks on a schedule, and how often.
- **Clock-change reporting.** Ask whether visits that touch either clock can be logged somewhere the Data Team can see (a field-visit log or a note in the station metadata), so future seams are explained rather than discovered.

## 6. Action items (Data Team)

1. **Fix the ingest so new rows land correctly.** In `get_fishtracks()`, replace the single `force_tz()` over all POSIXct columns with two targeted conversions and drop the now-unneeded `rowwise()`/`ungroup()`:

   ```r
   df <- df %>%
     mutate(station_id = station_id) %>%
     relocate(station_id) %>%
     left_join(tz_lookup, by = 'station_id') %>%
     mutate(
       TimeStamp = force_tz(TimeStamp, tzone = tz[1]),                    # logger: local standard time
       across(matches('^TagIDTimeStamp_'), ~ force_tz(.x, tzone = 'UTC')) # receiver: already UTC
     ) %>%
     select(-tz)
   ```

   Do this first: every hourly run until then adds rows that will need the backfill.

2. **Measure every station's offset from its full history** before backfilling, and look for any other seams:

   ```sql
   WITH lag AS (
     SELECT station_id,
            DATEFROMPARTS(YEAR(TimeStamp), MONTH(TimeStamp), 1) AS month,
            DATEDIFF(second, TagIDTimeStamp, TimeStamp) AS lag_s
     FROM dbo.event_animal
   )
   SELECT station_id, month,
          ROUND(AVG(lag_s) / 3600.0, 1) AS mean_offset_h,
          MIN(lag_s) AS min_s, MAX(lag_s) AS max_s, COUNT(*) AS n
   FROM lag
   GROUP BY station_id, month
   ORDER BY station_id, month;
   ```

   This also supplies the offset for 412109083063800, which the seven-day window missed.

3. **Backfill `TagIDTimeStamp_1`–`_30` in `dbo.event`.** One `UPDATE` per offset group, each subtracting the group's offset across all 30 columns (`DATEADD` on `NULL` returns `NULL`, so empty slots are safe). Illinois stations: −6 h. Ohio stations: −5 h. Station 05538010: −6 h for `TimeStamp < '2026-09-23 19:55'`, −5 h from 19:55 onward, which lands both halves on true UTC because the −5 h correction exactly absorbs the receiver being one hour slow. Wrap in a transaction, re-run the query in item 2 on the same connection, confirm every station's mean offset is within a few minutes of zero, then commit. The full 30-column statements can be generated on request.

4. **Note the residual at 05538010.** After items 1 and 3, rows from that station dated after 2026-09-23 19:55 UTC and before the receiver clock is corrected will carry `TagIDTimeStamp` one hour early. Record the interval in the project README once the correction date is known, rather than adding a per-station fudge to the ingest.

5. **One-time attribution check.** For the period before the `event_animal` view change, look for detections that fell within six hours before a tag's `deployment_start` under the old join and confirm none exist, or list them if they do:

   ```sql
   SELECT v.DetectionID, v.station_id, v.TimeStamp, v.TagID, p.animal_id, p.deployment_start
   FROM dbo.event_animal v
   JOIN dbo.deployment_intervals p ON p.TagID = v.TagID
   WHERE v.TimeStamp >= DATEADD(hour, -6, p.deployment_start)
     AND v.TimeStamp <  p.deployment_start;
   ```

6. **Document the conventions.** Add to the project README: both stored timestamp families are UTC; `TimeStamp` is the logger record time and the official reporting interval; `TagIDTimeStamp` is the receiver detection time; the app and `event_animal` use `TimeStamp`.

7. **Send the maintainer email** (drafted 2026-09-28) covering §5, once the September 23 visit has been confirmed internally.

8. **Add a monitoring query to the QA routine** (optional): a weekly per-station mean of `DATEDIFF(second, TagIDTimeStamp, TimeStamp)` that alerts when any station's mean moves by more than 30 minutes, so the next clock change is caught within a week rather than by accident.

## 7. Reference: the queries that found this

Seven-day offset by station:

```sql
SELECT station_id,
       MIN(DATEDIFF(second, TagIDTimeStamp, TimeStamp)) AS min_lag_s,
       AVG(DATEDIFF(second, TagIDTimeStamp, TimeStamp)) AS mean_lag_s,
       MAX(DATEDIFF(second, TagIDTimeStamp, TimeStamp)) AS max_lag_s
FROM dbo.event_animal
WHERE TimeStamp >= DATEADD(day, -7, SYSDATETIME())
GROUP BY station_id;
```

Change points at one station:

```sql
WITH per_record AS (
  SELECT DetectionID, TimeStamp,
         ROUND(AVG(DATEDIFF(second, TagIDTimeStamp, TimeStamp)) / 3600.0, 0) AS offset_h
  FROM dbo.event_animal
  WHERE station_id = '05538010'
  GROUP BY DetectionID, TimeStamp
),
flagged AS (
  SELECT *, LAG(offset_h) OVER (ORDER BY TimeStamp) AS prev_offset_h FROM per_record
)
SELECT TimeStamp, offset_h, prev_offset_h
FROM flagged
WHERE prev_offset_h IS NULL OR offset_h <> prev_offset_h
ORDER BY TimeStamp;
```

Logger cadence and record counter around the seam:

```sql
SELECT TimeStamp, Record,
       DATEDIFF(minute, LAG(TimeStamp) OVER (ORDER BY TimeStamp), TimeStamp) AS gap_min,
       Record - LAG(Record) OVER (ORDER BY TimeStamp) AS record_step
FROM dbo.event
WHERE station_id = '05538010'
  AND TimeStamp BETWEEN '2026-09-23 16:00' AND '2026-09-24 00:00'
ORDER BY TimeStamp;
```

Latest record versus retrieval time, per station (logger clock check):

```sql
SELECT station_id, MAX(TimeStamp) AS latest_record, MAX(DateRetrieved) AS latest_retrieval,
       DATEDIFF(minute, MAX(TimeStamp), MAX(DateRetrieved)) AS lag_min
FROM dbo.event
GROUP BY station_id
ORDER BY lag_min;
```
