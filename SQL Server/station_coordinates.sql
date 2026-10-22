-- Add receiver coordinates to dbo.station (for the Tag Lookup map).
-- Sources: NWIS site service (waterservices.usgs.gov/nwis/site) where the
-- site is registered; otherwise decoded from the USGS lat-long site number
-- (DDMMSS DDDMMSS NN). Big Timber's number does not decode cleanly; verify
-- its coordinates before relying on them. Credit Island is not on the USGS
-- Fish Tracks index and must be filled in by hand.
ALTER TABLE dbo.station ADD latitude float NULL, longitude float NULL;
GO
UPDATE dbo.station SET latitude = 41.69116667, longitude = -87.9637778 WHERE station_id = '05536890';  -- Above Dispersal Barrier (NWIS site service)
UPDATE dbo.station SET latitude = 41.64, longitude = -88.0599722 WHERE station_id = '05536995';  -- Below Dispersal Barrier (NWIS site service)
UPDATE dbo.station SET latitude = 41.505, longitude = -88.0997222 WHERE station_id = '05538010';  -- Above Brandon Rd. L&D (NWIS site service)
UPDATE dbo.station SET latitude = 41.5, longitude = -88.1069444 WHERE station_id = '05538020';  -- Below Brandon Rd. L&D (NWIS site service)
UPDATE dbo.station SET latitude = 41.39861111, longitude = -88.27055556 WHERE station_id = '05541498';  -- Above Dresden Isl. L&D (NWIS site service)
UPDATE dbo.station SET latitude = 41.3948333, longitude = -88.2693611 WHERE station_id = '412341088161001';  -- Dresden Generation Stn. (NWIS site service)
UPDATE dbo.station SET latitude = 41.33305556, longitude = -88.4669444 WHERE station_id = '411955088280601';  -- West Pit (NWIS site service)
-- Big Timber Backwater: site number 412652509111316 is not in NWIS and does not
-- decode cleanly (a plain decode gives ~41.448, -91.188, which is above L&D 16,
-- i.e. Pool 16, not Pool 17). Left NULL until verified:
-- UPDATE dbo.station SET latitude = ?, longitude = ? WHERE station_id = '412652509111316';
UPDATE dbo.station SET latitude = 41.22583, longitude = -91.08611 WHERE station_id = '411333091051000';  -- Cleveland Slough (P17) (decoded from site number (41 13 33 N, 091 05 10 W))
UPDATE dbo.station SET latitude = 41.1925, longitude = -91.0094444 WHERE station_id = '411133091003401';  -- Eliza Creek at Boston Bay (NWIS site service)
UPDATE dbo.station SET latitude = 41.3525, longitude = -83.1105556 WHERE station_id = '412109083063800';  -- Brady's Isl. (NWIS site service)
UPDATE dbo.station SET latitude = 41.43833, longitude = -83.03806 WHERE station_id = '412618083021701';  -- Ottawa Shooting Club (decoded from site number (41 26 18 N, 083 02 17 W))
-- UPDATE dbo.station SET latitude = ?, longitude = ? WHERE station_label = 'Credit Isl.';
GO
SELECT station_id, station_label, latitude, longitude FROM dbo.station ORDER BY plot_order;
