# geomarker 0.0.1.9000

# geomarker 0.0.1

* `get_traffic_summary()` now uses the final 2024 Highway Performance
  Monitoring System release and reads regional roadway candidates through the
  GeoPackage spatial index before calculating S2 intersections.
* Traffic data generation, validation, provenance, and release ownership now
  live in geomarker rather than appc.
* HPMS 2024 has substantial known data gaps for North Dakota and New Jersey;
  see the function documentation and the FHWA HPMS 2024 documentation before
  interpreting estimates in those states.
* The source also contains 55 retained roadway sections where truck and bus
  components exceed total AADT. The existing documented passenger subtraction
  is preserved and this count is recorded in the release manifest.
