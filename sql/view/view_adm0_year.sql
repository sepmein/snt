SELECT adm0.adm0_name AS adm0,
    ts_year.year,
    indicator.code AS index,
    malaria.value
FROM malaria
    JOIN adm0 ON malaria.adm0_id = adm0.gid
    JOIN ts_year ON malaria.ts_year_id = ts_year.id
    JOIN indicator ON malaria.indicator_id = indicator.id;