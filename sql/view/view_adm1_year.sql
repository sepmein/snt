SELECT adm1.adm0_name AS adm0,
    adm1.adm1_name AS adm1,
    ts_year.year,
    indicator.code AS index,
    malaria.value
FROM malaria
    JOIN adm1 ON malaria.adm1_id = adm1.gid
    JOIN ts_year ON malaria.ts_year_id = ts_year.id
    JOIN indicator ON malaria.indicator_id = indicator.id;