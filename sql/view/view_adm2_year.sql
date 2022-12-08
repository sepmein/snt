SELECT adm2.adm0_name AS adm0,
    adm2.adm1_name AS adm1,
    adm2.adm2_name AS adm2,
    ts_year.year,
    indicator.code AS index,
    malaria.value
FROM malaria
    JOIN adm2 ON malaria.adm2_id = adm2.gid
    JOIN ts_year ON malaria.ts_year_id = ts_year.id
    JOIN indicator ON malaria.indicator_id = indicator.id;