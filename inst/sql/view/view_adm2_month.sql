SELECT adm2.adm0_name AS adm0,
    adm2.adm1_name AS adm1,
    adm2.adm2_name AS adm2,
    ts_month.year,
    ts_month.month,
    indicator.code AS index,
    malaria.value
FROM malaria
    JOIN adm2 ON malaria.adm2_id = adm2.gid
    JOIN ts_month ON malaria.ts_month_id = ts_month.id
    JOIN indicator ON malaria.indicator_id = indicator.id;