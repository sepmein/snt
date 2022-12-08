SELECT adm1.adm0_name AS adm0,
    adm1.adm1_name AS adm1,
    ts_month.year,
    ts_month.month,
    indicator.code AS index,
    malaria.value
FROM malaria
    JOIN adm1 ON malaria.adm1_id = adm1.gid
    JOIN ts_month ON malaria.ts_month_id = ts_month.id
    JOIN indicator ON malaria.indicator_id = indicator.id;