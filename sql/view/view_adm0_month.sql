SELECT adm0.adm0_name AS adm0,
    ts_month.year,
    ts_month.month,
    indicator.code AS index,
    malaria.value
FROM malaria
    JOIN adm0 ON malaria.adm0_id = adm0.gid
    JOIN ts_month ON malaria.ts_month_id = ts_month.id
    JOIN indicator ON malaria.indicator_id = indicator.id;