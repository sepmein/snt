insert into malaria (adm1_id, ts_month_id, indicator_id, value) WITH g AS (
        SELECT gid
        FROM adm1
        WHERE guid = %s
    ),
    t AS (
        SELECT id
        FROM ts_month
        WHERE year = %s
            AND month = %s
    ),
    ind as (
        select id
        from indicator
        where name = %s
    )
select g.gid,
    t.id,
    ind.id,
    %s
from g,
    t,
    ind;