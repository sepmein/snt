insert into malaria (adm1_id, ts_year_id, indicator_id, value) 
WITH g AS (
        SELECT gid
        FROM adm1
        WHERE iso_3_code = %s and adm1_name = %s
    ),
    t AS (
        SELECT id
        FROM ts_year
        WHERE year = %s
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