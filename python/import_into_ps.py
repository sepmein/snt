import psycopg
import pandas as pd

rainfall_adm0 = pd.read_csv(r'~/x/snt/inst/extdata/rainfall_adm0.csv')

tuples = [tuple(x) for x in rainfall_adm0.to_numpy()]

# Connect to an existing database
with psycopg.connect("dbname=malaria user=sepmein") as conn:

    # Open a cursor to perform database operations
    with conn.cursor() as cur:

        # Pass data to fill a query placeholders and let Psycopg perform
        # the correct conversion (no SQL injections!)
        cur.executemany(
                """insert into malaria
                    (adm0_id, ts_year_id, indicator_id, value)
                    WITH
                      g AS (
                        SELECT gid
                        FROM adm0
                        WHERE adm0_name=%s
                      ),
                      t AS (
                        SELECT id
                        FROM ts_month
                        WHERE year=%s month=%s
                      ),
                      ind as (
  	                    select id
	                      from indicator
	                      where name=%s
                      )
                      select g.gid, t.id, ind.id, %s
                      from g, t, ind;""",
            tuples)

        # Make the changes to the database persistent
        conn.commit()

