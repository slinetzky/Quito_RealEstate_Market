CREATE TABLE public."house_rent" 
(nombre varchar(100),
 sector varchar(100),
 precio varchar(100),
 area int,
 habitaciones varchar(100),
 banos varchar(100),
 parqueadero int,
 fecha date
	);
CREATE TABLE public.house_sell
(
    nombre character varying(200),
    sector character varying(100),
    precio character varying(100),
    area character varying(100),
    habitaciones character varying(100),
    banos character varying(100),
    parqueadero character varying(100),
    fecha date
);


/* 





DATA CLEANING





*/


------------------------------------------------------------------------------------------------------------------------------------
/*

Remove wrong data: Some sectors have dates in them, with null in every other value

*/

DELETE --- 130 rows deleted
FROM public."house_sell"
WHERE sector LIKE '2%';










------------------------------------------------------------------------------------------------------------------------------------
/*

Clean sector by removing ", Quito"

*/

UPDATE public."house_rent"
SET sector = (
	CASE WHEN sector = 'Quito, Pichincha' THEN 'Pichincha'
		 ELSE TRIM(SPLIT_PART(sector, ',', 1))
		 END
);

---

UPDATE public."house_sell"
SET sector = (
	CASE WHEN sector = 'Quito, Pichincha' THEN 'Pichincha'
		 ELSE TRIM(SPLIT_PART(sector, ',', 1))
		 END
);










------------------------------------------------------------------------------------------------------------------------------------
/*

Convert price to int

*/

UPDATE Public."house_rent" 
SET precio = REPLACE(precio, '.', '' );
ALTER TABLE Public."house_rent"
ALTER COLUMN precio TYPE int USING precio::integer;

---

UPDATE public."house_sell" 
SET precio = REPLACE(precio, '.', '');
ALTER TABLE public."house_sell"
ALTER COLUMN precio TYPE int USING precio::integer;











------------------------------------------------------------------------------------------------------------------------------------
/*

Fix wrong area data type (some values (2817 values) have "m2" at the end)

*/

UPDATE public."house_sell"
SET area = CASE WHEN area LIKE '%.%' THEN TRIM(SPLIT_PART(area,'.',1))
				WHEN area LIKE '%m%' THEN TRIM(REPLACE(area,'m²',''))
				ELSE area
				END;
				
DELETE --2 rows deleted
FROM public."house_sell"
WHERE area LIKE '%e%';

ALTER TABLE public."house_sell"
ALTER COLUMN area TYPE int USING area::int;











------------------------------------------------------------------------------------------------------------------------------------
/*

Change "precio" (price) column name to "arriendo" (rent) in house rent datasets

*/

ALTER TABLE public."house_rent"
RENAME COLUMN precio TO arriendo;










------------------------------------------------------------------------------------------------------------------------------------
/*

Fix wrong beedrooms (habitaciones), bathroom (banos) and parking (parqueadero) data type and entries:

*/

UPDATE public."house_rent"
SET parqueadero = CASE WHEN banos LIKE '%estac%' THEN SPLIT_PART(banos, 'e', 1)::int
					   WHEN habitaciones LIKE '%estac%' THEN SPLIT_PART(habitaciones, 'e', 1)::int
					   ELSE parqueadero
					   END;
UPDATE public."house_rent"
SET banos = CASE WHEN habitaciones LIKE '%b%' THEN SPLIT_PART(habitaciones, 'b', 1)
				 ELSE banos
				 END;
UPDATE public."house_rent"
SET habitaciones = CASE WHEN habitaciones LIKE '%b%' OR habitaciones LIKE '%estac%' THEN NULL
						ELSE habitaciones
						END;
UPDATE public."house_rent"
SET banos = CASE WHEN banos LIKE '%estac%' THEN NULL
				 ELSE banos
				 END;
ALTER TABLE public."house_rent"
ALTER COLUMN habitaciones TYPE int USING habitaciones::integer;  		 
ALTER TABLE public."house_rent"
ALTER COLUMN banos TYPE int USING banos::integer;

---

UPDATE public."house_sell"
SET parqueadero = SPLIT_PART(parqueadero,'.',1);

ALTER TABLE public."house_sell"
ALTER COLUMN parqueadero TYPE int USING parqueadero::int

UPDATE public."house_sell"
SET parqueadero = CASE WHEN banos LIKE '%estac%' THEN SPLIT_PART(banos, 'e', 1)::int
					   WHEN habitaciones LIKE '%estac%' THEN SPLIT_PART(habitaciones, 'e', 1)::int
					   ELSE parqueadero
					   END;
UPDATE public."house_sell"
SET banos = CASE WHEN habitaciones LIKE '%b%' THEN SPLIT_PART(habitaciones, 'b', 1)
				 ELSE banos
				 END;
UPDATE public."house_sell"
SET habitaciones = CASE WHEN habitaciones LIKE '%b%' OR habitaciones LIKE '%estac%' THEN NULL
						ELSE habitaciones
						END;
UPDATE public."house_sell"
SET banos = CASE WHEN banos LIKE '%estac%' THEN NULL
				 ELSE banos
				 END;
ALTER TABLE public."house_sell"
ALTER COLUMN habitaciones TYPE int USING habitaciones::integer;

DELETE --12 rows deleted
FROM public."house_sell"
WHERE TRIM(banos) ~ '[^0-9]'

ALTER TABLE public."house_sell"
ALTER COLUMN banos TYPE int USING banos::integer;









------------------------------------------------------------------------------------------------------------------------------------
/*

REMOVE DUPLICATES: same name, sector, rooms, bathrooms and parking. Keep lowest price

*/

ALTER TABLE public."house_rent"
ADD COLUMN id SERIAL PRIMARY KEY;
WITH row_num_cte AS (SELECT ROW_NUMBER() OVER(PARTITION BY nombre,
														  sector, 
														  habitaciones,
														  banos, 
														  parqueadero 
											 ORDER BY arriendo ASC
					 						) AS row_num,
												 id
FROM public."house_rent")
DELETE --- 655 deleted duplicates
FROM public."house_rent"
WHERE id IN (SELECT id
				  FROM row_num_cte
				  WHERE row_num <> 1);
ALTER TABLE public."house_rent" DROP COLUMN id;

---

ALTER TABLE public."house_sell"
ADD COLUMN id SERIAL PRIMARY KEY;
WITH row_num_cte AS (SELECT ROW_NUMBER() OVER(PARTITION BY nombre,
														  sector, 
														  habitaciones,
														  banos, 
														  parqueadero 
											 ORDER BY precio ASC
					 						) AS row_num,
												 id
FROM public."house_sell")
DELETE --- 1534 deleted duplicates
FROM public."house_sell"
WHERE id IN (SELECT id
				  FROM row_num_cte
				  WHERE row_num <> 1);
ALTER TABLE public."house_sell" DROP COLUMN id;










------------------------------------------------------------------------------------------------------------------------------------
/*

Remove NN names

*/

DELETE --25 deleted
FROM public."house_sell"
WHERE nombre LIKE 'NN'









------------------------------------------------------------------------------------------------------------------------------------
/*

Replace NULL values with median

*/

UPDATE public."house_rent" ---93 NULL en habitaciones
SET habitaciones = subqueary.median
FROM (SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY habitaciones) AS median
	  FROM public."house_rent"
	 ) AS subqueary
WHERE habitaciones IS NULL;

UPDATE public."house_rent" ---22 NULL en banos
SET banos = subqueary.median
FROM (SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY banos) AS median
	  FROM public."house_rent"
	 ) AS subqueary
WHERE banos IS NULL;

---

UPDATE public."house_sell" ---332 NULL en habitaciones
SET habitaciones = subqueary.median
FROM (SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY habitaciones) AS median
	  FROM public."house_sell"
	 ) AS subqueary
WHERE habitaciones IS NULL;

UPDATE public."house_sell" ---48 NULL en banos
SET banos = subqueary.median
FROM (SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY banos) AS median
	  FROM public."house_sell"
	 ) AS subqueary
WHERE banos IS NULL;









------------------------------------------------------------------------------------------------------------------------------------
/*

Create column piscina (pool) = TRUE if word "piscina" is in name

*/

ALTER TABLE public."house_rent"
ADD COLUMN piscina boolean;
UPDATE public."house_rent"
SET piscina = CASE WHEN nombre IN (SELECT nombre FROM public."house_rent"
								   WHERE to_tsvector(nombre) @@ to_tsquery('piscina')
								  ) THEN TRUE
				   ELSE FALSE
				   END;
				   
---

ALTER TABLE public."house_sell"
ADD COLUMN piscina boolean;
UPDATE public."house_sell"
SET piscina = CASE WHEN nombre IN (SELECT nombre FROM public."house_sell"
								   WHERE to_tsvector(nombre) @@ to_tsquery('piscina')
								  ) THEN TRUE
				   ELSE FALSE
				   END;
	
	
	
	
	
	


------------------------------------------------------------------------------------------------------------------------------------				   
/*

Create column amoblado (furnitured) = TRUE if word amoblado is in name

*/

ALTER TABLE public."house_rent"
ADD COLUMN amoblado boolean;
UPDATE public."house_rent"
SET amoblado = CASE WHEN nombre IN (SELECT nombre FROM public."house_rent"
								   WHERE to_tsvector(nombre) @@ to_tsquery('amoblado') OR 
										 to_tsvector(nombre) @@ to_tsquery('amoblada')
								  ) THEN TRUE
				   ELSE FALSE
				   END;
				   
---

ALTER TABLE public."house_sell"
ADD COLUMN amoblado boolean;
UPDATE public."house_sell"
SET amoblado = CASE WHEN nombre IN (SELECT nombre FROM public."house_sell"
								   WHERE to_tsvector(nombre) @@ to_tsquery('amoblado') OR 
										 to_tsvector(nombre) @@ to_tsquery('amoblada')
								  ) THEN TRUE
				   ELSE FALSE
				   END;
				   
				   
				   
				   
		
		
				   
------------------------------------------------------------------------------------------------------------------------------------
/*

DELETE sectors with less than 10 observations

*/
DELETE ---85 deleted
FROM public."house_rent"
WHERE sector IN (SELECT sector FROM public."house_rent" 
				 GROUP BY sector 
				 HAVING COUNT(*)<10);
				 
---

DELETE ---28 deleted
FROM public."house_sell"
WHERE sector IN (SELECT sector FROM public."house_sell" 
				 GROUP BY sector 
				 HAVING COUNT(*)<10);
				 
				 
				 
				 
				 
				 
				 
------------------------------------------------------------------------------------------------------------------------------------				 
/*

ONLY KEEP observations with sectors in both tables

*/
DELETE --- 628 deleted
FROM public."house_sell"
WHERE sector NOT IN (
				 SELECT DISTINCT sector
				 FROM public."house_rent"
				 INTERSECT 
				 SELECT DISTINCT sector
				 FROM public."house_sell"
);

---

DELETE --- 0 deleted
FROM public."house_rent"
WHERE sector NOT IN (
				 SELECT DISTINCT sector
				 FROM public."house_sell"
				 INTERSECT 
				 SELECT DISTINCT sector
				 FROM public."house_rent"
);