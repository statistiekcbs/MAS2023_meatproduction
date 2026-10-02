SELECT DISTINCT
		skalnummer
		-- we gaan selecteren maar unieke rijen
		-- als er twee locaties van hetzelfde bedrijf met dezelfde (juiste) hoofdsbi zijn,
		-- gaan we de langste periode qua datum geldigheid nemen 
		,MAX(datum_certificatie_geldigheid) AS datum_certificatie_geldigheid 
		,MIN(datum_geldig_vanaf) AS datum_geldig_vanaf 
		,resultaat_certificatie 
		,huisnummer
		,postcode
		-- alleen voor debugging
		--,hoofdsbi
		--,bedrijf_id
		--,locatie_id
		FROM (
				-- selecteer leidende locaties met de juiste hoofdsbi	
				SELECT
    				bed.skalnummer
				,	cer.datum_certificatie_geldigheid 
				,	cer.datum_geldig_vanaf
				,   cer.resultaat_certificatie 
				,	loc.huisnummer
				,	loc.postcode
				,	act.hoofdsbi
				,	loc.bedrijf_id
				--,	loc.locatie_id
				FROM      {`tbl_sc_cert`}	cer 
				LEFT JOIN	{`tbl_sc_loca`}	loc
    				ON			  cer.locatie_id = loc.locatie_id
				LEFT JOIN	{`tbl_sc_bedr`}	bed
    				ON			  loc.bedrijf_id = bed.bedrijf_id
				LEFT JOIN	{`tbl_sc_actv`}	act
    				ON			  cer.activ_id = act.activ_id
				WHERE	cer.eind_datum IS NULL
    			AND act.hoofdsbi IN ({hoofsbi*})
				UNION 
				-- Selecteer de locaties van de bedrijven die ten minste één locatie hebben met de juiste hoofdsbi. 
				-- De kolommen datum_certificatie_geldigheid, datum_geldig_vanaf en resultaat_certificatie 
				-- komen van die locatie en de adresgegevens komen van de andere locaties.
				SELECT 
					alle_cert.skalnummer
				,	rund_cert.datum_certificatie_geldigheid 
				,	rund_cert.datum_geldig_vanaf
				,   rund_cert.resultaat_certificatie 
				,	alle_cert.huisnummer
				,	alle_cert.postcode
				,	alle_cert.hoofdsbi
				,	alle_cert.bedrijf_id
				--,	alle_cert.locatie_id
				FROM (
						SELECT 
    						bed.skalnummer
						,	loc.huisnummer
						,	loc.postcode
						,	loc.locatie_id
						,	act.hoofdsbi
						,	loc.bedrijf_id
						FROM      {`tbl_sc_cert`}	cer 
						LEFT JOIN	{`tbl_sc_loca`}	loc
    						ON			  cer.locatie_id = loc.locatie_id
						LEFT JOIN	{`tbl_sc_bedr`}	bed
    						ON			  loc.bedrijf_id = bed.bedrijf_id
						LEFT JOIN	{`tbl_sc_actv`}	act
    						ON			  cer.activ_id = act.activ_id
						WHERE cer.eind_datum IS NULL
						 AND act.hoofdsbi NOT IN ({hoofsbi*})
						) alle_cert
					INNER JOIN (
						SELECT 
    						loc.bedrijf_id
						,	loc.locatie_id
						,	cer.datum_certificatie_geldigheid 
						,	cer.datum_geldig_vanaf
						,   cer.resultaat_certificatie 
						FROM      {`tbl_sc_cert`}	cer 
						LEFT JOIN	{`tbl_sc_loca`}	loc
    						ON			  cer.locatie_id = loc.locatie_id
						LEFT JOIN	{`tbl_sc_bedr`}	bed
    						ON			  loc.bedrijf_id = bed.bedrijf_id
						LEFT JOIN	{`tbl_sc_actv`}	act
    						ON			  cer.activ_id = act.activ_id
						WHERE	cer.eind_datum IS NULL
    				  AND act.hoofdsbi IN ({hoofsbi*})) rund_cert
						ON alle_cert.bedrijf_id = rund_cert.bedrijf_id) t
		GROUP BY bedrijf_id, skalnummer, huisnummer, postcode, resultaat_certificatie