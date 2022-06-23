
catch=dbGetQuery(channel,"SELECT
marfissci.licences.licence_id,
marfissci.sd_log_eff_std_info.fv_hours_fished,
marfissci.sd_log_eff_std_info.fv_gear_code,
marfissci.gears.desc_eng              AS gear_description,
marfissci.sd_log_eff_std_info.fv_date_fished,
marfissci.sd_log_spc_std_info.fv_weight,
marfissci.unit_of_measures.desc_eng   AS measurement_unit,
a.data_value                          AS rivername_logbook,
TO_CHAR(marfissci.sd_log_eff_std_info.fv_date_fished, 'YYYY') year,
TO_CHAR(marfissci.sd_log_eff_std_info.fv_date_fished, 'MM') month,
TO_CHAR(marfissci.sd_log_eff_std_info.fv_date_fished, 'DD') day,
marfissci.sd_log_spc_std_info.fv_ssf_species_code,
LICENCE_TYPES.DESC_ENG AS licence_type,
marfissci.sd_log_spc_std_info.fv_catch_usage_code
FROM
marfissci.sd_log_eff_std_info                                                                      
RIGHT JOIN marfissci.sum_docs ON marfissci.sum_docs.sum_doc_id = marfissci.sd_log_eff_std_info.sum_doc_id
RIGHT JOIN marfissci.licences ON marfissci.licences.licence_id = marfissci.sum_docs.licence_id
RIGHT JOIN marfissci.licence_types ON marfissci.licence_types.licence_type_id = marfissci.licences.licence_type_id
LEFT JOIN marfissci.sd_log_spc_std_info ON marfissci.sd_log_spc_std_info.sd_log_id = marfissci.sd_log_eff_std_info.sd_log_id
LEFT JOIN marfissci.unit_of_measures ON marfissci.sd_log_spc_std_info.fv_unit_of_measure_id = marfissci.unit_of_measures.unit_of_measure_id
LEFT JOIN marfissci.gears ON marfissci.sd_log_eff_std_info.fv_gear_code = marfissci.gears.gear_code
LEFT JOIN (
  SELECT
  *
    FROM
  marfissci.sd_log_entrd_dets
  WHERE
  marfissci.sd_log_entrd_dets.sum_doc_defn_col_id = 605
) a ON marfissci.sd_log_eff_std_info.sd_log_id = a.sd_log_id
WHERE
marfissci.licences.species_code = 350
AND marfissci.sd_log_spc_std_info.fv_ssf_species_code = 350
--- AND marfissci.sd_log_spc_std_info.fv_ssf_species_code IN (100,110,121,130,143,144,149,158,171,174,175,181,199,
---                                                           200,250,
---                                                           350,352,354,355,357,358,359,360,363,365,366,367,368,389,399,
---                                                           601,650,707,711,999)")

didnotfish=dbGetQuery(channel,"SELECT   to_char(SUM_DOCS.END_DATE,'YYYY') year,
SUM_DOCS.LICENCE_ID,
SUM_DOCS.NIL_REPORT_FLAG
FROM marfissci.SUM_DOCS
LEFT JOIN  marfissci.LICENCES
ON LICENCES.LICENCE_ID = SUM_DOCS.LICENCE_ID
WHERE SUM_DOCS.SUM_DOC_DEFN_ID=11 AND
LICENCES.SPECIES_CODE                  = 350")


licencerenewals=dbGetQuery(channel,"SELECT licence_renewals.licence_year,
licence_renewals.licence_id,
LICENCE_TYPES.DESC_ENG AS licence_type
FROM marfis.licence_renewals
RIGHT JOIN marfissci.licences ON marfissci.licences.licence_id = marfis.licence_renewals.licence_id
RIGHT JOIN marfissci.licence_types ON marfissci.licence_types.licence_type_id = marfissci.licences.licence_type_id
WHERE LICENCES.SPECIES_CODE                  = 350")




