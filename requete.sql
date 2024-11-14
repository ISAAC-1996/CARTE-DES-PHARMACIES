
--Nombre de vaccins vendus

select datepart(iso_week, dateheure_ecollect) as num_semaine_2024,
sum(qt_vendu_lcollect) as qte,
SUM((pv_ttc_lcollect / (1 + (tva_lcollect / 100)))* qt_vendu_lcollect) as ca_ht 
from v_el_collect_ospharm
where n_auto_adhpha_ecollect in (select n_auto_adhpha from os_adhpha where dextinct_adhpha is null and syndic_adhpha = 1)
and n_auto_adhpha_ecollect not in (select n_auto_adhpha from os_completudepha where periode_completudepha between  202310 and 202411 and moisok_completudepha in (0,8,9))
and dateheure_ecollect between '2024-10-15 00:00:00.000' and '2024-11-09 23:59:00.000'
  and periode between 202410 and 202411
and n_auto_artic_lcollect in (select n_auto_artic from os_classif where n_auto_famille = '1J07A01')
group by datepart(iso_week, dateheure_ecollect)


--Nombre de vaccins vendus

select datepart(iso_week, dateheure_ecollect) as num_semaine_2024, 
	sum(qt_vendu_lcollect) as qte,
	SUM((pv_ttc_lcollect / (1 + (tva_lcollect / 100)))* qt_vendu_lcollect) as ca_ht, 
	count(distinct n_auto_adhpha_ecollect) as nbre_pheis
from v_el_collect_ospharm
	where n_auto_adhpha_ecollect in (select n_auto_adhpha from os_adhpha where dextinct_adhpha is null and syndic_adhpha = 1)
	and n_auto_adhpha_ecollect not in (select n_auto_adhpha from os_completudepha where periode_completudepha between  202310 and 202411 and moisok_completudepha in (0,8,9))
	and dateheure_ecollect between '2024-10-15 00:00:00.000' and '2024-11-09 23:59:00.000'
	  and periode between 202410 and 202411
	and n_auto_artic_lcollect in (select n_auto_artic from os_classif where n_auto_famille = '1J07A01')
	group by datepart(iso_week, dateheure_ecollect)