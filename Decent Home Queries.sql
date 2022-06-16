SELECT 
  c.lo_reference
, s.Record_No
, a.address
, c.Year_Built
, a.dh_heading
, a.dh_sub_heading
, a.element_code
, a.element
, a.sub_element_code
, a.sub_element
, a.material_code
, a.material
, a.Lifecycle
, a.DH_lifecycle
,MAX( a.last_renewed) as 'Last_Renewed'
, a.planned_year
, d.Failure_Year as 'Component_Failure_Year'
, a.Failure_Year as 'Overall_Failure_Year'
, CASE WHEN a.Failure_Year <= 2022 THEN 'Current'
ELSE 'Future'
END AS 'Future_Current'
, s.Renewal_Year
, s.Last_Edited AS 'Survey_Last_Edited'
, s.Last_Edited_By AS 'Survey_Last Edited_By'
, s.Comments
, c.Condition_Survey_Date
, c.Condition_Survey_By
from dba.View_DHReportSource_NoDuplicates a
join dba.dhslocationdecencysummary b on b.lo_code = a.lo_code
join dba.location c on  c.lo_code = b.lo_code
join dba.survey_t s on s.lo_code = b.lo_code and a.Element_Code = s.element_code and a.sub_element_code = s.sub_element_code and a.material_code = s.material_code
JOIN dba.DHSLocationDecency d ON
	a.lo_code = d.Lo_Code
	AND a.DH_Code = d.DH_Code
	AND a.dh_sub_code = d.DH_Sub_Code
where b.overall_decency IN ('Non-Decent')
GROUP BY
c.lo_reference
, s.Record_No
, a.address
, c.Year_Built
, a.dh_heading
, a.dh_sub_heading
, a.element_code
, a.element
, a.sub_element_code
, a.sub_element
, a.material_code
, a.material
, a.Lifecycle
, a.DH_lifecycle
, a.last_renewed
, a.planned_year
, d.Failure_Year
, a.Failure_Year
, s.Renewal_Year
, s.Last_Edited
, s.Last_Edited_By
, c.Condition_Survey_Date
, c.Condition_Survey_By
, s.Comments