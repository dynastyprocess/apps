USE dynastyprocess;

SELECT
        mfl.mfl_id AS mfl_id,
        COALESCE(mfl.sportsdata_id,sleeper.sportradar_id,sleeper2.sportradar_id,sleeper3.sportradar_id) AS sportradar_id,
        COALESCE(sleeper.gsis_id,sleeper2.gsis_id,sleeper3.gsis_id) AS gsis_id,
		fp.id as fantasypros_id,
        ffcalc.ffcalculator_id as ffcalculator_id,
        COALESCE(pfr_drafted.id_pfr,pfr_undrafted.id_pfr) as pfr_id,
        COALESCE(pfr_drafted.id_cfbref) as cfbref_id,
        COALESCE(sleeper.sleeper_id,sleeper2.sleeper_id,sleeper3.sleeper_id) as sleeper_id,
        COALESCE(mfl.espn_id,sleeper.espn_id,sleeper2.espn_id,sleeper3.espn_id) as espn_id,
        COALESCE(mfl.name, sleeper.full_name, sleeper2.merge_name,sleeper3.merge_name) AS name,
        COALESCE(mfl.merge_name,sleeper.merge_name, sleeper2.merge_name,sleeper3.merge_name) AS merge_name,
        COALESCE(mfl.position, sleeper.position,sleeper2.position,sleeper3.position) AS position,
        COALESCE(mfl.team,sleeper.team,sleeper2.team,sleeper3.team) AS team,
        COALESCE(mfl.age, sleeper.age,sleeper2.age,sleeper3.age) AS age,
        COALESCE(pfr_drafted.draft_year,mfl.draft_year) as draft_year,
        pfr_drafted.draft_round,
        pfr_drafted.draft_pick
       
    FROM mfl_players_current as mfl
    
    LEFT JOIN sleeper_players as sleeper ON mfl.sportsdata_id = sleeper.sportradar_id
    
    LEFT JOIN sleeper_players as sleeper2 ON mfl.stats_id = sleeper2.stats_id 
						AND sleeper.stats_id is NULL
	
    LEFT JOIN sleeper_players as sleeper3 ON mfl.espn_id = sleeper3.espn_id
						AND sleeper.espn_id is NULL
                        AND sleeper2.espn_id is NULL
	
    LEFT JOIN ffcalculator_adp_current as ffcalc on mfl.merge_name = ffcalc.merge_name 
	 					AND mfl.team = ffcalc.team
                        AND ffcalc.adp_type = 'redraft'
                        
	LEFT JOIN fp_ecr as fp on mfl.merge_name = fp.mergename
						AND mfl.team = fp.tm
                        AND fp.ecr_type = 'dp'
                        AND fp.scrape_date = (SELECT scrape_date from fp_ecr order by scrape_date desc limit 1)
	
    LEFT JOIN pfr_drafted on mfl.merge_name = pfr_drafted.merge_name
						AND mfl.draft_team = pfr_drafted.team
                        AND mfl.draft_year = pfr_drafted.draft_year
	
    LEFT JOIN pfr_undrafted on mfl.merge_name = pfr_undrafted.merge_name
						AND pfr_drafted.merge_name is null


    