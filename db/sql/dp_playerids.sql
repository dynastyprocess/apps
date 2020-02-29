USE DYNASTYPROCESS;

SELECT
        `MFL`.`mfl_id` AS `mfl_id`,
        COALESCE(`MFL`.`sportsdata_id`,
                `SLEEPER`.`sportradar_id`) AS `sportsdata_id`,
        `SLEEPER`.`gsis_id` AS `gsis_id`,
		fp_do.id as fantasypros_id,
        COALESCE(SLEEPER.sleeper_id,SLEEPER2.sleeper_id) as sleeper_id,
        COALESCE(MFL.espn_id,SLEEPER.espn_id,SLEEPER2.espn_id,SLEEPER3.espn_id) as espn_id,
        COALESCE(`MFL`.`name`, `SLEEPER`.`full_name`, SLEEPER2.merge_name) AS `name`,
        COALESCE(MFL.merge_name,SLEEPER.merge_name, SLEEPER2.merge_name) AS `merge_name`,
        COALESCE(`MFL`.`position`, `SLEEPER`.`position`,SLEEPER2.`position`) AS `position`,
        COALESCE(MFL.team,SLEEPER.team,SLEEPER2.team) AS `team`,
        COALESCE(`MFL`.`age`, `SLEEPER`.`age`,SLEEPER2.age) AS `age`,
        COALESCE(fp_do.ecr,400) as fp_do_ecr,
        fp_do.sd as fp_do_sd,
        COALESCE(fp_dp.ecr,220) as fp_dp_ecr,
        fp_dp.sd as fp_dp_sd
       
    FROM `mfl_players_current` as`MFL`
    
    LEFT JOIN `sleeper_players` as `SLEEPER` ON `MFL`.`sportsdata_id` = `SLEEPER`.`sportradar_id`
    
    LEFT JOIN `sleeper_players` as `SLEEPER2` ON MFL.stats_id = SLEEPER2.stats_id 
						AND SLEEPER.sportradar_id is NULL
	
    LEFT JOIN sleeper_players as SLEEPER3 ON MFL.espn_id = SLEEPER3.espn_id
						AND SLEEPER.espn_id is NULL
                        AND SLEEPER2.espn_id is NULL
    
    