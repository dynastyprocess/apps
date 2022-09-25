library(ffopportunity)
library(arrow)
library(nflreadr)

expected_points_df <- ffopportunity::ep_load(
  season = 2015: nflreadr:::most_recent_season(),
  type = "weekly")

write_parquet(expected_points_df, "data/expected_points_df.pdata")
