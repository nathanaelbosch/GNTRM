tf_last
fight <- tf_last[!win_after_tf & time_delta<5][2]
fight
id <- fight$match_id
teamfights_min[match_id==id] %>% tail(2)
