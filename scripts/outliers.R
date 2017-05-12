outliers = c(
  9946  # match[duration==max(match$duration)]$match_id
)

match <- match[!(match_id %in% outliers)]
player_time <- player_time[!(match_id %in% outliers)]
