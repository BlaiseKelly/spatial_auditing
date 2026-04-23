
dedup_play = function(df) {
  # process per fclass group
  result = df |>
    group_split(fclass) |>
    lapply(function(grp) {
      if (nrow(grp) <= 1) return(grp)
      
      # find which features overlap each other
      overlaps = st_intersects(grp)
      
      # calculate areas
      grp$area = as.numeric(st_area(grp))
      
      # mark duplicates — keep the largest in each overlap cluster
      keep = rep(TRUE, nrow(grp))
      for (i in seq_along(overlaps)) {
        if (!keep[i]) next
        neighbours = overlaps[[i]]
        neighbours = neighbours[neighbours != i]
        # drop smaller overlapping features
        smaller = neighbours[grp$area[neighbours] <= grp$area[i]]
        keep[smaller] = FALSE
      }
      
      grp[keep, ] |> select(-area)
    }) |>
    bind_rows()
  
  return(result)
}

dedup_cross_class = function(df) {
  # sort largest first — prefer keeping bigger features
  df$area = as.numeric(st_area(df))
  df = df |> arrange(desc(area))
  
  overlaps = st_intersects(df)
  keep = rep(TRUE, nrow(df))
  
  for (i in seq_along(overlaps)) {
    if (!keep[i]) next
    neighbours = overlaps[[i]]
    neighbours = neighbours[neighbours > i & keep[neighbours]]
    
    for (j in neighbours) {
      # what proportion of the smaller feature is covered?
      intersection_area = as.numeric(st_area(st_intersection(df[i, ], df[j, ])))
      overlap_pct = intersection_area / df$area[j]
      
      # if >80% covered by a larger feature, drop it
      if (overlap_pct > 0.8) keep[j] = FALSE
    }
  }
  
  df[keep, ] |> select(-area)
}

all_play_final = dedup_cross_class(all_play_clean)