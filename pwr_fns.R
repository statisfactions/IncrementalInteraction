
ren_pwr = function(df, oldname) {
  df[["pwr"]] = df[[oldname]]
  df[[oldname]] = NULL
  df
}


get_pwr = . %>%
  filter(pwr > 0.8) %>%
  filter(pwr == min(pwr)) %>%
  distinct

sim_pwr = . %>%
  summarize(pwr = mean(p.value < 0.05), groups = ".drop")

calc_pwr = function(df, oldname = "pwr") {
 if(oldname == "pwr")
   df %>% get_pwr
  else {
   df %>%
      ren_pwr(oldname = oldname) %>%
      get_pwr
  }
}

list_w_names = function(...) {
  dots_names <- sapply(substitute(list(...))[-1], deparse)

  structure(list(...), .Names = dots_names)

}

simpr_pwr_calc = .  %>%
  filter(term %in% "x1:x2") %>%
  group_by(n) %>%
  summarize(pwr = mean(p.value < 0.05), .groups = "drop") %>%
  rename(N = n)
