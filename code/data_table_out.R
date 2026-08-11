library(dplyr)
library(tidyr)

age_order <- c("15-24", "25-34", "35-44", "45-54", ">=55")

format_pct <- function(x) sprintf("%.2f", round(x * 100, 3))
calc_label <- function(x50, x2.5, x97.5) {
  paste0(format_pct(x50), " (", format_pct(x2.5), "-", format_pct(x97.5), ")")
}

prepare_df <- function(df, prefix){
  df %>%
    mutate(
      age = factor(age, levels = age_order),
      estimate = calc_label(X50., X2.5., X97.5.),
      colname = paste0(prefix, "_", age)
    ) %>%
    select(state2, colname, estimate)
}

inc_long <- prepare_df(clb_i_ct, "inc")
prev_long <- prepare_df(clb_pr_ct, "prev")

inc_wide <- inc_long %>% pivot_wider(names_from = colname, values_from = estimate)
prev_wide <- prev_long %>% pivot_wider(names_from = colname, values_from = estimate)

inc_cols <- paste0("inc_", age_order)
prev_cols <- paste0("prev_", age_order)

final <- inc_wide %>%
  full_join(prev_wide, by = "state2") %>%
  select(state2,
        all_of(inc_cols[inc_cols %in% names(.)]),
         all_of(prev_cols[prev_cols %in% names(.)])
  )

 write.csv(final, file = here("out/ct_tab.csv"), row.names = FALSE)

######
 
 inc_long <- prepare_df(clb_i_gc, "inc")
 prev_long <- prepare_df(clb_pr_gc, "prev")
 
 inc_wide <- inc_long %>% pivot_wider(names_from = colname, values_from = estimate)
 prev_wide <- prev_long %>% pivot_wider(names_from = colname, values_from = estimate)
 
 inc_cols <- paste0("inc_", age_order)
 prev_cols <- paste0("prev_", age_order)
 
 final <- inc_wide %>%
   full_join(prev_wide, by = "state2") %>%
   select(state2,
          all_of(inc_cols[inc_cols %in% names(.)]),
          all_of(prev_cols[prev_cols %in% names(.)])
   )
 
 write.csv(final, file = here("out/gc_tab.csv"), row.names = FALSE)

