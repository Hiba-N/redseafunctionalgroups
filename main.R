#load all tables

#shortlist columns

#intersect with red sea fishes

#save individual files

#find missing values using inter stock values

#merge all tables

#save collective file


red_sea_reproduction_traits <- infer_from_other_stocks(
  red_sea_reproduction_traits,
  reproduction
)

red_sea_stocks_traits <- infer_from_other_stocks(
  red_sea_stocks_traits,
  stocks
)

red_sea_popr_traits <- infer_from_other_stocks(
  red_sea_popr_traits,
  popr
)

red_sea_morphdat_traits <- infer_from_other_stocks(
  red_sea_morphdat_traits,
  morphdat
)

red_sea_popgrowth_traits <- infer_from_other_stocks(
  red_sea_popgrowth_traits,
  popgrowth
)

red_sea_eggs_traits <- infer_from_other_stocks(
  red_sea_eggs_traits,
  eggs
)

red_sea_ecology_traits <- infer_from_other_stocks(
  red_sea_ecology_traits,
  ecology
)



morphdat %>%
  count(spec_code) %>%
  filter(n > 1)