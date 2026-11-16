# remove DK group from ed.level 

#####################################
# Map Mindset and Pathways variables 
#####################################
# Mindset survey does not have some of the TT variables (especially for rural). 
# We will need to code proxies, and then test how proxies work. 


# Variables to determine eligibility/needed for model:
# urban/rural
table(MS$area_type, useNA = "ifany") # this looks like the right one to use, but the values are 1/2 and are not labeled 
table(MS$urban_enum, useNA = "ifany") # this is aligned with the PWS segments (see ptype_rural_* vars) so I think we can use instead

# ever given birth
table(MS$given_birth, useNA = "ifany") # assuming 1 = yes, 2 = no


# Variables that are exact matches or can be coded to be exact - NO ADDITIONAL CODING OR SLIGHT RECODE NEEDED:
# ed.level 
table(NN$ed.level, useNA = "ifany")
# clean ed.level and remove "dont' know" so it matches MS 
NN$ed.level <- factor(
  NN$ed.level,
  levels = c("no education", "primary", "secondary", "higher")
)
table(MS$edu, useNA = "ifany") 
# recode to match PWS
MS <- MS %>%
  mutate(ed.level = factor(
    edu,
    levels = c(1, 2, 3, 4),
    labels = c("no education", "primary", "secondary", "higher")
  ))
# check
table(MS$ed.level, useNA = "ifany") 

# hh.toilet.flush
table(NN$hh.toilet.flush)
table(MS$toilet, useNA = "ifany")
# recode to match PWS
MS <- MS %>%
  mutate(hh.toilet.flush = case_when(
    toilet == 1 ~ "Yes",
    toilet %in% c(2, 3, 4) ~ "No",
    TRUE ~ NA_character_
  ))
# check
table(MS$hh.toilet.flush, useNA = "ifany")

# med.dist
table(NN$med.dist)
table(MS$meddist, useNA = "ifany")
#recode to match PWS 
MS <- MS %>%
  mutate(med.dist = case_when(
    meddist %in% c(1, 2) ~ "No",
    meddist == 3 ~ "Yes",
    TRUE ~ NA_character_
  ),
  med.dist = factor(med.dist, levels = c("No", "Yes")))
# check
table(MS$med.dist)

# tv.yn
table(NN$tv.yn)
table(MS$tv, useNA = "ifany")
#recode to match PWS 
MS <- MS %>%
  mutate(tv.yn = case_when(
    tv %in% c(1, 2, 3) ~ "Yes",
    tv == 4 ~ "No",
    TRUE ~ NA_character_
  ),
  tv.yn = factor(tv.yn, levels = c("No", "Yes")))
# check
table(MS$tv.yn, useNA = "ifany")

# muslim
table(NN$muslim)
table(MS$muslim, useNA = "ifany")
MS <- MS %>%
  mutate(muslim = case_when(
    muslim == TRUE ~ "Yes",
    muslim == FALSE ~ "No",
    TRUE ~ NA_character_
  ))
MS$muslim <- factor(MS$muslim, levels = c("No", "Yes"))
# check
table(MS$muslim, useNA = "ifany")

# roof
table(NN$roof)
table(MS$dwelling_roof, useNA ="ifany")
# recode to match PWS 
MS <- MS %>%
  mutate(roof = case_when(
    grepl("^Finished roofing", dwelling_roof) ~ "Yes",
    grepl("^Natural roofing|^Rudimentary roofing|^No roof", dwelling_roof) ~ "No",
    dwelling_roof == "Other (specify)" ~ "No",
    TRUE ~ NA_character_
  ),
  roof = factor(roof, levels = c("No", "Yes")))
# check
table(MS$roof, useNA ="ifany")

# hh.mobile
table(NN$hh.mobile)
table(MS$has_phone, useNA = "ifany")
# recode to match PWS 
MS <- MS %>%
  mutate(hh.mobile = case_when(
    has_phone == "Owns a phone" ~ "Yes",
    has_phone == "Does not own a phone" ~ "No",
    TRUE ~ NA_character_
  ),
  hh.mobile = factor(hh.mobile, levels = c("No", "Yes")))
# check
table(MS$hh.mobile, useNA = "ifany")

# any.media.yn
table(NN$any.media.yn) # no one variable that matches, have to recode q211-q213
table(MS$q211, useNA = "ifany") # newspaper or magazine, NOTE: a lot of NAs 12,971
table(MS$q212, useNA = "ifany") # radio
table(MS$q213, useNA = "ifany") # TV 
# recode to match PWS 
MS <- MS %>%
  mutate(any.media.yn = case_when(
    q211 %in% c("At least once a week", "Less than once a week") |
      q212 %in% c("At least once a week", "Less than once a week") |
      q213 %in% c("At least once a week", "Less than once a week") ~ "Yes",
    q211 == "Not at all" &
      q212 == "Not at all" &
      q213 == "Not at all" ~ "No",
    TRUE ~ NA_character_
  ),
  any.media.yn = factor(any.media.yn, levels = c("No", "Yes")))
# check
table(MS$any.media.yn, useNA = "ifany") # many missing because of newspaper variable

# hh.noimp.housing
table(NN$hh.noimp.housing, useNA = "ifany") # Woman's household dwelling is made of unimproved walls AND floor (meaning natural or rudimentary)
table(MS$q305, useNA = "ifany") # "What is the main material of the dwelling floor"
table(MS$exterior_walls, useNA = "ifany") # exterior_walls: "To the enumerator: Observe the dwelling. What are the main materials of the exterior walls?"
# recode to match PWS 
MS <- MS %>%
  mutate(hh.noimp.housing = case_when(
    grepl("^(Natural|Rudimentary) floor", q305) &
      grepl("^(No walls|Natural walls|Rudimentary walls)", exterior_walls) ~ "Yes",
    grepl("^Finished floor", q305) |
      grepl("^Finished walls", exterior_walls) ~ "No",
    TRUE ~ NA_character_
  ),
  hh.noimp.housing = factor(hh.noimp.housing, levels = c("No", "Yes")))
# check 
table(MS$hh.noimp.housing, MS$q305, useNA = "ifany")
table(MS$hh.noimp.housing, MS$exterior_walls, useNA = "ifany")
table(MS$q305, MS$exterior_walls, useNA = "ifany")
table(MS$hh.noimp.housing, useNA = "ifany")

# ed.sec.higher
table(NN$ed.sec.higher, useNA = "ifany")
table(MS$ed.level)
# recode to match PWS 
MS <- MS %>%
  mutate(ed.sec.higher = case_when(
    ed.level %in% c("secondary", "higher") ~ "Yes",
    ed.level %in% c("no education", "primary") ~ "No",
    TRUE ~ NA_character_
  ),
  ed.sec.higher = factor(ed.sec.higher, levels = c("No", "Yes")))
# check 
table(MS$ed.sec.higher, MS$ed.level, useNA = "ifany")

# Variables that are completely missing from Mindset survey - NO CODING POSSIBLE:
# highestyearsedinHH.7plus
# mobile.financial
# jdwd.lrgpur.cat
# polygamy
# partner.ed.level.cat2
# water
# occ.type.cat
# jdwd.visit.cat
# age.1stbrth.cat1


# Variables that are in the NN TT and there are proxies available in the Mindset survey - RECODE TO MATCH MINDSET PROXY:
# wealth.sum.index.cat1- IN PATHWAYS:  Wealth measured as an index of household asset ownership 
# including electricity, tv, refrigerator, bank account, computer, car, and unimproved latrine. 
# IN MINDSET SURVEY: electricity (q309_field_list_1), radio (q309_field_list_2), tv (q309_field_list_3), 
# fixed phone line (q310_field_list_5), computer (q309_field_list_5), refrigerator (q309_field_list_6), 
# watch (q310_field_list_1), mobile phone (q310_field_list_2), motorcycle (q310_field_list_4), 
# animal drawn cart ((q310_field_list_6), car ((q310_field_list_7), motor boat ((q310_field_list_8), 
# toilet type (q1119 = 3 (No facility/bush/field) or 4 (Bucket in house))
table(NN$wealth.sum.index.cat1, useNA = "ifany")

# MS does not have bank account variable, so recreate this using phone variable instead
NN <- NN %>%
  mutate(
    wealth.sum.index =
      as.integer(hh.electricity == "Yes") +
      as.integer(hh.tv == "Yes") +
      as.integer(hh.refrig == "Yes") +
      as.integer(hh.mobile == "Yes") +
      as.integer(hh.computer == "Yes") +
      as.integer(hh.car == "Yes") +
      as.integer(hh.noimp.latrine == "No"),
    wealth.sum.index.cat1 = factor(
      case_when(
        wealth.sum.index %in% 0:1 ~ "0-1",
        wealth.sum.index %in% 2:7 ~ "2-7",
        TRUE ~ NA_character_
      ),
      levels = c("0-1", "2-7")
    )
  )
# check
table(NN$wealth.sum.index, useNA = "ifany")
table(NN$wealth.sum.index.cat1, useNA = "ifany")

# Create variable using Mindset vars
MS <- MS %>%
  mutate(
    wealth.sum.index.cat1 = 
      as.integer(q309_field_list_1 == "Yes") + # electricity
      as.integer(q309_field_list_3 == "Yes") + # tv
      as.integer(q309_field_list_6 == "Yes") + # refrigerator
      as.integer(q309_field_list_5 == "Yes") + # computer
      as.integer(q310_field_list_2 == "Yes") + # mobile phone to replace banking
      as.integer(q310_field_list_7 == "Yes") + # car
      case_when(
        q1119 %in% c("Flush toilet", "Pit latrine") ~ 1L, # improved toilet
        q1119 %in% c("Bucket in house", "No facility/bush/field") ~ 0L,
        TRUE ~ NA_integer_
      ),
    wealth.sum.index.cat1 = factor(
      case_when(
        wealth.sum.index.cat1 %in% 0:1 ~ "0-1",
        wealth.sum.index.cat1 %in% 2:7 ~ "2-7",
        TRUE ~ NA_character_
      ),
      levels = c("0-1", "2-7")
    )
  )
# check
table(MS$wealth.sum.index.cat1, useNA = "ifany")


# house.index - IN PATHWAYS: sum of hh.electricity, hh.radio, hh.tv, hh.refrig, hh.cupboard, hh.sofa, 
# hh.air, hh.iron, hh.generator, hh.fan. IN MINDSET SURVEY: electricity, radio, tv, fixed phone line, 
# computer, refrigerator, watch, mobile phone


# Variables that are not part of NN TT but are similar between the surveys - RECODE TO MATCH MINDSET IF NEEDED:
# wall material
MS <- MS %>%
  mutate(hh.noimp.wall = case_when(
    grepl("^(Natural walls|Rudimentary walls)", exterior_walls) ~ "Yes",
    grepl("^(Finished walls|No walls)", exterior_walls) ~ "No",
    TRUE ~ NA_character_
  ),
  hh.noimp.wall = factor(hh.noimp.wall, levels = c("No", "Yes")))
# check 
table(MS$hh.noimp.wall, useNA = "ifany")

# marital status (y/n)
MS <- MS %>%
  mutate(marr.cohab = case_when(
    marital_status %in% c("Married", "Living with partner") ~ "Yes",
    marital_status %in% c("Divorced", "Never married", "Other", "Separated", "Widowed") ~ "No",
    TRUE ~ NA_character_
  ),
  marr.cohab = factor(marr.cohab, levels = c("No", "Yes")))
# check
table(MS$marr.cohab, useNA = "ifany")
table(MS$marr.cohab, MS$marital_status, useNA = "ifany")

# husband work (y/n)
MS <- MS %>%
  mutate(partner.working.yn = case_when(
    q1111 == "Yes" ~ "Yes",
    q1111 == "No" ~ "No",
    TRUE ~ NA_character_
  ),
  partner.working.yn = factor(partner.working.yn, levels = c("No", "Yes")))
# check 
table(MS$partner.working.yn, useNA = "ifany")
table(MS$partner.working.yn, MS$q1111, useNA = "ifany")

# woman work (y/n)
MS <- MS %>%
  mutate(working.yn = case_when(
    q1112 == "Yes" ~ "Yes",
    q1112 == "No" ~ "No",
    TRUE ~ NA_character_
  ),
  working.yn = factor(working.yn, levels = c("No", "Yes")))
# check 
table(MS$working.yn, useNA = "ifany")

# cooking fuel (Woman's household uses clean fuels for cooking (electricity, liquefied petroleum gas (LPG), 
# natural gas, or biogas)), NN = hh.clean.fuel, HH = q1114_6 (electricity), q1114_3 (LPG/natural gas)
MS <- MS %>%
  mutate(hh.clean.fuel = case_when(
    q1114_6 == 1 | q1114_3 == 1 ~ "Yes", 
    q1114_6 == 0 & q1114_3 == 0 ~ "No",
    TRUE ~ NA_character_
  ),
  hh.clean.fuel = factor(hh.clean.fuel, levels = c("No", "Yes")))
# check 
table(MS$hh.clean.fuel, useNA = "ifany")

# barrier - cost (Woman reported cost for necessary treatment is a big problem when wanting 
# treatment or medical advice for herself when she is sick), NN = med.cost, MS = medcost
table(NN$med.cost)
table(MS$medcost)
MS <- MS %>%
  mutate(med.cost = case_when(
    medcost == 3 ~ "Yes",
    medcost %in% c(1, 2) ~ "No",
    TRUE ~ NA_character_
  ),
  med.cost = factor(med.cost, levels = c("No", "Yes")))
# check 
table(MS$med.cost, MS$q1107, useNA = "ifany")

# barrier - permission (Woman reported that getting permission to go to the doctor is a big 
# problem when wanting treatment or medical advice for herself when she is sick) 
# NN = med.permis, MS = q1102 (3=big problem)
MS <- MS %>%
  mutate(med.permis = case_when(
    q1102 == "Big problem" ~ "Yes",
    q1102 %in% c("Not a problem", "Small problem") ~ "No",
    TRUE ~ NA_character_
  ),
  med.permis = factor(med.permis, levels = c("No", "Yes")))
# check 
table(MS$med.permis, MS$q1102, useNA = "ifany")

# barrier - go alone (Woman reported not wanting to go alone is a big problem for wanting 
# treatment or medical advice for herself when she is sick) NN = med.alone, MS = q1109
MS <- MS %>%
  mutate(med.alone = case_when(
    q1109 == "Big problem" ~ "Yes",
    q1109 %in% c("Not a problem", "Small problem") ~ "No",
    TRUE ~ NA_character_
  ),
  med.alone = factor(med.alone, levels = c("No", "Yes")))
# check 
table(MS$med.alone, useNA = "ifany")
table(MS$med.alone, MS$q1109, useNA = "ifany")  

# FLAG cohabitate (y/n) - cannot find 

# age cohab (<19) (Woman's age at first cohabitation with a partner was less than 19 years) 
# NN = age.1stcohab, there is no variable for <19 in NN so have to recreate it for NN. 
# MS = married_early
table(NN$age.1stcohab.cat1, NN$marital.cat, useNA = "ifany")
table(NN$age.1stcohab)
NN <- NN %>%
  mutate(age.1stcohab.19 = case_when(
    age.1stcohab == "never partnered" ~ NA_real_,
    suppressWarnings(as.numeric(as.character(age.1stcohab))) < 19 ~ 1,
    suppressWarnings(as.numeric(as.character(age.1stcohab))) >= 19 ~ 0,
    TRUE ~ NA_real_
  ))

# check 
table(NN$age.1stcohab.19, NN$age.1stcohab, useNA = "ifany")
table(MS$married_early)
# rename to match PWS
MS <- MS %>% mutate(age.1stcohab.19 = as.integer(married_early)) %>%
  select(-married_early)
table(MS$age.1stcohab.19)

# occupation 
table(NN$occupation) # have to recreate this variable because we separated clerical/sales 
table(MS$q1112) # do you work 
table(MS$q1117) # what work do you do
NN <- NN %>%
  mutate(occupation.cat1 = case_when(
    occupation == "not working" ~ "not working",
    occupation %in% c("professional/technical/managerial") ~ "professional/mangerial",
    occupation %in% c("clerical", "sales") ~ "clerical/sales",
    occupation %in% c("agricultural - self employed", "agricultural - employee") ~ "service/domestic/other",
    occupation %in% c("skilled manual", "unskilled manual") ~ "manual",
    occupation %in% c("household and domestic", "services", "other") ~ "service/domestic/other",
    TRUE ~ NA_character_
  ),
  occupation.cat1 = factor(
    occupation.cat1,
    levels = c("manual", "not working", "professional/mangerial", "clerical/sales", "service/domestic/other")
  ))
# check 
table(NN$occupation.cat1, NN$occupation, useNA = "ifany")

MS <- MS %>%
  mutate(occupation.cat1 = case_when(
    q1112 == "No" ~ "not working",
    q1112 == "Yes" & q1117 == "Professional, technical, managerial" ~ "professional/mangerial",
    q1112 == "Yes" & q1117 == "Manual labor" ~ "manual",
    q1112 == "Yes" & q1117 == "Clerical or sales" ~ "clerical/sales",
    q1112 == "Yes" & q1117 %in% c("Household, domestic and services", "Other types of work") ~ "service/domestic/other",
    TRUE ~ NA_character_
  ),
  occupation.cat1 = factor(
    occupation.cat1,
    levels = c("manual", "not working", "professional/mangerial", "clerical/sales", "service/domestic/other")
  ))
# check
table(MS$q1117, MS$occupation.cat1, useNA = "ifany")
table(MS$occupation.cat1)

# religion (full categorical)
table(NN$religion)
table(MS$q220, MS$q1118)
MS <- MS %>%
  mutate(religion = case_when(
    q1118 == 1 ~ "muslim",
    q1118 == 2 ~ "catholic",
    q1118 == 3 ~ "other christian",
    q1118 == 4 ~ "traditionalist",
    q1118 == 5 ~ "other",
    TRUE ~ NA_character_
  ),
  religion = factor(religion,
                    levels = c("catholic", "muslim", "other", "other christian", "traditionalist")))
# check 
table(MS$q1118, MS$religion, useNA = "ifany")

# FLAG - partner age gap - no age var, save for later if needed
# FLAG - age, not in dataset, save for later if needed
# FLAG - migration - not in NN 
# FLAG - literacy - not in NN 

# newspaper frequency
MS <- MS %>%
  mutate(newsp.yn = case_when(
    q211 %in% c("At least once a week", "Less than once a week") ~ "Yes",
    q211 %in% c("Not at all", "Don't know") ~ "No",
    TRUE ~ NA_character_
  ),
  newsp.yn = factor(newsp.yn, levels = c("No", "Yes")))
# check 
table(MS$newsp.yn, useNA = "ifany")
table(MS$q211, MS$newsp.yn, useNA = "ifany")

# radio frequency
MS <- MS %>%
  mutate(rad.yn = case_when(
    q212 %in% c("At least once a week", "Less than once a week") ~ "Yes",
    q212 %in% c("Not at all", "Don't know") ~ "No",
    TRUE ~ NA_character_
  ),
  rad.yn = factor(rad.yn, levels = c("No", "Yes")))
# check 
table(MS$rad.yn, useNA = "ifany")
table(MS$q212, MS$rad.yn, useNA = "ifany")

# tv frequency
MS <- MS %>%
  mutate(tv.yn = case_when(
    q213 %in% c("At least once a week", "Less than once a week") ~ "Yes",
    q213 %in% c("Not at all", "Don't know") ~ "No",
    TRUE ~ NA_character_
  ),
  tv.yn = factor(tv.yn, levels = c("No", "Yes")))
# check 
table(MS$tv.yn, useNA = "ifany")
table(MS$q213, MS$tv.yn, useNA = "ifany")

# internet - used in the last 12 months 
table(NN$internet.use)
table(MS$q217) 
MS <- MS %>%
  mutate(internet.use = case_when(
    q217 == "Yes" ~ "Yes",
    q217 %in% c("No", "Don't know") ~ "No",
    TRUE ~ NA_character_
  ),
  internet.use = factor(internet.use, levels = c("No", "Yes")))
# check 
table(MS$q217, MS$internet.use, useNA = "ifany")

# mobile phone (woman)
table(NN$has.mobile)
table(MS$q214)
MS <- MS %>%
  mutate(has.mobile = case_when(
    q214 == "Yes" ~ "Yes",
    q214 %in% c("No", "Don't know") ~ "No",
    TRUE ~ NA_character_
  ),
  has.mobile = factor(has.mobile, levels = c("No", "Yes")))
# check 
table(MS$q214, MS$has.mobile, useNA = "ifany")

# smartphone
table(NN$smart.phone)
table(MS$q215)
MS <- MS %>%
  mutate(smart.phone = case_when(
    q215 == "Yes" ~ "Yes",
    q215 %in% c("No", "Don't know") ~ "No",
    TRUE ~ NA_character_
  ),
  smart.phone = factor(smart.phone, levels = c("No", "Yes")))
# check 
table(MS$q215, MS$smart.phone, useNA = "ifany")

# FLAG - internet ever (y/n)
# FLAG - hh members

# any hh disabilities
table(NN$any.hh.dis)
table(MS$q302) # any member with disability 
MS <- MS %>%
  mutate(any.hh.dis = case_when(
    q302 == "Yes" ~ "Yes",
    q302 %in% c("No", "Don't know") ~ "No",
    TRUE ~ NA_character_
  ),
  any.hh.dis = factor(any.hh.dis, levels = c("No", "Yes")))
# check 
table(MS$q302, MS$any.hh.dis, useNA = "ifany")

# num rooms sleeping
table(NN$hh.rooms.cat)
table(MS$q304)
MS <- MS %>%
  mutate(hh.rooms.cat = case_when(
    q304 == 1 ~ "1",
    q304 == 2 ~ "2",
    q304 == 3 ~ "3",
    q304 >= 4 ~ "4+",
    TRUE ~ NA_character_
  ),
  hh.rooms.cat = factor(hh.rooms.cat,
                        levels = c("1", "2", "3", "4+")))
# check 
table(MS$q304, MS$hh.rooms.cat, useNA = "ifany")

# floor material
table(NN$floor) # Woman's household dwelling is made of natural or rudimentary floor material
table(MS$q305)
MS <- MS %>%
  mutate(floor = case_when(
    grepl("^(Natural|Rudimentary) floor", q305) ~ "Yes",
    grepl("^Finished floor", q305) ~ "No",
    TRUE ~ NA_character_
  ),
  floor = factor(floor, levels = c("No", "Yes")))
# check
table(MS$q305, MS$floor, useNA = "ifany")

# own the home - not the same
table(NN$jnt.house.ownership) # Woman owns a house alone or jointly
table(MS$q311) # q311: Do you or someone living in this household own this dwelling?

# own ag land
table(NN$hh.land)
table(MS$q312)
MS <- MS %>%
  mutate(hh.land = case_when(
    q312 == "Yes" ~ "Yes",
    q312 %in% c("No", "Don't know") ~ "No",
    TRUE ~ NA_character_
  ),
  hh.land = factor(hh.land, levels = c("No", "Yes")))
# check 
table(MS$q312, MS$hh.land, useNA = "ifany")

# own livestock (individual types listed)
table(NN$hh.animal) # Woman's household owns livestock, herds or farm animals
table(MS$q313_animals_livestock) 
table(MS$q313_animals_herds)
table(MS$q313_animals_poultry)
table(MS$q313_animals_other)

MS <- MS %>%
  mutate(hh.animal = case_when(
    q313_animals_livestock == "Yes" |
      q313_animals_herds == "Yes" |
      q313_animals_poultry == "Yes" |
      q313_animals_other == "Yes" ~ "Yes",
    q313_animals_livestock == "No" &
      q313_animals_herds == "No" &
      q313_animals_poultry == "No" &
      q313_animals_other == "No" ~ "No",
    TRUE ~ NA_character_
  ),
  hh.animal = factor(hh.animal, levels = c("No", "Yes")))
# check 
table(MS$hh.animal, useNA = "ifany")

##############
# Save data 
##############

saveRDS(MS, "../data/MS_mapped.rds")
saveRDS(NN, "../data/NN_mapped.rds")
