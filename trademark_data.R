setwd("C:/Users/winco/OneDrive/Documents/BFFP/data")

#More here https://www.uspto.gov/ip-policy/economic-research/research-datasets/trademark-assignment-dataset
#Could be useful https://branddb.wipo.int/en/similarname?strategy=exact&brandName=&start=0&_=1673843200814
#https://www.uspto.gov/ip-policy/economic-research/research-datasets/trademark-case-files-dataset


library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(stringdist)
library(googlesheets4)
library(future.apply)

#Data cleanup ----
owner <- fread("owner.csv")
statement <- fread("statement.csv")

joined <- inner_join(owner, statement)

joined_small <- joined[str_detect(joined[["statement_type_cd"]], pattern = regex("PM", ignore_case = T)),] %>%
  distinct(own_name, statement_text) %>%
  mutate(statement_text = trimws(tolower(statement_text))) %>%
  group_by(statement_text) %>%
  summarise(companies = paste0(own_name, collapse = "||"))

fwrite(joined_small, "owner_lookup.csv")

#Name Matching ----
joined_small <- fread("owner_lookup.csv") 

unique_statements <- unique(c(joined_small$statement_text, joined_small$companies))

googlesheets4::gs4_deauth()

sheet_to_id <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1C3jBlcW-Mlc6PEte42lusXJ2kP4m892S4Yny9oKP0lA/edit?usp=sharing")

new_names <- left_join(sheet_to_id, joined_small, by = c("brand_name" = "statement_text")) %>%
  mutate(brands_pho = phonetic(brand_name)) %>%
  left_join(joined_small %>%
              mutate(statement_text_pho = phonetic(statement_text)) %>%
              group_by(statement_text_pho) %>%
              summarise(companies = paste0(companies, collapse = "||")), 
            by = c("brands_pho" = "statement_text_pho"))

fwrite(new_names %>%
         select(-parent_company_name), "new_names.csv")


#plan("multisession")
which(statements == sheet_to_id$brand_name)
row_ids <- vapply(sheet_to_id$brand_name, function(x){
  which(statements == x)
  }, 
  FUN.VALUE = 2)

sheet_to_id$us_trade_text = joined_small[["statement_text"]][row_ids]
sheet_to_id$us_trade_owners = joined_small[["companies"]][row_ids]

cola <- joined_small[str_detect(joined_small[["statement_text"]], pattern = regex("coca-cola", ignore_case = T)),]


most_owners <- sort(table(joined_small$own_name))

tail(most_owners)
