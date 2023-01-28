setwd("C:/Users/winco/OneDrive/Documents/BFFP/data")

#More here https://www.uspto.gov/ip-policy/economic-research/research-datasets/trademark-assignment-dataset
#Could be useful https://branddb.wipo.int/en/similarname?strategy=exact&brandName=&start=0&_=1673843200814
#https://www.uspto.gov/ip-policy/economic-research/research-datasets/trademark-case-files-dataset


library(data.table)
library(dplyr)
library(tidyr)
library(stringr)

owner <- fread("owner.csv")
statement <- fread("statement.csv")

joined <- inner_join(owner, statement)

joined_small <- joined[str_detect(joined[["statement_type_cd"]], pattern = regex("(DM)|(PM)", ignore_case = T)),] %>%
  distinct(serial_no, own_name, statement_text)

fwrite(joined_small, "owner_lookup.csv")

most_owners <- sort(table(joined_small$own_name))

tail(most_owners)

cola <- joined_small[str_detect(joined_small[["statement_text"]], pattern = regex("coca-cola", ignore_case = T)),]
