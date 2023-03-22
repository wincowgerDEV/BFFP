
#Libraries ----

library(safejoin)
library(dplyr)
library(data.table)
library(readr)
library(readxl)
library(stringr)
library(ggplot2)
library(stringdist)
library(fuzzyjoin)
library(WikidataR)
library(tidygeocoder)
library(mapview)
library(sf)  


#Functions ----
BootMean <- function(data) {
    B <- 100 #Change to 10k for full run
    mean <- numeric(B)
    n = length(data)
    
    set.seed(3437)
    for (i in 1:B) {
        boot <- sample(1:n, size=n, replace = TRUE)
        mean[i] <- mean(data[boot], na.rm = T)
    }
    return(quantile(mean, c(0.025,  0.975), na.rm = T))
}

BootMean_prob <- function(data, prob) {
  B <- 100 #Change to 10k for full run
  mean <- numeric(B)
  n = length(data)
  
  set.seed(3437)
  for (i in 1:B) {
    boot <- sample(1:n, size=n, prob = prob, replace = TRUE)
    mean[i] <- mean(data[boot], na.rm = T)
  }
  return(quantile(mean, c(0.025,  0.975), na.rm = T))
}



#Cleanup Events
#events <- read.csv("EventsCombined_2022.csv", encoding = "UTF-8", #quote = "", comment.char = "\\",
#                   row.names = NULL, 
#                   stringsAsFactors = FALSE)

#event_scrub <- events %>%
#    select(-first_name, -last_name, -email, -phone) %>%
#    mutate(organization = as.numeric(as.factor(organization)), name_of_lead = as.numeric(as.factor(name_of_lead)))

#write.csv(event_scrub, "events_scrubbed.csv")

#dat2018 <- read.csv("Forbes Global 2000 - 2018.csv") %>%
#    mutate(year = 2018) %>%
    
dat2019 <- read.csv("Forbes Global 2000 - 2019.csv") %>%
    mutate(parent_company = tolower(gsub("[[:punct:] ]+", "", Company)))
#dat2020 <- read_xlsx("Forbes Global 2000  - 2020.xlsx")
#dat2021 <- read_xlsx("Forbes Global 2000 - 2021.xlsx")

events <- read.csv("events_scrubbed.csv", encoding = "UTF-8", #quote = "", comment.char = "\\",
                   row.names = NULL, 
                   stringsAsFactors = FALSE) 

#skim_events <- skimr::skim(events)
#brands <- read_xlsx("brands_2.xlsx")

brands_plus <- read.csv("BrandsCombined_2022.csv", 
                        encoding = "UTF-8", 
                        stringsAsFactors = FALSE) %>%
  mutate(brand_name = trimws(tolower(brand_name))) %>%
  select(row_id, submission_type, submission_id, year, brand_name, parent_company, item_description, type_product, type_material, layer, total_count)

joined <- safe_inner_join(brands_plus, events %>% 
                              rename(event_total_count = total_count) %>%
                              filter(type_of_audit == "Outdoor"), check = "~uymn") 

brand_to_parent <- read.csv("cleanup_brand_to_parent.csv") %>%
                      select(-X)

unjoined <- anti_join(brand_to_parent, joined, by = "brand_name")

joined_clean <- joined %>%
    left_join(brand_to_parent %>% select(brand_name, parent_company_name, id)) %>%
    filter(as.numeric(event_total_count) > 2) %>%
    filter(as.numeric(total_count) > 0) %>%
    mutate(proportion = as.numeric(total_count) / as.numeric(event_total_count)) %>%
    filter(!is.na(proportion)) %>%
    filter(!year %in% c(2001, 2012, 2017)) %>%
    inner_join(group_by(., submission_type, submission_id, year) %>%
                   summarise(sum = sum(proportion)) %>%
                   filter(round(sum, 3) == 1)) %>%
    mutate(event_id = paste0(submission_type, "_", submission_id, "_", year)) %>%
    select(-file_name) 

#Proportion with ids
sum(as.numeric(joined_clean$total_count)[joined_clean$id != ""], na.rm = T)/sum(as.numeric(joined_clean$total_count), na.rm = T)

str(joined_clean)

fwrite(joined_clean, "joined_clean.csv")

#Data postprocessing workflow ----
joined_clean <- read.csv("joined_clean.csv")

brands_validated <- read.csv("brands_validated.csv") %>%
  select(brand_name, parent_company_name, id, validated) %>%
  distinct(brand_name, .keep_all = T) %>%
  right_join(joined_clean %>% select(-id) %>% rename(parent_company_name_old = parent_company_name)) %>%
  mutate(validated = ifelse(is.na(parent_company_name) & parent_company == "Unbranded", TRUE, validated)) %>%
  mutate(parent_company_name = ifelse(is.na(parent_company_name) & parent_company == "Unbranded", "unbranded", parent_company_name)) %>%
  mutate(parent_company_name = ifelse(is.na(parent_company_name), parent_company, parent_company_name)) %>%
  mutate(validated = ifelse(is.na(validated), FALSE, validated)) %>%
  mutate(parent_company_name = ifelse(parent_company_name == "NULL", brand_name, parent_company_name)) %>%
  mutate(parent_company_name = trimws(tolower(parent_company_name))) %>%
  mutate(id = ifelse(id == "" | is.na(id), parent_company_name, id)) %>%
  mutate(country = ifelse(country == "mexico", "Mexico", country),
         country = ifelse(country == "United Kingdom", "United Kingdom of Great Britain & Northern Ireland", country)) %>%
  mutate_at(c("brand_name", 
                  "parent_company_name", 
                  "id", 
                  "validated", 
                  "item_description", 
                  "type_product", 
                  "type_material", 
                  "layer", 
                  "is_trained", 
                  "city", 
                  "province", 
                  "country", 
                  "continent", 
                  "type_of_audit", 
                  'specifics_of_audit', 
                  "parent_company_name_old"), ~ trimws(tolower(.))) %>%
  mutate(type_product = case_when(
    type_product == "pp"  ~  "null",     
    type_product == "fg"  ~  "fishing gear",       
    type_product == "pc"  ~  "personal care",       
    type_product == "hp"  ~  "household products",       
    type_product == "sm"  ~  "smoking materials",       
    type_product == "pm"  ~  "packaging materials",       
    type_product == "fp"  ~  "food packaging",       
    type_product == "o"  ~  "other",       
    TRUE  ~ type_product)) %>% 
  mutate(type_material = case_when(
    type_material == "fp"  ~  "null",     
    type_material == "ml"  ~  "null",     
    type_material == "sl"  ~  "null",     
    TRUE  ~ type_material)) %>%
  mutate(layer = case_when(
    layer == "ml"  ~  "multi-layer",     
    layer == "sl"  ~  "single-layer",     
    layer == "o"  ~  "null",
    TRUE  ~ layer)) %>%
  mutate(is_trained = ifelse(is_trained == "rojaki", "null", is_trained)) %>%
  mutate(time_spent = case_when(
    time_spent < 0  ~  "null",     
    TRUE  ~ time_spent)) %>%
  mutate(specifics_of_audit = case_when(
    specifics_of_audit %in% c("brand audit",
                              "litter clean-up"
                              )  ~  "null",   
    specifics_of_audit %in% c("city / park / land", 
                              "city / park / other land", 
                              "city/park/land",
                              "land",
                              "city",
                              "landfill",
                              "market",
                              "tradisional market",
                              "school",
                              "school / office",
                              "school / office / institution",
                              "school/office",
                              "school/office (home)",
                              "mountain areas",
                              "office",
                              "park",
                              "openspace")  ~  "inland",     
    specifics_of_audit %in% c("coast / shoreline",
                              "coast/shoreline",
                              "coast / ocean / estuary",
                              "ocean")  ~  "coastal", 
    specifics_of_audit %in% c("lake", 
                              "river")  ~  "freshwater", 
    specifics_of_audit %in% c("ocean / river / lake", 
                              "ocean/river/lake",
                              "shoreline / river / lake"
                              )  ~  "other", 
    TRUE  ~ specifics_of_audit))
  
table(brands_validated$layer)
table(brands_validated$is_trained)
table(brands_validated$type_material)
table(brands_validated$type_product)
table(brands_validated$validated)
table(brands_validated$start_of_audit)
table(brands_validated$end_of_audit)
table(brands_validated$time_spent)
table(brands_validated$continent)
table(brands_validated$country)
table(brands_validated$specifics_of_audit)
table(brands_validated$end_of_audit)

wikidata_ids <- brands_validated %>%
  distinct(id) %>%
  filter(grepl("q(\\d{2,})", id) & !grepl("http", id)) #%>%

#for(row in 143:nrow(wikidata_ids)){
#  print(row)
#  try(
#    wikidata_ids[row, "name"] <- paste(vapply(unlist(str_extract_all(wikidata_ids[row, "id"], "q(\\d{2,})")), function(x) {trimws(tolower(WikidataR::find_item(x)[[1]]$label))}, FUN.VALUE = character(1)), collapse = ", "),
#    silent = T
#  )
#}  

#wikidata_ids2 <- wikidata_ids %>%
#  mutate(name = ifelse(is.na(name), id, name))

fwrite(wikidata_ids2, "C:/Users/winco/OneDrive/Documents/BFFP/global_brand_data/wikidata_ids2.csv")

wikidata_ids2 <- fread("C:/Users/winco/OneDrive/Documents/BFFP/global_brand_data/wikidata_ids2.csv")
valid_all <- expand.grid(event_id = unique(brands_validated$event_id), validated = unique(brands_validated$validated))

test_mean_percent_valid <- brands_validated %>%
  right_join(valid_all) %>%
  mutate(proportion = ifelse(is.na(proportion), 0, proportion)) %>%
  group_by(event_id, validated) %>%
  summarise(sum_prop = sum(proportion)) %>%
  group_by(validated) %>%
  summarise(mean_prop = mean(sum_prop))
  
sum(test_mean_percent_valid$mean_prop)

#locations_to_code <- brands_validated %>%
#  distinct(city, province, country, continent) %>%
#  mutate(across(everything(), ~gsub("null", "", .x))) %>%
#  geocode(city = city, state = province, country = country, method = 'osm', lat = latitude_specific, long = longitude_specific) %>%
#  geocode(country = country, method = 'osm', lat = latitude_country, long = longitude_country)  

#locations_to_code <- locations_to_code %>%
#  geocode(state = province, country = country, method = 'osm', lat = latitude_state, long = longitude_state)
  
#fwrite(locations_to_code, "C:/Users/winco/OneDrive/Documents/BFFP/global_brand_data/locations_to_code.csv")

locations_to_code <- fread("C:/Users/winco/OneDrive/Documents/BFFP/global_brand_data/locations_to_code.csv")

raw_processed_data <- brands_validated %>%
  mutate(volunteer = as.integer(volunteer)) %>%
  mutate(across(city:continent, ~gsub("null", "", .x))) %>%
  left_join(locations_to_code) %>%
  mutate(longitude_most_specific = ifelse(!is.na(longitude_specific), longitude_specific, ifelse(!is.na(longitude_state), longitude_state, longitude_country))) %>%
  mutate(latitude_most_specific = ifelse(!is.na(latitude_specific), latitude_specific, ifelse(!is.na(latitude_state), latitude_state, latitude_country))) %>%
  mutate(location_specificity = case_when(
    latitude_most_specific == latitude_specific & longitude_most_specific == longitude_specific  ~  "city",     
    latitude_most_specific == latitude_state & longitude_most_specific == longitude_state  ~  "state",     
    latitude_most_specific == latitude_country & longitude_most_specific == longitude_country ~  "country",     
    TRUE  ~ "other")) %>%
  left_join(wikidata_ids2) %>%
  mutate(parent_company_name = ifelse(!is.na(name), name, parent_company_name)) %>%
  mutate(start_of_audit = as.Date(start_of_audit, format = "%m/%d/%Y")) %>%
  mutate(end_of_audit = as.Date(end_of_audit, format = "%m/%d/%Y")) %>%
  mutate(time_spent = as.numeric(time_spent)) %>%
  mutate(time_spent = ifelse(time_spent == 0, NA, time_spent)) %>%
  select(-row_id, -X, -sum, -parent_company_name_old, -parent_company, -name, -latitude_specific, -longitude_specific, -latitude_country, -longitude_country, -latitude_state, -longitude_state) %>%
  group_by(across(c(everything(), -total_count, -proportion))) %>%
  summarise(total_count = sum(total_count)) %>%
  ungroup() %>%
  mutate(proportion = total_count/event_total_count)

summary(raw_processed_data$latitude_most_specific) #Still 4k NAs

Samples_Map <- raw_processed_data %>%
  distinct(longitude_most_specific, latitude_most_specific, event_id) %>%
  filter(!is.na(longitude_most_specific) & !is.na(latitude_most_specific)) %>% 
  st_as_sf(coords = c("longitude_most_specific", "latitude_most_specific"), crs = 4326, remove = FALSE)

mapview(Samples_Map,  legend = FALSE)

# Validate raw data ----
sum(is.na(raw_processed_data$brand_name)) == 0 #No NA brand names
sum(is.na(raw_processed_data$parent_company_name)) == 0 #No NA parent_company names
sum(is.na(raw_processed_data$id)) == 0 #No NA IDs

event_sum_1 <- raw_processed_data %>%
  group_by(event_id) %>%
  summarise(proportion_sum = sum(proportion)) %>%
  ungroup() %>%
  mutate(is_one = round(proportion_sum, 3) == 1) #Check that proportions add up to 1. 

all(event_sum_1$is_one)

all(unique(raw_processed_data$validated) %in% c("attempted", "true", "false")) #Only allowed values for validated. 

all(unique(raw_processed_data$submission_type) %in% c("Excel Template Old",
                                                  "Excel Template 2020",
                                                  "Excel Template 2021",
                                                  "123Forms Old",
                                                  "123Forms New",
                                                  "Trashblitz New",
                                                  "ThirdParty 2022",
                                                  "ThirdParty 2020")) #Check to make sure no new weird types. 

all(raw_processed_data$year > 2017 & raw_processed_data$year < 2023) #Valid years

all(unique(raw_processed_data$type_product) %in% c("null",
                                               "other",
                                               "food packaging",
                                               "household products",
                                               "smoking materials",
                                               "personal care",
                                               "fishing gear",
                                               "packaging materials"))

all(unique(raw_processed_data$type_material) %in% c("pet",
                                                "o",
                                                "pvc",
                                                "hdpe",
                                                "pp",
                                                "null",
                                                "ldpe",
                                                "ps"))

all(unique(raw_processed_data$layer) %in% c("single-layer",
                                        "null",
                                        "multi-layer",
                                        "unsure"))

all(raw_processed_data$total_count > 0)

all(unique(raw_processed_data$is_trained) %in% c("null", "no", "yes"))

all((raw_processed_data$time_spent > 0 & raw_processed_data$time_spent < 1500) | is.na(raw_processed_data$time_spent))

unique(raw_processed_data$type_of_audit) == "outdoor" 

all(unique(raw_processed_data$specifics_of_audit) %in% c("inland",
                                                   "coastal",
                                                   "null",
                                                   "freshwater",
                                                   "other"))

all(unique(raw_processed_data$is_trained) %in% c("yes",
                                                 "null",
                                                 "no"))

all(raw_processed_data$event_total_count >= raw_processed_data$total_count)

!any(is.na(raw_processed_data$event_total_count))

all(raw_processed_data$event_total_count > 0)

!any(is.na(raw_processed_data$total_count))

!any(is.na(raw_processed_data$proportion))

nrow(distinct(raw_processed_data)) == nrow(raw_processed_data)

all(raw_processed_data$longitude_most_specific >= -180 & raw_processed_data$longitude_most_specific <= 180 | is.na(raw_processed_data$longitude_most_specific))
all(raw_processed_data$latitude_most_specific >= -90 & raw_processed_data$latitude_most_specific <= 90 | is.na(raw_processed_data$longitude_most_specific))

all(unique(raw_processed_data$location_specificity) %in% c("city",    "country", "state",   "other"))

all(raw_processed_data$proportion > 0 & raw_processed_data$proportion <= 1)

# Type checks
is.character(raw_processed_data$brand_name)
is.character(raw_processed_data$parent_company_name)
is.character(raw_processed_data$id)
is.character(raw_processed_data$validated)
is.character(raw_processed_data$submission_type)
is.integer(raw_processed_data$submission_id)
is.integer(raw_processed_data$year)
is.character(raw_processed_data$item_description)
is.character(raw_processed_data$type_material)
is.integer(raw_processed_data$total_count)
is.integer(raw_processed_data$name_of_lead)
is.character(raw_processed_data$is_trained)
is.integer(raw_processed_data$organization)
is.integer(raw_processed_data$volunteer)
is.character(raw_processed_data$is_trained)
inherits(raw_processed_data$start_of_audit, "Date")
inherits(raw_processed_data$end_of_audit, "Date")
is.numeric(raw_processed_data$time_spent)
is.character(raw_processed_data$city)
is.character(raw_processed_data$country)
is.character(raw_processed_data$province)
is.character(raw_processed_data$continent)
is.character(raw_processed_data$type_of_audit)
is.character(raw_processed_data$specifics_of_audit)
is.integer(raw_processed_data$event_total_count)
is.numeric(raw_processed_data$proportion)
is.character(raw_processed_data$event_id)
is.numeric(raw_processed_data$longitude_most_specific)
is.numeric(raw_processed_data$latitude_most_specific)
is.character(raw_processed_data$location_specificity)

#Write after validation
fwrite(raw_processed_data, "C:/Users/winco/OneDrive/Documents/BFFP/global_brand_data/raw_processed_data.csv")

#New york

new_york <- raw_processed_data %>%
  filter(province == "new york" | city == "new york")

fwrite(new_york, "C:/Users/winco/OneDrive/Documents/BFFP/global_brand_data/new_york.csv")

# Data Analysis ----
raw_processed_data <- fread("C:/Users/winco/OneDrive/Documents/BFFP/global_brand_data/raw_processed_data.csv")



## Unbranded metrics ----
unbranded <- raw_processed_data %>%
    filter(id == "unbranded")

unbranded_clean <- unbranded %>%
                        dplyr::group_by(event_id, id) %>%
                        dplyr::summarize(proportion = sum(proportion)) %>%
                        ungroup()

mean(unbranded_clean$proportion)
BootMean(unbranded_clean$proportion)
hist(unbranded_clean$proportion)

#Analyze global proportions. ----
unique(raw_processed_data$event_id)
unique(raw_processed_data$name)

raw_processed_data_event_ag <- raw_processed_data %>%
                    filter(id != "unbranded") %>%
                    group_by(event_id, id, city, province, country, continent, year, specifics_of_audit) %>%
                    summarise(company_sum = sum(total_count)) %>%
                    ungroup() %>%
                    group_by(event_id) %>%
                    mutate(event_total_count_no_unbrand = sum(company_sum)) %>%
                    ungroup() %>%
                    mutate(proportion = company_sum/event_total_count_no_unbrand) 

fwrite(raw_processed_data_event_ag, "C:/Users/winco/OneDrive/Documents/BFFP/global_brand_data/raw_processed_data_event_ag.csv")

event_list <- raw_processed_data_event_ag %>%
  distinct(event_id, city, province, country, continent, year, specifics_of_audit, event_total_count_no_unbrand)

fwrite(event_list, "C:/Users/winco/OneDrive/Documents/BFFP/global_brand_data/event_list.csv")

sum(event_list$event_total_count_no_unbrand)

raw_prop_id_event <- raw_processed_data_event_ag %>%
  select(event_id, id,  proportion)

fwrite(raw_prop_id_event, "C:/Users/winco/OneDrive/Documents/BFFP/global_brand_data/raw_prop_id_event.csv")

proportion_grid <- expand.grid(event_id = unique(raw_processed_data_event_ag$event_id), 
                               id = unique(raw_processed_data_event_ag$id))

raw_processed_data_event_ag_2 <- right_join(event_list, proportion_grid) %>%
    left_join(raw_prop_id_event) %>%
    mutate(proportion = ifelse(is.na(proportion), 0, proportion)) %>%
    ungroup()
  
fwrite(raw_processed_data_event_ag_2, "C:/Users/winco/OneDrive/Documents/BFFP/global_brand_data/raw_processed_data_event_ag_2.csv")

brand_company_id <- raw_processed_data %>%
  distinct(brand_name, parent_company_name, id) %>%
  distinct(id, .keep_all = T)

fwrite(brand_company_id, "C:/Users/winco/OneDrive/Documents/BFFP/global_brand_data/brand_company_id.csv")

#Counts are log normally distributed meaning a few of the larger surveys could gobble up the smaller ones. 
#Definitely a count bias per country. 
raw_processed_data_event_ag %>%
  distinct(event_id, event_total_count_no_unbrand, country) %>%
  group_by(country) %>%
  summarise(mean_count = mean(event_total_count_no_unbrand)) %>%
  ggplot() +
    geom_point(aes(x = mean_count, y = reorder(country, mean_count))) +
  scale_x_log10() +
  theme_bw(base_size = 10) +
  labs(x = "Mean Event Count", y = "Country")

#possibly a count bias per year. 
raw_processed_data_event_ag %>%
  distinct(event_id, event_total_count_no_unbrand, year) %>%
  group_by(year) %>%
  summarise(mean_count = mean(event_total_count_no_unbrand)) %>%
  ggplot() +
  geom_point(aes(y = mean_count, x = year), size = 4) +
  scale_x_log10() +
  theme_bw(base_size = 15) +
  labs(y = "Mean Event Count", x = "Year")


#possibly a count bias per year. 
raw_processed_data_event_ag %>%
  distinct(event_id, year) %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  ggplot() +
  geom_point(aes(y = count, x = year), size = 4) +
  scale_x_log10() +
  theme_bw(base_size = 15) +
  labs(y = "Number of Events", x = "Year")

#summary stats, some countries have more events than others which would rate them lower. 
raw_processed_data_event_ag_2 %>%
            distinct(event_id, country) %>%
            group_by(country) %>%
            summarise(count = n()) %>%
        ggplot() +
        geom_point(aes(x = count, y = reorder(country, count))) +
        scale_x_log10() +
        theme_bw(base_size = 10) +
        labs(y = "Country", x = "Number of Events")

#nrow(distinct(joined_clean, event_id, brand_name))
#nrow(distinct(events, submission_type, submission_id, year))

#skimr::skim(joined_clean_2)

proportion_without_mean <- raw_processed_data_event_ag %>%
    group_by(id) %>%
    summarize(total_company_sum = sum(company_sum)) %>%
    ungroup() %>%
    mutate(proportion_aggregated = total_company_sum/sum(total_company_sum))

small_boot_name_ag <- proportion_without_mean %>%
    filter(proportion_aggregated > 0.01)

sum(proportion_without_mean$proportion_aggregated)

boot_name <- raw_processed_data_event_ag_2 %>%
    group_by(id) %>% 
    summarize(high = BootMean(proportion)[2], mean = mean(proportion), low = BootMean(proportion)[1])
    
small_boot_name <- boot_name %>%
    slice_max(mean, n = 20) %>%
    left_join(brand_company_id)

sum(boot_name$mean)
    
boot_name_sorted <- boot_name %>%
  mutate(rank = nrow(.) - rank(mean) + 1) %>%
  arrange(desc(mean)) %>%
  mutate(cumulative = cumsum(mean))

ggplot(boot_name_sorted) +
  geom_line(aes(y = cumulative, x = rank), linewidth = 3) +
  scale_x_log10() + 
  theme_classic(base_size = 20) +
  labs(x = "Number of Companies", y = "Cumulative Percent Contribution")

fwrite(boot_name, "C:/Users/winco/OneDrive/Documents/BFFP/global_brand_data/brand_name.csv")

boot_name <- fread("C:/Users/winco/OneDrive/Documents/BFFP/global_brand_data/brand_name.csv")
#Returns decrease exponentially for including more companies. 
ggplot(small_boot_name, aes(y = reorder(parent_company_name, mean), x = mean)) +
  geom_point() +
  geom_errorbar(aes(xmin=low, xmax=high)) + 
  theme_classic(base_size = 20) +
  labs(x = "Proportion", y = "Company")

# Correlation Elen ----
elen_data <- fread("G:/My Drive/MooreInstitute/Projects/Break Free From Plastic/data/Brand_progress/2022-Progress-Report-Data-Sheet-Final2_WC.csv") %>%
  mutate(ID = tolower(ID)) %>%
  inner_join(boot_name, by = c("ID" = "id")) %>%
  mutate(mass = as.numeric(gsub(",", "", `2021 total weight of new packaging (metric tonnes)`))) 

ggplot(elen_data %>%
         bind_rows(mutate(., `Sector (EMF input)` = "All")), aes(x = mass, y = mean, color = `Sector (EMF input)`, label = "Company name")) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  theme_classic(base_size = 15) +
  theme(legend.position = "none") +
  geom_smooth(method = "lm") +
  facet_grid(.~ `Sector (EMF input)`)

#library(ggrepel)

ggplot(elen_data, aes(x = mass, y = mean)) +
  geom_point() +
  #geom_text( aes(x = mass, y = mean, label = `Company name`), hjust = 0) +
  #geom_label_repel(aes(x = mass, y = mean, label = `Company name`), max.overlaps = 100) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "2021 Total Plastic Weight Produced (metric tonnes)", y = "Mean Proportion of Total Branded Waste") +
  theme_classic(base_size = 15) +
  #theme(legend.position = "none") +
  geom_smooth(method = "lm") +
  coord_fixed() 

hist(log10(elen_data$mean))
hist(log10(elen_data$mass))
full_model = lm(log10(mean)~log10(mass), data = elen_data)
summary(full_model)

fwrite(elen_data, "C:/Users/winco/OneDrive/Documents/BFFP/global_brand_data/elen_data.csv")
#Interpretation https://kenbenoit.net/assets/courses/ME104/logmodels2.pdf

#Country population boot ----
#Is this necessary? basically produces the result, complicates the analysis, and there is already decent spatial coverage. 
table(event_list$continent)
table(event_list$year)

population_data_2019 <- read.csv("country_population_data/API_SP.POP.TOTL_DS2_en_csv_v2_4902028_WC.csv")

unique(raw_processed_data_event_ag_2$country)[!unique(raw_processed_data_event_ag_2$country) %in% unique(population_data_2019$Country.Name)]

population_weighted_data <- raw_processed_data_event_ag_2 %>%
  inner_join(population_data_2019, by = c("country" = "Country.Name")) 

length(unique(population_weighted_data$event_id))

boot_name_pop_weighted <- population_weighted_data %>%
  group_by(id) %>% 
  summarize(high = BootMean_prob(proportion, prob = X2019)[2], mean = mean(proportion), low = BootMean_prob(proportion, prob = X2019)[1])


small_boot_name <- boot_name_pop_weighted %>%
  slice_max(high, n = 20) %>%
  left_join(brand_company_id)

ggplot(small_boot_name, aes(y = reorder(parent_company_name, high), x = high)) +
  #geom_point() +
  geom_errorbar(aes(xmin=low, xmax=high)) + 
  theme_classic(base_size = 20) +
  labs(x = "Proportion", y = "Company")

# Change through time for top 10. 

top_change <-  joined_clean_3 %>%
    filter(name %in% small_boot_name$name) %>%
    mutate(year = gsub(".{1,}_", "", event_id))



boot_name_top_year <- top_change %>%
    group_by(name, year) %>% 
    summarize(high = BootMean(proportion)[2], mean = mean(proportion), low = BootMean(proportion)[1])


ggplot(boot_name_top_year, aes(x = year, y = mean, color = name)) +
    geom_point() +
    geom_errorbar(aes(ymin=low, ymax=high)) +
    scale_color_viridis_d() +
    facet_wrap(.~name) + 
    theme_classic()


#Profits analysis ----
profit_join <- inner_join(boot_name, dat2019)

ggplot(profit_join, aes(x = mean, y = Profits, color = Sector)) +
    geom_point() + 
    scale_x_log10() + 
    geom_smooth(method = "lm") + 
    facet_wrap(.~Sector, scales = "free") +
    theme_classic()

ggplot(profit_join, aes(x = mean, y = Market.Value, color = Sector)) +
    geom_point() + 
    scale_x_log10() + 
    geom_smooth(method = "lm") + 
    facet_wrap(.~Sector, scales = "free") +
    theme_classic()


ggplot(profit_join, aes(x = mean, y = Revenue, color = Sector)) +
    geom_point() + 
    scale_x_log10() + 
    geom_smooth(method = "lm") + 
    facet_wrap(.~Sector, scales = "free") +
    theme_classic()



ggplot(profit_join, aes(x = mean, y = Assets, color = Sector)) +
    geom_point() + 
    scale_x_log10() + 
    geom_smooth(method = "lm") + 
    facet_wrap(.~Sector, scales = "free") +
    theme_classic()



ggplot(profit_join, aes(x = mean, y = Rank, color = Sector)) +
    geom_point() + 
    scale_x_log10() + 
    geom_smooth(method = "lm") + 
    facet_wrap(.~Sector, scales = "free") +
    theme_classic()


consumer_only_market.val <- profit_join %>%
    filter(Sector == "Consumer Staples")

#Pretty small amount, probably good for modeling that sector but not good for the overal 
sum(consumer_only_market.val$mean)

company_cumsum <- boot_name %>%
    arrange(desc(mean)) %>%
    mutate(percent_smaller = 1- 1:nrow(.)/nrow(.)) %>%
    mutate(rank = 1:nrow(.)) %>%
    mutate(cumsum_mean = cumsum(mean))

ggplot(company_cumsum) +
    geom_line(aes(x = rank, y = cumsum_mean)) + 
    scale_x_log10() + 
    theme_classic()














#Brand Key Creation -----
brands <- read.csv("BrandsCombined_2022.csv")

brand_key <- brands %>% 
  mutate(brand_name = 
           trimws(tolower(brand_name)),
         parent_company = 
           trimws(tolower(parent_company))) %>%
  distinct(brand_name, parent_company)

#test uniqueness. 
length(unique(brand_key$brand_name))

not_unique <- brand_key %>%
  group_by(brand_name) %>%
  summarise(count = n())

brand_to_parent <- read.csv("key-for-review_2.csv")

cleanup_brand_to_parent <- brand_to_parent %>%
  group_by(brand_name) %>%
  summarise(parent_company_name = toString(new_name), 
            id = toString(id), 
            sum = sum(sum)) %>%
  left_join(joined_clean %>% select(brand_name, country)) %>%
  group_by(brand_name, parent_company_name, id, sum) %>%
  summarise(country = toString(country))

sum(cleanup_brand_to_parent$sum[cleanup_brand_to_parent$id != ""])/sum(cleanup_brand_to_parent$sum)

write.csv(cleanup_brand_to_parent, "cleanup_brand_to_parent.csv")

parent_id <- brand_to_parent %>%
  distinct(new_name, id) %>%
  filter(id != "") %>%
  distinct(new_name, .keep_all = T)

length(unique(parent_id$new_name))

brand_to_parent_2 <- brand_to_parent %>%
  left_join(parent_id, by = "new_name") %>%
  distinct(brand_name, new_name, id.y)

#Test total accounted for. 
length(unique(brand_to_parent_2$brand_name))

sum(brand_to_parent_2$count)

not_unique <- brand_to_parent_2 %>%
  group_by(brand_name) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  inner_join(brand_to_parent_2)

has_id <- not_unique %>%
  filter(!is.na(id.y))

has_conflicting_ids <- has_id %>%
  group_by(brand_name) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  inner_join(has_id)

#Need to fix this. 
write.csv(has_conflicting_ids, "conflicting_ids.csv")

null_leftover <- read.csv("null_leftovers.csv") %>%
  filter(id != "")

good_key <- joined %>%
  bind_rows(null_leftover) %>%
  distinct(brand_name, id, name) %>%
  filter(name != "") %>%
  mutate(brand_name = trimws(tolower(brand_name)))

nulls <- joined %>%
  filter(parent_company_orig == "NULL") %>%
  select(brand_name, row_id, item_description) %>%
  mutate(brand_name = trimws(tolower(brand_name))) %>%
  mutate(item_description = trimws(tolower(item_description)))

matched_nulls <- inner_join(nulls, good_key, by = "brand_name") %>%
  distinct(row_id, .keep_all = T)

unmatched_nulls <- anti_join(nulls, matched_nulls)

matched_rows <- matched_nulls %>%
  select(row_id, id, name) %>%
  rename(null_id = id, null_name = name)

