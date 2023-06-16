
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
library(ggrepel)
library(ggtext)


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


#Read Data ----

events <- read.csv("github_data/events_scrubbed.csv", encoding = "UTF-8", #quote = "", comment.char = "\\",
                   row.names = NULL, 
                   stringsAsFactors = FALSE) 

brands <- read.csv("github_data/BrandsCombined_2022.csv")

brands_plus <- read.csv("github_data/BrandsCombined_2022.csv", 
                        encoding = "UTF-8", 
                        stringsAsFactors = FALSE) %>%
  mutate(brand_name = trimws(tolower(brand_name))) %>%
  select(row_id, submission_type, submission_id, year, brand_name, parent_company, item_description, type_product, type_material, layer, total_count)

brand_to_parent_2 <- read.csv("github_data/key-for-review_2.csv")

brand_to_parent <- read.csv("github_data/cleanup_brand_to_parent.csv") %>%
  select(-X)

joined_clean <- read.csv("github_data/joined_clean.csv")

brands_validated2 <- read.csv("github_data/todo1.csv") 

brands_validated_raw <- read.csv("github_data/brands_validated.csv") %>%
  filter(!brand_name %in% brands_validated2$brand_name) %>%
  bind_rows(brands_validated2 %>% select(-brand_total_count, -brand_frequency))

unique(brands_validated_raw$brand_name) |> length()

wikidata_ids2 <- fread("github_data/wikidata_ids2.csv")

locations_to_code <- fread("github_data/locations_to_code.csv")

load(file = "github_data/raw_processed_data.RData")

boot_name <- fread("github_data/brand_name.csv")

elen_data <- fread("github_data/2022-Progress-Report-Data-Sheet-Final2_WC.csv") %>%
  mutate(ID = tolower(ID)) %>%
  inner_join(boot_name, by = c("ID" = "id")) %>%
  mutate(mass = as.numeric(gsub(",", "", `2021 total weight of new packaging (metric tonnes)`))) %>%
  mutate(`Company name` = ifelse(stringi::stri_enc_isutf8(elen_data$`Company name`), `Company name`, "Loreal"))

population_data_2019 <- read.csv("github_data/country_population_data/API_SP.POP.TOTL_DS2_en_csv_v2_4902028_WC.csv")

event_list <- fread("github_data/event_list.csv")

raw_processed_data_event_ag <- fread("github_data/raw_processed_data_event_ag.csv")

raw_prop_id_event <- fread("github_data/raw_prop_id_event.csv")

raw_processed_data_event_ag_2 <- fread("github_data/raw_processed_data_event_ag_2.csv")

brand_company_id <- fread("github_data/brand_company_id.csv")

#Run this if needed. 
#fwrite(brand_company_id |> filter(brand_frequency > 10, brand_total_count > 100, validated == "false"), "todo.csv")

#Clean Data ----
##Brand Key Creation -----
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

cleanup_brand_to_parent <- brand_to_parent_2 %>%
  group_by(brand_name) %>%
  summarise(parent_company_name = toString(new_name), 
            id = toString(id), 
            sum = sum(sum)) %>%
  left_join(joined_clean %>% select(brand_name, country)) %>%
  group_by(brand_name, parent_company_name, id, sum) %>%
  summarise(country = toString(country))

sum(cleanup_brand_to_parent$sum[cleanup_brand_to_parent$id != ""])/sum(cleanup_brand_to_parent$sum)

write.csv(cleanup_brand_to_parent, "github_data/cleanup_brand_to_parent.csv")

##Join updated list and clean ----
joined <- inner_join(brands_plus, events %>% 
                              rename(event_total_count = total_count) %>%
                              filter(type_of_audit == "Outdoor")) 

unjoined <- anti_join(brand_to_parent, joined, by = "brand_name")

#Test
unique(brand_to_parent$brand_name) |> length() == length(brand_to_parent$brand_name)

joined_clean <- joined %>%
    left_join(brand_to_parent %>% select(brand_name, parent_company_name, id)) %>%
    filter(as.numeric(event_total_count) > 2) %>%
    filter(as.numeric(total_count) > 0) %>%
    mutate(proportion = as.numeric(total_count) / as.numeric(event_total_count)) %>%
    filter(!is.na(proportion)) %>%
    filter(!year %in% c(2001, 2012, 2017)) %>%
    inner_join(group_by(.data = ., submission_type, submission_id, year) %>%
                   summarise(sum = sum(proportion)) %>%
                   filter(round(sum, 3) == 1)) %>%
    mutate(event_id = paste0(submission_type, "_", submission_id, "_", year)) %>%
    select(-file_name) 


#Test diff between brand_to_parent and joined_clean sums
test_joined <- joined_clean %>%
  group_by(brand_name) %>%
  summarise(total_sum_joined_clean = sum(as.numeric(total_count))) %>%
  full_join(brand_to_parent) %>%
  ungroup() %>%
  mutate(is_equal = total_sum_joined_clean == sum) %>%
  mutate(difference = total_sum_joined_clean - sum)

#I think brand to parent was actually from before 2022, that explains why the values are almost always less and why there are new brands not present from before. I started the analysis in 2022 before the new data came on board. 
  
table(test_joined$is_equal)
summary(test_joined$difference)

#Proportion with ids
sum(as.numeric(joined_clean$total_count)[joined_clean$id != ""], na.rm = T)/sum(as.numeric(joined_clean$total_count), na.rm = T)

str(joined_clean)

fwrite(joined_clean, "github_data/joined_clean.csv")

#check that brands are either validated or not, not both. 

unique(brands_validated_raw$brand_name) |> length()

brands_validated <- brands_validated_raw %>%
  #filter(!(brand_name == "lotus" & validated == "FALSE")) %>%
  select(brand_name, parent_company_name, id, validated) %>%
  distinct(brand_name, .keep_all = T) %>% #Takes care of potential duplicate brands. 
  right_join(joined_clean %>% select(-id) %>% rename(parent_company_name_old = parent_company_name)) %>%
  mutate(total_count = as.integer(total_count)) %>%
  mutate(event_total_count = as.integer(event_total_count)) %>%
  mutate(validated = ifelse(is.na(validated), FALSE, validated)) %>%
  mutate(validated = ifelse(is.na(parent_company_name) & parent_company == "Unbranded", TRUE, validated)) %>%
  mutate(parent_company_name = ifelse(is.na(parent_company_name) & parent_company == "Unbranded", "unbranded", parent_company_name)) %>%
  mutate(parent_company_name = ifelse(is.na(parent_company_name), parent_company, parent_company_name)) %>%
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

##Harvest wikidata names ----
#Needs to be run if there are any new wikidata ids
wikidata_ids <- brands_validated %>%
  distinct(id) %>%
  filter(grepl("q(\\d{2,})", id) & !grepl("http", id)) #%>%

for(row in 1:nrow(wikidata_ids)){
  print(row)
  try(
    wikidata_ids[row, "name"] <- paste(vapply(unlist(str_extract_all(wikidata_ids[row, "id"], "q(\\d{2,})")), function(x) {trimws(tolower(WikidataR::find_item(x)[[1]]$label))}, FUN.VALUE = character(1)), collapse = ", "),
    silent = T
  )
}  

wikidata_ids2 <- wikidata_ids %>%
  mutate(name = ifelse(is.na(name), id, name))

fwrite(wikidata_ids2, "github_data/wikidata_ids2.csv")

##Harvest locations ----
# Needs to be run if there are any new locations
#locations_to_code <- brands_validated %>%
#  distinct(city, province, country, continent) %>%
#  mutate(across(everything(), ~gsub("null", "", .x))) %>%
#  geocode(city = city, state = province, country = country, method = 'osm', lat = latitude_specific, long = longitude_specific) %>%
#  geocode(country = country, method = 'osm', lat = latitude_country, long = longitude_country)  

#locations_to_code <- locations_to_code %>%
#  geocode(state = province, country = country, method = 'osm', lat = latitude_state, long = longitude_state)

#fwrite(locations_to_code, "github_data/locations_to_code.csv")

test_one <- brands_validated |>
  distinct(brand_name, validated) |>
  group_by(brand_name) |> 
  summarise(validated_concat = toString(validated)) |>
  filter(!validated_concat %in% c("true", "attempted", "false"))

table(test_one$validated_concat)

raw_processed_data <- brands_validated %>%
  mutate(validated = ifelse(brand_name %in% test_one$brand_name, "false", validated)) %>%
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
  mutate(proportion = total_count/event_total_count) %>%
  mutate_if(is.character, ~ifelse(.x %in% c("null", ""), NA, .x)) %>%
  select(-time_spent) %>%
  mutate(country = ifelse(country == "korea", "south korea", country))

#Check for missing lat/long
summary(raw_processed_data$latitude_most_specific) #Still 4k NAs

Samples_Map <- raw_processed_data %>%
  distinct(longitude_most_specific, latitude_most_specific, event_id) %>%
  filter(!is.na(longitude_most_specific) & !is.na(latitude_most_specific)) %>% 
  st_as_sf(coords = c("longitude_most_specific", "latitude_most_specific"), crs = 4326, remove = FALSE)

mapview(Samples_Map,  legend = FALSE)

brand_company_id <- raw_processed_data %>%
  group_by(brand_name, parent_company_name, id, validated) %>%
  summarise(brand_total_count = sum(total_count), brand_frequency = n()) %>%
  ungroup() %>%
  group_by(brand_name, validated) %>%
  mutate(brand_parent_occurances = n()) %>%
  group_by(brand_name, validated, brand_parent_occurances) %>%
  summarise(parent_company_name = toString(parent_company_name),
            id = toString(id),
            brand_total_count = sum(brand_total_count),
            brand_frequency = sum(brand_frequency)) %>%
  ungroup()

#test number to re-analyze
number <- brand_company_id %>% 
  mutate(priority = brand_frequency > 10 & validated == "false" & brand_total_count > 100) %>%
  filter(priority)

fwrite(brand_company_id, "github_data/brand_company_id.csv")
fwrite(number, "github_data/todo2.csv")

event_list <- raw_processed_data %>%
  distinct(event_id, city, province, country, continent, year, specifics_of_audit, event_total_count)

fwrite(event_list, "github_data/event_list.csv")

raw_prop_id_event <- raw_processed_data %>%
  select(event_id, id,  proportion)

fwrite(raw_prop_id_event, "github_data/raw_prop_id_event.csv")

# Validate raw data ----

#check that number of brand names in raw processed and brand_company_id are the same. 
length(unique(raw_processed_data$brand_name)) == nrow(brand_company_id)

#Test that counts haven't changed. 
sum(brand_to_parent$sum) == 681918 #Definitely different. Smaller by a lot. Due to unknowns being gone. 
sum(brands_validated$total_count) == 1873634
sum(as.numeric(joined_clean$total_count)) == 1873634
sum(as.numeric(raw_processed_data$total_count)) == 1873634

#test if reanalysis is needed. 
nrow(number) == 0

#test if brands are uniquely matched in brand company id. 
unique(brand_company_id$brand_name) |> length() == length(brand_company_id$brand_name)

#brands are only validated or not. 
raw_processed_data |>
  distinct(brand_name, validated) |>
  group_by(brand_name) |> 
  summarise(validated_concat = toString(validated)) |>
  filter(!validated_concat %in% c("true", "attempted", "false")) |>
  nrow() == 0

sum(is.na(raw_processed_data$parent_company_name)) == 0 #No NA parent_company names
sum(is.na(raw_processed_data$id)) == 0 #No NA IDs

#No null ids
!any(raw_processed_data$id == "null")
!any(raw_processed_data$id == "NULL")
!any(raw_processed_data$id == "")

#counts add up to one. 
raw_processed_data %>%
  group_by(event_id) %>%
  summarise(proportion_sum = sum(proportion)) %>%
  ungroup() %>%
  mutate(is_one = round(proportion_sum, 3) == 1) %>%
  pull(is_one) %>%
  all()

#valid inputs for controlled character values. 
all(unique(raw_processed_data$validated) %in% c("attempted", "true", "false")) #Only allowed values for validated. 

#Check to make sure no new weird types. 
all(unique(raw_processed_data$submission_type) %in% c("Excel Template Old",
                                                  "Excel Template 2020",
                                                  "Excel Template 2021",
                                                  "123Forms Old",
                                                  "123Forms New",
                                                  "Trashblitz New",
                                                  "ThirdParty 2022",
                                                  "ThirdParty 2020")) 

#Valid years
all(raw_processed_data$year > 2017 & raw_processed_data$year < 2023) 

all(unique(raw_processed_data$type_product) %in% c(
                                               "other",
                                               "food packaging",
                                               "household products",
                                               "smoking materials",
                                               "personal care",
                                               "fishing gear",
                                               "packaging materials")| is.na(unique(raw_processed_data$type_product))) 

all(unique(raw_processed_data$type_material) %in% c("pet",
                                                "o",
                                                "pvc",
                                                "hdpe",
                                                "pp",
                                                "ldpe",
                                                "ps") | is.na(unique(raw_processed_data$type_material)))

all(unique(raw_processed_data$layer) %in% c("single-layer",
                                        "multi-layer",
                                        "unsure") | is.na(unique(raw_processed_data$layer)))

all(raw_processed_data$total_count > 0)

all(unique(raw_processed_data$is_trained) %in% c("no", "yes") | is.na(unique(raw_processed_data$is_trained)))

unique(raw_processed_data$type_of_audit) == "outdoor" 

all(unique(raw_processed_data$specifics_of_audit) %in% c("inland",
                                                   "coastal",
                                                   "freshwater",
                                                   "other") | is.na(unique(raw_processed_data$specifics_of_audit)))


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

#warning/quality checks
lapply(raw_processed_data, function(x){sum(is.na(x))})
#unique match between brands and ids in raw_processed_data, allowed if the count and occurance of the brand is small. 
test_unique_brand_id_match <- raw_processed_data %>%
  distinct(brand_name, id) %>%
  group_by(brand_name) %>%
  summarise(count = n()) %>%
  filter(count > 1)

nrow(test_unique_brand_id_match) == 0


#Write after validation
fwrite(raw_processed_data, "github_data/raw_processed_data.csv")
save(raw_processed_data, file = "github_data/raw_processed_data.RData")

# Data Analysis ----
## Unbranded metrics ----
unbranded <- raw_processed_data %>%
    filter(id == "unbranded")

unbranded_clean <- unbranded %>%
                        dplyr::group_by(event_id, id) %>%
                        dplyr::summarize(proportion = sum(proportion)) %>%
                        ungroup()

#mean percent unbranded
mean(unbranded_clean$proportion)

#CIs around percent unbranded. 
BootMean(unbranded_clean$proportion)

##Global proportions. ----

raw_processed_data_event_ag <- raw_processed_data %>%
                    filter(id != "unbranded") %>%
                    group_by(event_id, id, city, province, country, continent, year, specifics_of_audit) %>%
                    summarise(company_sum = sum(total_count)) %>%
                    ungroup() %>%
                    group_by(event_id) %>%
                    mutate(event_total_count_no_unbrand = sum(company_sum)) %>%
                    ungroup() %>%
                    mutate(proportion = company_sum/event_total_count_no_unbrand) 

fwrite(raw_processed_data_event_ag, "github_data/raw_processed_data_event_ag.csv")


proportion_grid <- expand.grid(event_id = unique(raw_processed_data_event_ag$event_id), 
                               id = unique(raw_processed_data_event_ag$id))

raw_processed_data_event_ag_2 <- right_join(event_list, proportion_grid) %>%
    left_join(raw_prop_id_event) %>%
    mutate(proportion = ifelse(is.na(proportion), 0, proportion)) %>%
    ungroup()
  
fwrite(raw_processed_data_event_ag_2, "github_data/raw_processed_data_event_ag_2.csv")

#Counts are log normally distributed meaning a few of the larger surveys could gobble up the smaller ones. 

#Possibly a count bias per year. 
event_list %>%
  distinct(event_id, year) %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  ggplot() +
  geom_point(aes(y = count, x = year), size = 4) +
  scale_x_log10() +
  theme_bw(base_size = 15) +
  labs(y = "Number of Events", x = "Year")

#summary stats, some countries have more events than others which would rate them lower. 
event_list %>%
            distinct(event_id, country) %>%
            group_by(country) %>%
            summarise(count = n()) %>%
        ggplot() +
        geom_point(aes(x = count, y = reorder(country, count))) +
        scale_x_log10() +
        theme_bw(base_size = 9) +
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
    slice_max(mean, n = 100) %>%
    left_join(brand_company_id)

fwrite(small_boot_name %>% distinct(parent_company_name, id, mean), "github_data/small_brand_name.csv")

sum(boot_name$mean)
    
boot_name_sorted <- boot_name %>%
  mutate(rank = nrow(.) - rank(mean) + 1) %>%
  arrange(desc(mean)) %>%
  mutate(cumulative = cumsum(mean))

ggplot(boot_name_sorted) +
  geom_line(aes(y = cumulative*100, x = rank), linewidth = 3) +
  scale_x_log10() + 
  theme_classic(base_size = 20) +
  labs(x = "Number of Companies", y = "Cumulative Percent Contribution")

fwrite(boot_name, "github_data/brand_name.csv")

#Returns decrease exponentially for including more companies. 
ggplot(small_boot_name %>% filter(mean > 0.01), aes(y = reorder(parent_company_name, mean), x = mean*100)) +
  geom_point() +
  geom_errorbar(aes(xmin=low*100, xmax=high*100)) + 
  theme_classic(base_size = 20) +
  labs(x = "Mean Percent of Total Branded Waste", y = "Company")

## Correlation Elen ----
ggplot(elen_data %>%
         bind_rows(mutate(., `Sector (EMF input)` = "All")), aes(x = mass, y = mean, color = `Sector (EMF input)`, label = "Company name")) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  theme_classic(base_size = 15) +
  theme(legend.position = "none") +
  geom_smooth(method = "lm") +
  facet_grid(.~ `Sector (EMF input)`)


ggplot(elen_data, aes(x = mass, y = mean*100)) +
  geom_text_repel(aes(label = `Company name`), size = 2, max.overlaps = 100)+
  geom_point() +
  geom_smooth(method = "lm") +
  coord_fixed() +
  scale_x_log10(limits = c(1000,10000000)) +
  scale_y_log10(breaks = 10^(-5:2), limits = c(0.000001, 100)) +
  labs(x = "2021 Total Plastic Mass Produced (metric tonnes)", y = "Mean Percent of Total Branded Waste") +
  theme_classic(base_size = 15) 
  
hist(log10(elen_data$mean))
hist(log10(elen_data$mass))
full_model = lm(log10(mean)~log10(mass), data = elen_data)
summary(full_model)

fwrite(elen_data, "github_data/elen_data.csv")
#Interpretation https://kenbenoit.net/assets/courses/ME104/logmodels2.pdf

# Stats reported in paper and other validation checks ----

#total number of items recorded
sum(raw_processed_data$total_count)

#total number of branded items recorded
sum(raw_processed_data_event_ag$company_sum)

# total number of events
nrow(event_list)

# total number of countries
unique(event_list$country)

# total number of brands
length(unique(raw_processed_data$brand_name))

nrow(brand_company_id)

# total number and proportions of validated brands 
brand_company_id %>%
  group_by(validated) %>%
  summarise(total = sum(brand_total_count), n = sum(brand_frequency)) %>%
  mutate(prop_count = total/sum(total), prop_n = n/sum(n)) |>
  pull(total) |>
  sum()

#total number of locations specificities
raw_processed_data |>
  distinct(event_id, location_specificity) |>
  pull(location_specificity) |>
  table() 

raw_processed_data %>%
  group_by(validated) %>%
  summarise(total = sum(total_count), n = n()) %>%
  mutate(prop = total/sum(total))

# Extra Data Cleaning/Creation Scripts ----
##Cleanup Events to Anon Data ----
#events <- read.csv("EventsCombined_2022.csv", encoding = "UTF-8", #quote = "", comment.char = "\\",
#                   row.names = NULL, 
#                   stringsAsFactors = FALSE)

#event_scrub <- events %>%
#    select(-first_name, -last_name, -email, -phone) %>%
#    mutate(organization = as.numeric(as.factor(organization)), name_of_lead = as.numeric(as.factor(name_of_lead)))

#write.csv(event_scrub, "events_scrubbed.csv")

#dat2018 <- read.csv("Forbes Global 2000 - 2018.csv") %>%
#    mutate(year = 2018) %>%




