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

skim_events <- skimr::skim(events)
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
                   filter(sum == 1)) %>%
    mutate(event_id = paste0(submission_type, "_", submission_id, "_", year)) %>%
    select(-file_name) 

#Proportion with ids
sum(as.numeric(joined_clean$total_count)[joined_clean$id != ""], na.rm = T)/sum(as.numeric(joined_clean$total_count), na.rm = T)

str(joined_clean)

write.csv(joined_clean, "joined_clean.csv")

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
  mutate(id = ifelse(id == "" | is.na(id), parent_company_name, id))
  
raw_processed_data <- brands_validated %>%
  select(-X.1, -row_id, -X, -parent_company_name_old)

fwrite(raw_processed_data, "raw_processed_data.csv")

# Data Analysis ----
raw_processed_data <- fread("raw_processed_data.csv")

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

# Advanced parent merges ----
#parent_grid <- expand.grid(left = unique(joined_clean$parent_company), right = unique(joined_clean$parent_company))

#parent_grid$simi <- stringsim(as.character(parent_grid$left), as.character(parent_grid$right))

#similar <- parent_grid %>%
#    filter(simi != 1 & simi > 0.8 & simi < 0.9) %>%
#    arrange(desc(simi))

#head(similar)
#there is more information nested in other columns so need to be careful about deplicates and sum up wheever possible.  

#Analyze global proportions. ----
unique(joined_clean$event_id)
unique(joined_clean$name)

joined_clean_2 <- joined_clean %>%
                    filter(name != "Unbranded") %>%
                    group_by(event_id, name, id, city, province, country, year, specifics_of_audit) %>%
                    summarise(company_sum = sum(sum)) %>%
                    ungroup() %>%
                    group_by(event_id) %>%
                    mutate(event_total_count_no_unbrand = sum(company_sum)) %>%
                    ungroup() %>%
                    mutate(proportion = company_sum/event_total_count_no_unbrand) 

proportion_grid <- expand.grid(event_id = unique(joined_clean_2$event_id), 
                               name = unique(joined_clean_2$name))

joined_clean_3 <- right_join(joined_clean_2, proportion_grid) %>%
    mutate(proportion = ifelse(is.na(proportion), 0, proportion)) %>%
    ungroup()
    
str(joined_clean_3)

event_list <- joined_clean_3 %>%
    group_by(event_id) %>%
    summarize(sum = sum(proportion))

#Counts are log normally distributed meaning a few of the larger surveys could gobble up the smaller ones. 
#Definitely a count bias per country. 
joined_clean_2 %>%
  distinct(event_id, event_total_count_no_unbrand, country) %>%
  group_by(country) %>%
  summarise(mean_count = mean(event_total_count_no_unbrand)) %>%
  ggplot() +
    geom_point(aes(x = mean_count, y = reorder(country, mean_count))) +
    scale_x_log10() 

#Definitely a count bias per year. 
joined_clean_2 %>%
  distinct(event_id, event_total_count_no_unbrand, year) %>%
  group_by(year) %>%
  summarise(mean_count = mean(event_total_count_no_unbrand)) %>%
  ggplot() +
  geom_point(aes(x = mean_count, y = reorder(year, mean_count))) +
  scale_x_log10() 

#summary stats, some countries have more events than others which would rate them lower. 
  joined_clean_3 %>%
            distinct(event_id, country) %>%
            group_by(country) %>%
            summarise(count = n()) %>%
        ggplot() +
        geom_point(aes(x = count, y = reorder(country, count))) +
        scale_x_log10() 

#nrow(distinct(joined_clean, event_id, brand_name))
#nrow(distinct(events, submission_type, submission_id, year))

#skimr::skim(joined_clean_2)

proportion_without_mean <- joined_clean_2 %>%
    group_by(name) %>%
    summarize(total_company_sum = sum(company_sum)) %>%
    ungroup() %>%
    mutate(proportion_aggregated = total_company_sum/sum(total_company_sum))

small_boot_name_ag <- proportion_without_mean %>%
    filter(proportion_aggregated > 0.01)

sum(proportion_without_mean$proportion_aggregated)

boot_name <- joined_clean_3 %>%
    group_by(name) %>% 
    summarize(high = BootMean(proportion)[2], mean = mean(proportion), low = BootMean(proportion)[1])
    
small_boot_name <- boot_name %>%
    slice_max(mean, n = 20)

sum(boot_name$mean)
    
ggplot(small_boot_name, aes(y = reorder(name, mean), x = mean)) +
    geom_point() +
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

