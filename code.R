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

#Brand Key
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

parent_id <- brand_to_parent %>%
              distinct(new_name, id) %>%
              filter(id != "") %>%
              distinct(new_name, .keep_all = T)

length(unique(parent_id$new_name))

brand_to_parent_2 <- brand_to_parent %>%
  left_join(parent_id, by = "new_name") %>%
  distinct(brand_name, new_name, id.y)

length(unique(brand_to_parent_2$brand_name))

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
  filter(count > 1)

write.csv(not_unique, "not_unique_brands.csv")


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
brands <- read.csv("BrandsCombined-2022_Refined.csv", encoding = "UTF-8", #quote = "\\",#quote = "", 
               # row.names = NULL, 
                stringsAsFactors = FALSE)

brands_plus <- read.csv("BrandsCombined_2022.csv", 
                        encoding = "UTF-8", 
                        stringsAsFactors = FALSE) %>%
  select(row_id, parent_company) %>%
  rename(parent_company_orig = parent_company) %>%
  left_join(brands)

joined <- safe_inner_join(brands_plus, events %>% 
                              rename(event_total_count = total_count) %>%
                              filter(type_of_audit == "Outdoor"), check = "~uymn") 
table(events$type_of_audit)

#Fix nulls


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

joined_clean <- joined %>%
    left_join(matched_rows) %>%
    mutate(id = ifelse(!is.na(null_id), null_id, id)) %>%
    mutate(name = ifelse(!is.na(null_name), null_name, name)) %>%
    mutate(proportion = as.numeric(total_count) / as.numeric(event_total_count)) %>%
    filter(!is.na(proportion)) %>%
    filter(!year %in% c(2001, 2012, 2017)) %>%
    inner_join(group_by(., submission_type, submission_id, year) %>%
                   summarise(sum = sum(proportion)) %>%
                   filter(sum == 1)) %>%
    mutate(event_id = paste0(submission_type, "_", submission_id, "_", year)) %>%
    mutate(name = ifelse(name == "" & parent_company_orig == "NULL", brand_name, name)) %>%
    mutate(name = ifelse(name == "" & parent_company == "Unbranded", "Unbranded", name)) %>%
    mutate(name = ifelse(name == "", parent_company_orig, name)) %>%
    select(-file_name)  %>%
    filter(event_total_count > 2)

null_leftover <- inner_join(unmatched_nulls, joined_clean)
sum(as.numeric(null_leftover$total_count))/sum(as.numeric(joined_clean$total_count))
length(unique(joined_clean$brand_name))
length(unique(null_leftover$brand_name))

write.csv(null_leftover, "null_leftover.csv")

key_for_review <- joined_clean %>%
  group_by(brand_name, name, id) %>%
  summarise(sum = sum(as.numeric(total_count))) %>%
  ungroup() %>%
  filter(name != "Unbranded")

write.csv(key_for_review, "key_for_review.csv")

#Proportion with ids
sum(key_for_review$sum[key_for_review$id != ""])/sum(key_for_review$sum)

str(joined_clean)

write.csv(joined_clean, "joined_clean.csv")

raw_meta_search <- joined_clean %>%
  select(brand_name, parent_company_orig, item_description, type_product, type_material, type_of_audit, country, year, name, id) %>%
  distinct() %>%
  filter(id == "" & name != "Unbranded")

#Unbranded metrics ----
unbranded <- joined_clean %>%
    filter(name == "Unbranded")

unbranded_clean <- unbranded %>%
                        group_by(event_id, name) %>%
                        summarize(proportion = sum(proportion)) %>%
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
