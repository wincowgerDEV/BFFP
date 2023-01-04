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

#Fix nulls
good_key <- joined %>%
  distinct(brand_name, parent_company_orig, item_description, id, name) %>%
  filter(name != "") %>%
  mutate(brand_name = trimws(tolower(brand_name))) %>%
  mutate(item_description = trimws(tolower(item_description)))

nulls <- joined %>%
  filter(parent_company_orig == "NULL") %>%
  select(brand_name, row_id, item_description) %>%
  mutate(brand_name = trimws(tolower(brand_name))) %>%
  mutate(item_description = trimws(tolower(item_description)))


matched_nulls <- inner_join(nulls, good_key, by = "brand_name") %>%
                  distinct(row_id, .keep_all = T) %>%
                  mutate()

#Phonetic
good_key_ph <- joined %>%
  distinct(brand_name, parent_company_orig, item_description, id, name) %>%
  filter(name != "") %>%
  mutate(brand_name_1 = brand_name) %>%
  mutate(item_description_1 = item_description) %>%
  mutate(brand_name = phonetic(brand_name)) %>%
  mutate(item_description = phonetic(item_description))

nulls_ph <- joined %>%
  filter(parent_company_orig == "NULL") %>%
  select(brand_name, row_id, item_description) %>%
  mutate(brand_name_2 = brand_name) %>%
  mutate(item_description_2 = item_description) %>%
  mutate(brand_name = phonetic(brand_name)) %>%
  mutate(item_description = phonetic(item_description)) 

matched_nulls <- inner_join(nulls_ph, good_key_ph) %>%
  distinct(row_id, .keep_all = T)


joined_clean <- joined %>%
    mutate(proportion = as.numeric(total_count) / as.numeric(event_total_count)) %>%
    filter(!is.na(proportion)) %>%
    filter(!year %in% c(2001, 2012, 2017)) %>%
    inner_join(group_by(., submission_type, submission_id, year) %>%
                   summarise(sum = sum(proportion)) %>%
                   filter(sum == 1)) %>%
    mutate(event_id = paste0(submission_type, "_", submission_id, "_", year)) %>%
    mutate(parent_company = ifelse(parent_company_orig == "NULL", brand_name, parent_company)) %>% #Use brand name if parent company is null.
    mutate(parent_company = tolower(gsub("[[:punct:] ]+", "", parent_company))) 


#Unbranded metrics ----
unbranded <- joined_clean %>%
    filter(parent_company == "unbranded")

unbranded_clean <- unbranded %>%
                        group_by(event_id, parent_company) %>%
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
unique(joined_clean$parent_company)


joined_clean_2 <- joined_clean %>%
                    filter(parent_company != "unbranded") %>%
                    group_by(event_id, parent_company) %>%
                    summarise(company_sum = sum(sum)) %>%
                    ungroup() %>%
                    group_by(event_id) %>%
                    mutate(event_total_count_no_unbrand = sum(company_sum)) %>%
                    ungroup() %>%
                    mutate(proportion = company_sum/event_total_count_no_unbrand)
    
proportion_grid <- expand.grid(event_id = unique(joined_clean_2$event_id), parent_company = unique(joined_clean_2$parent_company))

    
 joined_clean_3 <- right_join(joined_clean_2, proportion_grid) %>%
    mutate(proportion = ifelse(is.na(proportion), 0, proportion)) %>%
    ungroup()
    
event_list <- joined_clean_3 %>%
    group_by(event_id) %>%
    summarize(sum = sum(proportion))


#nrow(distinct(joined_clean, event_id, brand_name))
#nrow(distinct(events, submission_type, submission_id, year))

#skimr::skim(joined_clean_2)

proportion_without_mean <- joined_clean_2 %>%
    group_by(parent_company) %>%
    summarize(total_company_sum = sum(company_sum)) %>%
    ungroup() %>%
    mutate(proportion_aggregated = total_company_sum/sum(total_company_sum))

small_boot_parent_company_ag <- proportion_without_mean %>%
    filter(proportion_aggregated > 0.01)

sum(proportion_without_mean$proportion_aggregated)

boot_parent_company <- joined_clean_3 %>%
    group_by(parent_company) %>% #need to add in zeros when parent company doesn't have any values. 
    summarize(high = BootMean(proportion)[2], mean = mean(proportion), low = BootMean(proportion)[1])
    
small_boot_parent_company <- boot_parent_company %>%
    slice_max(mean, n = 10)

sum(boot_parent_company$mean)
    
ggplot(small_boot_parent_company, aes(x = parent_company, y = mean)) +
    geom_point() +
    geom_errorbar(aes(ymin=low, ymax=high)) + 
    theme_classic()

# Change through time for top 10. 

top_change <-  joined_clean_3 %>%
    filter(parent_company %in% small_boot_parent_company$parent_company) %>%
    mutate(year = gsub(".{1,}_", "", event_id))


boot_parent_company_top_year <- top_change %>%
    group_by(parent_company, year) %>% #need to add in zeros when parent company doesn't have any values. 
    summarize(high = BootMean(proportion)[2], mean = mean(proportion), low = BootMean(proportion)[1])


ggplot(boot_parent_company_top_year, aes(x = year, y = mean, color = parent_company)) +
    geom_point() +
    geom_errorbar(aes(ymin=low, ymax=high)) +
    scale_color_viridis_d() +
    facet_wrap(.~parent_company) + 
    theme_classic()


#Profits analysis ----
profit_join <- inner_join(boot_parent_company, dat2019)

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

company_cumsum <- boot_parent_company %>%
    arrange(desc(mean)) %>%
    mutate(percent_smaller = 1- 1:nrow(.)/nrow(.)) %>%
    mutate(rank = 1:nrow(.)) %>%
    mutate(cumsum_mean = cumsum(mean))

ggplot(company_cumsum) +
    geom_line(aes(x = rank, y = cumsum_mean)) + 
    scale_x_log10() + 
    theme_classic()
