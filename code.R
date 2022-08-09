#Libraries ----

library(safejoin)
library(dplyr)
library(data.table)
library(readr)
library(readxl)
library(stringr)
library(ggplot2)
library(stringdist)

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
#events <- read.csv("BFFP Extract for Win - events.csv", encoding = "UTF-8", #quote = "", comment.char = "\\",
#                   row.names = NULL, 
#                   stringsAsFactors = FALSE)

#event_scrub <- events %>%
#    select(-first_name, -last_name, -email, -phone) %>%
#    mutate(organization = as.numeric(as.factor(organization)), name_of_lead = as.numeric(as.factor(name_of_lead)))

#write.csv(event_scrub, "events_scrubbed.csv")

events <- read.csv("events_scrubbed.csv", encoding = "UTF-8", #quote = "", comment.char = "\\",
                   row.names = NULL, 
                   stringsAsFactors = FALSE)

skim_events <- skimr::skim(events)
#brands <- read_xlsx("brands_2.xlsx")
brands <- read.csv("BFFP Extract for Win - brands.csv", encoding = "UTF-8", #quote = "\\",#quote = "", 
               # row.names = NULL, 
                stringsAsFactors = FALSE)

joined <- safe_inner_join(brands, events %>% rename(event_total_count = total_count), check = "~uymn")

    
joined_clean <- joined %>%
    mutate(proportion = as.numeric(total_count) / as.numeric(event_total_count)) %>%
    filter(!is.na(proportion)) %>%
    inner_join(group_by(., submission_type, submission_id, year) %>%
                   summarise(sum = sum(proportion)) %>%
                   filter(sum == 1)) %>%
    mutate(event_id = paste0(submission_type, "_", submission_id, "_", year)) %>%
    mutate(parent_company = tolower(gsub("[[:punct:] ]+", "", parent_company)))

unique(joined_clean$event_id)
unique(joined_clean$parent_company)

proportion_grid <- expand.grid(event_id = unique(joined_clean$event_id), parent_company = unique(joined_clean$parent_company))
parent_grid <- expand.grid(left = unique(joined_clean$parent_company), right = unique(joined_clean$parent_company))

parent_grid$simi <- stringsim(as.character(parent_grid$left), as.character(parent_grid$right))

similar <- parent_grid %>%
    filter(simi != 1 & simi > 0.8 & simi < 0.9) %>%
    arrange(desc(simi))

head(similar)
#there is more information nested in other columns so need to be careful about deplicates and sum up wheever possible.  
joined_clean_2 <- right_join(joined_clean %>%
                                 group_by(event_id, parent_company) %>%
                                 summarize(proportion = sum(proportion)), proportion_grid) %>%
    mutate(proportion = ifelse(is.na(proportion), 0, proportion))



nrow(distinct(joined_clean, event_id, brand_name))
nrow(distinct(events, submission_type, submission_id, year))


skimr::skim(joined_clean_2)

boot_parent_company <- joined_clean_2 %>%
    group_by(parent_company) %>% #need to add in zeros when parent company doesn't have any values. 
    summarize(high = BootMean(proportion)[2], mean = mean(proportion), low = BootMean(proportion)[1])
    

small_boot_parent_company <- boot_parent_company %>%
    filter(mean > 0.01)
    
ggplot(small_boot_parent_company, aes(x = parent_company, y = mean)) +
    geom_point() +
    geom_errorbar(aes(ymin=low, ymax=high)) 
