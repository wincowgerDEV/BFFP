#library(reticulate)
#py_install("openai",pip=TRUE)


#devtools::install_github("ben-aaron188/rgpt3")
library(rgpt3)
library(data.table)
library(dplyr)
library(httr)

api_key = readLines("G:/My Drive/MooreInstitute/Projects/TrashTaxonomy_2/openai.txt")

materials <- read.csv("G:/My Drive/MooreInstitute/Projects/TrashTaxonomy_2/Materials_Alias.csv")

model = 'text-embedding-ada-002'

unique_terms <- unique(materials$Alias)

embeddings <- lapply(unique_terms, function(name){
    input = name
    
    parameter_list = list(input = input, model = model)
    
    request_base = httr::POST(url = "https://api.openai.com/v1/embeddings", 
                              body = parameter_list, 
                              httr::add_headers(Authorization = paste("Bearer", api_key)),
                              encode = "json")
    
    output_base = httr::content(request_base)
    embedding_raw = to_numeric(unlist(output_base$data[[1]]$embedding))
    names(embedding_raw) = 1:1536
    data.table::as.data.table(as.list(embedding_raw)) %>%
        mutate(name = input)
})

material_embeddings <- rbindlist(embeddings)

material_dotprod <- data.table::transpose(material_embeddings, make.names = "name")

cross_product <- crossprod(data.matrix(material_dotprod))

fwrite(material_embeddings, "G:/My Drive/MooreInstitute/Projects/TrashTaxonomy_2/material_embeddings.csv")

#Search for new embedding. 
new_term <- "plastic coca cola bottle"

#Check if we already have it. 
if(!new_term %in% material_embeddings$name){
  embeddings_new <- lapply(new_term, function(name){
  input = name
  
  parameter_list = list(input = input, model = model)
  
  request_base = httr::POST(url = "https://api.openai.com/v1/embeddings", 
                            body = parameter_list, 
                            httr::add_headers(Authorization = paste("Bearer", api_key)),
                            encode = "json")
  
  output_base = httr::content(request_base)
  embedding_raw = to_numeric(unlist(output_base$data[[1]]$embedding))
  names(embedding_raw) = 1:1536
  data.table::as.data.table(as.list(embedding_raw)) %>%
    mutate(name = input)
  })
}


material_embeddings_new <- rbindlist(embeddings_new)

material_dotprod_new <- data.table::transpose(material_embeddings_new, make.names = "name")

cross_product <- crossprod(data.matrix(material_dotprod), material_dotprod_new[[1]])

#Top match for alias
top_alias = row.names(cross_product)[apply(cross_product, MARGIN = 2, FUN = which.max)]

#Top match for key
top_key = materials$Material[materials$Alias == top_alias]



## Brands ----

brands_to_embed <- read.csv("G:/My Drive/MooreInstitute/Projects/Break Free From Plastic/Code/BFFP/BrandsCombined_2022.csv", 
                            encoding = "UTF-8", 
                            stringsAsFactors = FALSE) %>%
    mutate(brand_name = trimws(tolower(brand_name))) %>%
    mutate(parent_company = trimws(tolower(parent_company))) %>%
    distinct(brand_name, parent_company)

brands_to_embed_linear <- unique(c(brands_to_embed$brand_name, brands_to_embed$parent_company))

for(name in 15487:length(brands_to_embed_linear)){
    print(name)
    Sys.sleep(0.1)
    input = brands_to_embed_linear[name]
    
    parameter_list = list(input = input, model = model)
    
    request_base = httr::POST(url = "https://api.openai.com/v1/embeddings", 
                              body = parameter_list, 
                              httr::add_headers(Authorization = paste("Bearer", api_key)),
                              encode = "json")
    
    output_base = httr::content(request_base)
    embedding_raw = to_numeric(unlist(output_base$data[[1]]$embedding))
    names(embedding_raw) = 1:1536
    fwrite(data.table::as.data.table(as.list(embedding_raw)) %>%
        mutate(name = input), paste0("C:/Users/winco/OneDrive/Documents/BFFP/data/embeddings/", name, ".csv"))
}

#bind brand embeddings

files <- list.files("C:/Users/winco/OneDrive/Documents/BFFP/data/embeddings", pattern = ".csv", full.names = T)

listed_files2 <- lapply(files, fread, header = T)

#listed_files2 <- listed_files

#test <- listed_files[[item]]

for(item in 1:length(listed_files2)){
  print(item)
  listed_files2[[item]]$name <- as.character(listed_files2[[item]]$name)
}

binded_files <- rbindlist(listed_files2, fill = T)

fwrite(binded_files, "C:/Users/winco/OneDrive/Documents/BFFP/data/brand_embeddings.csv")

#ML Creation ----
#https://www.r-bloggers.com/2021/04/random-forest-in-r/
#https://w.wiki/6LLb
#https://w.wiki/6LLi
#https://query.wikidata.org/#%23defaultView%3AGraph%0APREFIX%20gas%3A%20%3Chttp%3A%2F%2Fwww.bigdata.com%2Frdf%2Fgas%23%3E%0A%0ASELECT%20%3Fitem%20%3FitemLabel%20%3FlinkTo%20%7B%0A%7BSERVICE%20gas%3Aservice%20%7B%0A%20%20%20%20gas%3Aprogram%20gas%3AgasClass%20%22com.bigdata.rdf.graph.analytics.SSSP%22%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3Ain%20wd%3AQ723513%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3AtraversalDirection%20%22Undirected%22%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3Aout%20%3Fitem%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3Aout1%20%3Fdepth%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3AmaxIterations%2010%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3AlinkType%20wdt%3AP127.%0A%20%20%7D%7D%20UNION%0A%20%20%7BSERVICE%20gas%3Aservice%20%7B%0A%20%20%20%20gas%3Aprogram%20gas%3AgasClass%20%22com.bigdata.rdf.graph.analytics.SSSP%22%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3Ain%20wd%3AQ723513%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3AtraversalDirection%20%22Undirected%22%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3Aout%20%3Fitem%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3Aout1%20%3Fdepth%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3AmaxIterations%2010%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3AlinkType%20wdt%3AP749.%0A%20%20%7D%7D%20UNION%0A%20%20%7BSERVICE%20gas%3Aservice%20%7B%0A%20%20%20%20gas%3Aprogram%20gas%3AgasClass%20%22com.bigdata.rdf.graph.analytics.SSSP%22%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3Ain%20wd%3AQ723513%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3AtraversalDirection%20%22Undirected%22%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3Aout%20%3Fitem%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3Aout1%20%3Fdepth%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3AmaxIterations%2010%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3AlinkType%20wdt%3AP355.%0A%20%20%7D%0A%20%20%7D%0A%20%20UNION%0A%20%20%7BSERVICE%20gas%3Aservice%20%7B%0A%20%20%20%20gas%3Aprogram%20gas%3AgasClass%20%22com.bigdata.rdf.graph.analytics.SSSP%22%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3Ain%20wd%3AQ723513%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3AtraversalDirection%20%22Undirected%22%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3Aout%20%3Fitem%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3Aout1%20%3Fdepth%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3AmaxIterations%2010%20%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20gas%3AlinkType%20wdt%3AP1830.%0A%20%20%7D%0A%20%20%7D%0AOPTIONAL%20%7B%20%3Fitem%20wdt%3AP1830%20%3FlinkTo%20%7D%0AOPTIONAL%20%7B%20%3Fitem%20wdt%3AP355%20%3FlinkTo%20%7D%0AOPTIONAL%20%7B%20%3Fitem%20wdt%3AP749%20%3FlinkTo%20%7D%0AOPTIONAL%20%7B%20%3Fitem%20wdt%3AP127%20%3FlinkTo%20%7D%0ASERVICE%20wikibase%3Alabel%20%7Bbd%3AserviceParam%20wikibase%3Alanguage%20%22en%22%20%7D%0A%7D

library(randomForest)
library(caret)

binded_files <- fread("C:/Users/winco/OneDrive/Documents/BFFP/data/brand_embeddings.csv", header = T)

brand_cleanup <- fread("G:/My Drive/MooreInstitute/Projects/Break Free From Plastic/Code/BFFP/cleanup_brand_to_parent_to_model.txt")

model_data <- inner_join(brand_cleanup, binded_files, by = c("brand_name" = "name")) %>%
  mutate(id = ifelse(parent_company_name == "unbranded", parent_company_name, ifelse(parent_company_name == "other brand", parent_company_name, id))) %>%
  filter(id != "" & !is.na(id)) %>%
  mutate(parent_company = id) %>%
  select(-brand_name, -id, -link, -country, -sum, -parent_company_name) %>%
  inner_join(group_by(., parent_company) %>% summarise(count = n()) %>% filter(count > 2) %>% select(parent_company)) %>%
  na.omit(.) %>%
  mutate(parent_company = as.factor(parent_company))

names(model_data) <- paste0("V", names(model_data))

table(model_data$Vparent_company)[table(model_data$Vparent_company) > 1] %>% sort(.)

fwrite(model_data, "G:/My Drive/MooreInstitute/Projects/Break Free From Plastic/Code/BFFP/model_data.csv")

rf <- randomForest(Vparent_company~., data=model_data, proximity=TRUE)
print(rf)
plot(rf)

#Predictions
library(qs)
qsave(rf, "G:/My Drive/MooreInstitute/Projects/Break Free From Plastic/Code/BFFP/rfmodel.qs")

rf <- qread("G:/My Drive/MooreInstitute/Projects/Break Free From Plastic/Code/BFFP/rfmodel.qs")

All_Data <- brand_cleanup %>%
  left_join(binded_files, by = c("brand_name" = "name"))

names(All_Data) <- paste0("V", names(All_Data))

p1 <- predict(rf, All_Data)

write.csv(as.data.frame(p1), "G:/My Drive/MooreInstitute/Projects/Break Free From Plastic/Code/BFFP/mode_predictions.csv")

confusionMatrix(p1, model_data$Vparent_company)

MDSplot(rf, model_data$parent_company)

