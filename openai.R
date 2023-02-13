#library(reticulate)
#py_install("openai",pip=TRUE)


#devtools::install_github("ben-aaron188/rgpt3")
library(rgpt3)
library(data.table)
library(dplyr)

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
    
fwrite(material_embeddings, "G:/My Drive/MooreInstitute/Projects/TrashTaxonomy_2/material_embeddings.csv")

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

listed_files <- lapply(files, fread)

test <- listed_files[[20274]]

listed_files_character_names <- lapply(listed_files, function(x){
    x$name <- as.character(x$name)
    x
})

binded_files <- rbindlist(listed_files_character_names, fill = T)

fwrite(binded_files, "C:/Users/winco/OneDrive/Documents/BFFP/data/brand_embeddings.csv")

#ML Creation

googlesheets4::gs4_deauth()

sheet_to_id <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1C3jBlcW-Mlc6PEte42lusXJ2kP4m892S4Yny9oKP0lA/edit?usp=sharing")


