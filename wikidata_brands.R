#Maybe we don't need this though if we can just use open refine to clear up some of the issues in the brand classes. 

library(WikidataR)
library(WikidataQueryServiceR)
library(data.table)

values <- unlist(str_extract_all("Q54078Q5433101", "Q[:digit:]*"))
test <- paste(vapply(unlist(str_extract_all("Q5433101", "Q[:digit:]*")), function(x) {trimws(tolower(WikidataR::get_item(x)[[1]]$label))}, FUN.VALUE = character(1)), collapse = ", ")
trimws(tolower(test[[1]]$label))
test <- WikidataR::get_item("Q5433101", language = "en")[[1]]$label

paste(vapply(unlist(str_extract_all("q3295867q2509523", "q(\\d{2,})")), function(x) {trimws(tolower(WikidataR::find_item(x)[[1]]$label))}, FUN.VALUE = character(1)), collapse = ", ")

test <- WikidataR::find_item("q3295867", language = "en")[[1]]$label

test
test$labels$en$value
find_item("Paracetamol")
find_property("medical condition treated")

brands <- query_wikidata('SELECT DISTINCT ?item ?itemLabel WHERE {
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE]". }
  {
    SELECT DISTINCT ?item WHERE {
      ?item p:P31 ?statement0.
      ?statement0 (ps:P31) wd:Q431289.
    }
    LIMIT 1000000
  }
}')


logo_without_brand <- query_wikidata('SELECT DISTINCT ?item ?itemLabel WHERE {
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE]". }
  {
    SELECT DISTINCT ?item WHERE {
      ?item p:P279 ?statement0.
      ?statement0 (ps:P279/(wdt:P279*)) wd:Q431289.
    }
  }
}')


search_all_possible <- query_wikidata('SELECT DISTINCT ?item ?itemLabel WHERE {
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
  {
    SELECT DISTINCT ?item WHERE {
      {
        ?item p:P31 ?statement0.
        ?statement0 (ps:P31/(wdt:P279*)) wd:Q167270.
      }
      UNION
      {
        ?item p:P1716 ?statement1.
        ?statement1 (ps:P1716/(wdt:P279*)) _:anyValueP1716.
      }
      UNION
      {
        ?item p:P31 ?statement2.
        ?statement2 (ps:P31/(wdt:P279*)) wd:Q431289.
      }
      UNION
      {
        ?item p:P31 ?statement3.
        ?statement3 (ps:P31/(wdt:P279*)) wd:Q783794.
      }
      UNION
      {
        ?item p:P31 ?statement4.
        ?statement4 (ps:P31/(wdt:P279*)) wd:Q4830453.
      }
    }
    LIMIT 1000000
  }
}')


fwrite(search_all_possible, "search_all_possible.csv")
empty_list <- vector(mode = "list", length = length(search_all_possible$itemLabel))

for(item in 1:length(search_all_possible$itemLabel)){
  print(item)
  empty_list[item] <- get_item(search_all_possible$itemLabel[item])
}

all_brand_info <- get_item(search_all_possible$itemLabel[1])
all_brand_info2 <- get_item(search_all_possible$itemLabel[2])
test_list <- c(all_brand_info, all_brand_info2)
