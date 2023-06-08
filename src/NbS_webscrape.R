library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

### get WHO 53 countries
euro_countries <- read_xlsx("/Users/charlie/IDAlert_NBS_Indicator/data/[LCDE 2024] Country names and groupings.xlsx", skip =1)
euro_countries <- euro_countries %>% 
  select(`Country name`)


## unacity 2

get_pageinfo <- function(page_link) {
  
  socio_phrases <- c("Social justice and cohesion",
                     "Cultural heritage and sense of place",
                     "Education",
                     "Unknown",
                     "Other",
                     "Safety")
  
  ch_phrases <- c("Green space",                                      
                  "\\bhabitats and biodiversity \\(SDG 15\\)\\b",               
                  "Social justice",
                  "\\bcohesion and equity \\(SDG 10\\)\\b",                    
                  "Cultural heritage and cultural diversity",         
                  "Regeneration",                                     
                  "land-use and urban development",                   
                  "\\bSustainable consumption and production \\(SDG 12\\)\\b",  
                  "Climate action for adaptation",                    
                  "\\bresilience and mitigation \\(SDG 13\\)\\b",               
                  "Environmental quality",                            
                  "\\bEconomic development and employment \\(SDG 8\\)\\b",      
                  "\\bWater management \\(SDG 6\\)\\b",                         
                  "\\bCoastal resilience and marine protection \\(SDG 14\\)\\b",
                  "\\bInclusive and effective governance \\(SDG 16\\)\\b")
  
  intervention_page <- read_html(page_link) 
  
  name <- intervention_page %>% html_nodes("h1") %>% html_text()
  location <- intervention_page %>% html_nodes(".nbs-location strong") %>% 
    html_text()
  duration <- intervention_page %>% html_nodes(".nbs-duration") %>% 
    html_text()
  scale <- intervention_page %>% html_nodes(".nbs-scale") %>% 
    html_text()
  all_solutions <- intervention_page %>% html_nodes(".nbs-box-33 li") %>% 
    html_text() %>% paste(collapse = ", ")
  solutions <- intervention_page %>% html_nodes(".nbs-box-33 .depth-1") %>% 
    html_text() %>% paste(collapse = ", ")
  key_solutions <- intervention_page %>% html_nodes(".nbs-box-33 .depth-0") %>% 
    html_text() %>% paste(collapse = ", ")
  challenges <- intervention_page %>% html_nodes(".nbs-box-33+ .nbs-box ") %>% 
    html_text() %>% paste(collapse = ", ")
  key_challenges <- intervention_page %>% html_nodes(".nbs-box-33+ .nbs-box .depth-0") %>% 
    html_text() %>% paste(collapse = ", ")
  focus <- intervention_page %>% html_nodes("div.nbs-section-overview .nbs-longtext:nth-child(2) div") %>% 
    html_text() %>% paste(collapse = ", ")
  env_impacts <- intervention_page %>% html_nodes("div.nbs-section-monitoring .nbs-box:nth-child(1) li") %>% 
    html_text() %>% paste(collapse = ", ")
  socio_impacts <- intervention_page %>% html_nodes("div.nbs-section-monitoring .nbs-box~ .nbs-box+ .nbs-box li") %>% 
    html_text()
  key_socio_impacts <- intervention_page %>% html_nodes("div.nbs-section-monitoring .nbs-box~ .nbs-box+ .nbs-box .depth-0") %>% 
    html_text() %>% paste(collapse = ", ")
  conservation <- intervention_page %>% html_nodes(".nbs-biodiversity-conversation .depth-1") %>% 
    html_text() %>% paste(collapse = ", ")
  key_conservation <- intervention_page %>% html_nodes(".nbs-biodiversity-conversation .depth-0") %>% 
    html_text() %>% paste(collapse = ", ")
  restoration <- intervention_page %>% html_nodes(".nbs-biodiversity-restoration .depth-0") %>% 
    html_text() %>% paste(collapse = ", ")
  
  duration <- intervention_page %>% html_nodes(".nbs-duration") %>% 
    html_text() %>% str_split_fixed(": ", n=2)
  duration <- duration[2] %>% str_split_fixed(" –", n=2)
  begin_year <- duration[1]
  scale <- intervention_page %>% html_nodes(".nbs-scale") %>% 
    html_text() %>% str_split_fixed(": ", n=2)
  scale <- scale[2] %>% str_split_fixed("\n", n=2)
  scale <- scale[1]
  
  
  
  
  start_index <- which(grepl("Health and wellbeing", socio_impacts))
  if (length(start_index) > 0) {
    sublist <- socio_impacts[start_index:length(socio_impacts)]
    matching_indices <- grep(paste(socio_phrases, collapse = "|"), sublist)
    if (length(matching_indices) > 0) {
      sublist <- sublist[seq_len(matching_indices[1] - 1)]
      sublist <- sublist[-1]
      socio_impacts <- sublist %>% paste(collapse = ", ")
    } else {
      sublist <- sublist[-1]
      socio_impacts <- sublist %>% paste(collapse = ", ")
    }
  } else {
    socio_impacts <- socio_impacts %>% paste(collapse = ", ")
  }
  
  #cleaning challenges
  challenges <- strsplit(challenges, "\n")[[1]]
  challenges <- trimws(challenges)
  challenges <- challenges[challenges != ""]
  start_index_ch <- which(grepl("\\bHealth and well-being \\(SDG 3\\)\\b", challenges))
  
  # Extract the sublist starting from "Health and well-being (SDG 3)"
  if (length(start_index_ch) > 0) {
    sublist_ch <- challenges[start_index_ch:length(challenges)]
    matching_indices_ch <- grep(paste(ch_phrases, collapse = "|"), sublist_ch)
    if (length(matching_indices_ch) > 0) {
      trimmed_sublist_ch <- sublist_ch[seq_len(matching_indices_ch[1] - 1)]
      trimmed_sublist_ch <- trimmed_sublist_ch[-1]
      challenges <- trimmed_sublist_ch %>% paste(collapse = ", ")
    } else {
      sublist_ch <- sublist_ch[-1]
      challenges <- sublist_ch %>% paste(collapse = ", ")
    }
  } else {
    challenges <- challenges %>% paste(collapse = ", ")
  }
  
  
  
  nbs_page <- data.frame(name, location, begin_year, scale, solutions, key_solutions, all_solutions, challenges, key_challenges, focus, env_impacts, socio_impacts, key_socio_impacts, conservation, key_conservation, restoration)
  
  return(nbs_page)
}

nbs_df <- data.frame()

for (page_number in seq(from = 0, to = 77, by = 1)) {
  link <- paste("https://una.city/?search_api_fulltext=&page=", page_number, sep = "")
  page <- read_html(link)
  
  nbs_pages <- page %>% html_nodes("h3 a") %>%
    html_attr("href") %>%
    paste("https://www.una.city", ., sep="")
  
  for (i in nbs_pages) {
    region_check <- read_html(i)
    country <- region_check %>% html_nodes(".nbs-location strong") %>% 
      html_text() %>% str_split_fixed(", ", n=2)
    country = country[2]
    
    if (country %in% euro_countries$`Country name`) {
      pageinfo <- get_pageinfo(i)
      nbs_df <- rbind(nbs_df, pageinfo)
    }
    
  }
  
  print(paste("Page:", page_number))
}



write_rds(nbs_df, "data/nbs_df.rds")



