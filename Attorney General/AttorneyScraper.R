#Libraries
library(rvest)
library(stringr)
library(tidyverse)
library(RCurl)
library(plyr)
library(rlang)
library(writexl)
library(magrittr)
library(stringi)

#Functions
`%notin%` <- Negate(`%in%`)

#Get the link from the table (30 states and 2 territories)
tablePage <- read_html("https://ballotpedia.org/Attorney_General_elections,_2022")
totalRaceLinks <- tablePage %>% html_nodes("a") %>% html_attr("href")
links <- totalRaceLinks[139:147]
links <- c(links, totalRaceLinks[149:169])
links <- c(links, "/Guam_Attorney_General_election,_2022")
links <- c(links, "/Northern_Mariana_Islands_Attorney_General_election,_2022")
RaceLinks <- vector()
StateList <- vector()
for(i in 1:32){
  link <- paste("https://ballotpedia.org",links[i],sep = "")
  state <- link %>% str_extract("(?<=(org/)).+?(?=(_Att))")%>% str_replace_all("_", " ")
  RaceLinks <- c(RaceLinks, link)
  StateList <- c(StateList, state)
}
rm(tablePage, totalRaceLinks, links, i, link, state)

#Get the candidate's information from the race website
namesFin <- data.frame()
for(i in 1:32){
  url <- RaceLinks[i] #Gets the Ballotpedia URL for a state or territory
  webpage <- read_html(url) #Reads the URL
  
  #Retrieves the election type, year, and candidate names for all elections on the page
  states <- html_nodes(webpage,  '.votebox-header-election-type, .votebox-results-cell--text , .results_text, #Past_elections') 
  
  names <- html_text(states) #Changes the list to a vector
  
  #Reads how many candidates there are for 2022
  rows <- if(is_empty(names)==TRUE){0
  }else{if(TRUE %in% grepl("Past elections|primary|convention", names))
  {ifelse(min(grep("Past elections|primary|convention", names))==1, 
          0, min(grep("Past elections|primary|convention", names))-1)
  }else{length(names)}} 
  
  #Extracts names based on rows
  names <- if(rows %in% c(0,1)){0}else{names[1:rows]}
  
  #Creates a data frame with each candidates name, district, and a link for their individual candidate web page
  names <- data.frame(Names=if(rows %in% c(0,1)){"NA"}else{
    if(is_empty(grep("\t\t\t", names))){names[1:(rows-2)]}else{
      names[grep("\t\t\t", names)] %>% str_remove_all("\\\t")}}) %>% 
    mutate(Names = ifelse(Names=="NA", NA, str_remove_all(Names, "\\(.*") %>% str_trim()),
           State = StateList[i])
  linkpage <- read_html(url) %>% html_nodes(".votebox-results-cell--text") %$% data.frame(hrefs=as(., "character"))
  names$CandPage <- if(TRUE %in% is.na(names$Names)){NA}else{linkpage[1:nrow(names), 1] %>% str_extract("https.+?(?=(\"))") %>% str_sub( 1, -1)}
  namesFin <- rbind.fill(namesFin, names) #Adds state to total frame
}
namesFin["CandPage"][namesFin["CandPage"] == "https://ballotpedia.org/William_Robinson"] <- "https://ballotpedia.org/William_Robinson_(Colorado)"
namesFin <- namesFin %>% distinct() # remove duplicates
rm(linkpage, RaceLinks, states, names, webpage, url, rows)

#Get more information from the candidate's website
Handles <- data.frame()
for(i in 1:nrow(namesFin)){
  #Goes to candidate links (if working)
  if(is.na(namesFin[i,1])==FALSE & url.exists(namesFin[i, 3])==TRUE){
    InfoPage <- read_html(namesFin[i, 3])
    links <- InfoPage %>% html_nodes("a") %$% data.frame(hrefs=as(., "character")) #Reads webpage
    
    #Identifies Campaign Website
    CampWeb <- links[grep("Campaign website|Official website|Personal website", links$hrefs), 1] %>% str_extract("http.+?(?=(\" t))")
    CampWeb <- ifelse(is_empty(CampWeb)==TRUE, NA, CampWeb)
    
    #Identifies Campaign Twitter Handle
    CampHand <- links[grep("www.twitter.*Camp", links$hrefs), 1] %>% str_remove(".*com\\/") %>% str_remove("(\").*")
    CampHand <- ifelse(is_empty(CampHand)==TRUE, NA, CampHand)
    
    #Identifies Official Twitter Handle
    OffHand <- links[grep("www.twitter.*Off", links$hrefs), 1] %>% str_remove(".*com\\/") %>% str_remove("(\").*")
    OffHand <- ifelse(is_empty(OffHand)==TRUE, NA, OffHand)
    
    #Identifies Personal Twitter Handle
    PerHand <- links[grep("www.twitter.*Pers", links$hrefs), 1] %>% str_remove(".*com\\/") %>% str_remove("(\").*")
    PerHand <- ifelse(is_empty(PerHand)==TRUE, NA, PerHand)
    
    #Identifies Party
    partyLink <- InfoPage %>% html_nodes(".widget-row.value-only") %$% data.frame(hrefs=as(., "character"))
    Party <- ifelse(is_empty(grep("black", partyLink$hrefs))==FALSE,
                    partyLink[max(grep("black", partyLink$hrefs)),1] %>% str_sub(49,-15),
                    ifelse(is_empty(grep("Party|Independent|Unaffiliated|Nonpartisan", partyLink$hrefs)),
                           links[max(grep("Category.*Party|Independent|Unaffiliated", links$hrefs)), 1] %>% str_remove(".*Category:") %>% str_remove("(\").*"),
                           partyLink[max(grep("Party|Independent|Unaffiliated|Nonpartisan", partyLink$hrefs)),1]%>% 
                             str_extract("only.*(\")") %>% str_sub(6,-2)))
    
    Party <- ifelse(is_empty(Party)==TRUE, NA, Party)
    
    #Creates the dataframe
    df <- data.frame(Names=namesFin[i,1], CampWeb, CampHand, OffHand, PerHand, Party)
  }else {df <- data.frame(Names= namesFin[i,1], CampWeb = NA, CampHand = NA, OffHand = NA, PerHand = NA, Party=NA)} #Accounts for non-working links
  Handles <- rbind(Handles, df)
}

namesFin <- cbind(namesFin, Handles %>% select(-Names))

rm(df, links, CampHand, CampWeb, StateList, i, OffHand, Party, PerHand)

namesFin <- namesFin %>% mutate(Party = Party %>% str_remove(" (page does not exist)"))