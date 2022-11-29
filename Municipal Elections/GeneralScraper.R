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

# Create the table that contains all the race informations 
tablePage <- read_html("https://ballotpedia.org/United_States_municipal_elections,_2022#By_state")
totalRaceLinks <- tablePage %>% html_nodes("a") %>% html_attr("href")
links <- totalRaceLinks[166:385]
RaceLinks <- vector()
RaceNames <- vector()
LocList <- vector()
for(i in 1:length(links)){
  link <- paste("https://ballotpedia.org",links[i],sep = "")
  race <- ifelse(is_empty(grep("local", link)) ==FALSE,
                 "Local",
                 link %>% str_extract("(?<=(org/)).+?(?=(_ele))")%>% str_replace_all("_", " "))
  location <- ifelse(is_empty(grep("local", link)) ==FALSE,
                     link %>% str_extract("(?<=(org/)).+?(?=(_local))")%>% str_replace_all("_", " "),
                     link %>% str_extract("(?<=(in_)).+?(?=(2022))")%>% str_sub(1,-3) %>% str_replace_all("_", " "))
  RaceLinks <- c(RaceLinks, link)
  RaceNames <- c(RaceNames, race)
  LocList <- c(LocList, location)
}
TotalRaces <- data.frame(Links = RaceLinks, Races = RaceNames, Locations = LocList)
TotalRaces <- TotalRaces %>% distinct() # remove duplicates
Total <- split(TotalRaces, TotalRaces$Races)
rm(tablePage, totalRaceLinks, links, i, link, race, location, RaceLinks, RaceNames, LocList, TotalRaces)

#Mayoral Elections
MRaceLinks <- Total[["Mayoral"]][["Links"]]
MCityList <- Total[["Mayoral"]][["Locations"]]

#Get the candidate's information from the race website
MNamesFin <- data.frame()
for(i in 1:length(MRaceLinks)){
  url <- MRaceLinks[i] #Gets the Ballotpedia URL for a district
  webpage <- read_html(url) #Reads the URL
  
  #Retrieves the election type, year, and candidate names for all elections on the page
  districts <- html_nodes(webpage,  '.votebox-header-election-type, .votebox-results-cell--text , .results_text') 
  
  names <- html_text(districts) #Changes the list to a vector
  
  #Reads how many candidates there are for 2022
  rows <- if(is_empty(names)==TRUE){0
  }else{if(TRUE %in% grepl("primary", names))
  {ifelse(min(grep("primary", names))==1, 
          0, min(grep("primary", names))-1)
  }else{length(names)}} 
  
  #Extracts names based on rows (Also accounts for new districts in 2022)
  names <- if(rows %in% c(0,1)){0}else{names[1:rows]}
  
  #Creates a data frame with each candidates name, district, and a link for their individual candidate web page
  names <- data.frame(Names=if(rows %in% c(0,1)){"NA"}else{if(is_empty(grep("\t\t\t", names))){names[1:(rows-2)]}else{names[grep("\t\t\t", names)] %>% str_remove_all("\\\t")}}) %>% 
    mutate(Names = ifelse(Names=="NA", NA, str_remove_all(Names, "\\(.*") %>% str_trim()),
           City = MCityList[i])
  linkpage <- read_html(url) %>% html_nodes(".votebox-results-cell--text") %$% data.frame(hrefs=as(., "character"))
  names$CandPage <- if(TRUE %in% is.na(names$Names)){NA}else{linkpage[1:nrow(names), 1] %>% str_extract("https.+?(?=(\"))") %>% str_sub( 1, -1)}
  MNamesFin <- rbind.fill(MNamesFin, names) #Adds district to total frame
}
MNamesFin <- MNamesFin %>% distinct() # remove duplicates
rm(linkpage, MRaceLinks, MCityList, districts, names, webpage, url, rows)

#Get more information from the candidate's website
Handles <- data.frame()
for(i in 1:nrow(MNamesFin)){
  #Goes to candidate links (if working)
  if(is.na(MNamesFin[i,1])==FALSE & url.exists(MNamesFin[i, 3])==TRUE){
    InfoPage <- read_html(MNamesFin[i, 3])
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
                           links[max(grep("Category.*Party|Independent|Unaffiliated|Nonpartisan", links$hrefs)), 1] %>% str_remove(".*Category:") %>% str_remove("(\").*"),
                           partyLink[max(grep("Party|Independent|Unaffiliated|Nonpartisan", partyLink$hrefs)),1]%>% 
                             str_extract("only.*(\")") %>% str_sub(6,-2)))
    
    Party <- ifelse(is_empty(Party)==TRUE, NA, Party)
    
    #Creates the dataframe
    df <- data.frame(Names=MNamesFin[i,1], CampWeb, CampHand, OffHand, PerHand, Party)
  }else {df <- data.frame(Names= MNamesFin[i,1], CampWeb = NA, CampHand = NA, OffHand = NA, PerHand = NA, Party=NA)} #Accounts for non-working links
  Handles <- rbind(Handles, df)
}

MNamesFin <- cbind(MNamesFin, Handles %>% select(-Names))

rm(df, links, CampHand, CampWeb, i, OffHand, Party, PerHand)

MNamesFin <- MNamesFin %>% mutate(Party = Party %>% str_remove(" (page does not exist)"))

#City Elections
CRaceLinks <- Total[["City"]][["Links"]]
CCityList <- Total[["City"]][["Locations"]]

#Get the candidate's information from the race website
CNamesFinTest <- data.frame()
for(i in 1:length(CRaceLinks)){
  url <- CRaceLinks[i] #Gets the Ballotpedia URL for a district
  webpage <- read_html(url) #Reads the URL
  #Get the link for Canadiates
  if(grepl("Nevada", CCityList[i])){
    texts <- webpage %>% html_nodes(".results_text")
    texts <- texts[!grepl("primary", texts)] %$% data.frame(contents=as(., "character"))
    pageNames <- data.frame()
    for(row in texts$contents){
      race <- row %>% str_extract("((City )|(Comm)|(Mun)).+((ly)|(il)|(ney)|(ors)|(urt))")
      temptest <- strsplit(row, "<a")
      names <- data.frame(Names = temptest[[1]][2:length(temptest[[1]])] %>% str_extract("(?<=(\">)).*?(?=(</a>))")) %>% 
        mutate(Names = ifelse(Names=="NA", NA, Names %>% str_trim()),
               City = CCityList[i],
               CandPage = temptest[[1]][2:length(temptest[[1]])] %>% str_extract("https.*?(?=(\">))"),
               Race = race)
      pageNames <- rbind.fill(pageNames, names)
    }} else {
  tableLinks <- webpage %>%
    html_nodes("h2>span, div>table>tbody>tr>td>a,
               div>table>tbody>tr>td>b>u>a, table>tbody>tr>td>h4,
               div>div>div.race_header.nonpartisan>h5")
  tableLinks <- tableLinks[!grepl("<img", tableLinks)]
  startI <- grep("Ballot_access", tableLinks) +2
  endI <- min(grep("at_stake", tableLinks))
  tableLinks <- tableLinks[startI:endI]
  titleIndex <- grep("mw-headline", tableLinks)
  titleIndex <- append(titleIndex,length(tableLinks))
  pageNames <- data.frame()
  if(length(titleIndex) < 2){
    races <- webpage %>% html_nodes(".votebox-header-election-type")
    race <- as.character(races[1])%>% str_extract("(?<=(City )|(Comm)).+((ly)|(il)|(ney)|(ors))")
    names <- data.frame(Names = html_text(tableLinks[1:(length(tableLinks)-1)])) %>% 
      mutate(Names = ifelse(Names=="NA", NA, Names %>% str_trim()),
             City = CCityList[i],
             CandPage = as.character(tableLinks[1:(length(tableLinks)-1)]) %>% str_extract("https.+?(?=(\"))"),
             Race = race)
    pageNames <- rbind.fill(pageNames, names)
  }else{
  tracker <- ifelse((grepl("Colorado", CCityList[i]))|(titleIndex[length(titleIndex)]-titleIndex[length(titleIndex)-1])==1,
                    length(titleIndex)-2, length(titleIndex)-1)
  for(j in 1:tracker){
    race <- html_text(tableLinks[titleIndex[j]])
    rows <- titleIndex[j+1] - titleIndex[j]
    namesLinks <- tableLinks[(titleIndex[j]+1):(titleIndex[j]+rows-1)]
    names <- data.frame(Names = html_text(namesLinks)) %>% 
      mutate(Names = ifelse(Names=="NA", NA, Names %>% str_trim()),
             City = CCityList[i],
             CandPage = if(length(namesLinks) == 0){NA}else{
               as.character(namesLinks) %>% str_extract("https.+?(?=(\"))")},
             Race = race)
    pageNames <- rbind.fill(pageNames, names)
    # handle the special case for Colorado
    if(grepl("Colorado", CCityList[i])){
      cTableLinks <- webpage %>% html_nodes(".results_text a")
      cTableLinks <- cTableLinks[!grepl("2022", cTableLinks)]
      cNames <- data.frame(Names = html_text(cTableLinks)) %>% 
        mutate(Names = ifelse(Names=="NA", NA, Names %>% str_trim()),
               City = CCityList[i],
               CandPage = if(length(cTableLinks) == 0){NA}else{
                 as.character(cTableLinks) %>% str_extract("https.+?(?=(\"))")},
               Race = "Judicial offices")
      pageNames <- rbind.fill(pageNames, cNames)}
  }}}
  CNamesFinTest <- rbind.fill(CNamesFinTest, pageNames) #Adds district to total frame
}
CNamesFinTest <- CNamesFinTest %>% distinct() # remove duplicates
rm(linkpage, MRaceLinks, MCityList, districts, names, webpage, url, rows)

#Get more information from the candidate's website
Handles <- data.frame()
for(i in 1:nrow(MNamesFin)){
  #Goes to candidate links (if working)
  if(is.na(MNamesFin[i,1])==FALSE & url.exists(MNamesFin[i, 3])==TRUE){
    InfoPage <- read_html(MNamesFin[i, 3])
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
                           links[max(grep("Category.*Party|Independent|Unaffiliated|Nonpartisan", links$hrefs)), 1] %>% str_remove(".*Category:") %>% str_remove("(\").*"),
                           partyLink[max(grep("Party|Independent|Unaffiliated|Nonpartisan", partyLink$hrefs)),1]%>% 
                             str_extract("only.*(\")") %>% str_sub(6,-2)))
    
    Party <- ifelse(is_empty(Party)==TRUE, NA, Party)
    
    #Creates the dataframe
    df <- data.frame(Names=MNamesFin[i,1], CampWeb, CampHand, OffHand, PerHand, Party)
  }else {df <- data.frame(Names= MNamesFin[i,1], CampWeb = NA, CampHand = NA, OffHand = NA, PerHand = NA, Party=NA)} #Accounts for non-working links
  Handles <- rbind(Handles, df)
}

MNamesFin <- cbind(MNamesFin, Handles %>% select(-Names))

rm(df, links, CampHand, CampWeb, i, OffHand, Party, PerHand)

MNamesFin <- MNamesFin %>% mutate(Party = Party %>% str_remove(" (page does not exist)"))

#Municipal
#District_attorney .mw-headline