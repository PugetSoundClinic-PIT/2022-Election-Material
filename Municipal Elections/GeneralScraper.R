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
CNamesFin <- data.frame()
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
    html_nodes("h2>span, table>tbody>tr>td>a,
    div>table>tbody>tr>td>p>span>a,
    div>table>tbody>tr>td>b>u>a, 
    table>tbody>tr>td>h4,div>div>h5")
  tableLinks <- tableLinks[!grepl("<img", tableLinks)]
  startI <- min(grep("Ballot_access", tableLinks)) +2
  endI <- min(grep("at_stake|2022</h5>", tableLinks))
  tableLinks <- tableLinks[startI:endI]
  titleIndex <- grep("mw-headline", tableLinks)
  subTitleIndex <- grep("h5|h4", tableLinks)
  titleIndex <- append(titleIndex,length(tableLinks))
  subTitleIndex <- append(subTitleIndex,length(tableLinks))
  pageNames <- data.frame()
  if(length(titleIndex) < 2){
    races <- webpage %>% html_nodes(".votebox-header-election-type")
    race <- if(length(races)==0){ifelse(grepl("Seattle", CCityList[i]), "Municipal Court","City Council")
      }else{as.character(races[1])%>% str_extract("((City )|(Comm)).+((ly)|(il)|(ney)|(ors)|(sion))")}
    area <- if(length(races)==0){NA}else{
      if(grepl("District", as.character(races[1]))){
      as.character(races[1]) %>% str_extract("District.[:digit:]")
      }else if(grepl("Ward", as.character(races[1]))){
      as.character(races[1]) %>% str_extract("Ward[:digit:]")
      }else if (grepl("At-large", as.character(races[1]))){"At-large"
      }else if (grepl("Areawide", as.character(races[1]))){"Areawide"
      }else{NA}}
    if(TRUE %in% grepl("primary", tableLinks)){
      primaryI <- min(grep("primary", tableLinks))
      tableLinks <- tableLinks[1:primaryI]
    }
    tableLinks <- tableLinks[!grepl("votebox-header-election-type|h4|h5", tableLinks)]
    if(length(tableLinks)==0){next}
    names <- data.frame(Names = html_text(tableLinks[1:(length(tableLinks))])) %>% 
      mutate(Names = ifelse(Names=="NA", NA, Names %>% str_trim()),
             City = CCityList[i],
             CandPage = as.character(tableLinks[1:(length(tableLinks))]) %>% str_extract("https.+?(?=(\"))"),
             Race = ifelse(length(tableLinks)==0, NA, race))
    pageNames <- rbind.fill(pageNames, names)
  }else{
  tracker <- ifelse((grepl("Colorado", CCityList[i]))|(titleIndex[length(titleIndex)]-titleIndex[length(titleIndex)-1])==1,
                    length(titleIndex)-2, length(titleIndex)-1)
  for(t in 1:tracker){
    race <- html_text(tableLinks[titleIndex[t]])
    if(TRUE %notin% (subTitleIndex == (titleIndex[(t)]+1))){next}
    subTracker1 <- which(subTitleIndex == (titleIndex[(t)]+1))
    subTracker2 <- ifelse(t==tracker, length(subTitleIndex)-1,
                          which(subTitleIndex == (titleIndex[t+1]+1))-1)
    for(j in subTracker1:subTracker2){
      if(grepl("primary", tableLinks[subTitleIndex[j]])){next}
      #race <- html_text(tableLinks[titleIndex[j]]) %>% str_extract("((City )|(Comm)|(Anch)).+((ly)|(il)|(ney)|(ors))")
      rows <- subTitleIndex[j+1] - subTitleIndex[j]
      if(rows < 2){next}
      area <- if(grepl("District", tableLinks[subTitleIndex[j]])){
                     html_text(tableLinks[subTitleIndex[j]]) %>% str_extract("District.[:digit:]")
      }else if ( grepl("Ward", tableLinks[subTitleIndex[j]])){
        html_text(tableLinks[subTitleIndex[j]]) %>% str_extract("Ward.[:digit:]")
      }else if (grepl("Areawide", tableLinks[subTitleIndex[j]])){"Areawide"
      }else if (grepl("At-large", tableLinks[subTitleIndex[j]])){"At-large"}else{NA}
      namesLinks <- tableLinks[(subTitleIndex[j]+1):(subTitleIndex[j]+rows-1)]
      namesLinks <- namesLinks[!grepl("mw-headline", namesLinks)]
      if(length(namesLinks) == 0){next}
      names <- data.frame(Names = html_text(namesLinks)) %>% 
        mutate(Names = ifelse(Names=="NA", NA, Names %>% str_trim()),
               City = CCityList[i],
               CandPage = if(length(namesLinks) == 0){NA}else{
                 as.character(namesLinks) %>% str_extract("https.+?(?=(\"))")},
               Race = race,
               Area = area)
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
  }}}}
  CNamesFin <- rbind.fill(CNamesFin, pageNames) #Adds district to total frame
}
CNamesFin <- CNamesFin %>% distinct() # remove duplicates
CNamesFin <- CNamesFin[!is.na(CNamesFin$CandPage),]
rm(cNames, CRaceLinks, CCityList, pageNames, names, webpage, url, rows, cTableLinks)

#Get more information from the candidate's website
Handles <- data.frame()
for(i in 1:nrow(CNamesFin)){
  #Goes to candidate links (if working)
  if(is.na(CNamesFin[i,1])==FALSE & url.exists(CNamesFin[i, 3])==TRUE){
    InfoPage <- read_html(CNamesFin[i, 3])
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
    if(grepl("Mitch Perry",CNamesFin[i,1])){Party <- "Nonpartisan"}else{
    partyLink <- InfoPage %>% html_nodes(".widget-row.value-only") %$% data.frame(hrefs=as(., "character"))
    Party <- ifelse(is_empty(grep(" black", partyLink$hrefs))==FALSE,
                    partyLink[max(grep(" black", partyLink$hrefs)),1] %>% str_sub(49,-15),
                    ifelse(is_empty(grep("Party|Independent|Unaffiliated|Nonpartisan", partyLink$hrefs)),
                           links[max(grep("Category.*Party|Independent|Unaffiliated|Nonpartisan", links$hrefs)), 1] %>% str_remove(".*Category:") %>% str_remove("(\").*"),
                           partyLink[max(grep("Party|Independent|Unaffiliated|Nonpartisan", partyLink$hrefs)),1]%>% 
                             str_extract("only.*(\")") %>% str_sub(6,-2)))
    
    Party <- if(is_empty(Party)==TRUE){NA
    }else if(Party=="white"){"Nonpartisan"}
    else{Party}
    }
    #Creates the dataframe
    df <- data.frame(Names=CNamesFin[i,1], CampWeb, CampHand, OffHand, PerHand, Party)
  }else {df <- data.frame(Names= CNamesFin[i,1], CampWeb = NA, CampHand = NA, OffHand = NA, PerHand = NA, Party=NA)} #Accounts for non-working links
  Handles <- rbind(Handles, df)
}

CNamesFin <- cbind(CNamesFin, Handles %>% select(-Names))

rm(df, links, CampHand, CampWeb, i, OffHand, Party, PerHand)

CNamesFin <- CNamesFin %>% mutate(Party = Party %>% str_remove(" (page does not exist)"))

#Municipal Elections
MuRaceLinks <- Total[["Municipal"]][["Links"]]
MuCityList <- Total[["Municipal"]][["Locations"]]

#Get the candidate's information from the race website
MuNamesFin <- data.frame()
for(i in 1:length(MuRaceLinks)){
  url <- MuRaceLinks[i] #Gets the Ballotpedia URL for a district
  webpage <- read_html(url) #Reads the URL
  #Get the link for Canadiates
  tableLinks <- webpage %>%
    html_nodes("h2>span, table>tbody>tr>td>a,
    div>table>tbody>tr>td>p>span>a,
    div>table>tbody>tr>td>b>u>a, 
    table>tbody>tr>td>h4,div>div>h5")
  tableLinks <- tableLinks[!grepl("<img", tableLinks)]
  startI <- ifelse(TRUE %in% grepl("Ballot_access", tableLinks),
                   min(grep("Ballot_access", tableLinks))+2,
                   min(grep("Elections", tableLinks))+1)
  endI <- min(grep("at_stake|2022</h5>", tableLinks))
  tableLinks <- tableLinks[startI:endI]
  titleIndex <- grep("mw-headline", tableLinks)
  subTitleIndex <- grep("h5|h4", tableLinks)
  titleIndex <- append(titleIndex,length(tableLinks))
  subTitleIndex <- append(subTitleIndex,length(tableLinks))
  pageNames <- data.frame()
  if(length(titleIndex) < 2){
    races <- webpage %>% html_nodes(".votebox-header-election-type")
    race <- if(length(races)==0){"City Council"
    }else{as.character(races[1])%>% str_extract("((City )|(Comm)).+((ly)|(il)|(ney)|(ors)|(sion))")}
    area <- if(length(races)==0){NA}else{
      if(grepl("Areawide", as.character(races[1]))){"Areawide"
      }else if(grepl("Ward", as.character(races[1]))){
        as.character(races[1]) %>% str_extract("Ward[:digit:]")
      }else if (grepl("At-large", as.character(races[1]))){"At-large"
      }else if (grepl("District", as.character(races[1]))){
        as.character(races[1]) %>% str_extract("District.[:digit:]")
      }else{NA}}
    if(TRUE %in% grepl("primary", tableLinks)){
      primaryI <- min(grep("primary", tableLinks))
      tableLinks <- tableLinks[1:primaryI]
    }
    tableLinks <- tableLinks[!grepl("votebox-header-election-type|h4|h5", tableLinks)]
    if(length(tableLinks)==0){next}
    names <- data.frame(Names = html_text(tableLinks[1:(length(tableLinks))])) %>% 
      mutate(Names = ifelse(Names=="NA", NA, Names %>% str_trim()),
             City = MuCityList[i],
             CandPage = as.character(tableLinks[1:(length(tableLinks))]) %>% str_extract("https.+?(?=(\"))"),
             Race = ifelse(length(tableLinks)==0, NA, race))
    pageNames <- rbind.fill(pageNames, names)
  }else{
    tracker <- ifelse(titleIndex[length(titleIndex)]-titleIndex[length(titleIndex)-1]==1,
                      length(titleIndex)-2, length(titleIndex)-1)
    for(t in 1:tracker){
      race <- html_text(tableLinks[titleIndex[t]])
      subTracker1 <- which(subTitleIndex == (titleIndex[(t)]+1))
      subTracker2 <- ifelse(t==tracker, length(subTitleIndex)-1,
                            which(subTitleIndex == (titleIndex[t+1]+1))-1)
      if(TRUE %notin% (subTitleIndex == (titleIndex[t]+1))){next
      }else if(TRUE %notin% (subTitleIndex == (titleIndex[t+1]+1))){
        subTracker2 <- ifelse(t==tracker, length(subTitleIndex)-1,
                              which(subTitleIndex == (titleIndex[t+2]+1))-1)
      }
      for(j in subTracker1:subTracker2){
        if(grepl("primary", tableLinks[subTitleIndex[j]])){next}
        rows <- subTitleIndex[j+1] - subTitleIndex[j]
        if(rows < 2){next}
        area <- if(grepl("At-large", tableLinks[subTitleIndex[j]])){"At-large"
          }else if (grepl("Ward", tableLinks[subTitleIndex[j]])){
          html_text(tableLinks[subTitleIndex[j]]) %>% str_extract("Ward.[:digit:]")
            }else if (grepl("Areawide", tableLinks[subTitleIndex[j]])){"Areawide"
              }else if (grepl("District", tableLinks[subTitleIndex[j]])){
          html_text(tableLinks[subTitleIndex[j]]) %>% str_extract("District.[:digit:]")
                }else{NA}
        namesLinks <- tableLinks[(subTitleIndex[j]+1):(subTitleIndex[j]+rows-1)]
        namesLinks <- namesLinks[!grepl("mw-headline", namesLinks)]
        if(length(namesLinks) == 0){next}
        names <- data.frame(Names = html_text(namesLinks)) %>% 
          mutate(Names = ifelse(Names=="NA", NA, Names %>% str_trim()),
                 City = MuCityList[i],
                 CandPage = if(length(namesLinks) == 0){NA}else{
                   as.character(namesLinks) %>% str_extract("https.+?(?=(\"))")},
                 Race = race,
                 Area = area)
        pageNames <- rbind.fill(pageNames, names)
      }}}
  # handle the special case for New Mexico
  MuNamesFin <- rbind.fill(MuNamesFin, pageNames) #Adds district to total frame
  if(grepl("Adams County", MuCityList[i])){
    specialCase <- data.frame(Names = "MaryAnn Vielma",
                              City = MuCityList[i],
                              CandPage = "https://ballotpedia.org/MaryAnn_Vielma",
                              Race = "Judicial",
                              Area = area)
    MuNamesFin <- rbind.fill(MuNamesFin, specialCase)
  }else if(grepl("New Mexico", MuCityList[i])){
    cTableLinks <- webpage %>% html_nodes(".results_text a")
    cTableLinks <- cTableLinks[!grepl("2022", cTableLinks)]
    cNames <- data.frame(Names = html_text(cTableLinks)) %>% 
      mutate(Names = ifelse(Names=="NA", NA, Names %>% str_trim()),
             City = MuCityList[i],
             CandPage = if(length(cTableLinks) == 0){NA}else{
               as.character(cTableLinks) %>% str_extract("https.+?(?=(\"))")},
             Race = "Judicial offices")
    MuNamesFin <- rbind.fill(MuNamesFin, cNames[50:56,])}
}
MuNamesFin["CandPage"][MuNamesFin["CandPage"] == "https://ballotpedia.org/Kelly_Rowe"] <- "https://ballotpedia.org/Kelly_Rowe_(California)"
MuNamesFin["CandPage"][MuNamesFin["CandPage"] == "https://ballotpedia.org/Todd_Robinson"] <- "https://ballotpedia.org/Todd_Robinson_(Texas)"
MuNamesFin <- MuNamesFin %>% distinct() # remove duplicates
rm(linkpage, MuRaceLinks, MuCityList, districts, names, webpage, url, rows)

#Get more information from the candidate's website
Handles <- data.frame()
for(i in 1:nrow(MuNamesFin)){
  #Goes to candidate links (if working)
  if(is.na(MuNamesFin[i,1])==FALSE & url.exists(MuNamesFin[i, 3])==TRUE){
    InfoPage <- read_html(MuNamesFin[i, 3])
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
    df <- data.frame(Names=MuNamesFin[i,1], CampWeb, CampHand, OffHand, PerHand, Party)
  }else {df <- data.frame(Names= MuNamesFin[i,1], CampWeb = NA, CampHand = NA, OffHand = NA, PerHand = NA, Party=NA)} #Accounts for non-working links
  Handles <- rbind(Handles, df)
}

MuNamesFin <- cbind(MuNamesFin, Handles %>% select(-Names))

rm(df, links, CampHand, CampWeb, i, OffHand, Party, PerHand)

MuNamesFin <- MuNamesFin %>% mutate(Party = Party %>% str_remove(" (page does not exist)"))