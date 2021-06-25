library(httr)
library(tidyverse)
library(readr)
library(stringr)
library(janitor)
library(rvest)

# find team urls
team_url <- "https://www.uefa.com/uefaeuro-2020/teams/"
html <- paste(readLines(team_url), collapse="\n")
matched <- str_match_all(html, "<a href=\"(.*?)\"")
team_urls <- matched[[1]][15:38,2]
team_urls <- paste0("https://www.uefa.com", team_urls)
dput(team_urls)

final_output <- list()

for (i in 1:length(team_urls)){

  # loop through teams and extract player URLs
  team_name <- str_split(team_urls[i], pattern="--")[[1]][2]
  team_name <- gsub("-", " ", team_name)
  team_name <- gsub("/", "", team_name)
  team_name <- str_to_title(team_name)
  html <- paste(readLines(team_urls[i]), collapse="\n")
  matched <- str_match_all(html, "<a href=\"(.*?)\"")
  player_urls <- matched[[1]][str_which(matched[[1]][,2], "players"),2]
  player_urls <- paste0("https://www.uefa.com", player_urls)
  # replace special characters for Danish names
  player_urls <- gsub("&#230;", "æ", player_urls)
  # then encode in URL format
  player_urls <- URLencode(player_urls)

  output <- list()

  for (j in 1:length(player_urls)){
    
    # save html
    player_html <- player_urls[j] %>% read_html()
    
    # loop through players and extract stats
    stat_labels <- player_html %>% 
      html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "statistics--list--label", " " ))]') %>% 
      html_text()
  
    
    if (length(stat_labels)==0){
      output[[j]] <- NULL
    } else {
    
    stat_data <- player_html %>% 
      html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "statistics--list--data", " " ))]') %>% 
      html_text()
    
    num1 <- str_which(stat_labels, "Type of goal")
    num2 <- str_which(stat_labels, "Passing types")
    if (length(num1)==1){
      stat_labels <- stat_labels[-num1]
      stat_data <- stat_data[-num1]
    }
    
    if (length(num2)==1){
      stat_labels <- stat_labels[-num2]
      stat_data <- stat_data[-num2]
    }
    
    # extract player name
    node <- player_html %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "player-header_name", " " ))]')
    player_name <- paste0(str_extract_all(node, "(?<=>)(.*?)(?=<)")[[1]], collapse=" ")
    player_name  <- trimws(player_name)
    
    node <- player_html %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "player-profile__field", " " )) and (((count(preceding-sibling::*) + 1) = 2) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "player-profile__data", " " ))]')
    player_age <- paste0(str_extract_all(node, "(?<=>)(.*?)(?=<)")[[1]], collapse=" ")
    player_age  <- readr::parse_number(player_age)
    
    node <- player_html %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "player-profile__field", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "player-profile__data", " " ))]')
    player_club <- paste0(str_extract_all(node, "(?<=>)(.*?)(?=<)")[[1]], collapse=" ")
    
    
    # player_name1 <- str_split(player_urls[j], pattern="--")[[1]][2]
    # player_name1 <- gsub("-", " ", player_name1)
    # player_name1 <- gsub("/", "", player_name1)
    # player_name1 <- str_to_title(player_name1)
    
    # extract player position
    player_position <- player_html %>% 
      html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "player-header_category", " " ))]') %>% 
      html_text()
    
    # clean up stats into numeric format
    
    # clean up Cards variable
    num3 <- str_which(stat_labels, "Cards")
    if (length(num3)==1){
    temp <- str_split(stat_data[num3], "\r\n")
    yellow_card <- gsub(" ", "", temp[[1]][2])
    red_card <- gsub(" ", "", temp[[1]][3])
    }
    # clean up passing 
    num4 <- str_which(stat_labels, "Passing accuracy")
    if (length(num4)==1){
    stat_data[num4] <- str_split(stat_data[num4], "\r\n")[[1]][2]
    stat_data[num4] <- gsub(" ", "", stat_data[num4])
    temp <- str_split(stat_data[num4], "/")
    passes_completed <- temp[[1]][1]
    passes_total <- temp[[1]][2]
    }
    
    # clean up distance covered
    num5 <- str_which(stat_labels, "Distance covered")
    if (length(num5)==1){      
      stat_data[num5] <- readr::parse_number(stat_data[num5])
    }
    
    num6 <- str_which(stat_labels, "Minutes played")
    if (length(num6)==1){
      stat_data[num6] <- readr::parse_number(stat_data[num6])
    }
    
    num7 <- str_which(stat_labels, "Top speed")
    if (length(num7)==1){
      stat_data[num7] <- readr::parse_number(stat_data[num7])
    }
    
    

    # save full rows
    stat_data <- c(player_name, team_name, player_club, player_position, player_age, stat_data, yellow_card, red_card, passes_completed, passes_total)
    stat_labels <- c("Player", "Country", "Club", "Position",  "Age", stat_labels, "yellow_card", "red_card", "passes_completed", "passes_total")
    # create data frame
    my_dat <- data.frame(rbind(stat_data))
    names(my_dat) <- stat_labels
    rownames(my_dat) <- NULL
    my_dat <- my_dat %>% select(-c("Cards", "Passing accuracy"))
    my_dat <- my_dat %>% janitor::clean_names()

    output[[j]] <- my_dat
    }
  }
  temp <- do.call("bind_rows", output)
  rownames(temp) <- NULL
  final_output[[i]] <- temp
  temp <- NULL
}

uefa_dat <- do.call("bind_rows", final_output)
table(uefa_dat$club)
uefa_dat %>% group_by(club) %>% select(club) %>% tally() %>% arrange(-n)

write.csv(uefa_dat, "uefa_dat.csv")
