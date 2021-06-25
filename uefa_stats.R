# devtools::install_github("hadley/emo")
library(emo)
library(flexdashboard)
library(DT)
library(tidyverse)
library(dplyr)
library(htmltools)
library(htmlwidgets)
library(RColorBrewer)
#devtools::install_github(repo = "vadimus202/datatableEZ")
# library(datatableEZ)

dat <- read.csv("uefa_dat.csv")
names(dat)
dat <- dat %>% dplyr::select(-X)

dat <- dat %>% mutate(country_emoji_text = 
                        case_when(country=="North Macedonia" ~ "macedonia",
                                  country=="Turkey" ~ "tr",
                                  TRUE ~ tolower(gsub(" ", "_", country))))

dat$country_emoji <- map_chr(dat$country_emoji_text, emo::ji)
dat$country2 <- paste0(dat$country, " ", dat$country_emoji)

dat$pass_accuracy <- round(dat$passes_completed / dat$passes_total, 2)*100

names(dat)
datx <- dat %>% select(-c(country, country_emoji_text, country_emoji))

datx <- datx %>% select(player, country2, club, position, age, 
                        goals, assists, total_attempts, on_target,
                        distance_covered_km, top_speed_km_h,
                        passes_total, pass_accuracy, 
                        tackles, blocks, balls_recovered, clearances_completed,
                        fouls_committed, fouls_suffered)

table(datx$country2)
head(datx)
str(datx)

# View(emo::ji_find("flag"))
d <- datatable(datx,
               # caption = htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black; font-size:200% ;','EURO 2020 Player Stats'),
               colnames = c("Player" = "player",
                            "Country" = "country2",
                            "Club" = "club",
                            "Position" = "position",
                            "Age" = "age",
                            "Goals" = "goals",
                            "Assists" = "assists",
                            "Attempts" = "total_attempts",
                            "Attempts on target" = "on_target",
                            "Distance covered" = "distance_covered_km",
                            "Top speed" = "top_speed_km_h",
                            "Total passes" = "passes_total",
                            "Pass completion" = "pass_accuracy",
                            "Tackles" = "tackles",
                            "Blocks" = "blocks",
                            "Balls recovered" = "balls_recovered",
                            "Clearances" = "clearances_completed",
                            "Fouls committed" = "fouls_committed",
                            "Fouls suffered" = "fouls_suffered"),
          #font = "Helvetica",
          #style = "bootstrap",
          class = 'hover compact stripe',
          callback=JS("table.on( 'order.dt search.dt', function () {
                                table.column(0, {search:'applied', order:'applied'}).nodes().each( function (cell, i) {
                                      cell.innerHTML = i+1;});}).draw();"),
          extensions = c('Buttons', 
                         'Scroller', 
                         'Select', 'SearchPanes'),
          options = list(dom = 'Btip',
                         pageLength = 30,
                         buttons = list(list(extend = "colvis",
                                             columns = c(6:19),
                                             text="Select stats"),
                                        list(
                                          extend = "searchPanes",
                                          config = list(
                                            dtOpts = list(
                                              paging = FALSE
                                            )))),
                         deferRender = TRUE, 
                         #scrollY = "450px",
                         scrollX = TRUE,
                         scrollCollapse = TRUE,
                         paging = TRUE,
                         #autoWidth = TRUE,
                         language = list(searchPanes = list(collapse = "Filter"),
                                         colvis = list(collapse = "test")),
                         columnDefs = list(
                           list(width = "15px", targets=c(0,5)),
                           list(width = "200px", targets=c(1)),
                           list(width = "100px", targets=c(2:4)),
                           list(width = "15px", targets=c(7:19)),
                           list(visible = FALSE, targets=c(7:9,10:11,14:19)),
                           list(searchPanes = list(show = FALSE), targets = c(1,5:19)), 
                           # list(searchPanes = list(show = FALSE), targets = 0:5),
                           list(searchPanes = list(show = TRUE, controls=TRUE), targets = c(2,3,4))
                         ))) %>% 
  DT::formatString(columns=c(10), suffix="km") %>%
  DT::formatString(columns=c(11), suffix="km/h") %>%
  DT::formatString(columns=c(13), suffix="%")


d
colors <- colorRampPalette(brewer.pal(min(sum(num),9), "Pastel1"))(14)

for (i in 6:19){
    #If numeric add to the datatabls
    d <- d %>%
      formatStyle(c(i),
                  background = styleColorBar(range(datx[,i], na.rm=TRUE), colors[i-5]),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
    }


d

temp <- d %>% htmlwidgets::prependContent(
  htmltools::tags$head(list(
    htmltools::tags$link(href = "custom_css.css", rel = "stylesheet")
  ))
)

temp <- temp %>% htmlwidgets::prependContent(
  HTML(
    "<body><h1 style = 'font-family: Helvetica;'>EURO 2020 Player Stats</h1></body>"
  ))

temp <- temp %>% htmlwidgets::appendContent(
  HTML(
    "<body><span style = 'font-family: Helvetica;'>Stats from <a href='https://www.uefa.com/uefaeuro-2020/teams/'>UEFA</a></span></body>"
  ))

saveWidget(temp,"euro-2020-stats.html")
