## ---------------------------------------------------------------------------------------------------
##
## Script name: app.R
## Purpose of script: Shiny app - COVID Symptom Study Sweden
## Author: Hugo Fitipaldi
## Date Created: 2020-06-01
## Updated: 2021-04-19
##
## Email: hugo.fitipaldi@med.lu.se
##
## ---------------------------------------------------------------------------------------------------
##
## Notes: This is the general structure of our dashboard only. I have excluded sensitive files and information that cannot be shared on a open repository.
##   
## ---------------------------------------------------------------------------------------------------

## Load required packages

library(shiny)
library(shinydashboard)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(rio)
library(tidyverse)
library(ggrepel)
library(lubridate)
library(plotly)
library(DT)
library(devtools)
library(plotly)
library(leaftime)
library(htmltools)
library(xml2)
library(stringr)
library(scales)


## ---------------------------------------------------------------------------------------------------

## Data pre-processing 

# Create date filter
date_filter <- as.Date('2020-05-11')
#safety_filter <- as.Date('2020-11-08')

# National maps (Län)

shapeDatanew <- readOGR("data_se/geospatial_data/scb/Lan_Sweref99TM_region.shp")
shapeDatanew <- spTransform(shapeDatanew, CRS("+proj=longlat +ellps=GRS80"))

shapeDatanew$county <- c('Stockholm', 'Uppsala', 'Södermanland', 'Östergötland', 'Jönköping', 'Kronoberg', 'Kalmar', 
                         'Gotland', 'Blekinge', 'Skåne', 'Halland', 'Västra Götaland', 'Värmland', 'Örebro', 'Västmanland', 'Dalarna',
                         'Gävleborg', 'Västernorrland',  'Jämtland', 'Västerbotten', 'Norrbotten')

shapeData <- shapeDatanew

lan_all <- rio::import('data_se/prev_data/prevalence_counties.csv')
lan_all$date <- as.Date(lan_all$date, format ="%Y-%m-%d")
lan_all$adj_prev <- lan_all$adj_prev * 100
lan_all$adj_prev_ci_low <- lan_all$adj_prev_ci_low * 100
lan_all$adj_prev_ci_high <- lan_all$adj_prev_ci_high * 100

#lan_all <- lan_all %>%
#  group_by(NAME_1) %>%
#  arrange(Datum)

lan_all <- lan_all %>% mutate(popup_info = paste("<b>Län</b>", "<br/>",  county, "<br/>", "<b>Antal som bidrar</b>","<br/>", n_users,  "<br/>", "<b>% uppskattad förekomst av symtomatisk covid-19</b>",  "<br/>", as.character(gsub("\\.", ",", round(adj_prev, 2), '%'))))

lan_all_dates <- lan_all %>%
  filter(date >= date_filter)

# -------------------------------------
#safety filter
# lan_all_dates <- lan_all_dates %>%
#   filter(Datum <= safety_filter)
# -------------------------------------

tove_df <- lan_all_dates

All_preds <- merge(shapeData , lan_all_dates,  by = 'county', duplicateGeoms = TRUE)

# Regional maps (2-digit)

shapeData2 <- readOGR("data_se/geospatial_data/2_digit_SWEDEN/pnryta2_wgs84_shp_2016-01-04.shp")
shapeData2 <- spTransform(shapeData2, CRS("+proj=longlat +ellps=GRS80"))

sverige_all <- rio::import('data_se/prev_data/prevalence_two_digit_postcodes.csv')

postnord_df <- rio::import('data_se/general/PostNord_ort-v2.csv')
names(postnord_df) <- c("POSTALCODE", "ORT")

sverige_all$date <- as.Date(sverige_all$date, format ="%Y-%m-%d")

sverige_all$adj_prev <- sverige_all$adj_prev * 100
sverige_all$adj_prev_ci_low <- sverige_all$adj_prev_ci_low * 100
sverige_all$adj_prev_ci_high <- sverige_all$adj_prev_ci_high * 100

names(sverige_all)[names(sverige_all) == 'two_digit_postcode'] <- 'POSTALCODE'

map_detailed <- merge(sverige_all, postnord_df,  by = 'POSTALCODE')

map_detailed <- map_detailed %>%
  group_by(POSTALCODE) %>%
  arrange(date)

map_detailed <- map_detailed %>% mutate(popup_info = paste("<b>Postnummer</b>", "<br/>",  POSTALCODE, "<br/>", "<b>Antal som bidrar</b>","<br/>", n_users,  "<br/>", "<b>% uppskattad förekomst av symtomatisk covid-19</b>", "<br/>", as.character(gsub("\\.", ",", round(adj_prev, 2)))," %", "</br>","<b>Ort</b>", "</br>", ORT))

map_detailed_dates <- map_detailed %>%
  filter(date >= date_filter)

# -------------------------------------
#safety filter
# map_detailed_dates <- map_detailed_dates %>%
#   filter(Datum <= safety_filter)
# -------------------------------------


maria_df <- map_detailed_dates

MapData2 <- merge(shapeData2 , map_detailed_dates,  by = 'POSTALCODE', duplicateGeoms = TRUE)

rm(list=setdiff(ls(), c("MapData2", "maria_df", "All_preds", "tove_df", "safety_filter")))

#risk.bins <-c(0, 0.5, 1, 1.5, 2, 2.5, 3, 4, 10) 

#risk.bins2 <-c(0, 0.5, 1, 1.5, 2, 2.5, 3, 4, 10)

risk.bins <-c(0, 0.1, 0.2, 0.3, 0.4, 0.6, 0.8, 1.0, 10)

risk.bins2 <- c(0, 0.1, 0.2, 0.3, 0.4, 0.6, 0.8, 1.0, 10)

pal <- colorBin( palette = c('#f9dee2','#eabcc5', '#db9ba9','#cc798d', '#bd5870','#ae3654', '#a01538', "#7a0b28", "#5E0B21"),
                 bins=risk.bins, na.color = 'dimgrey')

pal2 <- colorBin( palette = c('#f9dee2','#eabcc5', '#db9ba9','#cc798d', '#bd5870','#ae3654', '#a01538', "#7a0b28", "#5E0B21"),
                  bins=risk.bins2, na.color = 'dimgrey')

# Tables

tove_df <- tove_df[,c(1, 2, 10, 10)]
names(tove_df) <- c("Datum", "Län", "% uppskattad förekomst av symtomatisk covid-19", "infektion")
tove_df$`% uppskattad förekomst av symtomatisk covid-19` <- gsub("\\.", ",", as.character(as.numeric(round(tove_df$`% uppskattad förekomst av symtomatisk covid-19`, 2))))

tove_df2 <- maria_df[,c(2,1,9)]
names(tove_df2) <- c("Datum", "2-siffrigt område", "% uppskattad förekomst av symtomatisk covid-19")
tove_df2$`% uppskattad förekomst av symtomatisk covid-19` <- gsub("\\.", ",", as.character(as.numeric(round(tove_df2$`% uppskattad förekomst av symtomatisk covid-19`, 2))))


# Histogram and pie chart

counts <- rio::import('data_se/general/counts.csv')
counts$gender <- factor(counts$gender, levels = c("Female", "Male"), labels = c('Kvinna', 'Man'))

# Preparing data for user download

counts_ladda <- counts
names(counts_ladda) <- c('aldersgrupp', 'kon', 'total')

all_preds_ladda <- rio::import('data_se/user_download/all_preds.csv')
all_preds_2siffror <- rio::import('data_se/user_download/all_preds_2siffror.csv')

# CI's line plot
intervals_all <-  rio::import("data_se/prev_data/prevalence_nationwide.csv")
intervals_all$adj_prev <- round(intervals_all$adj_prev * 100,2)
intervals_all$adj_prev_ci_low <- round(intervals_all$adj_prev_ci_low * 100,2)
intervals_all$adj_prev_ci_high <- round(intervals_all$adj_prev_ci_high * 100,2)

# -------------------------------------
#safety filter
# intervals_all  <- intervals_all  %>%
#   filter(curr_date <= safety_filter)
# -------------------------------------

# Information from ZOE

daily_numbers <- rio::import('data_se/general/daily_numbers.csv')

# Symptoms
symptoms_df <- rio::import("data_se/general/symptoms.csv")
symptoms_df$date <- as.Date(symptoms_df$date, format = "%Y-%m-%d")

# -------------------------------------
#safety filter
# symptoms_df   <- symptoms_df   %>%
#   filter(date  <= safety_filter)
# -------------------------------------


weight_df <- rio::import('data_se/general/symptom_barplot.csv')
xform <- list(title = "", categoryorder = "array",
              categoryarray = c("Förlust av lukt/smak","Ovanlig muskelsmärta", "Feber", "Hoppat över måltider",	"Manligt kön",
                                "Ihållande hosta","Diarré och förlust lukt/smak", "Manligt kön och förlust lukt/smak","Ovanlig trötthet",
                                "Frossa/huttrar och förlust lukt/smak", "Ont eller irritation i ögon och förlust lukt/smak",
                                "Hes röst och förlust lukt/smak", "Ont i halsen och förlust lukt/smak", "Ont i halsen"))


# CRUSH
uppsala_counts <- rio::import("data_se/general/uppsala_counts.csv")
names(uppsala_counts) <- c('POSTALCODE', 'Count')
shapeData4 <- readOGR("data_se/geospatial_data/5_digit_SWEDEN/pnryta5_wgs84_shp.shp")
shapeData4 <- spTransform(shapeData4, CRS("+proj=longlat +ellps=GRS80"))

shape_uppsala <- shapeData4[which(grepl("03", shapeData4$CODE_COUNT)), ]

#shape_uppsala$fake_pos_count <- sample(1:1000, 364, replace=T)
shape_uppsala2 <- merge(shape_uppsala, uppsala_counts, by = "POSTALCODE")

#pal_uppsala <- colorQuantile("Oranges", shape_uppsala$fake_pos_count, n = 7)
#pal_uppsala <- colorQuantile("Oranges", shape_uppsala2$Count, n = 7, na.color = 'dimgrey')

#0-10, 11-50, 51-100, 101-150, 151-200, 200+
risk.bins3 <-c(0, 10, 50, 100, 150, 200, 1000)

pal_uppsala <- colorBin( palette = "Greens",
                         bins=risk.bins3, na.color = 'dimgrey')

counts_uppsala <- rio::import("data_se/general/counts_uppsala.csv")
counts_uppsala$gender <- factor(counts_uppsala$gender, levels = c("Female", "Male"), labels = c('Kvinna', 'Man'))

colors_uppsala <- c("#A35E60", "#CC8B3C")

uppsala_cum <- rio::import("data_se/general/uppsala_cumulative.csv")


# -------------------------------------
#safety filter
# uppsala_cum    <- uppsala_cum  %>%
#   filter(created_at  <= safety_filter)
# -------------------------------------

upload_time <- rio::import('data_se/general/upload_time.csv')


confidence_interval <- paste0(gsub("\\.", ",", as.character(round(tail(intervals_all$adj_prev, 1), 2))), " % ", "(", gsub("\\.", ",", as.character(round(tail(intervals_all$adj_prev_ci_low, 1), 2))), "-", gsub("\\.", ",", as.character(round(tail(intervals_all$adj_prev_ci_high, 1), 2))), ")")


# Archive

nationella_senaste <- rio::import('data_se/archive/nationella_senaste.csv')
nationella_gammal <- rio::import('data_se/archive/nationella_gammal.csv')
lan_senaste <- rio::import('data_se/archive/lan_senaste.csv')
lan_gammal <- rio::import('data_se/archive/lan_gammal.csv')
siffror_senaste <- rio::import('data_se/archive/siffror_senaste.csv')
siffror_gammal <- rio::import('data_se/archive/siffror_gammal.csv')

## ---------------------------------------------------------------------------------------------------

## App's structure

sidebar <- dashboardSidebar(
  tags$head(tags$style(HTML('.logo {
                              background-color: #8E1A35 !important;
                              }
                              .navbar {
                              background-color: #8E1A35 !important;
                              }
                              .box.box-solid.box-primary>.box-header {
                              color:#fff;
                              background:#8E1A35
                              }
                              .box.box-solid.box-primary{
                              border-bottom-color:#8E1A35;
                              border-left-color:#8E1A35;
                              border-right-color:#8E1A35;
                              border-top-color:#8E1A35;
                              }
                              .fa-mobile-alt{color:#FFFFFF
                              }
                              .fa-users{color:#FFFFFF
                              }
                              .fa-thermometer-half{color:#FFFFFF
                              }
                              '))),
  sidebarMenu(
    menuItem(tabName = "welcome", "Om COVID Symptom Study", icon = icon("info")),
    menuItem(tabName = "national", "Sverigekartor", icon = icon("map-marker-alt")),
    menuItem(tabName = "regional", "Regionala kartor", icon = icon("map-marked-alt")),
    menuItem(tabName = "trends", "Trender", icon = icon("chart-line")),
    menuItem(tabName = "symptoms", "Symtom", icon = icon("notes-medical")),
    menuItem(tabName = "participants", "Deltagare", icon = icon("user-friends")),
    menuItem(tabName = "crush", "CRUSH Covid", icon = icon("thumbtack")),
    #menuItem(tabName = "skane", "Skåne", icon = icon("map-pin")),
    menuItem(tabName = "dataRepo", "Arkiv", icon = icon("archive")),
    menuItem(tabName = "dataportalen", "Covid-19 Data Portal", icon = icon("virus")),
    #tags$a(imageOutput("covidlogo"), href = "https://www.covid19app.lu.se/")
    imageOutput("covidlogo")
  )
)


body <- dashboardBody(
  # Adding google analytics
  # tags$head(HTML(
  #   "GOOGLE ANALYTICS CODE HERE"
  # )),
  ###
  tabItems(
    tabItem(tabName = "welcome",
            h2("COVID Symptom Study Sverige - Dashboard"),
            fluidRow(
              valueBox(value = tags$p(as.character(sub("\\s+$", "", gsub('(.{3})', '\\1 ', daily_numbers$patients_total))), style = "font-size: 90%;"), subtitle = "Deltagare i Sverige", icon = icon("users"), color = "navy"),
              valueBox(value = tags$p(confidence_interval, style = "font-size: 90%;"), subtitle = "uppskattad förekomst av symtomatisk covid-19", icon = icon("thermometer-half"), color = "navy"),
              valueBoxOutput("infektionBox"),
              
              valueBox(value = tags$p(paste0(substr(as.character(daily_numbers$assessments_total), start = 1, stop = 2),",", substr(as.character(daily_numbers$assessments_total), start = 3, stop = 3) ," miljoner"), style = "font-size: 90%;"), subtitle = "Antal rapporter i Sverige", icon = icon("mobile-alt"), color = "navy")
            ),
            fluidRow(
              box(width = 6, 
                  title = span(icon("info-circle"), "Om Dashboard"), status = "primary", solidHeader = TRUE, textOutput("info-circle"),
                  "Välkommen till COVID Symptom Study Sveriges dashboard med interaktiv grafik. Du navigerar via menyn till vänster där du hittar våra senaste nationella och regionala kartor. Använd datumreglaget för att välja det datum du vill visa för kartorna. Vi arbetar hela tiden med att utöka funktionaliteten och innehållet på denna sida så vi ber om ert tålamod under tiden som sidan utvecklas. För frågor och feedback - ",
                  a(actionButton(inputId = "email1", label = "mejla oss", 
                                 icon = icon("envelope", lib = "font-awesome")),
                    href="mailto:covid-symptom-study@med.lu.se"),
                  background = "navy",
                  style = "font-size: 150%;"
              ),
              box(width = 6, 
                  title = span(icon("exclamation-triangle"), "Information"), status = "primary", solidHeader = TRUE, textOutput("exclamation-triangle"),
                  "COVID Symptom Study Sveriges resultatsida har uppdaterats den 16 augusti 2021. Uppdateringen omfattar data för hela sommarperioden (inklusive uppehållet). Tidigare resultat hittar du via flikarna i menyn.",
                  style = "font-size: 120%;")
            ),
            fluidRow(
              imageOutput("tryscreen")
            )
    ),
    
    tabItem(tabName = "national",
            h2("Sverigekarta - län med 200 eller fler deltagare"),
            fluidRow(
              # This column is the total width of the app
              column(width = 12,
                     fluidRow(
                       # This column is half the width of the app and contains the table
                       column(width = 7,
                              leafletOutput(outputId = "nationalmap", height="750px")),
                       # This column is half the width of the app and contains everything else
                       column(width = 5,
                              # This row contains the dropdown box and slider (each set to half width)
                              fluidRow(
                                column(width = 12, 
                                       sliderInput("Datum",
                                                   "Välj datum:",
                                                   width = "100%",
                                                   min = as.Date("2020-05-11","%Y-%m-%d"),
                                                   max = as.Date(max(tove_df$Datum),"%Y-%m-%d"),
                                                   value=as.Date(max(tove_df$Datum), "%Y-%m-%d"),
                                                   step = 1,
                                                   timeFormat="%Y-%m-%d"),
                                       dataTableOutput(outputId = "tove_df")
                                )
                              ),
                              fluidRow(
                                downloadButton("downloadData", "Ladda ner som .csv-fil")
                              )
                       )
                     )
              )
            )
    ),
    
    tabItem(tabName = "regional",
            h2("Regionala kartor - postnummerområden med 200 eller fler deltagare"),
            fluidRow(
              # This column is the total width of the app
              column(width = 12,
                     fluidRow(
                       # This column is half the width of the app and contains the table
                       column(width = 7,
                              leafletOutput(outputId = "regionalmap", height="750px")),
                       # This column is half the width of the app and contains everything else
                       column(
                         sliderInput("Datum2",
                                     "Välj datum:",
                                     width = "100%",
                                     min = as.Date("2020-05-11","%Y-%m-%d"),
                                     max = as.Date(max(tove_df$Datum),"%Y-%m-%d"),
                                     value=as.Date(max(tove_df$Datum)),
                                     timeFormat="%Y-%m-%d"),
                         h3("Ange de två första siffrorna i postnumret för att söka"), 
                         width = 5,
                         # This row contains the dropdown box and slider (each set to half width)
                         fluidRow(
                           column(width = 12, 
                                  dataTableOutput(outputId = "tove_df2"))
                         ),
                         fluidRow(
                           downloadButton("downloadData3", "Ladda ner som .csv-fil")
                         )
                       )
                     )
                     
              )
            )
    ),
    
    tabItem(
      tabName = "trends",
      fluidRow(
        tabBox(width = 12, title = NULL, id = "tabset2",
               tabPanel("Uppskattning nationellt",plotlyOutput("lineplot1", height = 700)),
               #tabPanel("Uppskattning per län", plotlyOutput("lineplot2", height = 700)),
               tabPanel("Uppskattning per län", 
                        selectInput(
                          "myPicker","Välj län:",
                          sort(unique(lan_senaste$Lan)),
                          multiple = TRUE),
                        plotOutput("lineplot_CI", height = 500)
               )
        ),
        fluidRow(
          box(width = 8, title = span(icon("exclamation-triangle"), "Uppdatering"), status = "primary", solidHeader = TRUE, textOutput("exclamation-triangle3"),
              "Den 4 november uppdaterades frågorna om symtom i appen, bl.a. omformulerades frågan om feber. Detta medför att trenderna före och efter detta datum inte är helt jämförbara då frågorna och sättet deltagare rapporterar på skiljer sig från tidigare.", 
              tags$p("Den viktiga frågan kring lukt och smak är mycket central i vår modell då den representerar det symtom som skiljer sig mest åt mellan personer som har ett positivt PCR-test för covid-19 och personer som inte har det. Frågan har gjorts mer specifik och delats upp i två frågor (en om förlorat lukt-/smaksinne, en om förändringar i lukt-/smaksinne). Vi har nu lagt in båda dessa frågor i modellen och uppdaterat våra resultat från den 4:e november och framåt. Vi arbetar kontinuerligt med att uppdatera våra modeller."))
        )
      )
    ),
    
    
    tabItem(tabName = "symptoms",
            h2(""),
            fluidRow(
              tabBox(width = 12, title = NULL, id = "tabset1", 
                     tabPanel("Symtom med positiv vikt", plotlyOutput("symptoms_positiv", height = 500)),
                     tabPanel("Symtom med negativ vikt", plotlyOutput("symptoms_negativ", height = 500))
              )),
            fluidRow(
              box(width = 12, title = span(icon("chart-bar"), "Hur de olika symtomen viktas i modellen"), status = "primary", solidHeader = TRUE, textOutput("chart-bar2"),
                  plotlyOutput("weightplot", height = 500))
            )
    ),
    
    tabItem(tabName = "participants",
            h2(""),
            fluidRow(
              box(plotlyOutput("plot1", height = 500),title = span(icon("chart-bar"), "Ålders- och könsfördelning av deltagare"), status = "primary", solidHeader = TRUE, textOutput("chart-bar")),
              box(plotlyOutput("plot2", height = 500), title = span(icon("chart-pie"), "Könsfördelning"), status = "primary", solidHeader = TRUE, textOutput("chart-pie"))
            ),
            fluidRow(
              downloadButton("downloadData2", "Ladda ner som .csv-fil")
            )
    ),
    
    tabItem(tabName = "crush",
            h2(""),
            fluidRow(
              column(width = 6,
                     fluidRow(
                       valueBox(width = 12, tail(uppsala_cum$cumsum, 1), "Deltagare i Uppsala", icon = icon("users"), color = "olive")),
                     fluidRow(
                       box(width = 12, title = span(icon("exclamation-circle"), "CRUSH Covid Uppsala"), status = "primary", solidHeader = TRUE, textOutput("exclamation-circle"),
                           tags$p("COVID Symptom Study samverkar med forskningsprojektet CRUSH Covid i Uppsala. CRUSH Covid är ett innovativt tvärvetenskapligt forskningsprojekt i samverkan mellan Region Uppsala och forskare från Uppsala universitet. Syftet med projektet är att kartlägga och försöka dämpa lokala utbrott av covid-19 i Uppsala län."),
                           tags$p("CRUSH Covid kommer att bistå Region Uppsalas Smittskyddsenhet med kunskapsunderlag gällande smittläget i Uppsala län. Forskarna kommer att utarbeta en metod att kombinera information från flera olika datakällor och kontinuerligt rapportera tecken på lokala utbrott. COVID Symptom Study är en av dessa datakällor och vi välkomnar alla nya deltagare från Uppsala Län."),
                           #tags$a(imageOutput("uppsala_logo"), href = "https://www.covid19app.lu.se/"),
                           style = "font-size: 120%;")
                     )
              ),
              column(width = 6,
                     tabBox(width = 12, title = NULL, id = "tabset_up", 
                            tabPanel("Karta", leafletOutput(outputId = "uppsala", height=400)),
                            tabPanel("Deltagare - ålder/kön", plotlyOutput("plot_uppsala1", height = 400)),
                            tabPanel("Deltagare - kön", plotlyOutput("plot_uppsala2", height = 400))
                     )
              )
            ),
            fluidRow(
              box(width = 12, title = span(icon("chart-line"), "Trend deltagande i Uppsala Län"), status = "primary", solidHeader = TRUE, textOutput("chart-line7"),
                  plotlyOutput("uppsala_cum", height = 300))
            )
    ),
    
    tabItem(tabName = "dataRepo",
            h2("Arkiv"),
            tags$p(),
            tags$p("Den 21 januari 2021 uppdaterade vi vår prediktionsmodell (läs mer om detta",
                   tags$a(href="https://www.covid19app.lu.se/article/uppdatering-av-prediktionsmodell-0", "HÄR)."), 
                   "Det kommer fortfarande att vara möjligt att ladda ned filer med prediktioner från den gamla modellen under en viss tid framöver. Observera dock att dessa filer inte kommer att uppdateras lika ofta som filerna för den nya modellen (ca en gång i veckan).",
                   style = "font-size: 120%;"),
            tags$p(),
            tags$p(),
            h4(tags$b("Senaste modellen (tränad den 19 oktober 2020)")),
            tags$p(),
            tags$ul(
              tags$li("Nationella beräkningar - senaste modellen"),
              tags$p(),
              downloadButton("download_national_new", "Ladda ner som .csv-fil"),
              tags$p(),
              tags$li("Beräkningar per län - senaste modellen"),
              tags$p(),
              downloadButton("download_county_new", "Ladda ner som .csv-fil"),
              tags$p(),
              tags$li("Beräkningar per 2-siffrigt område - senaste modellen"),
              downloadButton("download_2digit_new", "Ladda ner som .csv-fil"),
              style = "font-size: 120%;"),
            tags$p(),
            tags$p(),
            h4(tags$b("Gamla modellen")),
            tags$p(),
            tags$ul(
              tags$li("Nationella beräkningar - gamla modellen"),
              tags$p(),
              downloadButton("download_national_old", "Ladda ner som .csv-fil"),
              tags$p(),
              tags$li("Beräkningar per län - gamla modellen"), 
              tags$p(),
              downloadButton("download_county_old", "Ladda ner som .csv-fil"),
              tags$p(),
              tags$li("Beräkningar per 2-siffrigt område - gamla modellen"),
              tags$p(),
              downloadButton("download_2digit_old", "Ladda ner som .csv-fil"),
              style = "font-size: 120%;"),
            tags$p(),
            h4(tags$b("covidsymptom R package")),
            tags$p("För användare av R - du kan nu installera vårt nya paket ", 
                   tags$a(href="https://github.com/csss-resultat/covidsymptom", "covidsymptom"), " där du kan importera aktuella data från COVID Symptom Study Sweden direkt till din arbetsmiljö."),
            tags$a(imageOutput("hex"))
            
    ),
    
    tabItem(tabName = "dataportalen",
            h2("Covid-19 Data Portal"),
            tags$p("Den svenska dataportalen för covid-19 tillhandahåller information, riktlinjer, verktyg och tjänster för att stödja forskare att använda svenska och europeiska tjänster för datadelning. Portalen är den svenska noden av-",
                   tags$a(href="https://www.covid19dataportal.org/", "European COVID-19 Data Portal"),
                   " och drivs av ",
                   tags$a(href="https://www.scilifelab.se/data/",  "SciLifeLab Data Centre"),
                   " och samarbetspartners.", 
                   style = "font-size: 120%;"),
            tags$a(imageOutput("dataportal"), href = "https://covid19dataportal.se/"
            )
            
    )
  )
)

customSentence <- function(numItems, type) {
  paste("Aktuellt")
}

# Function to call in place of dropdownMenu
dropdownMenuCustom <-     function (..., type = c("messages", "notifications", "tasks"), 
                                    badgeStatus = "primary", icon = NULL, .list = NULL, customSentence = customSentence) 
{
  type <- match.arg(type)
  if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li")
  dropdownClass <- paste0("dropdown ", type, "-menu")
  if (is.null(icon)) {
    icon <- switch(type, messages = shiny::icon("envelope"), 
                   notifications = shiny::icon("plus"), tasks = shiny::icon("tasks"))
  }
  numItems <- length(items)
  if (is.null(badgeStatus)) {
    badge <- NULL
  }
  else {
    badge <- span(class = paste0("label label-", badgeStatus), 
                  numItems)
  }
  tags$li(
    class = dropdownClass, 
    a(
      href = "#", 
      class = "dropdown-toggle", 
      `data-toggle` = "dropdown", 
      icon, 
      badge
    ), 
    tags$ul(
      class = "dropdown-menu", 
      tags$li(
        class = "header", 
        customSentence(numItems, type)
      ), 
      tags$li(
        tags$ul(class = "menu", items)
      )
    )
  )
}


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "COVID Symptom Study", titleWidth = 250,
                  dropdownMenuCustom(type = "notifications", customSentence = customSentence,
                                     notificationItem(
                                       text = paste0("Senast uppdaterad: ", upload_time$date, " kl. ", upload_time$time),
                                       icon = icon("clock"),
                                       status = "warning"
                                     ),
                                     notificationItem(
                                       text = "NYHETER: Förpublicerad artikel från studien",
                                       href = 'https://www.covid19app.lu.se/artikel/forpublicerad-artikel-fran-studien',
                                       icon = icon("newspaper"),
                                       status = "danger"
                                     ),
                                     notificationItem(
                                       text = "CRUSH Covid Uppsala",
                                       href = 'https://www.uu.se/forskning/projekt/crush-covid/',
                                       icon = icon("file"),
                                       status = "danger"
                                     ),
                                     notificationItem(
                                       text = "Följ oss",
                                       href = 'https://www.facebook.com/covidsymptomstudysverige',
                                       icon = icon("facebook"),
                                       status = "info"
                                     ),
                                     notificationItem(
                                       text = "Github",
                                       href = 'https://github.com/csss-resultat/',
                                       icon = icon("github"),
                                       status = "info"
                                     ),
                                     notificationItem(
                                       text = "Läs mer om CSSS",
                                       href = 'https://www.covid19app.lu.se/om-studien',
                                       icon = icon("info-circle"),
                                       status = "info"
                                     )
                  )),
  sidebar,
  body
)


server <- function(input, output) {
  
  ay <- list(
    tickfont = list(color = "red"),
    overlaying = "y",
    side = "right",
    title = "Antal deltagare som bidrar till dagliga beräkningen",
    range = c(0, 110000),
    gridcolor = "white",
    titlefont=list(color="red"),
    ticktext = list("0","20t", "40t", "60t", "80t"), 
    tickvals = list(0,20000,40000,60000,80000)
  )
  
  vline <- function(x = 0, color = "red") {
    list(
      type = "line", 
      y0 = 0, 
      y1 = 1, 
      yref = "paper",
      x0 = x, 
      x1 = x, 
      line = list(color = color, dash = 'dash')
    )
  }
  
  # CI's Line plot
  output$lineplot1 <- renderPlotly(
    plot_ly(intervals_all, x = ~date, y = ~adj_prev, type = 'scatter', mode = 'lines+markers', connectgaps = TRUE, 
            showlegend = TRUE, name = '% uppskattad förekomst', legendgroup = 'group1') %>%
      add_trace(x = ~date, y = ~n_users, type = 'scatter', mode = 'lines+markers', yaxis = "y2", name = "Deltagare", legendgroup = 'group2', line = list(color = 'rgb(205, 12, 24)'), marker = list(color = 'rgb(205, 12, 24)')) %>%
      layout(title = "",
             xaxis = list(title = "Datum", type = 'date', tickformat = "%Y-%m-%d"),
             yaxis = list (title = "Uppskattad förekomst av symtomatisk covid-19", titlefont=list(color="navy"), range = c(0, 1),
                           ticktext = list("0 %","O,2 %", "0,4 %", "0,6 %", "0,8 %", "1,0 %"), 
                           tickvals = list(0,0.2, 0.4, 0.6, 0.8, 1.0),
                           tickmode = "array",
                           tickfont = list(color = "navy")),
             yaxis2 = ay) %>% 
      add_trace(y = ~adj_prev_ci_low, type = 'scatter', mode = 'lines', connectgaps = TRUE,
                line = list(color = 'transparent'), fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)',
                showlegend = FALSE, name = 'Low CI', legendgroup = 'group1') %>%
      add_trace(y = ~adj_prev_ci_high, type = 'scatter', mode = 'lines', connectgaps = TRUE,
                line = list(color = 'transparent'), fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)',
                showlegend = FALSE, name = 'High CI', legendgroup = 'group1') %>%
      add_annotations(
        x="2020-08-15",
        y=2.8,
        text = paste0("<b><br>Den streckade linjen markerar 4 november,</br>då vår symtomrapportering förändrades.</b>"),
        showarrow = F
      ) %>%
      layout(shapes = list(vline(as.Date("2020-11-04"))))
  )
  
  # Add C logo
  output$covidlogo <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'covid_logo', '.png')))
    list(
      src = filename,
      style="display: block; margin-left: auto; margin-right: auto; margin-bottom: auto;",
      height = "30%"
    )
  }, deleteFile = FALSE)
  
  # Add universities' logo
  output$tryscreen <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'logos3', '.png')))
    list(
      src = filename,
      style="display: block; margin-left: auto; margin-right: auto; margin-bottom: auto;",
      height = "50%"
    )
  }, deleteFile = FALSE)
  
  
  output$dataportal <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'dataportalen2', '.png')))
    list(
      src = filename,
      style="display: block; margin-left: auto; margin-right: auto; margin-bottom: auto;",
      height = "120%"
    )
  }, deleteFile = FALSE)
  
  # National map (Län)
  
  dailyData <- reactive(All_preds[All_preds$date == format(input$Datum, '%Y-%m-%d'), ] )
  
  labels_to_place <- labels_to_place <- c("< 0,1 %", "0,1 - 0,2 %", "0,2 - 0,3 %", "0,3 - 0,4 %", "0,4 - 0,6 %", "0,6 - 0,8 %", "0,8 - 1,0 %", "> 1,0 %", "Otillräckligt underlag")
  
  
  output$nationalmap <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>%
      setView(lat = 63.3757026, lng = 16.9178035, zoom = 4.5) %>%
      addPolygons(
        data = dailyData(),
        color = 'black',
        fillColor =  ~pal(adj_prev),
        popup = dailyData()$popup_info,
        smoothFactor = 0.2, fillOpacity = 1,  weight = 1,
        highlightOptions = highlightOptions(stroke = 4, weight = 3, bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal,
                values  = dailyData()$adj_prev,
                position = "bottomright",
                title = "",
                opacity = 1,
                labFormat = function(type, cuts, p) {  # Here's the trick
                  paste0(labels_to_place)
                }) %>%
      addFullscreenControl(pseudoFullscreen = TRUE)
  })
  
  # add this code to avoid re-rendering maps. The issues is that if the map is not the landing page, a empty map will show up before the user selects a date
  
  # observe({
  #   leafletProxy(isolate("nationalmap")) %>%
  #     addProviderTiles("CartoDB.Positron") %>%
  #     addPolygons(
  #       data = dailyData(),
  #       color = 'black',
  #       fillColor =  ~pal(percent_infected_final),
  #       popup = dailyData()$popup_info,
  #       smoothFactor = 0.2, fillOpacity = 1,  weight = 1
  #     )
  # })
  
  # National predictions'table
  
  dailyData2 <- reactive(tove_df[tove_df$Datum == input$Datum,1:3])
  
  output$tove_df <- DT::renderDataTable(dailyData2(), rownames= FALSE, options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Swedish.json')))
  
  # Data to download 
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("CSSS_berakningar_lan_senaste_", gsub("-", "", upload_time$date), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(lan_senaste, file ,row.names = FALSE)
    }
  )
  
  # Regional maps 
  
  dailyData3 <- reactive(MapData2[MapData2$date == format(input$Datum2, '%Y-%m-%d'), ] )
  
  labels_to_place2 <- labels_to_place <- c("< 0,1 %", "0,1 - 0,2 %", "0,2 - 0,3 %", "0,3 - 0,4 %", "0,4 - 0,6 %", "0,6 - 0,8 %", "0,8 - 1,0 %", "> 1,0 %", "Otillräckligt underlag")
  
  output$regionalmap <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>%
      setView(lat = 63.3757026, lng = 16.9178035, zoom = 4.5) %>%
      addPolygons(
        data = dailyData3(),
        color = 'black',
        fillColor =  ~pal2(adj_prev),
        popup = dailyData3()$popup_info,
        smoothFactor = 0.2, fillOpacity = 1,  weight = 1,
        highlightOptions = highlightOptions(stroke = 4, weight = 3, bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal2,
                values  = dailyData3()$adj_prev,
                position = "bottomright",
                title = "",
                opacity = 1,
                labFormat = function(type, cuts, p) {  # Here's the trick
                  paste0(labels_to_place2)
                }) %>%
      addMiniMap(
        toggleDisplay = TRUE,
        position = "bottomleft") %>%
      addFullscreenControl(pseudoFullscreen = TRUE)
  }) 
  
  # add this code to avoid re-rendering maps. The issues is that if the map is not the landing page, a empty map will show up before the user selects a date
  
  # observe({
  #   leafletProxy(isolate("regionalmap")) %>%
  #     addProviderTiles("CartoDB.Positron") %>%
  #     addPolygons(
  #       data = dailyData3(),
  #       color = 'black',
  #       fillColor =  ~pal2(percent_infected_final),
  #       popup = dailyData3()$popup_info,
  #       smoothFactor = 0.2, fillOpacity = 1,  weight = 1
  #     )
  # })
  
  # Regional table
  
  dailyData4 <- reactive(tove_df2[tove_df2$Datum == input$Datum2,])
  
  output$tove_df2 <- DT::renderDataTable(dailyData4(), rownames= FALSE, options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Swedish.json')))
  
  # Data to download 
  
  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste("CSSS_berakningar_2siffror_senaste_", gsub("-", "", upload_time$date), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(siffror_senaste, file ,row.names = FALSE)
    }
  )
  
  
  
  # Län line plot
  
  filter1 <- reactive({
    lan_senaste %>% filter(Lan %in% input$myPicker)
  })
  
  output$lineplot_CI <- renderPlot({
    validate(
      need(input$myPicker, 'Välj minst ett län för att visa diagrammet')
    )
    #filter1 = filter1()
    ggplot(data = filter1(), aes(x = Datum, y = Uppskattning, color = Lan)) +
      geom_line() +
      geom_point(size = 0.5) +
      labs(x = "Datum", y = "Uppskattad förekomst", title = "% Uppskattad förekomst av symtomatisk covid-19", subtitle = "") +
      scale_x_date(date_breaks = "15 days") +
      scale_y_continuous(labels=comma_format(big.mark = ".", decimal.mark = ",")) +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), panel.grid.minor.x = element_blank(),
            axis.title.y = element_text(size = 14), axis.title.x = element_text(size = 16), legend.position = "none", 
            plot.title = element_text(size=22, hjust = 0.5), strip.text = element_text(size=18)) + 
      # Add the confidence intervals and make them slightly transparent
      geom_ribbon(aes(ymin = Low_CI, ymax = High_CI), color = "transparent", fill = "#a60f61", alpha = 0.09) +
      geom_vline(xintercept = ymd(20201104), linetype = "dashed") +
      facet_wrap(. ~ Lan, scales = "free") 
  })
  
  # Symptoms
  output$symptoms_positiv <- renderPlotly({
    plot_ly(symptoms_df, x = ~date, y = ~round(loss_of_smell, 2), name = 'Förlorat eller förändrat lukt-/smaksinne',
            type = 'scatter', mode = 'lines+markers') %>%
      #add_trace(y = ~shortness_of_breath, name = 'Måttliga/allvarliga andningsbesvär', mode = 'lines+markers') %>%
      add_trace(y = ~round(fever, 2), name = 'Feber', mode = 'lines+markers') %>%
      add_trace(y = ~round(fatigue, 2), name = 'Svår trötthet', mode = 'lines+markers') %>%
      add_trace(y = ~round(skipped_meals, 2), name = 'Aptitlöshet', mode = 'lines+markers') %>%
      add_trace(y = ~round(persistent_cough, 2), name = 'Ihållande hosta', mode = 'lines+markers') %>%
      add_trace(y = ~round(diarrhoea,2), name = 'Diarré', mode = 'lines+markers') %>% 
      add_trace(y = ~round(unusual_muscle_pains, 2), name = 'Ovanlig muskelsmärta', mode = 'lines+markers') %>% 
      layout(title = "Symtom med positiv vikt",
             xaxis = list(title = "Datum",
                          type = 'date',
                          tickformat = "%Y-%m-%d"),
             yaxis = list(title = "Andel deltagare med symtom",
                          ticktext = list("0","O,5 %", "1,0 %", "1,5 %", "2,0 %", "2,5 %", "3,0 %"), 
                          tickvals = list(0,0.5, 1, 1.5, 2, 2.5, 3),
                          tickmode = "array")) %>%
      layout(shapes = list(vline(as.Date("2020-11-04"))))
  })
  
  output$symptoms_negativ <- renderPlotly({
    plot_ly(symptoms_df, x = ~date, y = ~round(hoarse_voice,2), name = 'Heshet',
            mode = 'lines+markers', type = 'scatter',marker = list(color = 'rgb(251, 185, 4)') ,line = list(color = 'rgb(251, 185, 4)')) %>%
      #add_trace(y = ~abdominal_pain, name = 'Buksmärta',  mode = 'lines+markers', marker = list(color = 'rgb(249, 67, 67)') ,line = list(color = 'rgb(249, 67, 67)')) %>%
      #add_trace(y = ~delirium, name = 'Förvirring, desorientering, dåsighet',mode = 'lines+markers', marker = list(color = 'rgb(132, 124, 150)') ,line = list(color = 'rgb(132, 124, 150)')) %>%
      #add_trace(y = ~chest_pain, name = 'Bröstsmärta',mode = 'lines+markers', marker = list(color = 'rgb(101, 58, 33)') ,line = list(color = 'rgb(101, 58, 33)')) %>%
      add_trace(y = ~round(sore_throat, 2), name = 'Ont i halsen',  mode = 'lines+markers', marker = list(color = 'rgb(249, 67, 67)') ,line = list(color = 'rgb(249, 67, 67)')) %>%
      add_trace(y = ~round(chills_or_shivers,2), name = 'Frossa/huttrar',mode = 'lines+markers', marker = list(color = 'rgb(132, 124, 150)') ,line = list(color = 'rgb(132, 124, 150)')) %>%
      add_trace(y = ~round(eye_soreness,2), name = 'Ont eller irritation i ögon',mode = 'lines+markers', marker = list(color = 'rgb(101, 58, 33)') ,line = list(color = 'rgb(101, 58, 33)')) %>%
      layout(title = "Symtom med negativ vikt", 
             xaxis = list(title = "Datum",
                          type = 'date',
                          tickformat = "%Y-%m-%d"),
             yaxis = list(title = "Andel deltagare med symtom",
                          ticktext = list("0", "1,0 %", "2,0 %", "3,0 %", "4,0 %", "5,0 %", "6,0 %"), 
                          tickvals = list(0,1, 2, 3, 4, 5, 6),
                          tickmode = "array")) %>%
      layout(shapes = list(vline(as.Date("2020-11-04"))))
  })
  
  
  output$weightplot <- renderPlotly({
    weight_df %>%
      plot_ly(x = ~SWE, y = ~Weight, color = ~group_covid, type = "bar") %>%
      layout(separators = ",.", xaxis = xform, yaxis = list(title = "Vikt"))
  })
  
  observeEvent(input$button, {
    shinyjs::toggle("myBox")
  })
  
  # Histogram
  
  output$plot1 <- renderPlotly(
    plot_ly(counts,
            x = ~decade,
            y = ~count,
            color = ~gender,
            colors = c("#A60F61", "#193765")) %>%
      layout(yaxis = list(title = ''),
             xaxis = list(title = 'åldersgrupp'))
  )
  
  # Pie chart
  
  colors_pie <- c("#A60F61", "#193765")
  
  output$plot2 <- renderPlotly(
    plot_ly(counts, 
            labels = ~gender, 
            values = ~count, 
            type = 'pie', 
            marker = list(colors = colors_pie, line = list(color = '#FFFFFF', width = 1))) %>%
      layout(title = '',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  )
  
  # data to Download 
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("CSSS_deltagare_", gsub("-", "", upload_time$date), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(counts_ladda, file ,row.names = FALSE)
    }
  )
  
  
  # CRUSH COVID
  
  labels_to_place_uppsala <- c("0 - 10", "11 - 50", "51 - 100", "101 - 150", "151 - 200", "200+", "NA")
  #0-10, 11-50, 51-100, 101-150, 151-200, 200+
  output$uppsala <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>%
      setView(lat = 59.8602807, lng = 17.6261692, zoom = 12) %>%
      #setView(lat = 60.0314865, lng = 17.6483147, zoom = 18) %>%
      addPolygons(
        data = shape_uppsala2,
        color = 'black',
        fillColor =  ~pal_uppsala(Count),
        popup =  paste0("<b>Postnummer</b>", "<br/>",  shape_uppsala2$POSTALCODE, "<br/>", "<b>Antal som bidrar</b>","<br/>", shape_uppsala2$Count),
        smoothFactor = 0.2, fillOpacity = 1,  weight = 1,
        highlightOptions = highlightOptions(stroke = 4, weight = 3, bringToFront = TRUE)
      )  %>%
      addLegend(pal = pal_uppsala,
                values  = shape_uppsala2$Count,
                position = "bottomright",
                title = "# deltagare",
                opacity = 1,
                labFormat = function(type, cuts, p) {  # Here's the trick
                  paste0(labels_to_place_uppsala)
                }) %>%
      addFullscreenControl(pseudoFullscreen = TRUE)
  })
  
  output$plot_uppsala1 <- renderPlotly(
    plot_ly(counts_uppsala,
            x = ~decade,
            y = ~count,
            color = ~gender,
            colors = colors_uppsala) %>%
      layout(title = "Ålders- och könsfördelning av deltagare i Uppsala",
             yaxis = list(title = ''),
             xaxis = list(title = 'åldersgrupp'))
  )
  
  # Pie chart
  
  output$plot_uppsala2 <- renderPlotly(
    plot_ly(counts_uppsala, 
            labels = ~gender, 
            values = ~count, 
            type = 'pie', 
            marker = list(colors = colors_uppsala, line = list(color = '#FFFFFF', width = 1))) %>%
      layout(title = 'Könsfördelning av deltagare i Uppsala',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  )
  
  output$uppsala_cum <- renderPlotly(
    uppsala_cum %>%
      plot_ly( x = ~created_at, y = ~cumsum, type = 'scatter', mode = 'lines+markers', color = "green") %>%
      layout(title = "",
             xaxis = list(title = "Datum"),
             yaxis = list(title = "Antal deltagare", range = c(0,11000)))
  )
  
  
  # Data archive 
  
  output$download_national_new <- downloadHandler(
    filename = function() {
      paste("CSSS_nationella_berakningar_senaste_", gsub("-", "", upload_time$date), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(nationella_senaste, file ,row.names = FALSE)
    }
  )
  
  output$download_national_old <- downloadHandler(
    filename = function() {
      paste("CSSS_nationella_berakningar_gammal_", gsub("-", "", tail(nationella_gammal$Datum, 1)), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(nationella_gammal, file ,row.names = FALSE)
    }
  )
  
  output$download_county_new <- downloadHandler(
    filename = function() {
      paste("CSSS_berakningar_lan_senaste_", gsub("-", "", upload_time$date), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(lan_senaste, file ,row.names = FALSE)
    }
  )
  
  output$download_county_old <- downloadHandler(
    filename = function() {
      paste("CSSS_berakningar_lan_gammal_", gsub("-", "", tail(lan_gammal$datum, 1)), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(lan_gammal, file ,row.names = FALSE)
    }
  )
  
  output$download_2digit_new <- downloadHandler(
    filename = function() {
      paste("CSSS_berakningar_2siffror_senaste_", gsub("-", "", upload_time$date), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(siffror_senaste, file ,row.names = FALSE)
    }
  )
  
  output$download_2digit_old <- downloadHandler(
    filename = function() {
      paste("CSSS_berakningar_2siffror_gammal_", gsub("-", "", tail(siffror_gammal$Datum, 1)), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(siffror_gammal, file ,row.names = FALSE)
    }
  )
  
  output$hex <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'hex2', '.png')))
    list(
      src = filename,
      #style="display: block; margin: 20px auto 30px auto;",
      height = "40%"
    )
  }, deleteFile = FALSE)
  
}

# Run the app

shinyApp(ui, server)