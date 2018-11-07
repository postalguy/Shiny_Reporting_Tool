#Working
library("shinydashboard")
library(shiny)
library(plotly)
library(data.table)
library(dplyr)
library(DT)
library(leaflet)
library(leaflet.extras)
library("leaflet.minicharts")
library(flexdashboard)
source("Utilities.R")




main_Folder <- "C:/Users/Ismail AKRIM/Desktop/GIthub/Shinny Test/Test4/Monetize/Monetize/"


#################################################Lecture de données
DATA_ <- fread(paste0(main_Folder,"Donnes.csv"),stringsAsFactors = T)
DATA_$Client_Crea <- as.Date(DATA_$Client_Crea)
DATA_$TOP_DATA <- ifelse(DATA_$Data_Volume == 0 , 0,1)
DATA_$TOP_LTE <- ifelse(DATA_$Data_4G == 0 , 0, 1)

not_to_profile <- c("Longitude" , "Latitude","Long_Ville","Lat_Ville","Long_Region","Lat_Region")
URLS <-  fread(paste0(main_Folder,"Table_URL.csv"),stringsAsFactors = T)



POIS <- fread(paste0(main_Folder,"bts_week.csv"))
POIS <- POIS[, .N ,by=names(POIS)]

POIS.pts <- POIS[,3:5]
POIS.pts <- POIS[, list(Long = mean(lat_weekend,na.rm = T), Lat = mean(long_weekend,na.rm = T)), 
                       by = list(POI)]



#################################################Tableaux de Reporting Intermediaire

colors1 <- c('rgb(0,166,90)', 'rgb(147,147,147)')
colors2 <- c('rgb(221,75,57)', 'rgb(147,147,147)')
colors3 <- c( 'rgb(60,141,188)', 'rgb(147,147,147)')
colors_map <-  c('#FF1100','#2A2AD4','#469FAA','##46D1AA','##AAE055','#D4D42A','#FFFF00','#FFD400','#FFAA00','#FF7F00','#FF5500','#FF2A00')


## les Marques dans le parc
Marques <- as.data.table(table(DATA_$Equipement_Marque))
marque= levels(as.factor(DATA_$Equipement_Marque))

## les top 10 modèles dans le parc
DATA_$Modeles <- paste(DATA_$Equipement_Marque,DATA_$Equipement_Modele)
Modeles <- as.data.table(table(DATA_$Modeles))
marque= levels(as.factor(DATA_$Equipement_Marque))
setorder(Modeles,-N)
TOP10_Modeles <-head(Modeles,10)

####Noms des sites
bts <- unique(subset(DATA_, select=c( Longitude,Latitude)),by=c( "Longitude","Latitude"))
bts$nom <- paste("Site", 1:nrow(bts))

###  infos map pour terminaux
device_parc <- bts_device(DATA_,bts)

###Part de marché terminaux
parc_Marques = getMarques(DATA_)
parc_Marques_SMRT = getMarques(DATA_[DATA_$Equipement_Term_Type == "Smartphone",])
parc_Marques_FTR = getMarques(DATA_[DATA_$Equipement_Term_Type == "Feature Phone",])

## le top 10 des modèles 
parc_TOP10_Modeles = top10_Models(DATA_)

### Calculer les moyennes  
parc_moyennes = calculMoyenne(DATA_)

### Dashboards par Marque : 
Dash_table <- Brands_dashboard(DATA_)

## Retail 
# Repartition parc par ville: 
Cities <- unique(DATA_[,c("Geoloc_ville","Long_Ville" ,"Lat_Ville" )])
Regions <- unique(DATA_[,c("Geoloc_region","Long_Region" ,"Lat_Region" )])

parc_ville <- aggregate_villes(tab = DATA_,Cities = Cities,top = 100)
parc_region <- aggregate_region(tab = DATA_,Regions = Regions)           
       

##################################################################### UI 


header <- dashboardHeader( title = "Reporting Tool",   tags$li(a(href = 'https://github.com/postalguy',
                                                                 img(src = 'https://avatars2.githubusercontent.com/u/8229503?s=460&v=4',
                                                                     title = "Postalguy",height = "20px")),class = "dropdown")
)

sidebar <- dashboardSidebar( width = 300,
                             sidebarMenu(
                               menuItem("Préparation de la Cible",tabName = "prepare",icon = icon("glyphicon glyphicon-screenshot", lib = "glyphicon")
                                        
                               ),# F _ Selection Cible
                               menuItem("Exploration de la Cible",icon = icon("glyphicon glyphicon-tasks", lib = "glyphicon"),startExpanded = T,
                                        #Menu des secteurs
                                        menuItem("Secteurs",tabName = "Secteurs",startExpanded = T,
                                                 menuItem("Profiling",tabName="Profiling"),
                                                 menuItem("Retail", tabName = "Retail"),
                                                 menuItem("Telecom (Device Manifactures)", tabName = "Telecom")
                                                 
                                        )
                                        
                                        
                               ), #F _ Exploration de la cible
                               menuItem("Exportaion de la Cible/Ciblage",tabName = "Extract", icon= icon("glyphicon glyphicon-save" , lib="glyphicon")
                               )#F _ Exportation de la cible/ciblage
                               
                             )#F _ sidebar
)

body <- dashboardBody(
  tags$style("
             body {
             -moz-transform: scale(0.9, 0.9); 
             zoom: 0.9; 
             zoom: 90%; 
             .tab1 {
             background-color: #F39C12;
             }
             .tab2 {
             background-color: #006747;
             }
             }
             "),
  tabItems(
    tabItem(tabName = "prepare", 
            fluidPage(
              box(title ="Sélection de la Population 1",width = 400, status = "success",solidHeader = T, background = "black",collapsible = T,collapsed = T,
                  #Form Client 
                  box( title="Informations Client", solidHeader = T, status ="success",collapsible = T,collapsed = T,background = "black",
                       sliderInput(inputId = "Client_Age",label = "Age", min = min(DATA_$Client_Age), max = max(DATA_$Client_Age),value = c(min(DATA_$Client_Age),max(DATA_$Client_Age)),step = 1),
                       dateRangeInput(inputId= "Client_Crea", label="Date de Création", start =as.Date("1980/01/01"), end = Sys.Date(),format = "yyyy-mm-dd", startview = "year", weekstart = 0, language = "en", separator = " jusqu\'au "),
                       checkboxGroupInput("Client_Type", "Parc Client", choices = levels(as.factor(DATA_$Client_Type)), selected = levels(as.factor(DATA_$Client_Type))),
                       checkboxGroupInput("Client_Offer", "Plan Tarifaire", choices = levels(as.factor(DATA_$Client_Offer)),selected = levels(as.factor(DATA_$Client_Offer)) )
                       
                       
                  ),
                  #Form Geoloc 
                  box( title="Géolocalisation", solidHeader = T, status ="success",collapsible = T,collapsed = T,background = "black",
                       #"Geoloc_commune"   "Geoloc_ville"     "Geoloc_region"    "Geoloc_type" 
                       selectInput("Geoloc_commune","Commune",choices = levels(as.factor(DATA_$Geoloc_commune)), multiple = TRUE,selected = levels(as.factor(DATA_$Geoloc_commune)),selectize = F,size = 3),
                       selectInput("Geoloc_ville","Ville",choices = levels(as.factor(DATA_$Geoloc_ville)), multiple = TRUE , selected = levels(as.factor(DATA_$Geoloc_ville)),selectize = F,size = 3),
                       selectInput("Geoloc_region","Région",choices = levels(as.factor(DATA_$Geoloc_region)) , multiple = TRUE, selected = levels(as.factor(DATA_$Geoloc_region)),selectize = F,size = 3),
                       selectInput("Geoloc_type","Type",choices = levels(as.factor(DATA_$Geoloc_type)), multiple = TRUE, selected=levels(as.factor(DATA_$Geoloc_type)))
                  ),
                  #Form Communauté
                  box( title="Communauté", solidHeader = T, status ="success",collapsible = T,collapsed = T,background = "black",
                       selectInput("Communaute_Role","Role",choices = levels(as.factor(DATA_$Communaute_Role)), multiple = TRUE,selected = levels(as.factor(DATA_$Communaute_Role)) ),
                       sliderInput(inputId = "Communaute_Taille",label = "Taille de communauté", min = min(DATA_$Communaute_Taille), max = max(DATA_$Communaute_Taille),value = c(min(DATA_$Communaute_Taille),max(DATA_$Communaute_Taille)))
                  ),
                  
                  #Form Profil 
                  box( title="Profiling", solidHeader = T, status ="success",collapsible = T,collapsed = T,background = "black",
                       selectInput("Profile_1","Life Style",choices =levels(as.factor(DATA_$Profile_1)), multiple = TRUE,selected = levels(as.factor(DATA_$Profile_1)) ),
                       selectInput("Profile_2","Gestion de Solde",choices = levels(as.factor(DATA_$Profile_2)), multiple = TRUE ,selected = levels(as.factor(DATA_$Profile_2)) ),
                       selectInput("Profile_3","Spending",choices =levels(as.factor(DATA_$Profile_3)), multiple = TRUE ,selected = levels(as.factor(DATA_$Profile_3)) )
                  ),
                  
                  #Form Conso 
                  box( title="Consomation", solidHeader = T, status ="success",collapsible = T,collapsed = T, background = "black", 
                       #Voix
                       fluidPage(box(title = "Voix" , collapsible = T, status = "info",solidHeader = T,width = 500,collapsed = T,background = "black",
                                     
                                     box(h3("Nombre jours d'Activité"),background = "black",width = 450,solidHeader = T,
                                         fluidPage(    
                                           sliderInput(inputId = "Voix_Nbj_Activite",label = "", min = min(DATA_$Voix_Nbj_Activite), max = max(DATA_$Voix_Nbj_Activite),value = c(min(DATA_$Voix_Nbj_Activite),max(DATA_$Voix_Nbj_Activite)))
                                         )
                                     ),
                                     
                                     box(h3("Trafic National"),background = "black",width = 450,solidHeader = T,
                                         fluidPage(    
                                           sliderInput(inputId = "Voix_National",label = "Minutes Nationales", min = min(DATA_$Voix_National), max = max(DATA_$Voix_National) ,value = c(min(DATA_$Voix_National),max(DATA_$Voix_National))),
                                           sliderInput(inputId = "Voix_National_Nb",label = "Nombre d'appels Nationals", min = min(DATA_$Voix_National_Nb), max = max(DATA_$Voix_National_Nb) ,value = c(min(DATA_$Voix_National_Nb),max(DATA_$Voix_National_Nb))),
                                           sliderInput(inputId = "Voix_NbContact_National",label = "Nombre de contacts National", min = min(DATA_$Voix_NbContact_National) , max = max(DATA_$Voix_NbContact_National) ,value = c(min(DATA_$Voix_NbContact_National),max(DATA_$Voix_NbContact_National)))
                                         )
                                     ),
                                     
                                     box(h3("Trafic International"),background = "black",width = 450,solidHeader = T,
                                         fluidPage(
                                           sliderInput(inputId = "Voix_International",label = "Minutes Internationales", min = min(DATA_$Voix_International), max = max(DATA_$Voix_International) ,value = c(min(DATA_$Voix_International),max(DATA_$Voix_International))),
                                           sliderInput(inputId = "Voix_International_Nb",label = "Nombre d'appels Internationals", min = min(DATA_$Voix_International_Nb), max = max(DATA_$Voix_International_Nb) ,value = c(min(DATA_$Voix_International_Nb),max(DATA_$Voix_International_Nb))),
                                           sliderInput(inputId = "Voix_NbContact_International",label = "Nombre de contacts International", min = min(DATA_$Voix_NbContact_International), max = max(DATA_$Voix_NbContact_International) ,value = c(min(DATA_$Voix_NbContact_International),max(DATA_$Voix_NbContact_International)))
                                         )
                                     ),
                                     
                                     box(h3("Trafic Roaming"),background = "black",width = 450,solidHeader = T,
                                         fluidPage(
                                           sliderInput(inputId = "Voix_Roaming",label = "Minutes Roaming", min = min(DATA_$Voix_Roaming), max = max(DATA_$Voix_Roaming),value = c(min(DATA_$Voix_Roaming),max(DATA_$Voix_Roaming))),
                                           sliderInput(inputId = "Voix_Roaming_Nb",label = "Nombre d'appels Roaming", min = min(DATA_$Voix_Roaming_Nb), max = max(DATA_$Voix_Roaming_Nb),value = c(min(DATA_$Voix_Roaming_Nb),max(DATA_$Voix_Roaming_Nb))),
                                           sliderInput(inputId = "Voix_NbContact_Roaming",label = "Nombre de contacts Roaming", min = min(DATA_$Voix_NbContact_Roaming), max = max(DATA_$Voix_NbContact_Roaming),value = c(min(DATA_$Voix_NbContact_Roaming),max(DATA_$Voix_NbContact_Roaming)))
                                         )
                                     )
                       )
                       ),
                       #SMS
                       fluidPage(box(title = "SMS" , collapsible = T, status = "info",solidHeader = T,width = 500,collapsed = T,background = "black",
                                     sliderInput(inputId = "SMS_Nbj_Activite",label = "Nombre de messages", min = min(DATA_$SMS_Nbj_Activite), max = max(DATA_$SMS_Nbj_Activite),value = c(min(DATA_$SMS_Nbj_Activite),max(DATA_$SMS_Nbj_Activite))),
                                     sliderInput(inputId = "SMS_NB",label = "Nombre de messages", min = min(DATA_$SMS_NB), max = max(DATA_$SMS_NB),value = c(min(DATA_$SMS_NB),max(DATA_$SMS_NB))),
                                     sliderInput(inputId = "SMS__Contacts",label = "Nombre de contacts SMS", min = min(DATA_$SMS__Contacts), max = max(DATA_$SMS__Contacts),value = c(min(DATA_$SMS__Contacts),max(DATA_$SMS__Contacts)))
                       )
                       ),
                       #Data
                       fluidPage(box(title = "Data" , collapsible = T, status = "info",solidHeader = T,width = 500,collapsed = T,background = "black",
                                     box(h3("Général"),background = "black",width = 450,solidHeader = T,
                                         fluidPage(    
                                           sliderInput(inputId = "Data_Nbj_Activite",label = "Nombre de jours activité Data",min = min(DATA_$Data_Nbj_Activite), max = max(DATA_$Data_Nbj_Activite),value = c(min(DATA_$Data_Nbj_Activite),max(DATA_$Data_Nbj_Activite))),
                                           sliderInput(inputId = "Data_Volume",label = "Volume en (Go)", min = min(DATA_$Data_Volume), max = max(DATA_$Data_Volume),value = c(min(DATA_$Data_Volume),max(DATA_$Data_Volume)))
                                           
                                         )
                                     ),
                                     box(h3("Technologie Radio"),background = "black",width = 450,solidHeader = T,
                                         fluidPage(    
                                           sliderInput(inputId = "Data_2G",label = "Volume en 2G (Go)", min = min(DATA_$Data_2G), max = max(DATA_$Data_2G),value = c(min(DATA_$Data_2G),max(DATA_$Data_2G))),
                                           sliderInput(inputId = "Data_3G",label = "Volume en 3G (Go)", min = min(DATA_$Data_3G), max = max(DATA_$Data_3G),value = c(min(DATA_$Data_3G),max(DATA_$Data_3G))),
                                           sliderInput(inputId = "Data_4G",label = "Volume en 4G (Go)", min = min(DATA_$Data_4G), max = max(DATA_$Data_4G),value = c(min(DATA_$Data_4G),max(DATA_$Data_4G)))
                                         )
                                     ),
                                     box(h3("Roaming"),background = "black",width = 450,solidHeader = T,
                                         fluidPage(    
                                           sliderInput(inputId = "Data_Roaming",label = "Volume Roaming (Go)", min = min(DATA_$Data_Roaming), max = max(DATA_$Data_Roaming),value = c(min(DATA_$Data_Roaming),max(DATA_$Data_Roaming)))
                                         )
                                     ),
                                     box( title = "Application",background = "black",width = 450,collapsible = T,collapsed = T,
                                          fluidPage(    
                                            sliderInput(inputId = "Reseaux_Sociaux",label = "Reseaux Sociaux", min = min(DATA_$Reseaux_Sociaux), max = max(DATA_$Reseaux_Sociaux),value = c(min(DATA_$Reseaux_Sociaux),max(DATA_$Reseaux_Sociaux))),
                                            sliderInput(inputId = "Web",label = "Web", min = min(DATA_$Web), max = max(DATA_$Web),value = c(min(DATA_$Web),max(DATA_$Web))),
                                            sliderInput(inputId = "News",label = "News", min = min(DATA_$News), max = max(DATA_$News),value = c(min(DATA_$News),max(DATA_$News)))
                                          )
                                     )        
                       )
                       )
                  ),
                  #Form O_Money
                  box( title="Orange Money", solidHeader = T, status ="success",collapsible = T,collapsed = T,background = "black",
                       box( title = "Payement de Facture",background = "black",width = 450,collapsible = T,collapsed = T,
                            fluidPage(    
                              sliderInput(inputId = "Omoney_Billpay_mnt",label = "Montant", min = min(DATA_$Omoney_Billpay_mnt), max = max(DATA_$Omoney_Billpay_mnt),value = c(min(DATA_$Omoney_Billpay_mnt),max(DATA_$Omoney_Billpay_mnt))),
                              sliderInput(inputId = "Omoney_Billpay_Nbr",label = "Nombre",min = min(DATA_$Omoney_Billpay_Nbr), max = max(DATA_$Omoney_Billpay_Nbr),value = c(min(DATA_$Omoney_Billpay_Nbr),max(DATA_$Omoney_Billpay_Nbr)))
                            )
                       ),
                       box( title = "Crédit",background = "black",width = 450,collapsible = T,collapsed = T,
                            fluidPage(    
                              sliderInput(inputId = "Omoney_Cashin_mnt",label = "Montant", min = min(DATA_$Omoney_Cashin_mnt), max = max(DATA_$Omoney_Cashin_mnt),value = c(min(DATA_$Omoney_Cashin_mnt),max(DATA_$Omoney_Cashin_mnt))),
                              sliderInput(inputId = "Omoney_Cashin_Nbr",label = "Nombre",min = min(DATA_$Omoney_Cashin_Nbr), max = max(DATA_$Omoney_Cashin_Nbr),value = c(min(DATA_$Omoney_Cashin_Nbr),max(DATA_$Omoney_Cashin_Nbr)))
                            )
                       ),
                       box( title = "Débit",background = "black",width = 450,collapsible = T,collapsed = T,
                            fluidPage(    
                              sliderInput(inputId = "Omoney_Cashout_mnt",label = "Montant", min = min(DATA_$Omoney_Cashout_mnt), max = max(DATA_$Omoney_Cashout_mnt),value = c(min(DATA_$Omoney_Cashout_mnt),max(DATA_$Omoney_Cashout_mnt))),
                              sliderInput(inputId = "Omoney_Cashout_Nbr",label = "Nombre", min = min(DATA_$Omoney_Cashout_Nbr), max = max(DATA_$Omoney_Cashout_Nbr),value = c(min(DATA_$Omoney_Cashout_Nbr),max(DATA_$Omoney_Cashout_Nbr)))
                            )
                       ),
                       box( title = "Payement de marchandise",background = "black",width = 450,collapsible = T,collapsed = T,
                            fluidPage(    
                              sliderInput(inputId = "Omoney_merchpay_mnt",label = "Montant", min = min(DATA_$Omoney_merchpay_mnt), max = max(DATA_$Omoney_merchpay_mnt),value = c(min(DATA_$Omoney_merchpay_mnt),max(DATA_$Omoney_merchpay_mnt))),
                              sliderInput(inputId = "Omoney_merchpay_Nbr",label = "Nombre",min = min(DATA_$Omoney_merchpay_Nbr), max = max(DATA_$Omoney_merchpay_Nbr),value = c(min(DATA_$Omoney_merchpay_Nbr),max(DATA_$Omoney_merchpay_Nbr)))
                            )
                       ),
                       box( title = "Transfert d'argent",background = "black",width = 450,collapsible = T,collapsed = T,
                            fluidPage(    
                              sliderInput(inputId = "Omoney_P2P_mnt",label = "Montant", min = min(DATA_$Omoney_P2P_mnt), max = max(DATA_$Omoney_P2P_mnt),value = c(min(DATA_$Omoney_P2P_mnt),max(DATA_$Omoney_P2P_mnt))),
                              sliderInput(inputId = "Omoney_P2P_Nbr",label = "Nombre", min = min(DATA_$Omoney_P2P_Nbr), max = max(DATA_$Omoney_P2P_Nbr),value = c(min(DATA_$Omoney_P2P_Nbr),max(DATA_$Omoney_P2P_Nbr)))
                            )
                       ),
                       box( title = "Recharge Télecom",background = "black",width = 450,collapsible = T, collapsed = T,
                            fluidPage(    
                              sliderInput(inputId = "Omoney_RC_mnt",label = "Montant", min = min(DATA_$Omoney_RC_mnt), max = max(DATA_$Omoney_RC_mnt),value = c(min(DATA_$Omoney_RC_mnt),max(DATA_$Omoney_RC_mnt))),
                              sliderInput(inputId = "Omoney_RC_Nbr",label = "Nombre", min = min(DATA_$Omoney_RC_Nbr), max = max(DATA_$Omoney_RC_Nbr),value = c(min(DATA_$Omoney_RC_Nbr),max(DATA_$Omoney_RC_Nbr)))
                            )
                       )
                  )
                  ,
                  #Form Device
                  box( title="Equipement", solidHeader = T, status ="success",collapsible = T,collapsed = T,background = "black",
                       selectInput("Equipement_Marque","Marque",choices = levels(as.factor(DATA_$Equipement_Marque)), multiple = TRUE,selected = levels(as.factor(DATA_$Equipement_Marque))),
                       selectInput("Equipement_Modele","Modèle",choices =levels(as.factor(DATA_$Equipement_Modele)), multiple = TRUE,selected = levels(as.factor(DATA_$Equipement_Modele))),
                       selectInput("Equipement_Term_Type","Type de Terminal",choices = levels(as.factor(DATA_$Equipement_Term_Type)), multiple = TRUE, selected = levels(as.factor(DATA_$Equipement_Term_Type))),
                       selectInput("Equipement_4G","Compatibilité 4G",choices = levels(as.factor(DATA_$Equipement_4G)), multiple = TRUE, selected = levels(as.factor(DATA_$Equipement_4G))),
                       selectInput("Equipement_OS","Système d'exploitation",choices = levels(as.factor(DATA_$Equipement_OS)), multiple = TRUE,selected = levels(as.factor(DATA_$Equipement_OS)))
                  ),
                  #Form Out of Pocket
                  box( title="Out of Pocket", solidHeader = T, status ="success",collapsible = T,collapsed = T, background = "black",
                       sliderInput(inputId = "OOP_Telecom",label = "Budget Telecom", min = min(DATA_$OOP_Telecom), max = max(DATA_$OOP_Telecom),value = c(min(DATA_$OOP_Telecom),max(DATA_$OOP_Telecom))),
                       sliderInput(inputId = "OOP_Glon",label = "Budget Global", min = min(DATA_$OOP_Glon), max = max(DATA_$OOP_Glon),value = c(min(DATA_$OOP_Glon),max(DATA_$OOP_Glon))),
                       sliderInput(inputId = "OOP_Assurances",label = "Budget Assurances", min = min(DATA_$OOP_Assurances), max = max(DATA_$OOP_Assurances),value = c(min(DATA_$OOP_Assurances),max(DATA_$OOP_Assurances))),
                       sliderInput(inputId = "OOP_Automobile",label = "Budget Automobile",min = min(DATA_$OOP_Automobile), max = max(DATA_$OOP_Automobile),value = c(min(DATA_$OOP_Automobile),max(DATA_$OOP_Automobile))),
                       sliderInput(inputId = "OOP_Bank",label = "Budget Banque",min = min(DATA_$OOP_Bank), max = max(DATA_$OOP_Bank),value = c(min(DATA_$OOP_Bank),max(DATA_$OOP_Bank))),
                       sliderInput(inputId = "OOP_Retail",label = "Budget Achat depuis Retail", min = min(DATA_$OOP_Retail), max = max(DATA_$OOP_Retail),value = c(min(DATA_$OOP_Retail),max(DATA_$OOP_Retail))),
                       sliderInput(inputId = "OOP_Hotels",label = "Budget Voyages", min = min(DATA_$OOP_Hotels), max = max(DATA_$OOP_Hotels),value = c(min(DATA_$OOP_Hotels),max(DATA_$OOP_Hotels)))
                  ),
                  #Liste personnalisée
                  box( title="Liste prédéfinie", solidHeader = T,status ="warning",width = 12,collapsible = T,collapsed = T, background = "black",
                       fileInput("file1", "Liste de lignes prédéfinies (.csv)",
                                 multiple = FALSE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv"))
                  )
                  
              )
              ,
              box(title ="Sélection de la Population 2",width = 400, status = "danger",solidHeader = T, background = "black",collapsible = T,collapsed = T,
                  #Form Client 
                  box( title="Informations Client", solidHeader = T, status ="danger",collapsible = T,collapsed = T,background = "black",
                       sliderInput(inputId = "Client_Age2",label = "Age", min = min(DATA_$Client_Age), max = max(DATA_$Client_Age),value = c(min(DATA_$Client_Age),max(DATA_$Client_Age)),step = 1),
                       dateRangeInput(inputId= "Client_Crea2", label="Date de Création", start =as.Date("1980/01/01"), end = Sys.Date(),format = "yyyy-mm-dd", startview = "year", weekstart = 0, language = "en", separator = " jusqu\'au "),
                       checkboxGroupInput("Client_Type2", "Parc Client", choices = levels(as.factor(DATA_$Client_Type)), selected = levels(as.factor(DATA_$Client_Type))),
                       checkboxGroupInput("Client_Offer2", "Plan Tarifaire", choices = levels(as.factor(DATA_$Client_Offer)),selected = levels(as.factor(DATA_$Client_Offer)) )
                       
                       
                  ),
                  #Form Geoloc 
                  box( title="Géolocalisation", solidHeader = T, status ="danger",collapsible = T,collapsed = T,background = "black",
                       #"Geoloc_commune"   "Geoloc_ville"     "Geoloc_region"    "Geoloc_type" 
                       selectInput("Geoloc_commune2","Commune",choices = levels(as.factor(DATA_$Geoloc_commune)), multiple = TRUE,selected = levels(as.factor(DATA_$Geoloc_commune)),selectize = F,size = 3),
                       selectInput("Geoloc_ville2","Ville",choices = levels(as.factor(DATA_$Geoloc_ville)), multiple = TRUE , selected = levels(as.factor(DATA_$Geoloc_ville)),selectize = F,size = 3),
                       selectInput("Geoloc_region2","Région",choices = levels(as.factor(DATA_$Geoloc_region)) , multiple = TRUE, selected = levels(as.factor(DATA_$Geoloc_region)),selectize = F,size = 3),
                       selectInput("Geoloc_type2","Type",choices = levels(as.factor(DATA_$Geoloc_type)), multiple = TRUE, selected=levels(as.factor(DATA_$Geoloc_type)))
                  ),
                  #Form Communauté
                  box( title="Communauté", solidHeader = T, status ="danger",collapsible = T,collapsed = T,background = "black",
                       selectInput("Communaute_Role2","Role",choices = levels(as.factor(DATA_$Communaute_Role)), multiple = TRUE,selected = levels(as.factor(DATA_$Communaute_Role)) ),
                       sliderInput(inputId = "Communaute_Taille2",label = "Taille de communauté", min = min(DATA_$Communaute_Taille), max = max(DATA_$Communaute_Taille),value = c(min(DATA_$Communaute_Taille),max(DATA_$Communaute_Taille)))
                  ),
                  
                  #Form Profil 
                  box( title="Profiling", solidHeader = T, status ="danger",collapsible = T,collapsed = T,background = "black",
                       selectInput("Profile_12","Life Style",choices =levels(as.factor(DATA_$Profile_1)), multiple = TRUE,selected = levels(as.factor(DATA_$Profile_1)) ),
                       selectInput("Profile_22","Gestion de Solde",choices = levels(as.factor(DATA_$Profile_2)), multiple = TRUE ,selected = levels(as.factor(DATA_$Profile_2)) ),
                       selectInput("Profile_32","Spending",choices =levels(as.factor(DATA_$Profile_3)), multiple = TRUE ,selected = levels(as.factor(DATA_$Profile_3)) )
                  ),
                  
                  #Form Conso 
                  box( title="Consomation", solidHeader = T, status ="danger",collapsible = T,collapsed = T, background = "black", 
                       #Voix
                       fluidPage(box(title = "Voix" , collapsible = T, status = "info",solidHeader = T,width = 500,collapsed = T,background = "black",
                                     
                                     box(h3("Nombre jours d'Activité"),background = "black",width = 450,solidHeader = T,
                                         fluidPage(    
                                           sliderInput(inputId = "Voix_Nbj_Activite2",label = "", min = min(DATA_$Voix_Nbj_Activite), max = max(DATA_$Voix_Nbj_Activite),value = c(min(DATA_$Voix_Nbj_Activite),max(DATA_$Voix_Nbj_Activite)))
                                         )
                                     ),
                                     
                                     box(h3("Trafic National"),background = "black",width = 450,solidHeader = T,
                                         fluidPage(    
                                           sliderInput(inputId = "Voix_National2",label = "Minutes Nationales", min = min(DATA_$Voix_National), max = max(DATA_$Voix_National) ,value = c(min(DATA_$Voix_National),max(DATA_$Voix_National))),
                                           sliderInput(inputId = "Voix_National_Nb2",label = "Nombre d'appels Nationals", min = min(DATA_$Voix_National_Nb), max = max(DATA_$Voix_National_Nb) ,value = c(min(DATA_$Voix_National_Nb),max(DATA_$Voix_National_Nb))),
                                           sliderInput(inputId = "Voix_NbContact_National2",label = "Nombre de contacts National", min = min(DATA_$Voix_NbContact_National) , max = max(DATA_$Voix_NbContact_National) ,value = c(min(DATA_$Voix_NbContact_National),max(DATA_$Voix_NbContact_National)))
                                         )
                                     ),
                                     
                                     box(h3("Trafic International"),background = "black",width = 450,solidHeader = T,
                                         fluidPage(
                                           sliderInput(inputId = "Voix_International2",label = "Minutes Internationales", min = min(DATA_$Voix_International), max = max(DATA_$Voix_International) ,value = c(min(DATA_$Voix_International),max(DATA_$Voix_International))),
                                           sliderInput(inputId = "Voix_International_Nb2",label = "Nombre d'appels Internationals", min = min(DATA_$Voix_International_Nb), max = max(DATA_$Voix_International_Nb) ,value = c(min(DATA_$Voix_International_Nb),max(DATA_$Voix_International_Nb))),
                                           sliderInput(inputId = "Voix_NbContact_International2",label = "Nombre de contacts International", min = min(DATA_$Voix_NbContact_International), max = max(DATA_$Voix_NbContact_International) ,value = c(min(DATA_$Voix_NbContact_International),max(DATA_$Voix_NbContact_International)))
                                         )
                                     ),
                                     
                                     box(h3("Trafic Roaming"),background = "black",width = 450,solidHeader = T,
                                         fluidPage(
                                           sliderInput(inputId = "Voix_Roaming2",label = "Minutes Roaming", min = min(DATA_$Voix_Roaming), max = max(DATA_$Voix_Roaming),value = c(min(DATA_$Voix_Roaming),max(DATA_$Voix_Roaming))),
                                           sliderInput(inputId = "Voix_Roaming_Nb2",label = "Nombre d'appels Roaming", min = min(DATA_$Voix_Roaming_Nb), max = max(DATA_$Voix_Roaming_Nb),value = c(min(DATA_$Voix_Roaming_Nb),max(DATA_$Voix_Roaming_Nb))),
                                           sliderInput(inputId = "Voix_NbContact_Roaming2",label = "Nombre de contacts Roaming", min = min(DATA_$Voix_NbContact_Roaming), max = max(DATA_$Voix_NbContact_Roaming),value = c(min(DATA_$Voix_NbContact_Roaming),max(DATA_$Voix_NbContact_Roaming)))
                                         )
                                     )
                       )
                       ),
                       #SMS
                       fluidPage(box(title = "SMS" , collapsible = T, status = "info",solidHeader = T,width = 500,collapsed = T,background = "black",
                                     sliderInput(inputId = "SMS_Nbj_Activite2",label = "Nombre de messages", min = min(DATA_$SMS_Nbj_Activite), max = max(DATA_$SMS_Nbj_Activite),value = c(min(DATA_$SMS_Nbj_Activite),max(DATA_$SMS_Nbj_Activite))),
                                     sliderInput(inputId = "SMS_NB2",label = "Nombre de messages", min = min(DATA_$SMS_NB), max = max(DATA_$SMS_NB),value = c(min(DATA_$SMS_NB),max(DATA_$SMS_NB))),
                                     sliderInput(inputId = "SMS__Contacts2",label = "Nombre de contacts SMS", min = min(DATA_$SMS__Contacts), max = max(DATA_$SMS__Contacts),value = c(min(DATA_$SMS__Contacts),max(DATA_$SMS__Contacts)))
                       )
                       ),
                       #Data
                       fluidPage(box(title = "Data" , collapsible = T, status = "info",solidHeader = T,width = 500,collapsed = T,background = "black",
                                     box(h3("Général"),background = "black",width = 450,solidHeader = T,
                                         fluidPage(    
                                           sliderInput(inputId = "Data_Nbj_Activite2",label = "Nombre de jours activité Data",min = min(DATA_$Data_Nbj_Activite), max = max(DATA_$Data_Nbj_Activite),value = c(min(DATA_$Data_Nbj_Activite),max(DATA_$Data_Nbj_Activite))),
                                           sliderInput(inputId = "Data_Volume2",label = "Volume en (Go)", min = min(DATA_$Data_Volume), max = max(DATA_$Data_Volume),value = c(min(DATA_$Data_Volume),max(DATA_$Data_Volume)))
                                           
                                         )
                                     ),
                                     box(h3("Technologie Radio"),background = "black",width = 450,solidHeader = T,
                                         fluidPage(    
                                           sliderInput(inputId = "Data_2G2",label = "Volume en 2G (Go)", min = min(DATA_$Data_2G), max = max(DATA_$Data_2G),value = c(min(DATA_$Data_2G),max(DATA_$Data_2G))),
                                           sliderInput(inputId = "Data_3G2",label = "Volume en 3G (Go)", min = min(DATA_$Data_3G), max = max(DATA_$Data_3G),value = c(min(DATA_$Data_3G),max(DATA_$Data_3G))),
                                           sliderInput(inputId = "Data_4G2",label = "Volume en 4G (Go)", min = min(DATA_$Data_4G), max = max(DATA_$Data_4G),value = c(min(DATA_$Data_4G),max(DATA_$Data_4G)))
                                         )
                                     ),
                                     box(h3("Roaming"),background = "black",width = 450,solidHeader = T,
                                         fluidPage(    
                                           sliderInput(inputId = "Data_Roaming2",label = "Volume Roaming (Go)", min = min(DATA_$Data_Roaming), max = max(DATA_$Data_Roaming),value = c(min(DATA_$Data_Roaming),max(DATA_$Data_Roaming)))
                                         )
                                     ),
                                     box( title = "Application",background = "black",width = 450,collapsible = T,collapsed = T,
                                          fluidPage(    
                                            sliderInput(inputId = "Reseaux_Sociaux2",label = "Reseaux Sociaux", min = min(DATA_$Reseaux_Sociaux), max = max(DATA_$Reseaux_Sociaux),value = c(min(DATA_$Reseaux_Sociaux),max(DATA_$Reseaux_Sociaux))),
                                            sliderInput(inputId = "Web2",label = "Web", min = min(DATA_$Web), max = max(DATA_$Web),value = c(min(DATA_$Web),max(DATA_$Web))),
                                            sliderInput(inputId = "News2",label = "News", min = min(DATA_$News), max = max(DATA_$News),value = c(min(DATA_$News),max(DATA_$News)))
                                          )
                                     )        
                       )
                       )
                  ),
                  #Form O_Money
                  box( title="Orange Money", solidHeader = T, status ="danger",collapsible = T,collapsed = T,background = "black",
                       box( title = "Payement de Facture",background = "black",width = 450,collapsible = T,collapsed = T,
                            fluidPage(    
                              sliderInput(inputId = "Omoney_Billpay_mnt2",label = "Montant", min = min(DATA_$Omoney_Billpay_mnt), max = max(DATA_$Omoney_Billpay_mnt),value = c(min(DATA_$Omoney_Billpay_mnt),max(DATA_$Omoney_Billpay_mnt))),
                              sliderInput(inputId = "Omoney_Billpay_Nbr2",label = "Nombre",min = min(DATA_$Omoney_Billpay_Nbr), max = max(DATA_$Omoney_Billpay_Nbr),value = c(min(DATA_$Omoney_Billpay_Nbr),max(DATA_$Omoney_Billpay_Nbr)))
                            )
                       ),
                       box( title = "Crédit",background = "black",width = 450,collapsible = T,collapsed = T,
                            fluidPage(    
                              sliderInput(inputId = "Omoney_Cashin_mnt2",label = "Montant", min = min(DATA_$Omoney_Cashin_mnt), max = max(DATA_$Omoney_Cashin_mnt),value = c(min(DATA_$Omoney_Cashin_mnt),max(DATA_$Omoney_Cashin_mnt))),
                              sliderInput(inputId = "Omoney_Cashin_Nbr2",label = "Nombre",min = min(DATA_$Omoney_Cashin_Nbr), max = max(DATA_$Omoney_Cashin_Nbr),value = c(min(DATA_$Omoney_Cashin_Nbr),max(DATA_$Omoney_Cashin_Nbr)))
                            )
                       ),
                       box( title = "Débit",background = "black",width = 450,collapsible = T,collapsed = T,
                            fluidPage(    
                              sliderInput(inputId = "Omoney_Cashout_mnt2",label = "Montant", min = min(DATA_$Omoney_Cashout_mnt), max = max(DATA_$Omoney_Cashout_mnt),value = c(min(DATA_$Omoney_Cashout_mnt),max(DATA_$Omoney_Cashout_mnt))),
                              sliderInput(inputId = "Omoney_Cashout_Nbr2",label = "Nombre", min = min(DATA_$Omoney_Cashout_Nbr), max = max(DATA_$Omoney_Cashout_Nbr),value = c(min(DATA_$Omoney_Cashout_Nbr),max(DATA_$Omoney_Cashout_Nbr)))
                            )
                       ),
                       box( title = "Payement de marchandise",background = "black",width = 450,collapsible = T,collapsed = T,
                            fluidPage(    
                              sliderInput(inputId = "Omoney_merchpay_mnt2",label = "Montant", min = min(DATA_$Omoney_merchpay_mnt), max = max(DATA_$Omoney_merchpay_mnt),value = c(min(DATA_$Omoney_merchpay_mnt),max(DATA_$Omoney_merchpay_mnt))),
                              sliderInput(inputId = "Omoney_merchpay_Nbr2",label = "Nombre",min = min(DATA_$Omoney_merchpay_Nbr), max = max(DATA_$Omoney_merchpay_Nbr),value = c(min(DATA_$Omoney_merchpay_Nbr),max(DATA_$Omoney_merchpay_Nbr)))
                            )
                       ),
                       box( title = "Transfert d'argent",background = "black",width = 450,collapsible = T,collapsed = T,
                            fluidPage(    
                              sliderInput(inputId = "Omoney_P2P_mnt2",label = "Montant", min = min(DATA_$Omoney_P2P_mnt), max = max(DATA_$Omoney_P2P_mnt),value = c(min(DATA_$Omoney_P2P_mnt),max(DATA_$Omoney_P2P_mnt))),
                              sliderInput(inputId = "Omoney_P2P_Nbr2",label = "Nombre", min = min(DATA_$Omoney_P2P_Nbr), max = max(DATA_$Omoney_P2P_Nbr),value = c(min(DATA_$Omoney_P2P_Nbr),max(DATA_$Omoney_P2P_Nbr)))
                            )
                       ),
                       box( title = "Recharge Télecom",background = "black",width = 450,collapsible = T, collapsed = T,
                            fluidPage(    
                              sliderInput(inputId = "Omoney_RC_mnt2",label = "Montant", min = min(DATA_$Omoney_RC_mnt), max = max(DATA_$Omoney_RC_mnt),value = c(min(DATA_$Omoney_RC_mnt),max(DATA_$Omoney_RC_mnt))),
                              sliderInput(inputId = "Omoney_RC_Nbr2",label = "Nombre", min = min(DATA_$Omoney_RC_Nbr), max = max(DATA_$Omoney_RC_Nbr),value = c(min(DATA_$Omoney_RC_Nbr),max(DATA_$Omoney_RC_Nbr)))
                            )
                       )
                  )
                  ,
                  #Form Device
                  box( title="Equipement", solidHeader = T, status ="danger",collapsible = T,collapsed = T,background = "black",
                       selectInput("Equipement_Marque2","Marque",choices = levels(as.factor(DATA_$Equipement_Marque)), multiple = TRUE,selected = levels(as.factor(DATA_$Equipement_Marque))),
                       selectInput("Equipement_Modele2","Modèle",choices =levels(as.factor(DATA_$Equipement_Modele)), multiple = TRUE,selected = levels(as.factor(DATA_$Equipement_Modele))),
                       selectInput("Equipement_Term_Type2","Type de Terminal",choices = levels(as.factor(DATA_$Equipement_Term_Type)), multiple = TRUE, selected = levels(as.factor(DATA_$Equipement_Term_Type))),
                       selectInput("Equipement_4G2","Compatibilité 4G",choices = levels(as.factor(DATA_$Equipement_4G)), multiple = TRUE, selected = levels(as.factor(DATA_$Equipement_4G))),
                       selectInput("Equipement_OS2","Système d'exploitation",choices = levels(as.factor(DATA_$Equipement_OS)), multiple = TRUE,selected = levels(as.factor(DATA_$Equipement_OS)))
                  ),
                  #Form Out of Pocket
                  box( title="Out of Pocket", solidHeader = T, status ="danger",collapsible = T,collapsed = T, background = "black",
                       sliderInput(inputId = "OOP_Telecom2",label = "Budget Telecom", min = min(DATA_$OOP_Telecom), max = max(DATA_$OOP_Telecom),value = c(min(DATA_$OOP_Telecom),max(DATA_$OOP_Telecom))),
                       sliderInput(inputId = "OOP_Glon2",label = "Budget Global", min = min(DATA_$OOP_Glon), max = max(DATA_$OOP_Glon),value = c(min(DATA_$OOP_Glon),max(DATA_$OOP_Glon))),
                       sliderInput(inputId = "OOP_Assurances2",label = "Budget Assurances", min = min(DATA_$OOP_Assurances), max = max(DATA_$OOP_Assurances),value = c(min(DATA_$OOP_Assurances),max(DATA_$OOP_Assurances))),
                       sliderInput(inputId = "OOP_Automobile2",label = "Budget Automobile",min = min(DATA_$OOP_Automobile), max = max(DATA_$OOP_Automobile),value = c(min(DATA_$OOP_Automobile),max(DATA_$OOP_Automobile))),
                       sliderInput(inputId = "OOP_Bank2",label = "Budget Banque",min = min(DATA_$OOP_Bank), max = max(DATA_$OOP_Bank),value = c(min(DATA_$OOP_Bank),max(DATA_$OOP_Bank))),
                       sliderInput(inputId = "OOP_Retail2",label = "Budget Achat depuis Retail", min = min(DATA_$OOP_Retail), max = max(DATA_$OOP_Retail),value = c(min(DATA_$OOP_Retail),max(DATA_$OOP_Retail))),
                       sliderInput(inputId = "OOP_Hotels2",label = "Budget Voyages", min = min(DATA_$OOP_Hotels), max = max(DATA_$OOP_Hotels),value = c(min(DATA_$OOP_Hotels),max(DATA_$OOP_Hotels)))
                  ),
                  #Liste personnalisée
                  box( title="Liste prédéfinie", solidHeader = T,status ="warning",width = 12,collapsible = T,collapsed = T, background = "black",
                       fileInput("file2", "Liste de lignes prédéfinies (.csv)",
                                 multiple = FALSE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv"))
                  )
              )
              ,
              
              box(title ="Sélection de la Population 3",width = 400, status = "primary",solidHeader = T, background = "black",collapsible = T,collapsed = T,
                  #Form Client 
                  box( title="Informations Client", solidHeader = T, status ="primary",collapsible = T,collapsed = T,background = "black",
                       sliderInput(inputId = "Client_Age3",label = "Age", min = min(DATA_$Client_Age), max = max(DATA_$Client_Age),value = c(min(DATA_$Client_Age),max(DATA_$Client_Age)),step = 1),
                       dateRangeInput(inputId= "Client_Crea3", label="Date de Création", start =as.Date("1980/01/01"), end = Sys.Date(),format = "yyyy-mm-dd", startview = "year", weekstart = 0, language = "en", separator = " jusqu\'au "),
                       checkboxGroupInput("Client_Type3", "Parc Client", choices = levels(as.factor(DATA_$Client_Type)), selected = levels(as.factor(DATA_$Client_Type))),
                       checkboxGroupInput("Client_Offer3", "Plan Tarifaire", choices = levels(as.factor(DATA_$Client_Offer)),selected = levels(as.factor(DATA_$Client_Offer)) )
                       
                       
                  ),
                  #Form Geoloc 
                  box( title="Géolocalisation", solidHeader = T, status ="primary",collapsible = T,collapsed = T,background = "black",
                       #"Geoloc_commune"   "Geoloc_ville"     "Geoloc_region"    "Geoloc_type" 
                       selectInput("Geoloc_commune3","Commune",choices = levels(as.factor(DATA_$Geoloc_commune)), multiple = TRUE,selected = levels(as.factor(DATA_$Geoloc_commune)),selectize = F,size = 3),
                       selectInput("Geoloc_ville3","Ville",choices = levels(as.factor(DATA_$Geoloc_ville)), multiple = TRUE , selected = levels(as.factor(DATA_$Geoloc_ville)),selectize = F,size = 3),
                       selectInput("Geoloc_region3","Région",choices = levels(as.factor(DATA_$Geoloc_region)) , multiple = TRUE, selected = levels(as.factor(DATA_$Geoloc_region)),selectize = F,size = 3),
                       selectInput("Geoloc_type3","Type",choices = levels(as.factor(DATA_$Geoloc_type)), multiple = TRUE, selected=levels(as.factor(DATA_$Geoloc_type)))
                  ),
                  #Form Communauté
                  box( title="Communauté", solidHeader = T, status ="primary",collapsible = T,collapsed = T,background = "black",
                       selectInput("Communaute_Role3","Role",choices = levels(as.factor(DATA_$Communaute_Role)), multiple = TRUE,selected = levels(as.factor(DATA_$Communaute_Role)) ),
                       sliderInput(inputId = "Communaute_Taille3",label = "Taille de communauté", min = min(DATA_$Communaute_Taille), max = max(DATA_$Communaute_Taille),value = c(min(DATA_$Communaute_Taille),max(DATA_$Communaute_Taille)))
                  ),
                  
                  #Form Profil 
                  box( title="Profiling", solidHeader = T, status ="primary",collapsible = T,collapsed = T,background = "black",
                       selectInput("Profile_13","Life Style",choices =levels(as.factor(DATA_$Profile_1)), multiple = TRUE,selected = levels(as.factor(DATA_$Profile_1)) ),
                       selectInput("Profile_23","Gestion de Solde",choices = levels(as.factor(DATA_$Profile_2)), multiple = TRUE ,selected = levels(as.factor(DATA_$Profile_2)) ),
                       selectInput("Profile_33","Spending",choices =levels(as.factor(DATA_$Profile_3)), multiple = TRUE ,selected = levels(as.factor(DATA_$Profile_3)) )
                  ),
                  
                  #Form Conso 
                  box( title="Consomation", solidHeader = T, status ="primary",collapsible = T,collapsed = T, background = "black", 
                       #Voix
                       fluidPage(box(title = "Voix" , collapsible = T, status = "info",solidHeader = T,width = 500,collapsed = T,background = "black",
                                     
                                     box(h3("Nombre jours d'Activité"),background = "black",width = 450,solidHeader = T,
                                         fluidPage(    
                                           sliderInput(inputId = "Voix_Nbj_Activite3",label = "", min = min(DATA_$Voix_Nbj_Activite), max = max(DATA_$Voix_Nbj_Activite),value = c(min(DATA_$Voix_Nbj_Activite),max(DATA_$Voix_Nbj_Activite)))
                                         )
                                     ),
                                     
                                     box(h3("Trafic National"),background = "black",width = 450,solidHeader = T,
                                         fluidPage(    
                                           sliderInput(inputId = "Voix_National3",label = "Minutes Nationales", min = min(DATA_$Voix_National), max = max(DATA_$Voix_National) ,value = c(min(DATA_$Voix_National),max(DATA_$Voix_National))),
                                           sliderInput(inputId = "Voix_National_Nb3",label = "Nombre d'appels Nationals", min = min(DATA_$Voix_National_Nb), max = max(DATA_$Voix_National_Nb) ,value = c(min(DATA_$Voix_National_Nb),max(DATA_$Voix_National_Nb))),
                                           sliderInput(inputId = "Voix_NbContact_National3",label = "Nombre de contacts National", min = min(DATA_$Voix_NbContact_National) , max = max(DATA_$Voix_NbContact_National) ,value = c(min(DATA_$Voix_NbContact_National),max(DATA_$Voix_NbContact_National)))
                                         )
                                     ),
                                     
                                     box(h3("Trafic International"),background = "black",width = 450,solidHeader = T,
                                         fluidPage(
                                           sliderInput(inputId = "Voix_International3",label = "Minutes Internationales", min = min(DATA_$Voix_International), max = max(DATA_$Voix_International) ,value = c(min(DATA_$Voix_International),max(DATA_$Voix_International))),
                                           sliderInput(inputId = "Voix_International_Nb3",label = "Nombre d'appels Internationals", min = min(DATA_$Voix_International_Nb), max = max(DATA_$Voix_International_Nb) ,value = c(min(DATA_$Voix_International_Nb),max(DATA_$Voix_International_Nb))),
                                           sliderInput(inputId = "Voix_NbContact_International3",label = "Nombre de contacts International", min = min(DATA_$Voix_NbContact_International), max = max(DATA_$Voix_NbContact_International) ,value = c(min(DATA_$Voix_NbContact_International),max(DATA_$Voix_NbContact_International)))
                                         )
                                     ),
                                     
                                     box(h3("Trafic Roaming"),background = "black",width = 450,solidHeader = T,
                                         fluidPage(
                                           sliderInput(inputId = "Voix_Roaming3",label = "Minutes Roaming", min = min(DATA_$Voix_Roaming), max = max(DATA_$Voix_Roaming),value = c(min(DATA_$Voix_Roaming),max(DATA_$Voix_Roaming))),
                                           sliderInput(inputId = "Voix_Roaming_Nb3",label = "Nombre d'appels Roaming", min = min(DATA_$Voix_Roaming_Nb), max = max(DATA_$Voix_Roaming_Nb),value = c(min(DATA_$Voix_Roaming_Nb),max(DATA_$Voix_Roaming_Nb))),
                                           sliderInput(inputId = "Voix_NbContact_Roaming3",label = "Nombre de contacts Roaming", min = min(DATA_$Voix_NbContact_Roaming), max = max(DATA_$Voix_NbContact_Roaming),value = c(min(DATA_$Voix_NbContact_Roaming),max(DATA_$Voix_NbContact_Roaming)))
                                         )
                                     )
                       )
                       ),
                       #SMS
                       fluidPage(box(title = "SMS" , collapsible = T, status = "info",solidHeader = T,width = 500,collapsed = T,background = "black",
                                     sliderInput(inputId = "SMS_Nbj_Activite3",label = "Nombre de messages", min = min(DATA_$SMS_Nbj_Activite), max = max(DATA_$SMS_Nbj_Activite),value = c(min(DATA_$SMS_Nbj_Activite),max(DATA_$SMS_Nbj_Activite))),
                                     sliderInput(inputId = "SMS_NB3",label = "Nombre de messages", min = min(DATA_$SMS_NB), max = max(DATA_$SMS_NB),value = c(min(DATA_$SMS_NB),max(DATA_$SMS_NB))),
                                     sliderInput(inputId = "SMS__Contacts3",label = "Nombre de contacts SMS", min = min(DATA_$SMS__Contacts), max = max(DATA_$SMS__Contacts),value = c(min(DATA_$SMS__Contacts),max(DATA_$SMS__Contacts)))
                       )
                       ),
                       #Data
                       fluidPage(box(title = "Data" , collapsible = T, status = "info",solidHeader = T,width = 500,collapsed = T,background = "black",
                                     box(h3("Général"),background = "black",width = 450,solidHeader = T,
                                         fluidPage(    
                                           sliderInput(inputId = "Data_Nbj_Activite3",label = "Nombre de jours activité Data",min = min(DATA_$Data_Nbj_Activite), max = max(DATA_$Data_Nbj_Activite),value = c(min(DATA_$Data_Nbj_Activite),max(DATA_$Data_Nbj_Activite))),
                                           sliderInput(inputId = "Data_Volume3",label = "Volume en (Go)", min = min(DATA_$Data_Volume), max = max(DATA_$Data_Volume),value = c(min(DATA_$Data_Volume),max(DATA_$Data_Volume)))
                                           
                                         )
                                     ),
                                     box(h3("Technologie Radio"),background = "black",width = 450,solidHeader = T,
                                         fluidPage(    
                                           sliderInput(inputId = "Data_2G3",label = "Volume en 2G (Go)", min = min(DATA_$Data_2G), max = max(DATA_$Data_2G),value = c(min(DATA_$Data_2G),max(DATA_$Data_2G))),
                                           sliderInput(inputId = "Data_3G3",label = "Volume en 3G (Go)", min = min(DATA_$Data_3G), max = max(DATA_$Data_3G),value = c(min(DATA_$Data_3G),max(DATA_$Data_3G))),
                                           sliderInput(inputId = "Data_4G3",label = "Volume en 4G (Go)", min = min(DATA_$Data_4G), max = max(DATA_$Data_4G),value = c(min(DATA_$Data_4G),max(DATA_$Data_4G)))
                                         )
                                     ),
                                     box(h3("Roaming"),background = "black",width = 450,solidHeader = T,
                                         fluidPage(    
                                           sliderInput(inputId = "Data_Roaming3",label = "Volume Roaming (Go)", min = min(DATA_$Data_Roaming), max = max(DATA_$Data_Roaming),value = c(min(DATA_$Data_Roaming),max(DATA_$Data_Roaming)))
                                         )
                                     ),
                                     box( title = "Application",background = "black",width = 450,collapsible = T,collapsed = T,
                                          fluidPage(    
                                            sliderInput(inputId = "Reseaux_Sociaux3",label = "Reseaux Sociaux", min = min(DATA_$Reseaux_Sociaux), max = max(DATA_$Reseaux_Sociaux),value = c(min(DATA_$Reseaux_Sociaux),max(DATA_$Reseaux_Sociaux))),
                                            sliderInput(inputId = "Web3",label = "Web", min = min(DATA_$Web), max = max(DATA_$Web),value = c(min(DATA_$Web),max(DATA_$Web))),
                                            sliderInput(inputId = "News3",label = "News", min = min(DATA_$News), max = max(DATA_$News),value = c(min(DATA_$News),max(DATA_$News)))
                                          )
                                     )        
                       )
                       )
                  ),
                  #Form O_Money
                  box( title="Orange Money", solidHeader = T, status ="primary",collapsible = T,collapsed = T,background = "black",
                       box( title = "Payement de Facture",background = "black",width = 450,collapsible = T,collapsed = T,
                            fluidPage(    
                              sliderInput(inputId = "Omoney_Billpay_mnt3",label = "Montant", min = min(DATA_$Omoney_Billpay_mnt), max = max(DATA_$Omoney_Billpay_mnt),value = c(min(DATA_$Omoney_Billpay_mnt),max(DATA_$Omoney_Billpay_mnt))),
                              sliderInput(inputId = "Omoney_Billpay_Nbr3",label = "Nombre",min = min(DATA_$Omoney_Billpay_Nbr), max = max(DATA_$Omoney_Billpay_Nbr),value = c(min(DATA_$Omoney_Billpay_Nbr),max(DATA_$Omoney_Billpay_Nbr)))
                            )
                       ),
                       box( title = "Crédit",background = "black",width = 450,collapsible = T,collapsed = T,
                            fluidPage(    
                              sliderInput(inputId = "Omoney_Cashin_mnt3",label = "Montant", min = min(DATA_$Omoney_Cashin_mnt), max = max(DATA_$Omoney_Cashin_mnt),value = c(min(DATA_$Omoney_Cashin_mnt),max(DATA_$Omoney_Cashin_mnt))),
                              sliderInput(inputId = "Omoney_Cashin_Nbr3",label = "Nombre",min = min(DATA_$Omoney_Cashin_Nbr), max = max(DATA_$Omoney_Cashin_Nbr),value = c(min(DATA_$Omoney_Cashin_Nbr),max(DATA_$Omoney_Cashin_Nbr)))
                            )
                       ),
                       box( title = "Débit",background = "black",width = 450,collapsible = T,collapsed = T,
                            fluidPage(    
                              sliderInput(inputId = "Omoney_Cashout_mnt3",label = "Montant", min = min(DATA_$Omoney_Cashout_mnt), max = max(DATA_$Omoney_Cashout_mnt),value = c(min(DATA_$Omoney_Cashout_mnt),max(DATA_$Omoney_Cashout_mnt))),
                              sliderInput(inputId = "Omoney_Cashout_Nbr3",label = "Nombre", min = min(DATA_$Omoney_Cashout_Nbr), max = max(DATA_$Omoney_Cashout_Nbr),value = c(min(DATA_$Omoney_Cashout_Nbr),max(DATA_$Omoney_Cashout_Nbr)))
                            )
                       ),
                       box( title = "Payement de marchandise",background = "black",width = 450,collapsible = T,collapsed = T,
                            fluidPage(    
                              sliderInput(inputId = "Omoney_merchpay_mnt3",label = "Montant", min = min(DATA_$Omoney_merchpay_mnt), max = max(DATA_$Omoney_merchpay_mnt),value = c(min(DATA_$Omoney_merchpay_mnt),max(DATA_$Omoney_merchpay_mnt))),
                              sliderInput(inputId = "Omoney_merchpay_Nbr3",label = "Nombre",min = min(DATA_$Omoney_merchpay_Nbr), max = max(DATA_$Omoney_merchpay_Nbr),value = c(min(DATA_$Omoney_merchpay_Nbr),max(DATA_$Omoney_merchpay_Nbr)))
                            )
                       ),
                       box( title = "Transfert d'argent",background = "black",width = 450,collapsible = T,collapsed = T,
                            fluidPage(    
                              sliderInput(inputId = "Omoney_P2P_mnt3",label = "Montant", min = min(DATA_$Omoney_P2P_mnt), max = max(DATA_$Omoney_P2P_mnt),value = c(min(DATA_$Omoney_P2P_mnt),max(DATA_$Omoney_P2P_mnt))),
                              sliderInput(inputId = "Omoney_P2P_Nbr3",label = "Nombre", min = min(DATA_$Omoney_P2P_Nbr), max = max(DATA_$Omoney_P2P_Nbr),value = c(min(DATA_$Omoney_P2P_Nbr),max(DATA_$Omoney_P2P_Nbr)))
                            )
                       ),
                       box( title = "Recharge Télecom",background = "black",width = 450,collapsible = T, collapsed = T,
                            fluidPage(    
                              sliderInput(inputId = "Omoney_RC_mnt3",label = "Montant", min = min(DATA_$Omoney_RC_mnt), max = max(DATA_$Omoney_RC_mnt),value = c(min(DATA_$Omoney_RC_mnt),max(DATA_$Omoney_RC_mnt))),
                              sliderInput(inputId = "Omoney_RC_Nbr3",label = "Nombre", min = min(DATA_$Omoney_RC_Nbr), max = max(DATA_$Omoney_RC_Nbr),value = c(min(DATA_$Omoney_RC_Nbr),max(DATA_$Omoney_RC_Nbr)))
                            )
                       )
                  )
                  ,
                  #Form Device
                  box( title="Equipement", solidHeader = T, status ="primary",collapsible = T,collapsed = T,background = "black",
                       selectInput("Equipement_Marque3","Marque",choices = levels(as.factor(DATA_$Equipement_Marque)), multiple = TRUE,selected = levels(as.factor(DATA_$Equipement_Marque))),
                       selectInput("Equipement_Modele3","Modèle",choices =levels(as.factor(DATA_$Equipement_Modele)), multiple = TRUE,selected = levels(as.factor(DATA_$Equipement_Modele))),
                       selectInput("Equipement_Term_Type3","Type de Terminal",choices = levels(as.factor(DATA_$Equipement_Term_Type)), multiple = TRUE, selected = levels(as.factor(DATA_$Equipement_Term_Type))),
                       selectInput("Equipement_4G3","Compatibilité 4G",choices = levels(as.factor(DATA_$Equipement_4G)), multiple = TRUE, selected = levels(as.factor(DATA_$Equipement_4G))),
                       selectInput("Equipement_OS3","Système d'exploitation",choices = levels(as.factor(DATA_$Equipement_OS)), multiple = TRUE,selected = levels(as.factor(DATA_$Equipement_OS)))
                  ),
                  #Form Out of Pocket
                  box( title="Out of Pocket", solidHeader = T, status ="primary",collapsible = T,collapsed = T, background = "black",
                       sliderInput(inputId = "OOP_Telecom3",label = "Budget Telecom", min = min(DATA_$OOP_Telecom), max = max(DATA_$OOP_Telecom),value = c(min(DATA_$OOP_Telecom),max(DATA_$OOP_Telecom))),
                       sliderInput(inputId = "OOP_Glon3",label = "Budget Global", min = min(DATA_$OOP_Glon), max = max(DATA_$OOP_Glon),value = c(min(DATA_$OOP_Glon),max(DATA_$OOP_Glon))),
                       sliderInput(inputId = "OOP_Assurances3",label = "Budget Assurances", min = min(DATA_$OOP_Assurances), max = max(DATA_$OOP_Assurances),value = c(min(DATA_$OOP_Assurances),max(DATA_$OOP_Assurances))),
                       sliderInput(inputId = "OOP_Automobile3",label = "Budget Automobile",min = min(DATA_$OOP_Automobile), max = max(DATA_$OOP_Automobile),value = c(min(DATA_$OOP_Automobile),max(DATA_$OOP_Automobile))),
                       sliderInput(inputId = "OOP_Bank3",label = "Budget Banque",min = min(DATA_$OOP_Bank), max = max(DATA_$OOP_Bank),value = c(min(DATA_$OOP_Bank),max(DATA_$OOP_Bank))),
                       sliderInput(inputId = "OOP_Retail3",label = "Budget Achat depuis Retail", min = min(DATA_$OOP_Retail), max = max(DATA_$OOP_Retail),value = c(min(DATA_$OOP_Retail),max(DATA_$OOP_Retail))),
                       sliderInput(inputId = "OOP_Hotels3",label = "Budget Voyages", min = min(DATA_$OOP_Hotels), max = max(DATA_$OOP_Hotels),value = c(min(DATA_$OOP_Hotels),max(DATA_$OOP_Hotels)))
                  ),
                  #Liste personnalisée
                  box( title="Liste prédéfinie", solidHeader = T,status ="warning",width = 12,collapsible = T,collapsed = T, background = "black",
                       fileInput("file3", "Liste de lignes prédéfinies (.csv)",
                                 multiple = FALSE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv"))
                  )
              )
              ,actionButton("validate",width = 150,"Valider",style="color: #fff; background-color: #00A65A; border-color: #00A65A"),
              hr()
            ), #, FIN FLUIDPAGE
            
            
            div(style="display:inline-block",plotlyOutput("Data1")),
            div(style="display:inline-block",plotlyOutput("Data2")),
            div(style="display:inline-block",plotlyOutput("Data3")),
            
            br(),
            br(),
            br(),
            uiOutput("Results")
            
            
            
    ),
    #Partie exploration / Secteurs
    
    tabItem(
      tabName = "Secteurs"
      
    ),
    tabItem(
      tabName = "Profiling",
      fluidPage(
        h1("Profiling des Populations par Axe"),
        radioButtons("Prof_type_glob","Type de profiling",choices = c("Global" = "soft", "Décisif"="hard"),selected = "soft",inline = T),
        div(tags$img(src="Key_1.png")),
        
        box(title ="",solidHeader = T,width = 300,
            dataTableOutput("Prof_glob"))
      )
    ),
    tabItem(
      tabName = "Assurances",
      fluidPage( 
      )
    ),
    tabItem(
      tabName = "Automobile",
      fluidPage( 
      )
    ),
    tabItem(
      tabName = "Bank",
      fluidPage(
      )
    ),
    tabItem(
      tabName = "Communication",
      fluidPage(
      )
    ),
    tabItem(
      tabName = "Retail",
      fluidPage( 
        h1("Retail "),
        tabsetPanel(type = "pills",
                    tabPanel(title = h4("Parc"),
                             br(),
                             column(12, offset = 2,h3("MAP"),radioButtons("MAP_parc_Select",label = h3("Choix d'indicateur :"),choices = c("Parc","Villes de Concentration"),inline = T),
                                    leafletOutput("MAP_parc",height =500, width = 800)),
                             br(),br(),br(),
                             column(12, offset = 2,h3("Spending in Major Cities"),
                                    leafletOutput("Map_OOP_Parc", height = 500, width = 800)),
                             br(),br(),br(),
                             column(12, offset = 2,h3("Points d'intérêt"),
                                  box( width = 12,
                                  selectInput("POI_Select","Point :",choices = levels(as.factor(POIS$POI)),multiple = T,selected = NULL),     
                                  leafletOutput("Map_POI_Parc", height = 500, width = 800))
                                  ) 
                    ),
                    tabPanel(title = h4("Population 1"),
                             br(),
                             column(12, offset = 2,h3("MAP"),radioButtons("MAP_p1_Select",label = h3("Choix d'indicateur :"),choices = c("Parc","Villes de Concentration"),inline = T),
                                    leafletOutput("MAP_p1",height =500, width = 800)),
                             br(),br(),br(),
                             column(12, offset = 2,h3("Spending in Major Cities"),
                                    leafletOutput("Map_OOP_Pop1", height = 500, width = 800))
                    ),
                    tabPanel(title = h4("Population 2"),
                             br(),
                             column(12, offset = 2,h3("MAP"),radioButtons("MAP_p2_Select",label = h3("Choix d'indicateur :"),choices = c("Parc","Villes de Concentration"),inline = T),
                                    leafletOutput("MAP_p2",height =500, width = 800)),
                             br(),br(),br(),
                             column(12, offset = 2,h3("Spending in Major Cities"),
                                    leafletOutput("Map_OOP_Pop2", height = 500, width = 800))       
                    ),
                    tabPanel(title = h4("Population 3"),
                             br(),
                             column(12, offset = 2,h3("MAP"),radioButtons("MAP_p3_Select",label = h3("Choix d'indicateur :"),choices = c("Parc","Villes de Concentration"),inline = T),
                                    leafletOutput("MAP_p3",height =500, width = 800)),
                             br(),br(),br(),
                             column(12, offset = 2,h3("Spending in Major Cities"),
                                    leafletOutput("Map_OOP_Pop3", height = 500, width = 800))
                    )
        )
      )
    ),
    tabItem(
      tabName = "Hotels",
      fluidPage(
      )
    ),
    tabItem(
      tabName = "Voyage",
      fluidPage(
      )
    ),
    tabItem(
      tabName = "Sport",
      fluidPage(
      )
    ),
    tabItem(
      tabName = "Entreprises",
      fluidPage(
      )
    ),
    tabItem(
      tabName = "Telecom",
      fluidPage( 
        h1("Vue sur les Equipements "),
        tabsetPanel(type = "pills",
                    tabPanel(title = h4("Parc"),
                             br(),br(),br(),
                             box(title = "Part de Marché", solidHeader = T, status ="warning",width = 12,
                                box(plotlyOutput("parc_MARQUES"),width = 4),
                                     box(plotlyOutput("parc_MARQUES_FTR"),width = 4),
                                box(plotlyOutput("parc_MARQUES_SMRT"),width = 4)
                             ),
                             box(title = "TOP 10 Devices", solidHeader = T, status ="warning",width = 12,
                                 plotlyOutput("parc_TOP10_Modeles")
                             ),
                             box(title = "Usage Telecom", solidHeader = T, status ="warning",width = 12,
                                 box(title = "Voix", solidHeader = T, status ="warning",
                                    div(style= 'overflow-x: scroll', dataTableOutput("parc_mean_voix"))
                                 ),
                                 box(title = "SMS", solidHeader = T, status ="warning",
                                     div(style= 'overflow-x: scroll', dataTableOutput("parc_mean_sms"))
                                 ),
                                 box(title = "Data", solidHeader = T, status ="warning",width=12,
                                     div(style= 'overflow-x: scroll', dataTableOutput("parc_mean_data"))
                                 ),
                                 box(title = "Centre d'interet", solidHeader = T, status ="warning",width=12,
                                     div(style= 'overflow-x: scroll',  dataTableOutput("parc_mean_reseaux_sociaux"))
                                 )  
                             ),
                             box(title = "Distribution des équipements", solidHeader = T, status ="warning",width = 12,
                                 br(),
                                 leafletOutput("MAP_parc_Equipement",height =500, width = '100%')
                             )
                    ),
                    tabPanel(title = h4("OEM Porfolio"),
                             br(),
                             br(),
                             box(title = "Portefolio des Marques", solidHeader = T, status ="primary",width = 12,
                                 column(width = 8,offset = 2,
                                 fluidRow(
                                   selectInput("Dash_Equipement",label = h3(strong("Marque :")),choices = levels(as.factor(DATA_$Equipement_Marque)), multiple = FALSE,selected = "",width ='100%'),
                                   br(),
                                   br(),
                                   h3(strong("Marché")),
                                   valueBoxOutput("Nbr_Device"),
                                   valueBoxOutput("Prt_Device"),
                                   valueBoxOutput("Top_Device"),
                                   hr(),
                                   h3(strong("Répartiton Géographique")),
                                   br(),
                                   leafletOutput("Dash_Equipement",height =500, width = '100%')
                                 ),
                                 br(),
                                 br(),
                                 br(),
                                   h2(strong("Usage moyen par Utilisateur")),
                                 br(),
                                 h3(strong("Voix")),
                                 br(),
                                 column(width = 4,gaugeOutput("Voix_gauge",width = '100%',height = 'auto')),
                                 column(width=4,gaugeOutput("Voix_Nb_gauge",width = '100%',height = 'auto')),
                                 column(width =4 ,gaugeOutput("Voix_Freq_gauge",width = '100%',height = 'auto')),
                                 br(),
                                 h3(strong("SMS")),
                                 br(),
                                 column(width = 6,gaugeOutput("SMS_gauge",width = '100%',height = 'auto')),
                                 column(width =6 ,gaugeOutput("SMS_Freq_gauge",width = '100%',height = 'auto')),
                                 br(),
                                 h3(strong("Data")),
                                 br(),
                                 column(width = 6,gaugeOutput("DATA_gauge",width = '100%',height = 'auto')),
                                 column(width =6 ,gaugeOutput("DATA_Freq_gauge",width = '100%',height = 'auto'))
                            
                                 )
                             )
                    
                    ),
                    tabPanel(title = h4("Population 1"),
                             br(),br(),br(),
                             box(title = "Part de Marché", solidHeader = T, status ="success",
                                 plotlyOutput("p1_MARQUES")
                             ),
                             box(title = "TOP 10 Devices", solidHeader = T, status ="success",
                                 plotlyOutput("p1_TOP10_Modeles")
                             ),
                             box(title = "Usage Telecom", solidHeader = T, status ="success",width = 12,
                                 box(title = "Voix", solidHeader = T, status ="success",
                                     div(style= 'overflow-x: scroll', dataTableOutput("p1_mean_voix"))
                                 ),
                                 box(title = "SMS", solidHeader = T, status ="success",
                                     div(style= 'overflow-x: scroll', dataTableOutput("p1_mean_sms"))
                                 ),
                                 box(title = "Data", solidHeader = T, status ="success",width=12,
                                     div(style= 'overflow-x: scroll', dataTableOutput("p1_mean_data"))
                                 ),
                                 box(title = "Centre d'interet", solidHeader = T, status ="success",width=12,
                                     div(style= 'overflow-x: scroll', dataTableOutput("p1_mean_reseaux_sociaux"))
                                 )
                             ),
                             box(title = "Distribution des Equipements", solidHeader = T, status ="success",width = 12,
                                 #selectInput("MAP_p1_Equipement","Marques :",choices = levels(as.factor(DATA_$Equipement_Marque)), multiple = TRUE,selected = "",width = 400),
                                 br(),
                                 br(),
                                 leafletOutput("MAP_p1_Equipement",height =500, width = '100%')
                             )
                    ),
                    tabPanel(title = h4("Population 2"),
                             br(),br(),br(),
                             box(title = "Part de Marché", solidHeader = T, status ="danger",
                                 plotlyOutput("p2_MARQUES")
                             ),
                             box(title = "TOP 10 Devices", solidHeader = T, status ="danger",
                                 plotlyOutput("p2_TOP10_Modeles")
                             ),
                             box(title = "Usage Telecom", solidHeader = T, status ="danger",width = 12,
                                 box(title = "Voix", solidHeader = T, status ="danger",
                                     div(style= 'overflow-x: scroll', dataTableOutput("p2_mean_voix"))
                                 ),
                                 box(title = "SMS", solidHeader = T, status ="danger",
                                     div(style= 'overflow-x: scroll', dataTableOutput("p2_mean_sms"))
                                 ),
                                 box(title = "Data", solidHeader = T, status ="danger",width=12,
                                     div(style= 'overflow-x: scroll',  dataTableOutput("p2_mean_data"))
                                 ),
                                 box(title = "Centre d'interet", solidHeader = T, status ="danger",width=12,
                                     div(style= 'overflow-x: scroll', dataTableOutput("p2_mean_reseaux_sociaux"))
                                 )
                             ),
                             box(title = "Distribution des Equipements", solidHeader = T, status ="danger",width = 12,
                                 #selectInput("MAP_p2_Equipement","Marques :",choices = levels(as.factor(DATA_$Equipement_Marque)), multiple = TRUE,selected = "",width = 400),
                                 br(),
                                 br(),
                                 leafletOutput("MAP_p2_Equipement",height =500, width = '100%')
                             )
                    ),
                    tabPanel(title = h4("Population 3"),
                             br(),br(),br(),
                             box(title = "Part de Marché", solidHeader = T, status ="primary",
                                 plotlyOutput("p3_MARQUES")
                             ),
                             box(title = "TOP 10 Devices", solidHeader = T, status ="primary",
                                 plotlyOutput("p3_TOP10_Modeles")
                             ),
                             box(title = "Usage Telecom", solidHeader = T, status ="primary",width = 12,
                                 box(title = "Voix", solidHeader = T, status ="primary",
                                     div(style= 'overflow-x: scroll', dataTableOutput("p3_mean_voix"))
                                 ),
                                 box(title = "SMS", solidHeader = T, status ="primary",
                                     div(style= 'overflow-x: scroll', dataTableOutput("p3_mean_sms"))
                                 ),
                                 box(title = "Data", solidHeader = T, status ="primary",width=12,
                                     div(style= 'overflow-x: scroll', dataTableOutput("p3_mean_data"))
                                 ),
                                 box(title = "Centre d'interet", solidHeader = T, status ="primary",width=12,
                                     div(style= 'overflow-x: scroll', dataTableOutput("p3_mean_reseaux_sociaux"))
                                 )
                             ),
                             box(title = "Distribution des Equipements", solidHeader = T, status ="primary",width = 12,
                                 #selectInput("MAP_p3_Equipement","Marques :",choices = levels(as.factor(DATA_$Equipement_Marque)), multiple = TRUE,selected = "",width = 400),
                                 br(),
                                 br(),
                                 leafletOutput("MAP_p3_Equipement",height =500, width = '100%')
                             )
                    )
        )
      )
    ),
    tabItem(
      tabName = "OTT",
      fluidPage( 
        h1("Over The Top Services "),
        tabsetPanel(type = "pills",
                    tabPanel(title = h4("Parc"),
                             br(),br(),br(),
                             box(title = "Centre D'intéret", solidHeader = T, status ="warning",width = 12,
                                 plotlyOutput("OTT_CI")
                                 ),
                             box(title = "Nombre d'utilisateurs de services", solidHeader = T, status ="warning",width = 12,
                                 selectInput("OTT_Service","Services :",choices = c("Netflix","Facebook","Twitter","Whatsapp"),multiple = TRUE),
                                 dataTableOutput("OTT_Service"))
                    ),
                    tabPanel(title = h4("Population 1"),
                             br(),br(),br(),
                             box(title = "Centre D'intéret", solidHeader = T, status ="success",width = 12)
                    ),
                    tabPanel(title = h4("Population 2"),
                             br(),br(),br(),
                             box(title = "Centre D'intéret", solidHeader = T, status ="danger",width = 12)
                    ),
                    tabPanel(title = h4("Population 3"),
                             br(),br(),br(),
                             box(title = "Centre D'intéret", solidHeader = T, status ="primary",width = 12)
                    )
        )   
      )
    ),
    
    #####Exportation
    tabItem(tabName = "Extract",
            fluidPage(
              
              column(5,offset = 3,
                     h2("Exportation de la cible"),
                     column(3,offset=2,downloadButton("downloadData", "Download"))
              )
            )
    )
  )#F_tabItems
)




##################################################################### FIN UI

############ Traitement Serveur ##########
server <- function(input, output,session) {
  #Initialisation des populations
  session_elements.Populations <- reactiveValues(Population1 = NULL,Population2 = NULL,Population3 = NULL)
  session_elements.Tabs <- reactiveValues(p2_region =NULL,p1_region=NULL,p3_region=NULL,
                                          p1_Terms_use=NULL,p2_Terms_use=NULL,p3_Terms_use=NULL,
                                          p1_TOP_Models=NULL,p2_TOP_Models=NULL,p3_TOP_Models=NULL,
                                          p1_Marques = NULL,p2_Marques = NULL,p3_Marques = NULL)
  
  
  #FOnctions réactives
  #Preparation de la table 
  
  profilier <- reactive({
    POP1_NUM <- as.data.table(Filter(is.numeric, session_elements.Populations$Population1))
    POP1_NUM$POP <- "P1"
    POP1_NUM$MSISDN <- NULL
    POP1_NUM <- POP1_NUM[,lapply(.SD,mean,na.rm=T),by=POP]
    POP1_NUM <- melt(POP1_NUM,id="POP")
    POP1_NUM$value <- round(POP1_NUM$value)
    POP1_NUM$POP <- NULL
    
    POP2_NUM <- as.data.table(Filter(is.numeric, session_elements.Populations$Population2))
    POP2_NUM$POP <- "P2"
    POP2_NUM$MSISDN <- NULL
    POP2_NUM <- POP2_NUM[,lapply(.SD,mean,na.rm=T),by=POP]
    POP2_NUM <- melt(POP2_NUM,id="POP")
    POP2_NUM$value <- round(POP2_NUM$value)
    POP2_NUM$POP <- NULL
    
    POP3_NUM <- as.data.table(Filter(is.numeric, session_elements.Populations$Population3))
    POP3_NUM$POP <- "P3"
    POP3_NUM$MSISDN <- NULL
    POP3_NUM <- POP3_NUM[,lapply(.SD,mean,na.rm=T),by=POP]
    POP3_NUM <- melt(POP3_NUM,id="POP")
    POP3_NUM$value <- round(POP3_NUM$value)
    POP3_NUM$POP <- NULL
    
    BD_NUM <- as.data.table(Filter(is.numeric, DATA_))
    BD_NUM$POP <- "ALL"
    BD_NUM$MSISDN <- NULL
    BD_NUM <- BD_NUM[,lapply(.SD,mean,na.rm=T),by=POP]
    BD_NUM <- melt(BD_NUM,id="POP")
    liste_vars <- BD_NUM$variable
    BD_NUM$value <- round(BD_NUM$value)
    BD_NUM$POP <- NULL
    
    PROFILING <- as.data.table(cbind(as.character(BD_NUM[[1]]),POP1_NUM$value,POP2_NUM$value,POP3_NUM$value,BD_NUM$value))
    colnames(PROFILING) <- c("Variable","P1","P2","P3","Parc")
    PROFILING$P1_ <- round((POP1_NUM$value * 100 / BD_NUM$value) - 100,0)
    PROFILING$P2_ <- round((POP2_NUM$value * 100 / BD_NUM$value) - 100,0)
    PROFILING$P3_ <- round((POP3_NUM$value * 100 / BD_NUM$value) - 100,0)
    
    return(PROFILING[ ( ! PROFILING$Variable  %in% not_to_profile) ,])
  })
  

  ###########################################################################################################Selection des populations
  output$Data1 <- renderPlotly({
    input$validate
    if(input$validate == 0)
      return()
    isolate({
      if(is.null(input$file1))
      {
        
        withProgress(message = 'Calcule de la population 1', value = 0, {
          
          N <- 5
          
          incProgress(1/N)
          session_elements.Populations$Population1 <-  DATA_ %>%
            filter( Client_Age >= input$Client_Age[1] , Client_Age <= input$Client_Age[2]  , Communaute_Taille >= input$Communaute_Taille[1] , Communaute_Taille <= input$Communaute_Taille[2]  , Voix_Nbj_Activite >= input$Voix_Nbj_Activite[1] , Voix_Nbj_Activite <= input$Voix_Nbj_Activite[2]  , Voix_National >= input$Voix_National[1] , Voix_National <= input$Voix_National[2]  , Voix_National_Nb >= input$Voix_National_Nb[1] , Voix_National_Nb <= input$Voix_National_Nb[2]  , Voix_NbContact_National >= input$Voix_NbContact_National[1] , Voix_NbContact_National <= input$Voix_NbContact_National[2]  , Voix_International >= input$Voix_International[1] , Voix_International <= input$Voix_International[2]  , Voix_International_Nb >= input$Voix_International_Nb[1] , Voix_International_Nb <= input$Voix_International_Nb[2]  , Voix_NbContact_International >= input$Voix_NbContact_International[1] , Voix_NbContact_International <= input$Voix_NbContact_International[2]  , Voix_Roaming >= input$Voix_Roaming[1] , Voix_Roaming <= input$Voix_Roaming[2]  , Voix_Roaming_Nb >= input$Voix_Roaming_Nb[1] , Voix_Roaming_Nb <= input$Voix_Roaming_Nb[2]  , Voix_NbContact_Roaming >= input$Voix_NbContact_Roaming[1] , Voix_NbContact_Roaming <= input$Voix_NbContact_Roaming[2]  , SMS_Nbj_Activite >= input$SMS_Nbj_Activite[1] , SMS_Nbj_Activite <= input$SMS_Nbj_Activite[2]  , SMS_NB >= input$SMS_NB[1] , SMS_NB <= input$SMS_NB[2]  , SMS__Contacts >= input$SMS__Contacts[1] , SMS__Contacts <= input$SMS__Contacts[2]  , Data_Nbj_Activite >= input$Data_Nbj_Activite[1] , Data_Nbj_Activite <= input$Data_Nbj_Activite[2]  , Data_Volume >= input$Data_Volume[1] , Data_Volume <= input$Data_Volume[2]  , Data_2G >= input$Data_2G[1] , Data_2G <= input$Data_2G[2]  , Data_3G >= input$Data_3G[1] , Data_3G <= input$Data_3G[2]  , Data_4G >= input$Data_4G[1] , Data_4G <= input$Data_4G[2]  , Data_Roaming >= input$Data_Roaming[1] , Data_Roaming <= input$Data_Roaming[2]  , Reseaux_Sociaux >= input$Reseaux_Sociaux[1] , Reseaux_Sociaux <= input$Reseaux_Sociaux[2]  , Web >= input$Web[1] , Web <= input$Web[2]  , News >= input$News[1] , News <= input$News[2]  , Omoney_Billpay_mnt >= input$Omoney_Billpay_mnt[1] , Omoney_Billpay_mnt <= input$Omoney_Billpay_mnt[2]  , Omoney_Billpay_Nbr >= input$Omoney_Billpay_Nbr[1] , Omoney_Billpay_Nbr <= input$Omoney_Billpay_Nbr[2]  , Omoney_Cashin_mnt >= input$Omoney_Cashin_mnt[1] , Omoney_Cashin_mnt <= input$Omoney_Cashin_mnt[2]  , Omoney_Cashin_Nbr >= input$Omoney_Cashin_Nbr[1] , Omoney_Cashin_Nbr <= input$Omoney_Cashin_Nbr[2]  , Omoney_Cashout_mnt >= input$Omoney_Cashout_mnt[1] , Omoney_Cashout_mnt <= input$Omoney_Cashout_mnt[2]  , Omoney_Cashout_Nbr >= input$Omoney_Cashout_Nbr[1] , Omoney_Cashout_Nbr <= input$Omoney_Cashout_Nbr[2]  , Omoney_merchpay_mnt >= input$Omoney_merchpay_mnt[1] , Omoney_merchpay_mnt <= input$Omoney_merchpay_mnt[2]  , Omoney_merchpay_Nbr >= input$Omoney_merchpay_Nbr[1] , Omoney_merchpay_Nbr <= input$Omoney_merchpay_Nbr[2]  , Omoney_P2P_mnt >= input$Omoney_P2P_mnt[1] , Omoney_P2P_mnt <= input$Omoney_P2P_mnt[2]  , Omoney_P2P_Nbr >= input$Omoney_P2P_Nbr[1] , Omoney_P2P_Nbr <= input$Omoney_P2P_Nbr[2]  , Omoney_RC_mnt >= input$Omoney_RC_mnt[1] , Omoney_RC_mnt <= input$Omoney_RC_mnt[2]  , Omoney_RC_Nbr >= input$Omoney_RC_Nbr[1] , Omoney_RC_Nbr <= input$Omoney_RC_Nbr[2]  , OOP_Telecom >= input$OOP_Telecom[1] , OOP_Telecom <= input$OOP_Telecom[2]  , OOP_Glon >= input$OOP_Glon[1] , OOP_Glon <= input$OOP_Glon[2]  , OOP_Assurances >= input$OOP_Assurances[1] , OOP_Assurances <= input$OOP_Assurances[2]  , OOP_Automobile >= input$OOP_Automobile[1] , OOP_Automobile <= input$OOP_Automobile[2]  , OOP_Bank >= input$OOP_Bank[1] , OOP_Bank <= input$OOP_Bank[2]  , OOP_Retail >= input$OOP_Retail[1] , OOP_Retail <= input$OOP_Retail[2] , (Client_Type %in%  input$Client_Type) , (Client_Offer %in%  input$Client_Offer) , (Geoloc_commune %in%  input$Geoloc_commune) , (Geoloc_ville %in%  input$Geoloc_ville) , (Geoloc_region %in%  input$Geoloc_region) , (Geoloc_type %in%  input$Geoloc_type) , (Communaute_Role %in%  input$Communaute_Role) , (Profile_1 %in%  input$Profile_1) , (Profile_2 %in%  input$Profile_2) , (Profile_3 %in%  input$Profile_3) , (Equipement_Marque %in%  input$Equipement_Marque) , (Equipement_Modele %in%  input$Equipement_Modele) , (Equipement_Term_Type %in%  input$Equipement_Term_Type) , (Equipement_4G %in%  input$Equipement_4G) , (Equipement_OS %in%  input$Equipement_OS))#, Client_Crea >= input$Client_Crea & Client_Crea <= input$Client_Crea ) 
          incProgress(2/N)
          session_elements.Tabs$p1_region <- aggregate_region(tab =session_elements.Populations$Population1,Regions = Regions )
          incProgress(3/N)
          session_elements.Tabs$p1_Marques <- getMarques(session_elements.Populations$Population1)
          incProgress(4/N)
          session_elements.Tabs$p1_TOP_Models <- top10_Models(session_elements.Populations$Population1)
          incProgress(5/N)
          session_elements.Tabs$p1_Terms_use <- calculMoyenne(session_elements.Populations$Population1)
          
        })
        
         }  else {
        tryCatch(
          {
            liste_1 <- fread(input$file1$datapath ,header = F)
          },
          error = function(e) {
            stop(safeError(e))
          }
        )
           
           withProgress(message = 'Calcule de la population 1', value = 0, {
             
             N <- 5
             
             incProgress(1/N)
             session_elements.Populations$Population1 <- DATA_[which(DATA_$MSISDN %in% liste_1[[1]]),]
             incProgress(2/N)
             session_elements.Tabs$p1_region <- aggregate_region(tab =session_elements.Populations$Population1,Regions = Regions )
             incProgress(3/N)
             session_elements.Tabs$p1_Marques <- getMarques(session_elements.Populations$Population1)
             incProgress(4/N)
             session_elements.Tabs$p1_TOP_Models <- top10_Models(session_elements.Populations$Population1)
             incProgress(5/N)
             session_elements.Tabs$p1_Terms_use <- calculMoyenne(session_elements.Populations$Population1)
             
           })
        
         }
     
      
       Pie <- data.frame(rbind(c("Population 1",nrow(session_elements.Populations$Population1)),c("Others",nrow(DATA_)-nrow(session_elements.Populations$Population1))))
      
      plot_ly(Pie, labels = ~X1, values = ~X2, type = 'pie',textposition = 'inside',
              textinfo = 'label+value+percent',
              insidetextfont = list(color = '#000000'),
              hoverinfo = 'text',
              text = ~paste( X2, ' Clients'),
              marker = list(colors = colors1,
                            line = list(color = '#FFFFFF', width = 1)),
              showlegend = FALSE) %>%
        layout(title = 'Population 1',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               autosize = F, width = 300, height = 300)
      
    })
  })
  
  
  output$Data2 <- renderPlotly({
    input$validate
    if(input$validate == 0)
      return()
    isolate({
      if(is.null(input$file2)){
        withProgress(message = 'Calcule de la population 2', value = 0, {
          
          N <- 5
          
          incProgress(1/N)
          session_elements.Populations$Population2 <-  DATA_ %>%
            filter( Client_Age >= input$Client_Age2[1] , Client_Age <= input$Client_Age2[2]  , Communaute_Taille >= input$Communaute_Taille2[1] , Communaute_Taille <= input$Communaute_Taille2[2]  , Voix_Nbj_Activite >= input$Voix_Nbj_Activite2[1] , Voix_Nbj_Activite <= input$Voix_Nbj_Activite2[2]  , Voix_National >= input$Voix_National2[1] , Voix_National <= input$Voix_National2[2]  , Voix_National_Nb >= input$Voix_National_Nb2[1] , Voix_National_Nb <= input$Voix_National_Nb2[2]  , Voix_NbContact_National >= input$Voix_NbContact_National2[1] , Voix_NbContact_National <= input$Voix_NbContact_National2[2]  , Voix_International >= input$Voix_International2[1] , Voix_International <= input$Voix_International2[2]  , Voix_International_Nb >= input$Voix_International_Nb2[1] , Voix_International_Nb <= input$Voix_International_Nb2[2]  , Voix_NbContact_International >= input$Voix_NbContact_International2[1] , Voix_NbContact_International <= input$Voix_NbContact_International2[2]  , Voix_Roaming >= input$Voix_Roaming2[1] , Voix_Roaming <= input$Voix_Roaming2[2]  , Voix_Roaming_Nb >= input$Voix_Roaming_Nb2[1] , Voix_Roaming_Nb <= input$Voix_Roaming_Nb2[2]  , Voix_NbContact_Roaming >= input$Voix_NbContact_Roaming2[1] , Voix_NbContact_Roaming <= input$Voix_NbContact_Roaming2[2]  , SMS_Nbj_Activite >= input$SMS_Nbj_Activite2[1] , SMS_Nbj_Activite <= input$SMS_Nbj_Activite2[2]  , SMS_NB >= input$SMS_NB2[1] , SMS_NB <= input$SMS_NB2[2]  , SMS__Contacts >= input$SMS__Contacts2[1] , SMS__Contacts <= input$SMS__Contacts2[2]  , Data_Nbj_Activite >= input$Data_Nbj_Activite2[1] , Data_Nbj_Activite <= input$Data_Nbj_Activite2[2]  , Data_Volume >= input$Data_Volume2[1] , Data_Volume <= input$Data_Volume2[2]  , Data_2G >= input$Data_2G2[1] , Data_2G <= input$Data_2G2[2]  , Data_3G >= input$Data_3G2[1] , Data_3G <= input$Data_3G2[2]  , Data_4G >= input$Data_4G2[1] , Data_4G <= input$Data_4G2[2]  , Data_Roaming >= input$Data_Roaming2[1] , Data_Roaming <= input$Data_Roaming2[2]  , Reseaux_Sociaux >= input$Reseaux_Sociaux2[1] , Reseaux_Sociaux <= input$Reseaux_Sociaux2[2]  , Web >= input$Web2[1] , Web <= input$Web2[2]  , News >= input$News2[1] , News <= input$News2[2]  , Omoney_Billpay_mnt >= input$Omoney_Billpay_mnt2[1] , Omoney_Billpay_mnt <= input$Omoney_Billpay_mnt2[2]  , Omoney_Billpay_Nbr >= input$Omoney_Billpay_Nbr2[1] , Omoney_Billpay_Nbr <= input$Omoney_Billpay_Nbr2[2]  , Omoney_Cashin_mnt >= input$Omoney_Cashin_mnt2[1] , Omoney_Cashin_mnt <= input$Omoney_Cashin_mnt2[2]  , Omoney_Cashin_Nbr >= input$Omoney_Cashin_Nbr2[1] , Omoney_Cashin_Nbr <= input$Omoney_Cashin_Nbr2[2]  , Omoney_Cashout_mnt >= input$Omoney_Cashout_mnt2[1] , Omoney_Cashout_mnt <= input$Omoney_Cashout_mnt2[2]  , Omoney_Cashout_Nbr >= input$Omoney_Cashout_Nbr2[1] , Omoney_Cashout_Nbr <= input$Omoney_Cashout_Nbr2[2]  , Omoney_merchpay_mnt >= input$Omoney_merchpay_mnt2[1] , Omoney_merchpay_mnt <= input$Omoney_merchpay_mnt2[2]  , Omoney_merchpay_Nbr >= input$Omoney_merchpay_Nbr2[1] , Omoney_merchpay_Nbr <= input$Omoney_merchpay_Nbr2[2]  , Omoney_P2P_mnt >= input$Omoney_P2P_mnt2[1] , Omoney_P2P_mnt <= input$Omoney_P2P_mnt2[2]  , Omoney_P2P_Nbr >= input$Omoney_P2P_Nbr2[1] , Omoney_P2P_Nbr <= input$Omoney_P2P_Nbr2[2]  , Omoney_RC_mnt >= input$Omoney_RC_mnt2[1] , Omoney_RC_mnt <= input$Omoney_RC_mnt2[2]  , Omoney_RC_Nbr >= input$Omoney_RC_Nbr2[1] , Omoney_RC_Nbr <= input$Omoney_RC_Nbr2[2]  , OOP_Telecom >= input$OOP_Telecom2[1] , OOP_Telecom <= input$OOP_Telecom2[2]  , OOP_Glon >= input$OOP_Glon2[1] , OOP_Glon <= input$OOP_Glon2[2]  , OOP_Assurances >= input$OOP_Assurances2[1] , OOP_Assurances <= input$OOP_Assurances2[2]  , OOP_Automobile >= input$OOP_Automobile2[1] , OOP_Automobile <= input$OOP_Automobile2[2]  , OOP_Bank >= input$OOP_Bank2[1] , OOP_Bank <= input$OOP_Bank2[2]  , OOP_Retail >= input$OOP_Retail2[1] , OOP_Retail <= input$OOP_Retail2[2]  , OOP_Hotels >= input$OOP_Hotels2[1] , OOP_Hotels <= input$OOP_Hotels2[2] , (Client_Type %in%  input$Client_Type2) , (Client_Offer %in%  input$Client_Offer2) , (Geoloc_commune %in%  input$Geoloc_commune2) , (Geoloc_ville %in%  input$Geoloc_ville2) , (Geoloc_region %in%  input$Geoloc_region2) , (Geoloc_type %in%  input$Geoloc_type2) , (Communaute_Role %in%  input$Communaute_Role2) , (Profile_1 %in%  input$Profile_12) , (Profile_2 %in%  input$Profile_22) , (Profile_3 %in%  input$Profile_32) , (Equipement_Marque %in%  input$Equipement_Marque2) , (Equipement_Modele %in%  input$Equipement_Modele2) , (Equipement_Term_Type %in%  input$Equipement_Term_Type2) , (Equipement_4G %in%  input$Equipement_4G2) , (Equipement_OS %in%  input$Equipement_OS2) ) 
          
          incProgress(2/N)
          session_elements.Tabs$p2_region <- aggregate_region(tab =session_elements.Populations$Population2,Regions = Regions )
          incProgress(3/N)
          session_elements.Tabs$p2_Marques <- getMarques(session_elements.Populations$Population2)
          incProgress(4/N)
          session_elements.Tabs$p2_TOP_Models <- top10_Models(session_elements.Populations$Population2)
          incProgress(5/N)
          session_elements.Tabs$p2_Terms_use <- calculMoyenne(session_elements.Populations$Population2)
          
        })
        
      }else{
        tryCatch(
          {
            liste_2 <- fread(input$file2$datapath ,header = F)
          },
          error = function(e) {
            stop(safeError(e))
          }
        )
        withProgress(message = 'Calcule de la population 1', value = 0, {
          
          N <- 5
          
          incProgress(1/N)
          session_elements.Populations$Population2 <- DATA_[which(DATA_$MSISDN %in% liste_2[[1]]),]
          incProgress(2/N)
          session_elements.Tabs$p2_region <- aggregate_region(tab =session_elements.Populations$Population2,Regions = Regions )
          incProgress(3/N)
          session_elements.Tabs$p2_Marques <- getMarques(session_elements.Populations$Population2)
          incProgress(4/N)
          session_elements.Tabs$p2_TOP_Models <- top10_Models(session_elements.Populations$Population2)
          incProgress(5/N)
          session_elements.Tabs$p2_Terms_use <- calculMoyenne(session_elements.Populations$Population2)
          
        })
      }
     
      
      Pie <- data.frame(rbind(c("Population 2",nrow(session_elements.Populations$Population2)),c("Others",nrow(DATA_)-nrow(session_elements.Populations$Population2))))
      plot_ly(Pie, labels = ~X1, values = ~X2, type = 'pie',textposition = 'inside',
              textinfo = 'label+value+percent',
              insidetextfont = list(color = '#000000'),
              hoverinfo = 'text',
              text = ~paste( X2, ' Clients'),
              marker = list(colors = colors2,
                            line = list(color = '#FFFFFF', width = 1)),
              showlegend = FALSE) %>%
        layout(title = 'Population 2',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               autosize = F, width = 300, height = 300)
    })
  })
  
  output$Data3 <- renderPlotly({
    input$validate
    if(input$validate == 0)
      return()
    isolate({
      if(is.null(input$file3)){
        withProgress(message = 'Calcule de la population 3', value = 0, {
          
          N <- 5
          
          incProgress(1/N)
          session_elements.Populations$Population3 <-  DATA_ %>%
            filter( Client_Age >= input$Client_Age3[1] , Client_Age <= input$Client_Age3[2]  , Communaute_Taille >= input$Communaute_Taille3[1] , Communaute_Taille <= input$Communaute_Taille3[2]  , Voix_Nbj_Activite >= input$Voix_Nbj_Activite3[1] , Voix_Nbj_Activite <= input$Voix_Nbj_Activite3[2]  , Voix_National >= input$Voix_National3[1] , Voix_National <= input$Voix_National3[2]  , Voix_National_Nb >= input$Voix_National_Nb3[1] , Voix_National_Nb <= input$Voix_National_Nb3[2]  , Voix_NbContact_National >= input$Voix_NbContact_National3[1] , Voix_NbContact_National <= input$Voix_NbContact_National3[2]  , Voix_International >= input$Voix_International3[1] , Voix_International <= input$Voix_International3[2]  , Voix_International_Nb >= input$Voix_International_Nb3[1] , Voix_International_Nb <= input$Voix_International_Nb3[2]  , Voix_NbContact_International >= input$Voix_NbContact_International3[1] , Voix_NbContact_International <= input$Voix_NbContact_International3[2]  , Voix_Roaming >= input$Voix_Roaming3[1] , Voix_Roaming <= input$Voix_Roaming3[2]  , Voix_Roaming_Nb >= input$Voix_Roaming_Nb3[1] , Voix_Roaming_Nb <= input$Voix_Roaming_Nb3[2]  , Voix_NbContact_Roaming >= input$Voix_NbContact_Roaming3[1] , Voix_NbContact_Roaming <= input$Voix_NbContact_Roaming3[2]  , SMS_Nbj_Activite >= input$SMS_Nbj_Activite3[1] , SMS_Nbj_Activite <= input$SMS_Nbj_Activite3[2]  , SMS_NB >= input$SMS_NB3[1] , SMS_NB <= input$SMS_NB3[2]  , SMS__Contacts >= input$SMS__Contacts3[1] , SMS__Contacts <= input$SMS__Contacts3[2]  , Data_Nbj_Activite >= input$Data_Nbj_Activite3[1] , Data_Nbj_Activite <= input$Data_Nbj_Activite3[2]  , Data_Volume >= input$Data_Volume3[1] , Data_Volume <= input$Data_Volume3[2]  , Data_2G >= input$Data_2G3[1] , Data_2G <= input$Data_2G3[2]  , Data_3G >= input$Data_3G3[1] , Data_3G <= input$Data_3G3[2]  , Data_4G >= input$Data_4G3[1] , Data_4G <= input$Data_4G3[2]  , Data_Roaming >= input$Data_Roaming3[1] , Data_Roaming <= input$Data_Roaming3[2]  , Reseaux_Sociaux >= input$Reseaux_Sociaux3[1] , Reseaux_Sociaux <= input$Reseaux_Sociaux3[2]  , Web >= input$Web3[1] , Web <= input$Web3[2]  , News >= input$News3[1] , News <= input$News3[2]  , Omoney_Billpay_mnt >= input$Omoney_Billpay_mnt3[1] , Omoney_Billpay_mnt <= input$Omoney_Billpay_mnt3[2]  , Omoney_Billpay_Nbr >= input$Omoney_Billpay_Nbr3[1] , Omoney_Billpay_Nbr <= input$Omoney_Billpay_Nbr3[2]  , Omoney_Cashin_mnt >= input$Omoney_Cashin_mnt3[1] , Omoney_Cashin_mnt <= input$Omoney_Cashin_mnt3[2]  , Omoney_Cashin_Nbr >= input$Omoney_Cashin_Nbr3[1] , Omoney_Cashin_Nbr <= input$Omoney_Cashin_Nbr3[2]  , Omoney_Cashout_mnt >= input$Omoney_Cashout_mnt3[1] , Omoney_Cashout_mnt <= input$Omoney_Cashout_mnt3[2]  , Omoney_Cashout_Nbr >= input$Omoney_Cashout_Nbr3[1] , Omoney_Cashout_Nbr <= input$Omoney_Cashout_Nbr3[2]  , Omoney_merchpay_mnt >= input$Omoney_merchpay_mnt3[1] , Omoney_merchpay_mnt <= input$Omoney_merchpay_mnt3[2]  , Omoney_merchpay_Nbr >= input$Omoney_merchpay_Nbr3[1] , Omoney_merchpay_Nbr <= input$Omoney_merchpay_Nbr3[2]  , Omoney_P2P_mnt >= input$Omoney_P2P_mnt3[1] , Omoney_P2P_mnt <= input$Omoney_P2P_mnt3[2]  , Omoney_P2P_Nbr >= input$Omoney_P2P_Nbr3[1] , Omoney_P2P_Nbr <= input$Omoney_P2P_Nbr3[2]  , Omoney_RC_mnt >= input$Omoney_RC_mnt3[1] , Omoney_RC_mnt <= input$Omoney_RC_mnt3[2]  , Omoney_RC_Nbr >= input$Omoney_RC_Nbr3[1] , Omoney_RC_Nbr <= input$Omoney_RC_Nbr3[2]  , OOP_Telecom >= input$OOP_Telecom3[1] , OOP_Telecom <= input$OOP_Telecom3[2]  , OOP_Glon >= input$OOP_Glon3[1] , OOP_Glon <= input$OOP_Glon3[2]  , OOP_Assurances >= input$OOP_Assurances3[1] , OOP_Assurances <= input$OOP_Assurances3[2]  , OOP_Automobile >= input$OOP_Automobile3[1] , OOP_Automobile <= input$OOP_Automobile3[2]  , OOP_Bank >= input$OOP_Bank3[1] , OOP_Bank <= input$OOP_Bank3[2]  , OOP_Retail >= input$OOP_Retail3[1] , OOP_Retail <= input$OOP_Retail3[2]  , OOP_Hotels >= input$OOP_Hotels3[1] , OOP_Hotels <= input$OOP_Hotels3[2] ,(Client_Type %in%  input$Client_Type3) , (Client_Offer %in%  input$Client_Offer3) , (Geoloc_commune %in%  input$Geoloc_commune3) , (Geoloc_ville %in%  input$Geoloc_ville3) , (Geoloc_region %in%  input$Geoloc_region3) , (Geoloc_type %in%  input$Geoloc_type3) , (Communaute_Role %in%  input$Communaute_Role3) , (Profile_1 %in%  input$Profile_13) , (Profile_2 %in%  input$Profile_23) , (Profile_3 %in%  input$Profile_33) , (Equipement_Marque %in%  input$Equipement_Marque3) , (Equipement_Modele %in%  input$Equipement_Modele3) , (Equipement_Term_Type %in%  input$Equipement_Term_Type3) , (Equipement_4G %in%  input$Equipement_4G3) , (Equipement_OS %in%  input$Equipement_OS3) ) 
          incProgress(2/N)
          session_elements.Tabs$p3_region <- aggregate_region(tab =session_elements.Populations$Population3,Regions = Regions )
          incProgress(3/N)
          session_elements.Tabs$p3_Marques <- getMarques(session_elements.Populations$Population3)
          incProgress(4/N)
          session_elements.Tabs$p3_TOP_Models <- top10_Models(session_elements.Populations$Population3)
          incProgress(5/N)
          session_elements.Tabs$p3_Terms_use <- calculMoyenne(session_elements.Populations$Population3)
          
        })
      }else{
        tryCatch(
          {
            liste_3 <- fread(input$file3$datapath ,header = F)
          },
          error = function(e) {
            stop(safeError(e))
          }
        )
        session_elements.Populations$Population3 <- DATA_[which(DATA_$MSISDN %in% liste_3[[1]]),]
        #Tables vaiables
        session_elements.Tabs$p3_region <- aggregate_region(tab =session_elements.Populations$Population3,Regions = Regions )
        session_elements.Tabs$p3_Marques <- getMarques(session_elements.Populations$Population3)
        session_elements.Tabs$p3_TOP_Models <- top10_Models(session_elements.Populations$Population3)
        session_elements.Tabs$p3_Terms_use <- calculMoyenne(session_elements.Populations$Population3)
      }
      
      
      Pie <- data.frame(rbind(c("Population 3",nrow(session_elements.Populations$Population3)),c("Others",nrow(DATA_)-nrow(session_elements.Populations$Population3))))
      
      plot_ly(Pie, labels = ~X1, values = ~X2, type = 'pie',textposition = 'inside',
              textinfo = 'label+value+percent',
              insidetextfont = list(color = '#000000'),
              hoverinfo = 'text',
              text = ~paste( X2, ' Clients'),
              marker = list(colors = colors3,
                            line = list(color = '#FFFFFF', width = 1)),
              showlegend = FALSE) %>%
        layout(title = 'Population 3',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               autosize = F, width = 300, height = 300)
    })
  })
  
  #################################################################################PROFILING
  
  output$Prof_glob <- renderDataTable({
    if(!is.null(session_elements.Populations$Population1) & !is.null(session_elements.Populations$Population2) & !is.null(session_elements.Populations$Population3)){
      
      PROFILING <- profilier()
      if(input$Prof_type_glob == "hard"){
        PROFILING <- PROFILING[!((PROFILING$P1_ >= -10 & PROFILING$P1_ <= 10 )&(PROFILING$P2_ >= -10 & PROFILING$P2_ <= 10 )&(PROFILING$P3_ >= -10 & PROFILING$P3_ <= 10 )),]
      }
      interval <- c(             -100,               -75,               -50,               -25,               -10,                 10,              25,                 50,               75,                100,              150)
      colorz <-  c('RGB(70,70,255)','RGB(100,100,255)','RGB(131,131,255)','RGB(162,162,255)','RGB(193,193,255)','RGB(255,255,255)','RGB(255,222,212)','RGB(255,190,170)','RGB(255,157,127)','RGB(255,125,85)','RGB(255,56,42)','RGB(255,31,27)')
      
      datatable(PROFILING,caption = 'Caractérisation des populations selectionnées par rapport au Parc',
                options = list(pageLength = nrow(PROFILING),columnDefs = list(list(targets = 5:7, visible = FALSE)),searching = T, paging = F, bInfo=F),rownames = F,class = 'cell-border stripe',extensions = 'Responsive'
                
      ) %>% formatStyle(
        'P1', 'P1_',
        backgroundColor = styleInterval(interval, colorz)
      )%>% formatStyle(
        'P2', 'P2_',
        backgroundColor = styleInterval(interval,colorz)
      )%>% formatStyle(
        'P3', 'P3_',
        backgroundColor = styleInterval(interval, colorz)
      )%>% formatStyle(
        'Variable', backgroundColor = styleEqual(c('Client_Age','Communaute_Taille','Voix_Nbj_Activite','Voix_National','Voix_National_Nb','Voix_NbContact_National','Voix_International','Voix_International_Nb','Voix_NbContact_International','Voix_Roaming','Voix_Roaming_Nb','Voix_NbContact_Roaming','SMS_Nbj_Activite','SMS_NB','SMS__Contacts','Data_Nbj_Activite','Data_Volume','Data_2G','Data_3G','Data_4G','Data_Roaming','Reseaux_Sociaux','Web','News','Omoney_Billpay_mnt','Omoney_Billpay_Nbr','Omoney_Cashin_mnt','Omoney_Cashin_Nbr','Omoney_Cashout_mnt','Omoney_Cashout_Nbr','Omoney_merchpay_mnt','Omoney_merchpay_Nbr','Omoney_P2P_mnt','Omoney_P2P_Nbr','Omoney_RC_mnt','Omoney_RC_Nbr','OOP_Telecom','OOP_Glon','OOP_Assurances','OOP_Automobile','OOP_Bank','OOP_Retail','OOP_Hotels'), c('#FF9C63','#FF9C63','#7FCD71','#7FCD71','#7FCD71','#7FCD71','#7FCD71','#7FCD71','#7FCD71','#7FCD71','#7FCD71','#7FCD71','#00FF80','#00FF80','#00FF80','#00ECAA','#00ECAA','#00ECAA','#00ECAA','#00ECAA','#00ECAA','#00ECAA','#00ECAA','#00ECAA','#00DAD4','#00DAD4','#00DAD4','#00DAD4','#00DAD4','#00DAD4','#00DAD4','#00DAD4','#00DAD4','#00DAD4','#00DAD4','#00DAD4','#00C8FF','#00C8FF','#00C8FF','#00C8FF','#00C8FF','#00C8FF','#00C8FF'))
      )
    }else {
      return()
    }
  })
  
  ###########################################" EQUIEPEMENT
  
  ###   le parc ###
  
  output$parc_MARQUES <- renderPlotly({
    
    plot_ly(parc_Marques, labels = ~V1, values = ~N, type = 'pie',textposition = 'inside',
            textinfo = 'label+value+percent',
            insidetextfont = list(color = '#000000'),
            hoverinfo = 'text',
            text = ~paste( V1, ' Device'),
            marker = list(line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE) %>%
      layout(
        title = '   Global',
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        autosize = T)
  })
  
  output$parc_MARQUES_SMRT <- renderPlotly({
    
    plot_ly(parc_Marques_SMRT, labels = ~V1, values = ~N,textposition = 'inside',
            textinfo = 'label+value+percent',
            insidetextfont = list(color = '#000000'),
            hoverinfo = 'text',
            text = ~paste( V1, ' Device'),
            marker = list(line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE) %>%
      add_pie(hole = 0.3) %>%
      layout(
        title = '   Smartphones',
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        autosize = T)
  })
  output$parc_MARQUES_FTR <- renderPlotly({
    
    plot_ly(parc_Marques_FTR, labels = ~V1, values = ~N,textposition = 'inside',
            textinfo = 'label+value+percent',
            insidetextfont = list(color = '#000000'),
            hoverinfo = 'text',
            text = ~paste( V1, ' Device'),
            marker = list(line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE) %>%
      add_pie(hole = 0.3) %>%
      layout(
        title = 'Feature Phones',
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        autosize = T )
  })
  
  
  output$parc_TOP10_Modeles <- renderPlotly({
    
    f <- list(color = "#7f7f7f")
    x <- list( titlefont = f)
    y <- list( titlefont = f)
    plot_ly(parc_TOP10_Modeles, x = ~V1, y = ~N,
            color = 'rgb(243,156,18)'
    ) %>%
      layout(xaxis = x, yaxis = y )
  })
  
  output$parc_mean_voix <- renderDataTable({
    datatable(parc_moyennes,options = list(pageLength = 5,columnDefs = list(list(targets = c(1,3:42), visible = FALSE)),searching = T, paging = F, bInfo=F),rownames = F,class = 'cell-border stripe'  )  %>% 
      formatStyle(c("voix_duree"),
                  background = styleColorBar(range(parc_moyennes$voix_duree), '#F9D08F'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      ) %>%
      formatStyle(c("voix_nb"),
                  background = styleColorBar(range(parc_moyennes$voix_nb), '#F9D08F'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      )  %>% 
      formatStyle(c("voix_nbcontact"),
                  background = styleColorBar(range(parc_moyennes$voix_nbcontact), '#F9D08F'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      ) 
  })
  
  output$parc_mean_sms <- renderDataTable({
    datatable(parc_moyennes,options = list(pageLength = 5, columnDefs = list(list(targets = c(1:11, 15:45), visible = FALSE)),searching = T, paging = F, bInfo=F),rownames = F,class = 'cell-border stripe'  )  %>% 
      formatStyle(c("SMS_Nbj_Activite"),
                  background = styleColorBar(range(parc_moyennes$SMS_Nbj_Activite), '#F9D08F'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      ) %>%
      formatStyle(c("SMS_NB"),
                  background = styleColorBar(range(parc_moyennes$SMS_NB), '#F9D08F'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      )  %>% 
      formatStyle(c("SMS__Contacts"),
                  background = styleColorBar(range(parc_moyennes$SMS__Contacts), '#F9D08F'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      ) 
  })
  
  
  output$parc_mean_data <- renderDataTable({
    datatable(parc_moyennes,options = list(pageLength = 5, columnDefs = list(list(targets = c(1:14, 21:45), visible = FALSE)),searching = T, paging = F, bInfo=F),rownames = F,class = 'cell-border stripe'  )  %>% 
      formatStyle(c("Data_Nbj_Activite"),
                  background = styleColorBar(range(parc_moyennes$Data_Nbj_Activite), '#F9D08F'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      ) %>%
      formatStyle(c("Data_Volume"),
                  background = styleColorBar(range(parc_moyennes$Data_Volume), '#F9D08F'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      )  %>% 
      formatStyle(c("Data_2G"),
                  background = styleColorBar(range(parc_moyennes$Data_2G), '#F9D08F'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      )  %>% 
      formatStyle(c("Data_3G"),
                  background = styleColorBar(range(parc_moyennes$Data_3G), '#F9D08F'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      )  %>% 
      formatStyle(c("Data_4G"),
                  background = styleColorBar(range(parc_moyennes$Data_4G), '#F9D08F'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      )   %>% 
      formatStyle(c("Data_Roaming"),
                  background = styleColorBar(range(parc_moyennes$Data_Roaming), '#F9D08F'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      )
  })
  
  output$parc_mean_reseaux_sociaux <- renderDataTable({
    datatable(parc_moyennes,options = list(pageLength = 5, columnDefs = list(list(targets = c(1:20, 24:45), visible = FALSE)),searching = T, paging = F, bInfo=F),rownames = F,class = 'cell-border stripe'  )  %>% 
      formatStyle(c("Reseaux_Sociaux"),
                  background = styleColorBar(range(parc_moyennes$Reseaux_Sociaux), '#F9D08F'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      ) %>%
      formatStyle(c("Web"),
                  background = styleColorBar(range(parc_moyennes$Web), '#F9D08F'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      )  %>% 
      formatStyle(c("News"),
                  background = styleColorBar(range(parc_moyennes$News), '#F9D08F'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      ) 
  })
  

  
  #### la population 1 ####
  camembert( input, output,"p1_MARQUES",session_elements.Tabs$p1_Marques)
  
  histogramme( input, output,"p1_TOP10_Modeles", session_elements.Tabs$p1_TOP_Models )
  
  sous_tab_voix( input, output, "p1_mean_voix", session_elements.Tabs$p1_Terms_use, '#86D4B1')
  
  sous_tab_sms( input, output, "p1_mean_sms" , session_elements.Tabs$p1_Terms_use, '#86D4B1')
  
  sous_tab_data( input, output, "p1_mean_data" , session_elements.Tabs$p1_Terms_use, '#86D4B1')
  
  sous_tab_reseaux_sociaux( input, output, "p1_mean_reseaux_sociaux" , session_elements.Tabs$p1_Terms_use, '#86D4B1')
  
  #### la population 2 ####
  
  camembert( input, output,"p2_MARQUES",session_elements.Tabs$p2_Marques)
  
  histogramme( input, output,"p2_TOP10_Modeles", session_elements.Tabs$p2_TOP_Models )
  
  sous_tab_voix( input, output, "p2_mean_voix", session_elements.Tabs$p2_Terms_use, '#E67D70')
  
  sous_tab_sms( input, output, "p2_mean_sms" , session_elements.Tabs$p2_Terms_use, '#E67D70')
  
  sous_tab_data( input, output, "p2_mean_data" , session_elements.Tabs$p2_Terms_use, '#E67D70')
  
  sous_tab_reseaux_sociaux( input, output, "p2_mean_reseaux_sociaux" , session_elements.Tabs$p2_Terms_use, '#E67D70')
  
  
  #### la population 3 ####
  
  camembert( input, output,"p3_MARQUES",session_elements.Tabs$p3_Marques)
  
  histogramme( input, output,"p3_TOP10_Modeles", session_elements.Tabs$p3_TOP_Models )
  
  sous_tab_voix( input, output, "p3_mean_voix", session_elements.Tabs$p3_Terms_use, '#97C2DB')
  
  sous_tab_sms( input, output, "p3_mean_sms" , session_elements.Tabs$p3_Terms_use, '#97C2DB')
  
  sous_tab_data( input, output, "p3_mean_data" , session_elements.Tabs$p3_Terms_use, '#97C2DB')
  
  sous_tab_reseaux_sociaux( input, output, "p3_mean_reseaux_sociaux" , session_elements.Tabs$p3_Terms_use, '#97C2DB')
  
  
    ########## MAPS pour les Equiepements
  
  output$MAP_parc_Equipement <- renderLeaflet({
      
          sel_smart <- bts_device(DATA_[DATA_$Equipement_Term_Type == "Smartphone",],bts)
          sel_feature <- bts_device(DATA_[DATA_$Equipement_Term_Type == "Feature Phone",],bts)
          mymap <- leaflet(device_parc)  %>% 
            addProviderTiles("CartoDB.Positron",options = providerTileOptions(minZoom=6, maxZoom=14)) %>%
            setView(lng = 9.7856173, lat = 4.0410433 , zoom = 6.7) %>%
            addCircleMarkers(data = device_parc,lng = device_parc$Longitude, lat = device_parc$Latitude,
                             radius = 10 * sqrt(device_parc$Nbre) / sqrt(max(device_parc$Nbre)), color = colors_map[device_parc$col],
                             popup = ~ paste0(as.character(device_parc$Nbre)," Devices"),
                             group = "Global"
                             
                             ) %>%
            addCircleMarkers(data =  sel_smart,lng = sel_smart$Longitude, lat = sel_smart$Latitude,
                             radius = 10 * sqrt(sel_smart$Nbre) / sqrt(max(sel_smart$Nbre)), color = colors_map[sel_smart$col],
                             popup = ~ paste0(as.character(sel_smart$Nbre)," Devices"),
                             group = "Smartphones"
                             ) %>%
            addCircleMarkers(data = sel_feature,lng = sel_feature$Longitude, lat = sel_feature$Latitude,
                             radius =10 * sqrt(sel_feature$Nbre) / sqrt(max(sel_feature$Nbre)), color = colors_map[sel_feature$col],
                             popup = ~ paste0(as.character(sel_feature$Nbre)," Devices"),
                             group = "Feature Phones"
                             ) %>%
            addLayersControl(
                               baseGroups = c("Global", "Smartphones", "Feature Phones"),
                               options = layersControlOptions(collapsed = FALSE))
            
  }) 

  #P1
  output$MAP_p1_Equipement <- renderLeaflet({
    
    if(!is.null(session_elements.Populations$Population1)) {
    device_p1 <- bts_device(as.data.table(session_elements.Populations$Population1),bts)
    
    sel_smart <- bts_device(as.data.table(session_elements.Populations$Population1[session_elements.Populations$Population1$Equipement_Term_Type == "Smartphone",]),bts)
    sel_feature <- bts_device(as.data.table(session_elements.Populations$Population1[session_elements.Populations$Population1$Equipement_Term_Type == "Feature Phone",]),bts)
    
    leaflet(device_p1)  %>% 
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(minZoom=6, maxZoom=14)) %>%
      setView(lng = 9.7856173, lat = 4.0410433 , zoom = 6.7) %>%
      addCircleMarkers(data = device_p1,lng = device_p1$Longitude, lat = device_p1$Latitude,
                       radius = 10 * sqrt(device_p1$Nbre) / sqrt(max(device_p1$Nbre)), color = colors_map[device_p1$col],
                       popup = ~ paste0(as.character(device_p1$Nbre)," Devices"),
                       group = "Global"
      ) %>%
      addCircleMarkers(data =  sel_smart,lng = sel_smart$Longitude, lat = sel_smart$Latitude,
                       radius = 10 * sqrt(sel_smart$Nbre) / sqrt(max(sel_smart$Nbre)), color = colors_map[sel_smart$col],
                       popup = ~ paste0(as.character(sel_smart$Nbre)," Devices"),
                       group = "Smartphones"
      ) %>%
      addCircleMarkers(data = sel_feature,lng = sel_feature$Longitude, lat = sel_feature$Latitude,
                       radius = 10 * sqrt(sel_feature$Nbre) / sqrt(max(sel_feature$Nbre)), color = colors_map[sel_feature$col],
                       popup = ~ paste0(as.character(sel_feature$Nbre)," Devices"),
                       group = "Feature Phones"
      ) %>%
      addLayersControl(
        baseGroups = c("Global", "Smartphones", "Feature Phones"),
        options = layersControlOptions(collapsed = FALSE))
    } else 
      return()
  })
  #P2
  output$MAP_p2_Equipement <- renderLeaflet({
    if(!is.null(session_elements.Populations$Population2)) {
      device_p1 <- bts_device(as.data.table(session_elements.Populations$Population2),bts)
      
      sel_smart <- bts_device(as.data.table(session_elements.Populations$Population2[session_elements.Populations$Population2$Equipement_Term_Type == "Smartphone",]),bts)
      sel_feature <- bts_device(as.data.table(session_elements.Populations$Population2[session_elements.Populations$Population2$Equipement_Term_Type == "Feature Phone",]),bts)
      
      leaflet(device_p1)  %>% 
        addProviderTiles("CartoDB.Positron",options = providerTileOptions(minZoom=6, maxZoom=14)) %>%
        setView(lng = 9.7856173, lat = 4.0410433 , zoom = 6.7) %>%
        addCircleMarkers(data = device_p1,lng = device_p1$Longitude, lat = device_p1$Latitude,
                         radius = 10 * sqrt(device_p1$Nbre) / sqrt(max(device_p1$Nbre)), color = colors_map[device_p1$col],
                         popup = ~ paste0(as.character(device_p1$Nbre)," Devices"),
                         group = "Global"
        ) %>%
        addCircleMarkers(data =  sel_smart,lng = sel_smart$Longitude, lat = sel_smart$Latitude,
                         radius = 10 * sqrt(sel_smart$Nbre) / sqrt(max(sel_smart$Nbre)), color = colors_map[sel_smart$col],
                         popup = ~ paste0(as.character(sel_smart$Nbre)," Devices"),
                         group = "Smartphones"
        ) %>%
        addCircleMarkers(data = sel_feature,lng = sel_feature$Longitude, lat = sel_feature$Latitude,
                         radius = 10 * sqrt(sel_feature$Nbre) / sqrt(max(sel_feature$Nbre)), color = colors_map[sel_feature$col],
                         popup = ~ paste0(as.character(sel_feature$Nbre)," Devices"),
                         group = "Feature Phones"
        ) %>%
        addLayersControl(
          baseGroups = c("Global", "Smartphones", "Feature Phones"),
          options = layersControlOptions(collapsed = FALSE)) %>%
          addLegend(labels = LETTERS[1:3], colors = c("blue", "red", "green"))
    } else 
      return()
  })
  #P3
  output$MAP_p3_Equipement <- renderLeaflet({
    if(!is.null(session_elements.Populations$Population3)) {
      device_p1 <- bts_device(as.data.table(session_elements.Populations$Population3),bts)
      
      sel_smart <- bts_device(as.data.table(session_elements.Populations$Population3[session_elements.Populations$Population3$Equipement_Term_Type == "Smartphone",]),bts)
      sel_feature <- bts_device(as.data.table(session_elements.Populations$Population3[session_elements.Populations$Population3$Equipement_Term_Type == "Feature Phone",]),bts)
      
      leaflet(device_p1)  %>% 
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = 9.7856173, lat = 4.0410433 , zoom = 6.7) %>%
        addCircleMarkers(data = device_p1,lng = device_p1$Longitude, lat = device_p1$Latitude,
                         radius = 10 * sqrt(device_p1$Nbre) / sqrt(max(device_p1$Nbre)), color = colors_map[device_p1$col],
                         popup = ~ paste0(as.character(device_p1$Nbre)," Devices"),
                         group = "Global",
                         options = providerTileOptions(minZoom=6, maxZoom=14)
        ) %>%
        addCircleMarkers(data =  sel_smart,lng = sel_smart$Longitude, lat = sel_smart$Latitude,
                         radius = 10 * sqrt(sel_smart$Nbre) / sqrt(max(sel_smart$Nbre)), color = colors_map[sel_smart$col],
                         popup = ~ paste0(as.character(sel_smart$Nbre)," Devices"),
                         group = "Smartphones",
                         options = providerTileOptions(minZoom=6, maxZoom=14)
        ) %>%
        addCircleMarkers(data = sel_feature,lng = sel_feature$Longitude, lat = sel_feature$Latitude,
                         radius = 10 * sqrt(sel_feature$Nbre) / sqrt(max(sel_feature$Nbre)), color = colors_map[sel_feature$col],
                         popup = ~ paste0(as.character(sel_feature$Nbre)," Devices"),
                         group = "Feature Phones",
                         options = providerTileOptions(minZoom=6, maxZoom=14)
        ) %>%
        addLayersControl(
          baseGroups = c("Global", "Smartphones", "Feature Phones"),
          options = layersControlOptions(collapsed = FALSE))
    } else 
      return()
  })
  
# Terminaux Dashboards : 
  output$Nbr_Device <- renderValueBox({
    shinydashboard::valueBox(
      subtitle = "Téléphones",
      value =Dash_table[Dash_table$Equipement_Marque == input$Dash_Equipement,]$Devices,
      icon = icon("mobile")
    )
  })
  output$Prt_Device <- renderValueBox({
    shinydashboard::valueBox(
      subtitle = "Des Clients l'utilisent",
      value = paste0(round(Dash_table[Dash_table$Equipement_Marque == input$Dash_Equipement,]$Devices/sum(Dash_table$Devices)*100,0),"%"),
      icon = icon("percent")
    )
  })
  output$Top_Device <- renderValueBox({
    shinydashboard::valueBox(
      subtitle = paste0("est le TOP Selling de ",input$Dash_Equipement),
      value =Dash_table[Dash_table$Equipement_Marque == input$Dash_Equipement,]$STAR,
      icon = icon("star")
    )
  })
  
  #output$Voix_gauge <- renderGauge({
  #  gauge(Dash_table[Dash_table$Equipement_Marque == input$Dash_Equipement,]$Voix, min = 0, max = max(Dash_table$Voix),label = "Minutes Moyennes",gaugeSectors(
  #    success = c(0.8*max(Dash_table$Voix), 1*max(Dash_table$Voix)), warning = c(0.40*max(Dash_table$Voix), 0.79*max(Dash_table$Voix)), danger = c(0, 0.39*max(Dash_table$Voix))) )
  #})
  
  DashGauge(input,output,res = "Voix_gauge",indicateur = "Voix",label_ = "Minutes",Dash_table)
  DashGauge(input,output,res = "Voix_Nb_gauge",indicateur = "NB_APP",label_ = "Nombre d'appels",Dash_table)
  DashGauge(input,output,res = "Voix_Freq_gauge",indicateur = "Mean_Usage_Voix",label_ = "Jours d'utilisation",Dash_table)
  
  DashGauge(input,output,res = "SMS_gauge",indicateur = "SMS",label_ = "Messages",Dash_table)
  DashGauge(input,output,res = "SMS_Freq_gauge",indicateur = "Mean_Usage_SMS",label_ = "Jours d'utilisation",Dash_table)
  
  DashGauge(input,output,res = "DATA_gauge",indicateur = "Volume_DATA",label_ = "Go",Dash_table)
  DashGauge(input,output,res = "DATA_Freq_gauge",indicateur = "Mean_Usage_DATA",label_ = "Jours d'utilisation",Dash_table)
  
  
  
  
  output$Dash_Equipement <- renderLeaflet({
    Sel <- bts_device(DATA_[which(DATA_$Equipement_Marque == input$Dash_Equipement),],bts)
    sel_smart <- bts_device(DATA_[(DATA_$Equipement_Marque == input$Dash_Equipement) & DATA_$Equipement_Term_Type == "Smartphone",],bts)
    sel_feature <- bts_device(DATA_[(DATA_$Equipement_Marque == input$Dash_Equipement) & DATA_$Equipement_Term_Type == "Feature Phone",],bts)
    pal_sel <- colorNumeric(
      palette = colorRampPalette(c('green', 'red'))(length(Sel$Nbre)), 
      domain = Sel$Nbre)
    pal_smart <- colorNumeric(
      palette = colorRampPalette(c('green', 'red'))(length(sel_smart$Nbre)), 
      domain = sel_smart$Nbre)
    pal_feature  <- colorNumeric(
      palette = colorRampPalette(c('green', 'red'))(length(sel_feature$Nbre)), 
      domain = sel_feature$Nbre)
    
    mymap <- leaflet(Sel)  %>% 
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(minZoom=6, maxZoom=14)) %>%
      setView(lng = 11.908214, lat = 5.422864 , zoom = 6.7) %>%
      addCircleMarkers(data = Sel ,lng = Sel$Longitude, lat = Sel$Latitude, 
                       radius = 10 * sqrt(Sel$Nbre) / sqrt(max(Sel$Nbre)), color = ~ pal_sel(Nbre),
                       weight = 10 * sqrt(Sel$Nbre) / sqrt(max(Sel$Nbre)),
                       group = "Global",
                       popup = ~ paste0(as.character(Sel$Nbre)," Devices"
                       ) 
      )%>%
      addCircleMarkers(data =  sel_smart,lng = sel_smart$Longitude, lat = sel_smart$Latitude,
                       radius = 10 * sqrt(sel_smart$Nbre) / sqrt(max(sel_smart$Nbre)), color = ~ pal_smart(Nbre),
                       popup = ~ paste0(as.character(sel_smart$Nbre)," Devices"),
                       group = "Smartphones",
                       options = providerTileOptions(minZoom=6, maxZoom=14)
      ) %>%
      addCircleMarkers(data = sel_feature,lng = sel_feature$Longitude, lat = sel_feature$Latitude,
                       radius = 10 * sqrt(sel_feature$Nbre) / sqrt(max(sel_feature$Nbre)), color = ~ pal_feature(Nbre),
                       popup = ~ paste0(as.character(sel_feature$Nbre)," Devices"),
                       group = "Feature Phones",
                       options = providerTileOptions(minZoom=6, maxZoom=14)
      ) %>%
      addLayersControl(
        baseGroups = c("Global", "Smartphones", "Feature Phones"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
 
  
#######################################################################  RETAIL

  
  #POPULATION PAR REGION
map_population(input,output,element = "MAP_parc",population = parc_region,color = "#FF7700",Selection = input$MAP_parc_Select)
map_population(input,output,element = "MAP_p1",population = session_elements.Tabs$p1_region,color = "#00A65A",Selection = input$MAP_parc_Select)
map_population(input,output,element = "MAP_p2",population = session_elements.Tabs$p2_region,color = "#DD4B39",Selection = input$MAP_parc_Select)
map_population(input,output,element = "MAP_p3",population = session_elements.Tabs$p3_region,color = "#3C8DBC",Selection = input$MAP_parc_Select)
 
 #Out of Pocket
map_oop( input, output, DATA_ , "Map_OOP_Parc",Cities,c("#C8958C", "#FF9200", "#FF7621", "#FF915E", "#FF6E1A","#F37300"))
map_oop( input, output, session_elements.Populations$Population1 , "Map_OOP_Pop1",Cities,c("#68978C", "#639782", "#80C415", "#52C81B", "#77BB73","#03A965"))
map_oop( input, output, session_elements.Populations$Population2 , "Map_OOP_Pop2",Cities,c("#BB958C", "#E08E35", "#DBB426", "#D97A33", "#E5B56F","#DB4218"))
map_oop( input, output, session_elements.Populations$Population3 , "Map_OOP_Pop3",Cities,c("#686F8C", "#3A8EBA", "#26C4EC", "#357AB7", "#77B5FE","#22427C"))


# POIs: 
output$Map_POI_Parc <- renderLeaflet({
  #POIS.pts
  if(is.null(input$POI_Select)){
    leaflet(POIS) %>% 
      addProviderTiles("CartoDB.Positron",options = providerTileOptions(minZoom=6,maxZoom =14)) %>%
      setView(lng = 9.696360,lat = 4.043290, zoom= 7.5) %>%
      addMinicharts(
        POIS$Longitude, POIS$Latitude,
        chartdata = POIS$N,
        showLabels = T,
        width = 40 * sqrt(POIS$N) / sqrt(max(POIS$N)),
        fillColor =  "#FF7700",
        opacity = 0.8,
        popup = popupArgs(noPopup = T)
      ) %>%
      addCircleMarkers( POIS$Longitude, POIS$Latitude,label =POIS$Origine,opacity = 0.2,labelOptions = labelOptions(noHide=T,direction = 'bottom',offset = c(0,20))
                       ,popup = popupArgs(noPopup = T),color = "#FF7700",
                       radius =1 * sqrt(POIS$N) / sqrt(max(POIS$N))
      )%>% 
      
      addMarkers(data=POIS.pts,lng = POIS.pts$Long, lat = POIS.pts$Lat,
                 label = POIS.pts$POI,
                 labelOptions = labelOptions(noHide=T,direction = 'bottom'))
    
  }else{
    POINTS <- POIS[which(POIS$POI %in% input$POI_Select),]
    POINTS.pts <- POIS.pts[which(POIS.pts$POI %in% input$POI_Select), ]
    
    leaflet(POINTS) %>% 
      addProviderTiles("CartoDB.Positron",options = providerTileOptions(minZoom=6,maxZoom =14)) %>%
      setView(lng = 9.696360,lat = 4.043290, zoom= 9.5) %>%
      addMinicharts(
        POINTS$Longitude, POINTS$Latitude,
        chartdata = POINTS$N,
        showLabels = T,
        width = 40 * sqrt(POINTS$N) / sqrt(max(POINTS$N)),
        fillColor =  "#FF7700",
        opacity = 0.8,
        popup = popupArgs(noPopup = T)
      ) %>%
      addCircleMarkers( POINTS$Longitude, POINTS$Latitude,label =POINTS$Origine,opacity = 0.2,labelOptions = labelOptions(noHide=T,direction = 'bottom',offset = c(0,20))
                        ,popup = popupArgs(noPopup = T),color = "#FF7700",
                        radius =1 * sqrt(POINTS$N) / sqrt(max(POINTS$N))
      )%>% 
      
      addMarkers(data=POINTS.pts,lng = POINTS.pts$Long, lat = POINTS.pts$Lat,
                 label = POINTS.pts$POI,
                 labelOptions = labelOptions(noHide=T,direction = 'bottom'))
  }
  
})

}# F _ SERVER


shinyApp(
  ui = dashboardPage( title = "Inbox Monetize",skin = "green",
                      header,
                      sidebar,
                      body
  ),
  server = server
)
