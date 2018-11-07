###########################UTILITIES###############################

### DASHBOARDING BRANDS 
Brands_dashboard <- function(DATA_){
  Brands_Dash_P1 <- DATA_[, list(Volume_DATA = mean(Data_Volume,na.rm=T), 
                                 NB_APP= mean(Voix_National_Nb+Voix_International_Nb+Voix_Roaming_Nb,na.rm=T),
                                 Voix = mean(Voix_National+Voix_International+Voix_Roaming,na.rm=T),
                                 SMS = mean(SMS_NB,na.rm = T),
                                 Mean_Usage_DATA = mean(Data_Nbj_Activite, na.rm = T),
                                 Mean_Usage_SMS = mean(SMS_Nbj_Activite, na.rm = T),
                                 Mean_Usage_Voix = mean(Data_Nbj_Activite, na.rm = T),
                                 TOP_DATA = sum(TOP_DATA, na.rm = T),
                                 TOP_LTE = sum(TOP_LTE, na.rm = T)
  ),
  by = c("Equipement_Marque")]
  Brands_Dash_P2 <- DATA_ [, .N, by= c("Equipement_Marque")]
  setnames(Brands_Dash_P2, c('N'),c('Devices'))
  Brands_Dash <- merge(Brands_Dash_P2,Brands_Dash_P1,by=("Equipement_Marque"))
  for (i in 2:ncol(Brands_Dash)) {
    Brands_Dash[[i]] <- round( Brands_Dash[[i]] ,0)
  }
  Stars <- NULL
  for(v in levels(DATA_$Equipement_Marque)){
    mod <- DATA_[DATA_$Equipement_Marque == v,] [, .N, by= c("Equipement_Modele")]
    setorder(mod, - N)
    setnames(mod,c('Equipement_Modele'),c("STAR"))
    mod <- cbind(c(v),mod)
    setnames(mod,c('V1'),c("Equipement_Marque"))
    
    #
    Stars <- rbind(Stars,mod[1,1:2])
  }
  Brands_Dash <- merge(Brands_Dash,Stars,by=("Equipement_Marque"))
  return(Brands_Dash)
}

DashGauge <- function ( input, output,res,indicateur,label_,Dash_table)
{ 
  output[[res]] <- renderGauge({
    gauge(Dash_table[Dash_table$Equipement_Marque == input$Dash_Equipement,][[indicateur]], min = 0, max = max(Dash_table[[indicateur]]),label = label_,gaugeSectors(
      success = c(0.8*max(Dash_table[[indicateur]]), 1*max(Dash_table[[indicateur]])), warning = c(0.40*max(Dash_table[[indicateur]]), 0.79*max(Dash_table[[indicateur]])), danger = c(0, 0.39*max(Dash_table[[indicateur]]))) )
  })
}

###  infos map pour terminaux

bts_device = function (tab,bts)
{
  terminaux <- subset(tab, select=c(Equipement_Marque, Longitude,Latitude))
  nb_dev <- terminaux [, .N, by= c("Longitude","Latitude")]
  setnames(nb_dev, c('N'),c('Nbre'))
  res <- merge(nb_dev,bts,  by=c( "Longitude","Latitude"))
  res <- as.data.table(res)
  res <- setorder(res,Nbre)
  res$Rank <- rank(res$Nbre,ties.method = "first")
  res$col <- round(res$Rank/nrow(res)*12 ,0)
  res$col <- ifelse(res$col == 12, 11,res$col)
  return(res)
}

###### Cartographie
input_map =function (tab)
{
  nb_clts <- tab[, .N, by= c("Longitude","Latitude")]
  data <- subset(tab, Data_Volume>0)
  nb_data <- data [, .N, by= c("Longitude","Latitude")]
  setnames(nb_data, c('N'),c('N_data'))
  moy_data <- tab[, lapply(.SD, mean, na.rm=T), by=c("Longitude","Latitude"), .SDcols=c("Data_Volume")]
  moy_data <- moy_data[, lapply(.SD, round), by=c("Longitude","Latitude")]
  nb_sms <- tab[, lapply(.SD, sum, na.rm=T), by=c("Longitude","Latitude"), .SDcols=c("SMS_NB")]
  DATA_$voix_nb <- rowSums(tab[,c("Voix_National_Nb","Voix_International_Nb","Voix_Roaming_Nb")],na.rm=T)
  nb_voix <- tab[, lapply(.SD, sum, na.rm=T), by=c("Longitude","Latitude"), .SDcols=c("voix_nb")]
  DATA_$voix_duree <- rowSums(tab[,c("Voix_National","Voix_International","Voix_Roaming")],na.rm=T)
  moy_voix <- tab[, lapply(.SD, mean, na.rm=T), by=c("Longitude","Latitude"), .SDcols=c("voix_duree")]
  moy_voix <- moy_voix[, lapply(.SD, round), by=c("Longitude","Latitude")]
  res <- merge(nb_clts,merge(nb_data,merge(moy_data,merge(nb_sms,merge(nb_voix,moy_voix, by= c("Longitude","Latitude")), by= c("Longitude","Latitude")), by= c("Longitude","Latitude")), by= c("Longitude","Latitude")), by= c("Longitude","Latitude"))
  return (res)
}


## part de marché des terminaux
getMarques = function(t)
{ Marques <- as.data.table(table(t$Equipement_Marque))
return(Marques)
}

## le top 10 des modèles 
top10_Models  = function(t) 
{
  t$Modeles <- paste(t$Equipement_Marque,t$Equipement_Modele)
  Modeles <- as.data.table(table(t$Modeles))
  setorder(Modeles,-N)
  top10 <-head(Modeles,10)
  return(top10)
}

### fonction pour calculer les moyennes  
calculMoyenne = function( tab )
{
  moy <- as.data.table(tab[,c(-1:-10, -12:-14,-50:-53, -61,-62:-69)])
  moy$voix_duree <- rowSums(moy[,c("Voix_National","Voix_International","Voix_Roaming")],na.rm=T)
  moy$voix_nb <- rowSums(moy[,c("Voix_National_Nb","Voix_International_Nb","Voix_Roaming_Nb")],na.rm=T)
  moy$voix_nbcontact <- rowSums(moy[,c("Voix_NbContact_National","Voix_NbContact_International","Voix_NbContact_Roaming")],na.rm=T)
  moyenne <- moy[, lapply(.SD, mean, na.rm=T), by="Equipement_Marque"]
  moyennes <- moyenne[, lapply(.SD, round), by="Equipement_Marque"]
  return (moyennes)
}



##### camembert
camembert = function ( input, output,res,tab)
{ 
  output[[res]] <- renderPlotly({
    
    if(!is.null(tab)){
      plot_ly(tab, labels = ~V1, values = ~N, type = 'pie',textposition = 'inside',
              textinfo = 'label+value+percent',
              insidetextfont = list(color = '#000000'),
              hoverinfo = 'text',
              text = ~paste( V1, ' Device'),
              marker = list(line = list(color = '#FFFFFF', width = 1)),
              #The 'pull' attribute can also be used to create space between the sectors
              showlegend = FALSE) %>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               autosize = T)
    }else return()
  })
}


##### histogramme 

histogramme = function ( input, output,res, tab )
{
  output[[res]] <- renderPlotly({
    input$validate
    if(input$validate == 0)
      return()
    f <- list(color = "#7f7f7f")
    x <- list(titlefont = f)
    y <- list(titlefont = f)
    plot_ly(tab, x = ~V1, y = ~N) %>%
      layout(xaxis = x, yaxis = y, autosize = T)
  })
}


###### tableau 

tableau_render <- function ( input, output, p, res, color)
{
  reactive({
    MOY_VOIX <- as.character(paste0(p,"_mean_voix"))
    output[[MOY_VOIX]] <- renderDataTable({  
      input$validate
      if(input$validate == 0)
        return()
      datatable(res,options = list(pageLength = 5,columnDefs = list(list(targets = c(1,3:42), visible = FALSE)),searching = T, paging = F, bInfo=F),rownames = F,class = 'cell-border stripe'  )  %>% 
        formatStyle(c("voix_duree"),
                    background = styleColorBar(range(res$voix_duree), color),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
        ) %>%
        formatStyle(c("voix_nb"),
                    background = styleColorBar(range(res$voix_nb), color),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
        )  %>% 
        formatStyle(c("voix_nbcontact"),
                    background = styleColorBar(range(res$voix_nbcontact), color),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
        ) 
    })
    MOY_SMS <- as.character(paste0(p,"_mean_sms"))
    output[[MOY_SMS]] <- renderDataTable({ 
      input$validate
      if(input$validate == 0)
        return()
      datatable(res,options = list(pageLength = 5, columnDefs = list(list(targets = c(1:11, 15:45), visible = FALSE)),searching = T, paging = F, bInfo=F),rownames = F,class = 'cell-border stripe'  )  %>% 
        formatStyle(c("SMS_Nbj_Activite"),
                    background = styleColorBar(range(res$SMS_Nbj_Activite), color),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
        ) %>%
        formatStyle(c("SMS_NB"),
                    background = styleColorBar(range(res$SMS_NB), color),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
        )  %>% 
        formatStyle(c("SMS__Contacts"),
                    background = styleColorBar(range(res$SMS__Contacts), color),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
        ) 
    })
    MOY_DATA <- as.character(paste0(p,"_mean_data"))
    output[[MOY_DATA]] <- renderDataTable({ 
      input$validate
      if(input$validate == 0)
        return()
      datatable(res,options = list(pageLength = 5, columnDefs = list(list(targets = c(1:14, 21:45), visible = FALSE)),searching = T, paging = F, bInfo=F),rownames = F,class = 'cell-border stripe'  )  %>% 
        formatStyle(c("Data_Nbj_Activite"),
                    background = styleColorBar(range(res$Data_Nbj_Activite), color),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
        ) %>%
        formatStyle(c("Data_Volume"),
                    background = styleColorBar(range(res$Data_Volume), color),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
        )  %>% 
        formatStyle(c("Data_2G"),
                    background = styleColorBar(range(res$Data_2G), color),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
        )  %>% 
        formatStyle(c("Data_3G"),
                    background = styleColorBar(range(res$Data_3G), color),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
        )  %>% 
        formatStyle(c("Data_4G"),
                    background = styleColorBar(range(res$Data_4G), color),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
        )   %>% 
        formatStyle(c("Data_Roaming"),
                    background = styleColorBar(range(res$Data_Roaming), color),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
        )
    })
    MOY_RES <- as.character(paste0(p,"_mean_reseaux_sociaux"))
    output[[MOY_RES]] <- renderDataTable({ 
      input$validate
      if(input$validate == 0)
        return()
      datatable(res,options = list(pageLength = 5, columnDefs = list(list(targets = c(1:20, 24:45), visible = FALSE)),searching = T, paging = F, bInfo=F),rownames = F,class = 'cell-border stripe'  )  %>% 
        formatStyle(c("Reseaux_Sociaux"),
                    background = styleColorBar(range(res$Reseaux_Sociaux), color),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
        ) %>%
        formatStyle(c("Web"),
                    background = styleColorBar(range(res$Web), color),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
        )  %>% 
        formatStyle(c("News"),
                    background = styleColorBar(range(res$News), color),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
        ) 
    })
  })
}


######## sous fonctions  #########

#### voix ####

sous_tab_voix <- function ( input, output, p , res, color)
{
  
  output[[p]] <- renderDataTable({  
    input$validate
    if(input$validate == 0)
      return()
    datatable(res,options = list(pageLength = 5,columnDefs = list(list(targets = c(1,3:42), visible = FALSE)),searching = T, paging = F, bInfo=F),rownames = F,class = 'cell-border stripe'  )  %>% 
      formatStyle(c("voix_duree"),
                  background = styleColorBar(range(res$voix_duree), color),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      ) %>%
      formatStyle(c("voix_nb"),
                  background = styleColorBar(range(res$voix_nb), color),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      )  %>% 
      formatStyle(c("voix_nbcontact"),
                  background = styleColorBar(range(res$voix_nbcontact), color),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      ) 
  })}

#### sms ####

sous_tab_sms <- function ( input, output, p , res, color)
{
  output[[p]] <- renderDataTable({
    input$validate
    if(input$validate == 0)
      return()
    datatable(res,options = list(pageLength = 5, columnDefs = list(list(targets = c(1:11, 15:45), visible = FALSE)),searching = T, paging = F, bInfo=F),rownames = F,class = 'cell-border stripe'  )  %>% 
      formatStyle(c("SMS_Nbj_Activite"),
                  background = styleColorBar(range(res$SMS_Nbj_Activite), color),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      ) %>%
      formatStyle(c("SMS_NB"),
                  background = styleColorBar(range(res$SMS_NB), color),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      )  %>% 
      formatStyle(c("SMS__Contacts"),
                  background = styleColorBar(range(res$SMS__Contacts), color),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      ) 
  })}

#### data ####

sous_tab_data <- function ( input, output, p , res, color)
{
  output[[p]] <- renderDataTable({
    input$validate
    if(input$validate == 0)
      return()
    datatable(res,options = list(pageLength = 5, columnDefs = list(list(targets = c(1:14, 21:45), visible = FALSE)),searching = T, paging = F, bInfo=F),rownames = F,class = 'cell-border stripe'  )  %>% 
      formatStyle(c("Data_Nbj_Activite"),
                  background = styleColorBar(range(res$Data_Nbj_Activite), color),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      ) %>%
      formatStyle(c("Data_Volume"),
                  background = styleColorBar(range(res$Data_Volume), color),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      )  %>% 
      formatStyle(c("Data_2G"),
                  background = styleColorBar(range(res$Data_2G), color),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      )  %>% 
      formatStyle(c("Data_3G"),
                  background = styleColorBar(range(res$Data_3G), color),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      )  %>% 
      formatStyle(c("Data_4G"),
                  background = styleColorBar(range(res$Data_4G), color),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      )   %>% 
      formatStyle(c("Data_Roaming"),
                  background = styleColorBar(range(res$Data_Roaming), color),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      )
  })}

#### reseaux_sociaux ####

sous_tab_reseaux_sociaux <- function ( input, output, p , res, color)
{
  output[[p]] <- renderDataTable({
    input$validate
    if(input$validate == 0)
      return()
    datatable(res,options = list(pageLength = 5, columnDefs = list(list(targets = c(1:20, 24:45), visible = FALSE)),searching = T, paging = F, bInfo=F),rownames = F,class = 'cell-border stripe'  )  %>% 
      formatStyle(c("Reseaux_Sociaux"),
                  background = styleColorBar(range(res$Reseaux_Sociaux), color),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      ) %>%
      formatStyle(c("Web"),
                  background = styleColorBar(range(res$Web), color),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      )  %>% 
      formatStyle(c("News"),
                  background = styleColorBar(range(res$News), color),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      ) 
  })}


aggregate_villes <- function(tab,Cities,top){
  tab <- as.data.table(tab)
  parc_ville <- tab[, .N ,by=c("Geoloc_ville")]
  parc_ville <- merge(parc_ville,Cities,by = "Geoloc_ville",all.x  = T)
  setorder(parc_ville, -N)
  return(head(parc_ville,top))
}

aggregate_region <- function(tab,Regions){
  tab <- as.data.table(tab)
  parc_region <- tab[, .N ,by=c("Geoloc_region")]
  parc_region <- merge(parc_region,Regions,by = "Geoloc_region",all.x  = T)   
}

map_population <- function(input, output,element,population,color,Selection){
  
      output[[element]] <- renderLeaflet(
        if(!is.null(population)){
          mymap <- leaflet(population)  %>% 
            addProviderTiles("CartoDB.Positron", options = providerTileOptions(minZoom=6, maxZoom=14)) %>%
            setView(lng = 11.908214, lat = 5.422864 , zoom = 6.5) %>%
            addCircleMarkers(population$Long_Region, population$Lat_Region,
                             radius = 0.1,label = population$Geoloc_region,
                             labelOptions = labelOptions(noHide = T,offset = c(0,10),direction = 'bottom',opacity = 1)
            )%>%
            addMinicharts(
              population$Long_Region, population$Lat_Region,
              chartdata = population$N,
              showLabels = T,
              width = 50 * sqrt(population$N) / sqrt(max(population$N)),
              fillColor = color,
              opacity = 0.8,
              popup = popupArgs(noPopup = T)
            )
        }else return())  
    
}


#### OOP DES POPULATIONS
oop_secteur <- function(tab,Cities)
{
  tab <- as.data.table(tab)
  OOP <- tab[, list(telecom = sum(OOP_Telecom,na.rm=T), assurance= sum(OOP_Assurances,na.rm=T),automobile = sum(OOP_Automobile,na.rm=T) ,bank = sum(OOP_Bank,na.rm=T), retail = sum(OOP_Retail,na.rm=T),hotel = sum(OOP_Hotels,na.rm=T)),by = c("Geoloc_ville")]
  OOP_F <- merge(OOP, Cities, by= "Geoloc_ville" )
  OOP_F$total <- rowSums(OOP_F[,c("telecom","assurance","automobile","bank", "retail","hotel")],na.rm=T)
  OOP_F <-setorder(OOP_F, -total)
  OOP_F <- head(OOP_F,20)
  OOP_F$Telecom <- round(OOP_F$telecom / OOP_F$total*100,digits = 0)
  OOP_F$Assurance <- round(OOP_F$assurance / OOP_F$total*100,digits = 0)
  OOP_F$Automobile <- round(OOP_F$automobile / OOP_F$total*100,digits = 0)
  OOP_F$Bank <- round(OOP_F$bank / OOP_F$total*100,digits = 0)
  OOP_F$Retail <- round(OOP_F$retail / OOP_F$total*100,digits = 0)
  OOP_F$Hotel <- round(OOP_F$hotel / OOP_F$total*100,digits = 0)
  return(OOP_F)
}


#### map des OOP des populations
map_oop <- function ( input, output, tab , pop,Cities,colors)
{  
  
  
    output[[pop]] <- renderLeaflet({ 
      if (!is.null(tab)) {
      OOP_F <- oop_secteur(tab,Cities)
      leaflet(OOP_F) %>%
        addProviderTiles("CartoDB.Positron",options = providerTileOptions(minZoom=6,maxZoom =14)) %>%
        setView(lng = 11.90821,lat = 5.42286, zoom= 5.5) %>%
        addMinicharts(
          OOP_F$Long_Ville, OOP_F$Lat_Ville,
          type = "pie",
          chartdata = OOP_F[, c("Telecom", "Assurance", "Automobile", "Bank", "Retail", "Hotel")], 
          colorPalette = colors, 
          width = 60 * sqrt(OOP_F$total) / sqrt(max(OOP_F$total)), transitionTime = 0
          
        ) %>%
        addCircleMarkers(OOP_F$Long_Ville, OOP_F$Lat_Ville,label =OOP_F$Geoloc_ville,opacity = 0,labelOptions = labelOptions(noHide=T,direction = 'bottom',offset = c(0,20))
                         , popup =paste0("Telecom    : ",OOP_F$Telecom,"% <br/>", 
                                        "Assurance  :", OOP_F$Assurance,"% <br/>", 
                                        "Automobile :", OOP_F$Automobile,"% <br/>", 
                                        "Bank       :", OOP_F$Bank, "% <br/>", 
                                        "Retail     :", OOP_F$Retail,"% <br/>", 
                                        "Hotel      :", OOP_F$Hotel, "%" ) 
                         ,radius =30 * sqrt(OOP_F$total) / sqrt(max(OOP_F$total))
        )
      }else return()
      })
  
  
  }
