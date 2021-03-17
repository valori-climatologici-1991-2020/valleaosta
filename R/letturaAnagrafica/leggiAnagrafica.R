#17 marzo 2021
#Il programma legge le pagine del Centro Funzionale della Valle d'Aosta ed estrae le informazioni relative all'anagrafica delle stazioni.
#Il codice di ogni stazione viene estratto dai nomi dei file scaricati dallo stesso sito del Centro Funzionale.
library("tidyverse")
library("rvest")
library("xml2")

#Dal sito e' possibile scaricare dati di piu' anni relativi a un parametro per un massimo di 50 stazioni per volta
list.files(pattern="^Dati.+csv$")->ffile
str_extract(ffile,pattern="[[:digit:]]+")->CODICI

#la temperatura sembra avere piu' stazioni della precipitazione: nella stessa directory abbiamo messo dati di temperatura e precipitazione
#in modo di avere tutti i codici che ci servono, unique elimina i codici comuni tra precipitazione e temperatura
unique(CODICI)->CODICI

purrr::map_dfr(CODICI,.f=function(codice){

  #pagina dell'anagrafica
  xml2::read_html(glue::glue("https://presidi2.regione.vda.it/str_dataview_station/{codice}"))->pagina

  #La pagina dell'anagrafica ha tre tabelle : una contiene nome della stazione,comune, quota; una seconda tabella contienele coordinate UTM; una terza contiene le coordinate wgs84
  purrr::map_dfr(1:3,.f=function(indice){
    
    pagina %>%
      rvest::html_node(xpath=glue::glue("/html/body/div[2]/div/section/div/div/div/div[2]/div[4]/div/div[2]/table[{indice}]")) %>%
      rvest::html_table()
    
  })->df

  #la colonna Comune non riporta il nome completo della stazione (come nel caso di HisCentral). 
  #Per leggere il nome completo dobbiamo leggere il titolo (h2) della pagina
  pagina %>%
    rvest::html_node(xpath=glue::glue("/html/body/div[2]/div/section/div/div/div/div[2]/div[4]/div/div[2]/h2/strong")) %>%
    rvest::html_text()->SiteName
  

  df$SiteCode<-as.character(codice)
  df$SiteName<-SiteName
  Sys.sleep(3)
 
  df
  
})->finale


str_remove(finale$X1,":")->finale$X1

finale %>% 
  spread(key=X1,value=X2)->finale2

finale2 %>%
  rename(Longitude=Longitudine,Latitude=Latitudine,Elevation=Quota) %>%
  mutate(Elevation=str_extract(Elevation,"[[:digit:]]+"),
         Longitude=str_extract(Longitude,"[\\.[:digit:]]+"),
         Latitude=str_extract(Latitude,"[\\.[:digit:]]+"))->finale2

#saveRDS(finale2,"ana.RDS")
write_delim(finale2,file="anagrafica_valle_aosta_rivista_17marzo2021.csv",delim=";",col_names = TRUE)
