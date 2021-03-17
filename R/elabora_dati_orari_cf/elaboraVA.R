#17 marzo 2021: legge i dati di precipitazione/temperatura orari scaricati dal sito del Centro Funzionale della Valle d'AOsta (https://cf.regione.vda.it)
rm(list=objects())
library("tidyverse")
library("guido")
library("seplyr")
library("visdat")

PARAM<-c("Prec","Tmax","Tmin")[3]

if(grepl("^P.+",PARAM)){
  aggrega<-purrr::partial(.f=sum,na.rm=FALSE)
}else if(grepl("^T.+x$",PARAM)){
  aggrega<-purrr::partial(.f=max,na.rm=FALSE)
}else if(grepl("^T.+n$",PARAM)){
  aggrega<-purrr::partial(.f=min,na.rm=FALSE)
}

read_delim("../anagrafica_valle_aosta_rivista_17marzo2021.csv",delim=";",col_names = TRUE)->ana
list.files(pattern="^Dati.+csv$")->ffile

purrr::map(ffile,.f=function(nomeFile){
  
  str_extract(nomeFile,"[[:digit:]]+")->codice
  which(ana$SiteCode==codice)->riga
  ana[riga,]$SiteID->ID
  if(length(riga)!=1) browser()
  
  read_fwf(nomeFile,skip = 7,
           col_positions = fwf_positions(start=c(1,12,21),end=c(10,19,25),col_names = c("yymmdd","hhmmss","value")),
           skip_empty_rows = TRUE,
           #col_names = FALSE,
           col_types = cols(yymmdd=col_date(),hhmmss=col_character(),value=col_double()),
           locale = locale(decimal_mark = ","))->dati

  dati %>%
    tidyr::separate(yymmdd,into=c("yy","mm","dd"),sep="-") %>%
    mutate(yy=as.integer(yy),mm=as.integer(mm),dd=as.integer(dd))->dati2

  
  dati2 %>%
    group_by(yy,mm,dd) %>%
    summarise(conta=n()) %>%
    ungroup()->conteggio
  
  left_join(dati2,conteggio)->dati2
  
  dati2 %>%
    filter(conta>24)->doppioni
  if(nrow(doppioni)!=0) stop()
  
  dati2 %>%
    filter(conta==24) %>%
    group_by(yy,mm,dd) %>%
    summarise(aggregato=aggrega(value)) %>%
    ungroup() %>%
    dplyr::select(yy,mm,dd,aggregato) %>%
    seplyr::rename_se(c(ID:="aggregato"))
  
})->listaOut #fine purrr

purrr::map(listaOut,"yy") %>%
  purrr::map_int(.f=min)->anniMinimi

purrr::map(listaOut,"yy") %>%
  purrr::map_int(.f=max)->anniMassimi

min(anniMinimi)->annoI
max(anniMassimi)->annoF

purrr::reduce(listaOut,.f = left_join,.init = creaCalendario(annoI,annoF))->finale

print(skimr::skim(finale))
print(vis_miss(finale %>% filter(yy==annoI)))
print(vis_miss(finale %>% filter(yy==annoF)))


write_delim(finale,file=glue::glue("{PARAM}_{annoI}_{annoF}_valleaosta.csv"),delim=";",col_names = TRUE)