library("tidyverse")
library("readxl")
library("janitor")
library("rgdal")
library("leaflet")
library("scales")
#ons_deaths_source = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtablesweek18.xlsx"
# ons_deaths_source_old = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtables.xlsx"
ons_deaths_dest = "data/ons_deaths.xlsx"
#download.file(url=ons_deaths_source, destfile=ons_deaths_dest, method = "auto", quiet=FALSE)

#TODO: update these dates everytime new death data downloaded from ONS
deaths_date = "1st May 2020"
registered_date = "9th May 2020"

weekly_deaths = read_excel(ons_deaths_dest, sheet="Occurrences - All data", skip=3) %>% 
  clean_names() %>%
  filter(geography_type == "Local Authority") %>%
  mutate(place_of_death = str_replace(place_of_death," establishment",""))

deaths = weekly_deaths %>% 
  group_by(area_code, area_name, cause_of_death, place_of_death) %>%
  summarise(number_of_deaths = sum(number_of_deaths)) %>%
  ungroup() 

#population = read_csv("data/la_population.csv") %>% select(1:3)
population = read_excel("data/la_population_2019.xlsx") %>% clean_names() %>% select(1,2,4)

ethnicity = read_csv("data/ethnicity_summary_lad.csv") %>% 
  filter(ethnicity=="White") %>% 
  mutate(BAME=1-proportion) %>% 
  select(LAD19CD,BAME)

plot_la_ethnicity_deaths = function(show_all_cause){
  graph_data = deaths %>% 
    group_by(area_code, cause_of_death) %>% 
    summarise(total = sum(number_of_deaths)) %>% 
    ungroup() %>% 
    inner_join(population, by=c("area_code"="lad19cd")) %>%
    inner_join(ethnicity, by=c("area_code"="LAD19CD")) %>%
    mutate(deaths_per_100k = 100000*total/all_ages, 
           London = if_else(grepl("^E09", area_code),"London","non-London")) %>%
    select(LA = lad19nm, BAME, `Deaths per 100k population`=deaths_per_100k, `Cause of Death`=cause_of_death, `Total Deaths`=total, London)
  
  plot_title = "Number of deaths in LA against BAME (%)"
  
  if(!show_all_cause){
    graph_data = subset(graph_data,`Cause of Death`=="COVID 19")
    plot_title = "Number of COVID-19 deaths in LA against BAME (%)"
  }
  eth_plot = ggplot(graph_data, aes(x=BAME,y=`Deaths per 100k population`,label=LA)) +
    geom_point(aes(colour=London)) + 
    geom_smooth(method="lm") +
    xlab("Percentage of population BAME (%)") +
    ylab("Number of deaths per 100k population") +
    scale_x_continuous(labels = scales::percent) +
    scale_colour_manual(values = c("London"="red","non-London"="black")) +
    theme_bw(base_size = 10) + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "lines"),
          legend.position = "top",
          legend.title = element_blank()) +
    labs(title = plot_title,
         subtitle = paste0("Deaths up to ", deaths_date),
         caption = paste0("Data are from the ONS deaths (occurrences registered by ",registered_date,") and ONS census 2011 (ethnicity)\nPlot by Miqdad Asaria (@miqedup))")) 
  if(show_all_cause){
    eth_plot = eth_plot +  facet_wrap(.~`Cause of Death`, scales="free_y")
  }
  
  return(eth_plot)
}


plot_la_deaths = function(la_name, show_all_cause){
  if(la_name=="England and Wales"){
    la_deaths = deaths %>% 
      group_by(cause_of_death, place_of_death) %>% 
      summarise(number_of_deaths = sum(number_of_deaths)) %>% 
      ungroup()
  } else {
    la_deaths = deaths %>% filter(area_name == la_name)
  }
  totals = la_deaths %>% 
    group_by(cause_of_death) %>% 
    summarise(total = sum(number_of_deaths)) %>% 
    ungroup() %>% 
    spread(cause_of_death, total) 
  
  plot_title = paste0("Number of deaths in ",la_name," (up to 10th April 2020)")
  
  if(!show_all_cause){
    la_deaths = subset(la_deaths,cause_of_death=="COVID 19")
    plot_title = paste0("Number of COVID-19 deaths in ",la_name," (up to ",deaths_date,")")
  }
  
  la_plot = ggplot(la_deaths, aes(x=place_of_death, y=number_of_deaths, group=cause_of_death, fill=cause_of_death)) +
    geom_col(position="dodge", colour="black") + 
    geom_text(aes(label = scales::comma(number_of_deaths)), position = position_dodge(width = 1), vjust = -0.5, hjust=0.5, size=3.5) +
    xlab("Place of death") +
    ylab("Number of deaths") +
    scale_fill_manual(values = c("All causes"="#003F5C","COVID 19"="#E69F00")) + 
    theme_bw(base_size = 15) + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "lines"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank(), 
          legend.position = "top") +
    labs(title = plot_title,
         subtitle = paste0("Total COVID-19: ",comma(totals[["COVID 19"]],1)," Total All causes: ",comma(totals[["All causes"]],1)),
         caption = paste0("Plot by Miqdad Asaria (@miqedup) | Data are from the ONS deaths (occurrences registered by ",registered_date,")")) 
  
  return(la_plot)
}

plot_la_deaths_by_week = function(la_name, show_all_cause){
  if(la_name=="England and Wales"){ 
    graph_data = weekly_deaths %>% 
      group_by(cause_of_death, week_number, place_of_death) %>%
      summarise(number_of_deaths=sum(number_of_deaths)) 
    
  } else {
    graph_data = weekly_deaths %>% 
      filter(area_name == la_name) %>% 
      select(cause_of_death, week_number, place_of_death, number_of_deaths)
  }
  
  plot_title = paste0("Number of deaths in ",la_name)
  if(!show_all_cause){
    graph_data = subset(graph_data,cause_of_death=="COVID 19")
    plot_title = paste0("Number of COVID-19 deaths in ",la_name)
  }
  
  la_plot = ggplot(graph_data, 
                   aes(x=week_number, y=number_of_deaths, group=place_of_death, colour=place_of_death)) +
    geom_point() + 
    geom_smooth(se=FALSE) +
    xlab("Week") +
    ylab("Number of deaths") +
    scale_color_brewer(type="qual") + 
    facet_wrap(.~cause_of_death, scales="free_y") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "lines"),
          legend.title = element_blank(), 
          legend.position = "top") +
    labs(title = plot_title,
         subtitle = paste0("Deaths by week up to ",deaths_date),
         caption = paste0("Plot by Miqdad Asaria (@miqedup) | Data are from the ONS deaths (occurrences registered by ",registered_date,")"))
  
  return(la_plot)
}

get_la_list = function(){
  deaths %>% distinct(area_name) %>% arrange(area_name) %>% pull()
}

raw_data = function(){
  raw_data = deaths %>% 
    spread(place_of_death, number_of_deaths) %>% 
    mutate(Total = `Care home` + Elsewhere + Home + Hospice + Hospital + `Other communal`) %>%
    inner_join(population %>% select(area_code=lad19cd,population=all_ages)) %>%
    mutate(`Deaths per 100k pop` = round(Total*100000/population)) %>%
    select(LA = area_name, Population = population, Cause = cause_of_death, Hospital, `Care home`, Home, Hospice, `Other communal`, Elsewhere, Total, `Deaths per 100k pop`) %>%
    arrange(desc(Cause),desc(`Deaths per 100k pop`))
  return(raw_data)
}


make_popup_messages = function(map){
  popup_messages = paste0("<b>Name: </b>",map$lad19nm,"<br>",
                          "<b>Total Population: </b>",scales::comma(map$Population),"<br>",
                          "<b>COVID-19 Deaths</b> <br>",
                          "<b>Total (per 100k pop): </b>",map$Total," (",round(map$`Total per 100k`,1),")<br>",
                          "<b>Hospital (per 100k pop): </b>",map$Hospital," (",round(map$`Hospital per 100k`,1),")<br>",
                          "<b>Care home (per 100k pop): </b>",map$`Care home`," (",round(map$`Care home per 100k`,1),")<br>",
                          "<b>Home (per 100k pop): </b>",map$Home," (",round(map$`Home per 100k`,1),")<br>",
                          "<b>Hospice (per 100k pop): </b>",map$Hospice," (",round(map$`Hospice per 100k`,1),")<br>",
                          "<b>Other communal (per 100k pop): </b>",map$`Other communal`," (",round(map$`Other communal per 100k`,1),")<br>",
                          "<b>Elsewhere (per 100k pop): </b>",map$Elsewhere," (",round(map$`Elsewhere per 100k`,1),")<br>")
  
  return(popup_messages)  
}


choropleth_map = function(){
  la_map = readOGR(dsn=paste0("data/Local_Authority_Districts_December_2019_Boundaries_UK_BUC/"), verbose=FALSE, stringsAsFactors=FALSE)
  la_map = spTransform(la_map, CRS("+proj=longlat +ellps=WGS84"))
  death_data = deaths %>% 
    filter(cause_of_death=="COVID 19") %>%
    spread(place_of_death, number_of_deaths) %>% 
    mutate(Total = `Care home` + Elsewhere + Home + Hospice + Hospital + `Other communal`) %>%
    inner_join(population %>% select(area_code=lad19cd,population=all_ages)) %>%
    mutate(`Care home per 100k` = `Care home`*100000/population,
           `Elsewhere per 100k` = Elsewhere*100000/population,
           `Home per 100k` = Home*100000/population,
           `Hospice per 100k` = Hospice*100000/population,
           `Hospital per 100k` = Hospital*100000/population,
           `Other communal per 100k` = `Other communal`*100000/population,
           `Total per 100k` = round(Total*100000/population,1)) %>%
    select(lad19cd=area_code, area_name, Population = population, Cause = cause_of_death, Hospital, `Care home`, Home, Hospice, `Other communal`, Elsewhere, Total, 
           `Total per 100k`, `Hospital per 100k`, `Care home per 100k`, `Home per 100k`, `Hospice per 100k`, `Other communal per 100k`, `Elsewhere per 100k`)
  
  la_map = subset(la_map, lad19cd %in% death_data$lad19cd)
  la_map@data = inner_join(la_map@data, death_data)
  
  popup_message = make_popup_messages(la_map@data)
  
  total_pal = colorBin("Reds", la_map$`Total per 100k`, n=5, pretty = FALSE)
  
  labels = sprintf(
    "<strong>%s</strong><br/>%g COVID-19 deaths per 100k",
    la_map$lad19nm, la_map$`Total per 100k`
  ) %>% lapply(htmltools::HTML)
  
  choropleth_map = leaflet(la_map) %>% 
    addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE)) %>%
    addPolygons(stroke = TRUE, 
                smoothFactor = 1, 
                fillOpacity = 0.9, 
                weight = 1, 
                popup = popup_message, 
                fillColor = total_pal(la_map$`Total per 100k`), 
                color="black",
                group="Total",
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "12px",
                  direction = "auto")) %>%
    addLegend(pal = total_pal, values = ~`Total per 100k`, opacity = 0.7, 
              title = "Deaths per 100k<br> population", 
              position = "topright",
              labFormat = labelFormat(transform = function(x) round(x))
    ) %>%
    setView(lng=mean(la_map$long), lat=mean(la_map$lat), zoom=7) 
  return(choropleth_map)
}
