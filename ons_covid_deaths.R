library("tidyverse")
library("readxl")
library("RSQLite")
library("janitor")

# ons_deaths_source = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtables.xlsx"
# ons_deaths_dest = "data/ons_deaths.xlsx"
# download.file(url=ons_deaths_source, destfile=ons_deaths_dest, method = "auto", quiet=FALSE)

deaths = read_excel("data/ons_deaths.xlsx", sheet="Occurrences - All data", skip=3) %>% 
  clean_names() %>%
  filter(geography_type == "Local Authority") %>%
  group_by(area_code, area_name, cause_of_death, place_of_death) %>%
    summarise(number_of_deaths = sum(number_of_deaths)) %>%
  ungroup() %>%
  mutate(place_of_death = str_replace(place_of_death," establishment",""))

population = read_csv("data/la_population.csv")

plot_la_deaths = function(la_name){
  la_deaths = deaths %>% filter(area_name == la_name)
  totals = la_deaths %>% 
    group_by(cause_of_death) %>% 
    summarise(total = sum(number_of_deaths)) %>% 
    ungroup() %>% 
    spread(cause_of_death, total) 
  
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
    labs(title = paste0("Number of deaths in ",la_name," (up to 10th April 2020)"),
         subtitle = paste0("Total COVID-19: ",totals[[1]]," Total All causes: ",totals[[2]]),
         caption = "Plot by Miqdad Asaria (@miqedup) | Data are from the ONS deaths (occurrences registered by 18th April 2020)") 
  
  return(la_plot)
}

get_la_list = function(){
  deaths %>% distinct(area_name) %>% arrange(area_name) %>% pull()
}

raw_data = function(){
  raw_data = deaths %>% 
    spread(place_of_death, number_of_deaths) %>% 
    mutate(Total = `Care home` + Elsewhere + Home + Hospice + Hospital + `Other communal`) %>%
    inner_join(population %>% select(area_code=area_codes,population=all_ages)) %>%
    mutate(`Deaths per 100k pop` = round(Total*100000/population)) %>%
    select(LA = area_name, Population = population, Cause = cause_of_death, Hospital, `Care home`, Home, Hospice, `Other communal`, Elsewhere, `Deaths per 100k pop`) %>%
    arrange(desc(Cause),desc(`Deaths per 100k pop`))
  return(raw_data)
}


