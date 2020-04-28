library("tidyverse")
library("janitor")
library("RSQLite")
library("lubridate")

plot_cases = function(){
  # download latest cases from PHE
  try(write_csv(read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"), "data/phe_cases.csv"), silent=TRUE)
  cases = read_csv("data/phe_cases.csv") %>% 
    clean_names() %>%
    filter(specimen_date==max(specimen_date) & area_type=="Upper tier local authority") %>%
             select(area_name, area_code, report_date=specimen_date, cases = starts_with("cumulative"))
  
  ethnicity = read_csv("data/ethnicity_summary_utla.csv") %>% 
    filter(ethnicity=="White") %>% 
    mutate(BAME=1-proportion) %>% 
    select(UTLA19CD,BAME)
  
  population = read_csv("data/population_utla.csv")
  
  graph_data = ethnicity %>% 
    inner_join(population) %>% 
    inner_join(cases, by=c("UTLA19CD"="area_code")) %>%
    mutate(cases_per_100k = cases*100000/population,
           London = if_else(grepl("^E09", UTLA19CD),"London","non-London")) %>%
    arrange(desc(cases_per_100k)) %>%
    select(LA=UTLA19NM, BAME, `Cases per 100k population`=cases_per_100k,London)
  
  report_date = max(cases$report_date)
  report_date = paste(day(report_date),month(report_date,TRUE,FALSE),year(report_date),sep=" ")
  
  eth_cases_plot = ggplot(graph_data, aes(x=BAME,y=`Cases per 100k population`)) +
      geom_point(aes(colour=London)) + 
      geom_smooth(method="lm") +
      xlab("Percentage of population BAME (%)") +
      ylab("Number of cases per 100k population") +
      scale_colour_manual(values = c("London"="red","non-London"="black")) +
      scale_x_continuous(labels = scales::percent) +
      theme_bw(base_size = 10) + 
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "lines"),
            legend.position = "top") +
      labs(title = paste0("Number of COVID-19 cases in UTLA against BAME (%) - ",report_date),
           subtitle = paste0("Cases up to ", report_date),
           caption = "Data are from the PHE cases and ONS census 2011 (ethnicity)\nPlot by Miqdad Asaria (@miqedup)") 
    
  return(eth_cases_plot)

}

