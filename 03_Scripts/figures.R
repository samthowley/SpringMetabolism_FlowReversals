library(tools)


file.names <- list.files("04_Outputs/flood impacts", pattern = "\\.csv$", full.names = TRUE)

data_list <- map(file.names, ~ {
  df <- read_csv(.x)
  stub <- file_path_sans_ext(basename(.x))   # e.g. "DO"
  
  # Keep ID, Date, flood unprefixed:
  keep <- c("ID", "flood")
  cols_to_change <- setdiff(names(df), keep)
  
  names(df)[names(df) %in% cols_to_change] <-
    paste(stub, names(df)[names(df) %in% cols_to_change], sep = "_")
  
  df
})


impacts <- data_list %>%
  reduce(left_join, by = c("ID", "flood"))%>% 
  filter(!is.na(flood))%>%
  mutate(stage_difference=depth_maximum-depth_base)

(a<-impacts %>%
    # filter(ID=='AM', 
    #        !is.na(flood), 
    #        #flood %in% c()
    # ) %>%
    ggplot(aes(x = stage_difference, y = DO_percent.change)) +
    geom_point()+
    geom_smooth(method = 'lm')+
    facet_wrap(~ ID, scales = "free")+theme_minimal())


ggplotly(a)


write_csv(impacts, "test.csv")
