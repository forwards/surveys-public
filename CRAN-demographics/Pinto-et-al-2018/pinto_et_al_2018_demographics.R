  library(readr)
  library(ggplot2)
  library(dplyr)
  
  ## load Pinto et al. 2018 survey data 
  surv_path <- 'https://raw.githubusercontent.com/fronchetti/SANER-2018/master/Survey/actual.csv'
  surv_data <- readr::read_csv(surv_path)
  rm(surv_path)
  
  surv_data %>% 
    glimpse()
  
  ## select relevant columns (gender, age, location, and education)
  df <- surv_data %>% 
    select(1:5)
  rm(surv_data)
  
  ## rename columns with short names
  names(df) <- c('timestamp', 'age', 'gender', 'location', 'highest_education')
  
  ## a bit of variable cleaning
  df$age <- gsub('--', 'to', df$age)
  df$gender[df$gender == 'non-binary'] <- 'Non-binary'
  df$age <- as.factor(df$age)
  df$gender <- as.factor(df$gender)
  df$location <- as.factor(df$location) 
  df$highest_education <- factor(df$highest_education, 
                                 levels = c("Bachelor's", "Master's", "Doctorate's", NA))
  
  ## repeated objects to be used in plots below
  sample_size <- 'Total sample size is 1,553 respondents'
  data_src <- 'Data source: Pinto et al., 2018, DOI: 10.1109/SANER.2018.8330263'
  
  ## Gender plot -----
  gender_perc <- df %>%
    group_by(gender) %>%
    summarise(n = n()) %>%
    mutate(perc = (floor((n / sum(n))*1000)/1000)*100) %>% 
    select(-c(n)) %>% 
    ungroup()
  
  ggplot(gender_perc, aes(x = gender, y = perc)) +
    geom_col() +
    geom_text(aes(label = paste0(perc, '%')), vjust = -0.45, size = 4.5) +
    labs(title = 'Percent of respondents by self-identified gender',
         caption = paste0(sample_size, " \n\n", data_src)) +
    ylab('Percent of respondents') +
    xlab('') +
    theme_minimal() + 
    theme(text = element_text(size = 14),
          axis.text = element_text(size = 12))
  
  ## Age categories plot ---- 
  age_perc <- df %>%
    group_by(age) %>%
    summarise(n = n()) %>%
    mutate(perc = (floor((n / sum(n))*1000)/1000)*100) %>% 
    ungroup()
  
  ggplot(age_perc, aes(x = age, y = perc, fill = age)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = paste0(perc, '%')), vjust = -0.45, size = 4.5) +
    labs(title = 'Percent of respondents by self-identified age category',
         caption = paste0(sample_size, " \n\n", data_src)) +
    ylab('Percent of respondents') +
    xlab('self-identified age category') +
    theme_minimal() + 
    theme(text = element_text(size = 14),
          axis.text = element_text(size = 12)) + 
    scale_fill_brewer(palette = 'Set1', na.value = '#d3d3d3')

  ## Region/location plot ----
  loc_perc <- df %>%
    group_by(location) %>%
    summarise(n = n()) %>%
    mutate(perc = (floor((n / sum(n))*1000)/1000)*100) %>% 
    arrange(desc(perc, location)) %>% 
    ungroup()
  
  ggplot(loc_perc, aes(x = reorder(location, -perc), y = perc)) +
    geom_col() +
    geom_text(aes(label = paste0(perc, '%')), vjust = -0.45, size = 4.5) +
    labs(title = 'Percent of respondents by self-identified region',
         caption = paste0(sample_size, " \n\n", data_src)) +
    ylab('Percent of respondents') +
    xlab('') +
    theme_minimal() + 
    theme(text = element_text(size = 14),
          axis.text = element_text(size = 12)) 
  
  ## Education plot ----
  educ_perc <- df %>%
    group_by(highest_education) %>%
    summarise(n = n()) %>%
    mutate(perc = (floor((n / sum(n))*1000)/1000)*100) %>% 
    ungroup()
  
  ggplot(educ_perc, aes(x = highest_education, y = perc, fill = highest_education)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = paste0(perc, '%')), vjust = -0.45, size = 4.5) +
    labs(title = 'Percent of respondents by self-identified highest level of education completed or in-progress',
         caption = paste0(sample_size, " \n\n", data_src)) +
    ylab('Percent of respondents') +
    xlab('') +
    theme_minimal() + 
    theme(text = element_text(size = 13.5),
          axis.text = element_text(size = 12)) + 
    scale_fill_brewer(palette = 'Set1', na.value = '#d3d3d3')
  