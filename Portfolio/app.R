library(shiny)
library(tidyverse)
library(DT)
library(shinydashboard)
library(shinydashboardPlus)
library(gendercoder)
library(gtsummary)
library(plotly)

# DATA2x02 survey + cleaning
open_2x02 <- function(x){
  dat = readr::read_tsv(x)
  
  old_names <- colnames(dat)
  new_names <- c("timestamp","covid_positive","living_arrangements",
                 "height","uni_travel_method","uni_travel_listen","spain_budget",
                 "feel_overseas","feel_anxious","study_hrs","read_news",
                 "study_load","work","lab_zoom","social_media","gender",
                 "sleep_time","wake_time","random_number","steak_preference",
                 "dominant_hand","normal_advanced","exercise_hrs",
                 "employment_hrs","city","weekly_saving","hourly_plan",
                 "weeks_behind","assignment_on_time","used_r_before","team_role",
                 "data2x02_hrs","social_media_hrs","uni_year","sport","wam",
                 "shoe_size","decade_selection")
  
  # overwrite the old names with the new names:
  colnames(dat) <- new_names
  return(dat)
}
dat_2x02 <- open_2x02("Data/DATA2x02 survey (2022) - Form responses 1.tsv")
clean_2x02 <- function(dat) {
  # height- we don't want anyone too tall, and we want to convert all 
  # measurements to cm. Those measured in foot are removed.
  dat = dat %>% 
    dplyr::mutate(
      height_clean = readr::parse_number(height),
      height_clean = case_when(
        height_clean <= 2.4 ~ height_clean * 100,
        height_clean <= 9 ~ NA_real_,
        TRUE ~ height_clean
      )
    )
  
  # Gender
  dat = dat %>% mutate(
    gender_clean = gendercoder::recode_gender(gender)
  )
  
  # Social Media
  dat = dat %>% mutate(
    social_media_clean = tolower(social_media),
    social_media_clean = str_replace_all(social_media_clean, '[[:punct:]]',' '),
    social_media_clean = stringr::word(social_media_clean),
    social_media_clean = case_when(
      stringr::str_starts(social_media_clean,"ins") ~ "instagram",
      stringr::str_starts(social_media_clean,"ti") ~ "tiktok",
      stringr::str_starts(social_media_clean,"mess") ~ "facebook",
      stringr::str_starts(social_media_clean,"n") ~ "none",
      is.na(social_media_clean) ~ "none",
      TRUE ~ social_media_clean
    ),
    social_media_clean = tools::toTitleCase(social_media_clean),
    social_media_clean = forcats::fct_lump_min(social_media_clean, min = 10)
  )
  
  # Data2x02 hours
  dat = dat %>% 
    dplyr::mutate(
      data2x02_hrs_clean = data2x02_hrs,
      data2x02_hrs_clean = case_when(
        data2x02_hrs_clean <= 25 ~ data2x02_hrs_clean,
        data2x02_hrs_clean > 25 ~ NA_real_,
        TRUE ~ data2x02_hrs_clean
      )
    )
  
  # Exercise hours
  # Don't want those who exercise more than 30 hours
  dat = dat %>% 
    dplyr::mutate(
      exercise_hrs_clean = exercise_hrs,
      exercise_hrs_clean = case_when(
        exercise_hrs_clean <= 30 ~ exercise_hrs_clean,
        exercise_hrs_clean > 30 ~ NA_real_,
        TRUE ~ exercise_hrs_clean
      )
    )
  
  # Social media hours
  dat = dat %>% 
    dplyr::mutate(
      social_media_hrs_clean = social_media_hrs,
      social_media_hrs_clean = case_when(
        social_media_hrs_clean <= 12 ~ social_media_hrs_clean,
        social_media_hrs_clean > 12 ~ NA_real_,
        TRUE ~ social_media_hrs_clean
      )
    )
  
  # Wam
  dat = dat %>% 
    dplyr::mutate(
      wam_clean = wam,
      wam_clean = case_when(
        wam_clean >= 50 ~ wam_clean,
        wam_clean < 50 ~ NA_real_,
        TRUE ~ wam_clean
      )
    )
  
  # Employment hours
  dat = dat %>% 
    dplyr::mutate(
      employment_hrs_clean = employment_hrs,
      employment_hrs_clean = case_when(
        employment_hrs_clean <= 40 ~ employment_hrs_clean,
        employment_hrs_clean > 40 ~ NA_real_,
        TRUE ~ employment_hrs_clean
      )
    )
  
  # Study hours
  dat = dat %>% 
    dplyr::mutate(
      study_hrs_clean = study_hrs,
      study_hrs_clean = case_when(
        study_hrs_clean <= 50 ~ study_hrs_clean,
        study_hrs_clean > 50 ~ NA_real_,
        TRUE ~ study_hrs_clean
      )
    )
  return(dat)
}
dat_2x02_clean <- clean_2x02(dat_2x02)

categorical_vars = c("Gender" = "gender_clean",
                     "Had covid before" = "covid_positive",
                     "Living arrangements" = "living_arrangements",
                     "Favourite social media" = "social_media_clean",
                     "Read news" = "read_news",
                     "Study load" = "study_load",
                     "Work" = "work",
                     "Steak preference" = "steak_preference",
                     "Dominant hand" = "dominant_hand",
                     "Decade selection" = "decade_selection",
                     "Normal or advanced DATA2x02" = "normal_advanced",
                     "Number of days planned by the hour" = "hourly_plan",
                     "Assignment submitted on time" = "assignment_on_time"
)
continuous_vars = c("Height" = "height_clean",
                    "Exercise hours" = "exercise_hrs_clean",
                    "Employment hours" = "employment_hrs_clean",
                    "Hours spent on DATA2x02 per week" = "data2x02_hrs_clean",
                    "Wam" = "wam_clean",
                    "Hours spent on social media" = "social_media_hrs_clean",
                    "Anxious Scale (More anxious in ascending order)" = "feel_anxious",
                    "Feel about travelling overseas (More keen in ascending order)" = "feel_overseas",
                    "Study hours per week" = "study_hrs_clean"
)


ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Bryan Ng"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About me", tabName = "aboutme", icon = icon("user")),
      menuItem("Project 1 - DATA2x02 survey", tabName = "pro1", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      tabItem(tabName = "aboutme",
              h2("About me"),
              p("I am a third year student at the University of Sydney studying
                Data Science and Computer Science under the degree Bachelor of
                Science (Advanced) / Master of Mathematical Sciences")
      ),
      tabItem(tabName = "pro1",
              fluidPage(
                titlePanel("Data 2902 Survey Analysis"),
                
                sidebarPanel(
                  conditionalPanel(
                    condition = "input.tabselected == 1",
                    selectInput(inputId = "plots",
                                label = "Choose a plot",
                                choices = c("Barplot",
                                            "Histogram",
                                            "Comparative barplot",
                                            "Comparative boxplot",
                                            "Scatterplot"))
                  )
                ),

                  mainPanel(
                    tabsetPanel(
                      type = "tabs", id = "tabselected", selected = 1,
                      tabPanel("Plots", plotOutput("plot1", height = 250), value = 1),
                      tabPanel("Tests",
                               value = 2
                      ),
                      tabPanel("Data",
                               dataTableOutput("survey_dt"),
                               value = 3)
                    )
                  )
                )
              )

              
    )
  )
)


server <- function(input, output, session) {
  
  output$survey_dt = renderDataTable({
    datatable(dat_2x02_clean)
  })
  
  output$plot1 <- renderPlot({
    set.seed(122)
    histdata <- rnorm(500)
    # data <- histdata[seq_len(input$slider)]
    hist(histdata)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
