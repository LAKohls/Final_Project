library(reactable)
library(leaflet)
library(tidyverse)
library(ggplot2)
library(shinyWidgets)
library(lubridate)
library(hablar)
library(tigris)
library(sp)
library(rgeos)
library(rgdal)
library(maptools)
library(dplyr)
library(scales)
library(maps)
library(likert)
library(tidygeocoder)
library(stringr)
library(ggrepel)
library(tidycensus)
library(shiny)
library(shinydashboard)
library(shinythemes)

# Set Census API 
key <- "30f8a6b33c7a3c3ddc67053c0d75dfc692f6081a" 
col_pal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#E69F00")

data<- read.csv("Data.csv")
today<- Sys.Date()
data$DOB<- mdy(data$DOB)
data$age<- (year(today)- year(data$DOB))
data$gross_income<- as.numeric(data$gross_income)
data <- data %>%
    arrange(age) %>%
    group_by(PropertyName) %>%
    mutate(median_age = median(age, na.rm= TRUE)) %>%
    ungroup() %>%
    arrange(age) %>%
    mutate(total_medianage = median(age, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(age2 = case_when(
        between(age, 0,4) ~ "Pre-School (0-4)",
        between(age, 4,20)~ "Elementary (5-10)",
        between(age, 11,13)~"Middle School (11-13)",
        between(age, 14,18) ~ "High School (14-18)",
        between(age, 19,24) ~ "Young Adults (19-24)",
        between(age, 25,35)~ "Early Career (25-35)",
        between(age, 36,54) ~ "Middle Age (36-54)",
        between(age, 54, 200) ~ "Senior(55+)")) %>%
    mutate(health_ins_type = case_when(
        ins_CHIP == "yes" ~ "CHIP",
        ins_medicaid == "yes" ~ "Medicaid",
        ins_medicare == "yes" ~ "Medicare",
        ins_private == "yes" ~ "Private",
        ins_VA == "yes" ~ "VA"
    )) %>%
    mutate(safe_building = factor(safe_building, levels = c("Always", "Most", "Sometimes", "Never", "Refused", "Not_collected"))) %>%
    mutate(safe_neighborhood = factor(safe_building, levels = c("Always", "Most", "Sometimes", "Never", "Refused", "Not_collected"))) %>%
    mutate(race = case_when(
        race_amer_in == "yes" ~ "Am_Indian",
        race_asian == "yes" ~ "Asian",
        race_black == "yes" ~ "Black",
        race_white == "yes" ~ "White", 
        race_haw_pi == "yes" ~ "Haw_Pi",
        race_other == "yes" ~ "Other"))



data$age2[is.na(data$age2)]<- "Not Collected"
data$age2<-  factor(data$age2, levels =c("Pre-School (0-4)",
                                         "Elementary (5-10)",
                                         "Middle School (11-13)",
                                         "High School (14-18)",
                                         "Young Adults (19-24)",
                                         "Early Career (25-35)",
                                         "Middle Age (36-54)",
                                         "Senior(55+)", "Not Collected"))
median_age<- median(data$age, na.rm= TRUE)
median_income<- median(data$gross_income, na.rm= TRUE)

unique_properties<- data %>%
    select(PropertyName) %>%
    arrange(PropertyName)


unique_properties<- unique(unique_properties$PropertyName)


graph_picker_data <- data %>%    
    select(-race_collected, -ins_CHIP, -ins_medicaid, -ins_medicare, -ins_other, -ins_private, -ins_VA, -PropertyID, -HouseholdID, -ResidentID, -DOB, -move_out_date, -move_in_date, -race_amer_in, -race_asian, -race_black, -race_white, -race_other, -move_out_reason, -race_haw_pi)


graph_picker_color<- graph_picker_data %>%
    select(-relationship, -PropertyName)


library(tidyverse)
library(tidygeocoder)
library(stringr)
library(dplyr)
library(ggplot2)
library(maps)
library(ggrepel)
library(tidycensus)
library(dplyr)
library(knitr)


addresses<- read.csv("addresses.csv")

census_full1 <- addresses %>% geocode(addresses, 
                                      method = 'census', full_results = TRUE, return_type = 'geographies', return_addresses = FALSE)

census_full1$state_fips<- str_pad(census_full1$state_fips, 2, side = "left", pad = "0")
census_full1$county_fips<- str_pad(census_full1$county_fips, 3, side = "left", pad = "0")

census_full1$census_tract<- str_pad(census_full1$census_tract, 6, side = "left", pad = "0")
census_full1$census_block<-str_pad(census_full1$census_block, 4, side = "left", pad = "0")




census_full1<- census_full1 %>%
    mutate(GEOID = paste0(state_fips, county_fips, census_tract))


all_vars <- c(
    total_pop = "B01003_001",
    median_income = "B19013_001",
    median_rent = "B25064_001",
    poverty = "B06012_002",
    poverty_1.5x ="B06012_003",
    poverty_2x ="B06012_004",
    edu_no_highschool ="B06009_002",
    edu_highschool ="B06009_003",
    edu_bachelors ="B06009_005",
    edu_graduate ="B06009_006",
    employed ="B23025_004",
    unemployed ="B23025_005",
    total_labor_force ="B23025_002",
    african_american="B02009_001",
    european_american="C02003_003",
    asian_american="C02003_006",
    native_american="C02003_005",
    pacific_islander="C02003_007",
    hispanic_latino="B03002_012",
    total_male ="B01001_002",
    total_female="B01001_026",
    age_median="B01002_001",
    earnings_median="B24081_001",
    health_carecoverage= "C27021_001")


multi_state_tract <- get_acs(
    geography = "tract",
    variables = all_vars,
    state = census_full1$state_fips,
    year = 2018,
    survey = "acs5",
    geometry = FALSE,
    key = key) %>%
    filter(GEOID %in% census_full1$GEOID)

multi_state_tract<- multi_state_tract %>%
    select(-moe)

multi_state_tract<- unique(multi_state_tract)

attach(multi_state_tract)
test<- pivot_wider(multi_state_tract, names_from= "variable", values_from= "estimate")

test2<- merge(census_full1, test,  by = "GEOID")
test2<- test2 %>%
    mutate(Median_Income= median_income,
           Median_Rent = median_rent, 
           Male = (total_male/total_pop)*100,
           Female= (total_female/ total_pop)*100,
           Hispanic = (hispanic_latino/total_pop)*100,
           Black = (african_american/total_pop)*100, 
           Asian = (asian_american/total_pop)*100,
           White = (european_american/total_pop)*100,
           Pacific_Islander = (pacific_islander/total_pop)*100,
           Native_American = (native_american/total_pop)*100,
           Employed = (employed/total_labor_force)*100, 
           Unemployed = (unemployed / total_labor_force)*100, 
           Poverty = (poverty/total_pop)*100,
           Poverty_1.5x = (poverty_1.5x/total_pop)*100,
           Poverty_2x = (poverty_2x/total_pop)*100,
           No_Highschool= (edu_no_highschool/total_pop)*100,
           Highschool_Degree = (edu_highschool/total_pop)*100,
           Bachelors = (edu_bachelors/total_pop)*100,
           Graduate = (edu_graduate/total_pop)*100,
           HealthCare_Coverage = (health_carecoverage/total_pop)*100)

census_table_output<- test2 %>%
    select(PropertyName, Median_Income, Median_Rent, Male, Female, Employed, Unemployed, Black, White, Hispanic, Native_American, Asian, Pacific_Islander, Poverty, Poverty_1.5x, Poverty_2x, No_Highschool, Highschool_Degree, Bachelors, Graduate, HealthCare_Coverage)


#### UI 

ui<- dashboardPage(
    dashboardHeader(title = "Operation Pathways"),
    dashboardSidebar(
        sidebarMenu(
            pickerInput(
                inputId = "properties",
                label = "Select A Property",
                choices = unique_properties, 
                selected = unique_properties[1:length(unique_properties)],
                multiple = TRUE,
                options = pickerOptions(
                    actionsBox = TRUE,
                    title = "Please Select A Property")
            ),
            
            menuItem("Pathways Overview", tabName = "widgets", icon=icon("home")),
            menuItem("Analyze Data", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Census Data", tabName = "explore", icon = icon("th")),
            menuItem("Dataset", tabName = "census", icon = icon("search"))
        )),
    
    
    dashboardBody(
        tabItems(
            
            tabItem(tabName= "widgets", 
                    h1("Overview of All Properties"),
                    fluidRow(
                        valueBoxOutput("householdsBox"),
                        valueBoxOutput("individualBox"),
                        valueBoxOutput("ageBox")
                        
                    ),
                    fluidRow(
                        valueBoxOutput("incomeBox"),
                        infoBoxOutput("healthcareBox"),
                        infoBoxOutput("schoolBox") 
                    ),
                    fluidRow(
                        
                        box(title = "Population", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, plotOutput("pop_graph"), width = 50),
                        box(title = "Age", status = "warning", solidHeader = TRUE,
                            collapsible = TRUE, plotOutput("age_breakdown_graph"), width = 50) 
                    ),
                    fluidRow(
                        box(title = "Safety", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, plotOutput("safety_graph"), width = "100%"),
                    )
            ),
            
            tabItem(tabName = "dashboard",
                    h1("Graph Creation"),
                    
                    pickerInput(
                        inputId = "x_axis",
                        label = "Select X Axis",
                        choices = c(
                            "PropertyName" = "PropertyName",
                            "Race" = "race",
                            "Age_Category" = "age2",
                            "Health_Ins_Type" = "health_ins_type"), 
                        selected = "race",
                        multiple = FALSE,
                        options = pickerOptions(
                            actionsBox = TRUE,
                            title = "Please Select A Variable")
                    ), 
                    pickerInput(
                        inputId = "x_color",
                        label = "Select Color",
                        choices = colnames(graph_picker_color), 
                        selected = colnames(graph_picker_color)[1],
                        multiple = FALSE,
                        options = pickerOptions(
                            actionsBox = TRUE,
                            title = "Please Select A Variable")
                    ),
                    
                    
                    
                    fluidRow(
                        h1("Your Graphed Data"),
                        
                        box(plotOutput("user_create"), width = "100%"))
            ),
            
            tabItem(tabName = "explore",
                    h1("Census Data"),
                    
                    fluidRow(
                        h1("Census Data Table By Property")),
                    
                    fluidRow(downloadLink("downloadData", "Download"),
                             
                             reactableOutput("censustable2"))
            ),   
            
            
            tabItem(tabName = "census",
                    h1("Exploring Your Dataset"),
                    reactableOutput("censustable")
                    
            ) 
        )) 
)

#####################################################################
#####################################################################
#####################################################################


server<- function(input, output){
    
    data_select<- reactive({
        data %>%
            filter(PropertyName %in% input$properties)})
    
    census_select<- reactive({ 
        data <- readRDS(paste0(input$censustype,".Rds"))})
    
    census_data<- reactive({
        data<- census_table_output
    })
    
    ############ Dashboard Boxes ########################################
    # Households 
    output$householdsBox <- renderValueBox({
        data_select<- data_select()
        households<- length(unique(data_select$HouseholdID))
        valueBox(
            paste0(households), "Households Served", icon = icon("home"),
            color = "olive"
        )
    })
    # Individuals 
    output$individualBox <- renderValueBox({
        data_select<- data_select()
        individuals<- length(unique(data_select$ResidentID))
        valueBox(
            paste0(individuals), "Individuals Served", icon = icon("user"),
            color = "yellow"
        )
    })
    # AGE
    output$ageBox<- renderValueBox({
        data_select<- data_select() %>%
            arrange(age)
        median_age<- median(data_select$age, na.rm= TRUE)
        valueBox(
            paste0(median_age), "Median Age", icon= icon("user"), color = "purple")
    })
    # Income
    output$incomeBox<- renderValueBox({
        data_select<- data_select() %>%
            arrange(gross_income)
        
        mean_income<-round(mean(data_select$gross_income, na.rm= TRUE), digits = 0)
        valueBox(
            paste0("$", mean_income), "Average Reported Income", icon= icon("credit-card"), color = "light-blue")
    })
    
    # Healthcare
    output$healthcareBox<- renderValueBox({
        data_select<- data_select()
        percent_healthcare<- round(sum((data_select$insurance_yes_no == "yes")/length(data$insurance_yes_no))*100, digits = 0)
        valueBox(
            paste0(percent_healthcare, "%"), "Reported Having Healthcare", icon= icon("heart"), color = "maroon")
    })
    
    
    # Schooling
    output$schoolBox<- renderValueBox({
        
        data_select<- data_select()
        data_ed<- data_select %>%
            filter(edu_attained != "Not_collected") %>%
            filter(edu_attained != "")
        data_NoHS<- data_ed %>%
            filter(edu_attained == "NoHS")
        percent_edu<-  round(length(data_NoHS$edu_attained)/length(data_ed$edu_attained)*100)
        
        valueBox(
            paste0(percent_edu, "%"), "HS Diploma or Higher", icon= icon("certificate"), color = "teal")
    })
    
    # Gender Box 
    output$genderBox <- renderInfoBox({
        female<- data_select() %>%
            filter(gender == "Female")
        
        male<- data_select() %>%
            filter(gender== "Male")
        unknown<- data_select() %>%
            filter(gender == "Unknown")
        
        table(data$gender)
        
        percent_female<- round(length(female$gender)/(length(male$gender) + length(female$gender)+length(unknown$gender))*100)
        
        data_gender<- 
            valueBox(
                paste0(percent_female, "%"), "Female Residents", icon = icon("list"),
                color = "aqua")
    })
    
    #####################################  GRAPHS  ############################################
    
    output$age_breakdown_graph <- renderPlot({
        data2<- data_select()
        ggplot(data2, aes(x=age2, fill = age2))+
            geom_histogram(stat="count", fill = "lightblue")+
            ggtitle("Property Age Breakdown")+
            xlab("Age Ranges")+
            ylab("Total Count")+
            labs(fill = "Breakdown")+
            theme_minimal()
    })
    
    ######### Pop_graph ##########
    
    output$pop_graph <- renderPlot({
        data2<- data_select() %>%
            group_by(PropertyName) %>%
            tally() %>%
            arrange(desc(n)) %>%
            mutate(PropertyName = fct_reorder(PropertyName, n))
        
        ggplot(data=data2, aes(x= PropertyName, y = n))+
            geom_col(fill = "lightblue")+
            ggtitle("Properties By Population") +
            xlab("Property Name") +
            ylab("Total Records")+
            theme_minimal()+
            theme(plot.title= element_text(hjust= 0.5))
        
        
    })
    
    ############################## AGE 
    
    output$propage_graph <- renderPlot({
        data2<- data_select() %>%
            
            ggplot(data=data2, aes(x= PropertyName, y = median_age))+
            geom_col(fill = "lightblue")+
            coord_flip()+
            ggtitle("Properties By Age") +
            xlab("Property Name") +
            ylab("Total Records")+
            theme(plot.title= element_text(hjust= 0.5))
    })
    
    
    ############ USER CREATE GRAPH #############
    output$user_create<- renderPlot({
        data2<- data_select()
        
        ggplot(data=data2,na.rm=TRUE, aes(x= .data[[input$x_axis]], fill = .data[[input$x_color]]))+
            geom_bar()+
            theme_minimal(base_size = 25)+
            theme(axis.text.x = element_text(angle = 45, hjust=1))+
            scale_fill_manual(values = col_pal)
    })
    
    ############### Likert: Safety
    
    output$safety_graph<- renderPlot({
        
        data_select<- data_select()
        
        likert_safe<- data_select %>%
            filter(safe_building %in% c("Always", "Most", "Sometimes", "Never")) %>%
            filter(safe_neighborhood %in% c("Always", "Most", "Sometimes", "Never")) %>%
            select(safe_neighborhood, safe_building)
        
        
        names(likert_safe) <- c(
            safe_building= "I feel safe in my building",
            safe_neighborhood = "I feel safe in my neighborhood")
        
        likert_safe$`I feel safe in my building`<- factor(likert_safe$`I feel safe in my building`, levels = c("Never", "Sometimes", "Most", "Always"))
        likert_safe$`I feel safe in my neighborhood`<- factor(likert_safe$`I feel safe in my neighborhood`, levels = c("Never","Sometimes","Most","Always"))
        
        likert_safe<- as.data.frame(likert_safe)
        likert(likert_safe)
        
        levels(likert_safe$`I feel safe in my building`)
        levels(likert_safe$`I feel safe in my neighborhood`)
        p<- likert(likert_safe)
        
        plot(p, main = "Feelings of Safety")
    })
    
    
    output$censustable2<-renderReactable({
        reactable(census_data(), filterable= TRUE, sortable = TRUE)
    })
    
    
    
    output$table <-  renderReactable({
        react_data<- data%>%
            select(-PropertyID, -HouseholdID, -ResidentID, -DOB, -move_out_date, -move_in_date, -race_amer_in, -race_asian, -race_black, -race_white, -race_other, -move_out_reason, -race_haw_pi)
        
        
        reactable(react_data, filterable = TRUE, sortable = TRUE)})
    
    
    output$censustable <-  renderReactable({
        
        reactable(data_select(), filterable = TRUE, sortable = TRUE)})
    
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("census", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(census_data(), file, row.names = FALSE)})
    
    #############################################################
    
}


shinyApp(ui, server)