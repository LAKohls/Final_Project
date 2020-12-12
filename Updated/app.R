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


source("./global.R")

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
                        selected = "PropertyName",
                        multiple = FALSE,
                        options = pickerOptions(
                            actionsBox = TRUE,
                            title = "Please Select A Variable")
                    ), 
                    pickerInput(
                        inputId = "x_color",
                        label = "Select Color",
                        choices = c(  "Age"= "age2", 
                                      "Race" = "race", 
                                      "Health_Ins_Type" = "health_ins_type", 
                                      "Employed" = "employed", 
                                      "Safe_in_Building" = "safe_building", 
                                      "Safe_in_Neighborhood" = "safe_neighborhood", 
                                      "Registered_to_Vote" = "voter_reg", 
                                      "Has_Usual_Medial_Provider" = "usual_provider", 
                                      "Routine_Checkups" = "routine_checkup", 
                                      "Visited ER in Last Year" = "er_visits", 
                                      "Has Experienced Food Insecurity" = "food_insec", 
                                      "Experiences Physical Barriers" = "physical_bar", 
                                      "Experiences Mental Barriers" = "mental_bar", 
                                      "Has Insurance" = "insurance_yes_no",
                                      "Has Checking Account" = "check_acct_res",
                                      "Has Savings Account" = "save_acct_res" ), 
                        selected = "race",
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
        
        x<- na.omit(.data[[input$x_axis]])
        fill<- na.omit(.data[[input$x_color]])
        
        ggplot(data=data2, aes(x= .data[[input$x_axis]], fill = .data[[input$x_color]]))+
            geom_bar(na.rm= TRUE)+
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