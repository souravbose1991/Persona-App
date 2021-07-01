
options(shiny.reactlog=TRUE)
options(shiny.maxRequestSize=500*1024^2)  # 500MB Data Import for Shiny Support Enabled

# packages <- c("dplyr", "shinydashboard", "shiny", "reticulate", "slickR", "shinyjs", "quanteda", "plotly",
#               "shinyWidgets", "tools", "textreadr", "shinyalert", "tm", "stringr", "tokenizers")


packages <- c("cluster", "tidyverse", "dplyr", "ggplot2", "readr", "Rtsne", "openxlsx", "shiny", 
              "gifski",  "janitor", "caret", "shinydashboard", "shinyjs", "shinyalert",
              "shinyWidgets", "plotly", "DT", "RColorBrewer", "rpart", "rpart.plot", 
              "rpart.utils", "stringr", "reshape2", "party", "partykit")


lapply(packages,library,character.only = TRUE)


ui <- dashboardPage( skin = "blue",
                     dashboardHeader(title = "Persona Analyzer"),
                     dashboardSidebar(
                       # img(src = "logo/Logo.png", height="230px", width="230px"),
                       sidebarMenu(
                         menuItem("Supervised Learning", tabName = "supl",icon = icon("gear")),
                         menuItem("UnSupervised Learning", tabName = "unsupl",icon=icon("th"))
                         # menuItem("About", tabName = "hlp",icon = icon("info"))
                       )
                     ),
                     dashboardBody(
                       useShinyjs(),
                       useShinyalert(),
                       
                       tabItems(
                         tabItem(
                           tabName = "supl",
                           
                           fluidRow(
                             
                             
                             box(title = "Input", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                                 column(12,fileInput("dataset_upld", "Choose File To Upload")),
                                 
                                 column( 3, selectInput("colname_t", "Choose Event Variable", choices = c())),
                                 column( 3, selectInput("evt_val_ss", "Choose Event Value", choices = c())),
                                 column(12, br()),
                                 column(12,
                                        infoBoxOutput("obs_ss"),
                                        infoBoxOutput("vars_ss"),
                                        infoBoxOutput("numvars"),
                                        infoBoxOutput("charvars"),
                                        infoBoxOutput("eventdist")
                                 ),
                                 column(12, br()),
                                 
                                 column(6, selectInput("analysis","Type of Exploration", choices=c("Univariate","Bivariate","Event Rate"))),
                                 column(6, selectInput("graph1","Plot Type",choices=c())),
                                 
                                 column(3,selectInput("xaxis.ss","X-Axis",c())),
                                 column(3,selectInput("yaxis.ss","Y-Axis",c())),
                                 column(3,selectInput("zaxis.ss","KPI",c())),
                                 
                                 column(2, hr(),actionButton("action1", "Submit", class="btn-primary", icon = icon("area-chart")), offset = 1)
                                 
                             )),
                           
                           
                           fluidRow(
                             
                             box(title ="Graphical Visualisation", status= "primary", width=7, solidHeader = TRUE, collapsible = TRUE,
                                 column(12, plotlyOutput("myplot.ss"))),
                             
                             box(title = "Model Framework", status = "primary", width = 5, solidHeader = TRUE, collapsible = TRUE,
                                 column(12, selectInput("col_drp_ss", "Drop Variables", choices = c(), multiple = T)),
                                 column(6, selectInput("modltyp", label = "Model Type", choices = list("Logistic Regression" = 1, "Decision Trees" = 2, 
                                                                                                       "Random Forest" = 3, "Gradient Boosting" = 4,
                                                                                                       "Xgboost" = 5,"Neural Network"= 6), 
                                                       selected = 2)),
                                 column(6, checkboxInput("hyp_bool", "Define Hyper Parameters")),
                                 
                                 column(12, sliderInput("data_split_sb", "Train-Data Split", 
                                                        min = 1, max = 100, value = 70, step=1, post = "%")),
                                 
                                 column(12, textInput("hyparmt_1", label = "Max Depth",
                                                      value = paste(c(), collapse = ", "),
                                                      placeholder = "Default: 4, 6, 8 ...")),
                                 
                                 column(12, textInput("hyparmt_2", label = "Min Spit",
                                                      value = paste(c(), collapse = ", "),
                                                      placeholder = "Default: 100, 200, 400 ...")),
                                 
                                 column(12, textInput("hyparmt_3", label = "Sample Weightage",
                                                      value = paste(c(), collapse = ", "),
                                                      placeholder = "Default: 0.2, 0.4, 0.6 ...")),
                                 
                                 
                                 column(12, br()),
                                 column(2, actionButton("action3", "Submit", class="btn-primary", icon = icon("area-chart")), offset = 9))
                             
                             
                             # br(),
                             # 
                             # box(title = "Imputation", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                             #   
                             #   column( 6, checkboxGroupInput("EDAChoice", label = "EDA", 
                             #                      choices = list("Missing Value Treatment" = 1, "Outlier Treatment" = 2, "Scaling" = 3),
                             #                      selected = 1)),
                             #   column(2, br(), br(), actionButton("action2", "Submit", class="btn-primary",icon = icon("gear")), offset = 10)),
                             # br(),
                             # box(title = "Choose your model", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                             # column( 6, selectInput("colname_t1", "Drop Redundant Variables", choices = c())),
                             # column(6, selectInput("modltyp", label = "Type of Modelling", choices = list("Logistic Regression" = 1, "Decision Trees" = 2, "Random Forest" = 3, "Gradient Boosting" = 4,"Xgboost" = 5,"Neural Network"= 6),selected = 2)),
                             # 
                             # tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: red}")),
                             # column(12, 
                             #        
                             #        column(3,id="dtshow1", sliderInput("hyparmt_1", "Tuning Parameter CP ", min = 0, max = 1, value = 0.01)),
                             #        
                             #        column(3,id="dtshow2", sliderInput("hyparmt_2", "Max depth ", min = 1, max = 30, value = 4,step=1)),
                             #        
                             #        column(3, id="dtshow3",sliderInput("hyparmt_3", "Min Bucket ", min = 1, max = 60, value = 45,step=1)),
                             #        
                             #        column(3, id="dtshow4",sliderInput("hyparmt_4", "Min Split ", min = 1, max = 60, value = 45,step=1))
                             #        
                             #        ),
                             
                             
                             
                             # tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: green}")),
                             
                             
                             
                           ),
                           
                           fluidRow(
                             
                             box(title = "Model Performance", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                                 
                                 column(12, h4(tags$b("Model Structure")), plotOutput("model_plot")),
                                 column(12, br()),
                                 column(12, h4(tags$b("Confusion matrix")), dataTableOutput("conf_mat"))
                             )
                           ),
                           
                           fluidRow(
                             
                             box(title = "Persona", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                                 dataTableOutput("model_rules")
                             )
                           )
                           
                           
                           
                         ),
                         
                         tabItem(
                           tabName = "unsupl",
                           
                           fluidRow(
                             
                             box(title = "Clustering", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                                 
                                 column(8, selectInput("col_drp_sb", "Choose Variables to Drop", choices = c(), multiple = T)),
                                 column(2, actionButton("drp_var_sb", "Suggest #Clusters", class="btn-primary", icon = icon("area-chart")), offset = 2),
                                 column(12, br(), br()),
                                 column(8, plotOutput("silhot_sb")),
                                 column(4, selectInput("nclus_sb","Number of Clusters",choices=c(seq(2, 10)))),
                                 column(2, br(), br(), actionButton("clus_ac_sb", "Run Clustering", class="btn-primary", icon = icon("area-chart")))
                             )),
                           
                           fluidRow(
                             
                             box(title = "Segmentation Plots", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                                 
                                 column(8, plotlyOutput("tsne_sb")),
                                 column(12, dataTableOutput("clus_rules_sb"))
                             ))
                           
                         )
                         
                         
                       )))

# tabItem(
#   tabName = "hlp",
#   column(12, offset = 1, slickROutput("guide", height = 800, width='85%'))



server <- function(session, input, output) {
  set.seed(42)
  
  
  ########################### Supervised Learning #############################
  
  
  
  inFile_df <-reactive({
    inFile_df <- input$dataset_upld
    if (is.null(inFile_df))
      return(NULL)
    read.table(inFile_df$datapath, header = TRUE, strip.white=TRUE, sep = ",")
  })
  
  
  ######### Reactive Values' Set #################
  
  p_mod <- reactiveValues()
  
  set.seed(42)
  
  
  observeEvent(input$dataset_upld, {
    
    shinyjs::disable("modltyp")
    
    p_mod$data_all <- inFile_df()
    p_mod$data_all1 <- p_mod$data_all[complete.cases(p_mod$data_all),]
    # p_mod$data_all1 <- p_mod$data_all1 %>% mutate_if(is.character, str_trim)
    # p_mod$data_all1<-na.omit(p_mod$data_all1)
    # p_mod$data_all1<-remove_empty(p_mod$data_all1, which = c("rows", "cols"))
    # p_mod$data_all1 <- p_mod$data_all1[!apply(is.na(p_mod$data_all1) | p_mod$data_all1 == " ", 1, all),]
    
    p_mod$x<- nrow(p_mod$data_all1)
    p_mod$y <- ncol(p_mod$data_all1)
    
    p_mod$z1_mod1<- sum(unlist(sapply(p_mod$data_all1,is.numeric)))
    # p_mod$z2_mod1<- sum(unlist(sapply(raw_file_mod,is.character)))
    p_mod$z2_mod1<- p_mod$y - p_mod$z1_mod1
    
    # p_eda$data2_mod <- head(raw_file_eda,50)
    p_mod$colname <-colnames(p_mod$data_all1)
    updateSelectInput(session, "colname_t", choices = p_mod$colname)
    
    showModal(modalDialog(HTML(
      paste0("Data Uploaded with ", nrow(p_mod$data_all)," rows.", "<br>", 
             "After removing Missing Values data has ", nrow(p_mod$data_all1), " rows.")), 
      easyClose =  T, footer = modalButton("OK")))
    
  })
  
  
  observeEvent(p_mod$data_all1, {
    
    updateSelectInput(session, "col_drp_sb", choices = colnames(p_mod$data_all1))
    
  })
  
  
  observeEvent(input$colname_t, {
    
    p_mod$ev_val <- unique(p_mod$data_all1[,input$colname_t])
    updateSelectInput(session, "evt_val_ss", choices = p_mod$ev_val)
    updateSelectInput(session, "col_drp_ss", choices = setdiff(colnames(p_mod$data_all1), input$colname_t))
    
  })
  
  
  observeEvent(input$evt_val_ss, {
    
    p_mod$ev_rate <- nrow(p_mod$data_all1[which(p_mod$data_all1[,input$colname_t]==input$evt_val_ss),])/nrow(p_mod$data_all1)
    
  })
  
  
  
  observeEvent(paste0(input$dataset_upld,input$analysis), {
    
    if(input$analysis == "Univariate") {
      shinyjs::hide(id = "yaxis.ss")
      shinyjs::hide(id = "zaxis.ss")
      updateSelectInput(session, "graph1", choices = c("Histogram", "Density Plot", "Box Plot"))
      
    } else if(input$analysis == "Bivariate") {
      shinyjs::show(id = "yaxis.ss")
      shinyjs::hide(id = "zaxis.ss")
      updateSelectInput(session, "graph1", choices = c("Scatter Plot", "Bar Plot", "Heat Map"))
      
    } else if(input$analysis == "Event Rate") {
      shinyjs::hide(id = "yaxis.ss")
      shinyjs::hide(id = "zaxis.ss")
      updateSelectInput(session, "graph1", choices = c("Event Rate Plot"))
      
    }
    
  })
  
  
  
  observeEvent(paste0(input$dataset_upld,input$graph1,input$colname_t), {
    
    if(input$graph1 == "Box Plot") {
      p_mod$temp_choice <- setdiff(names(dplyr::select_if(p_mod$data_all1,is.numeric)), input$colname_t)
      updateSelectInput(session, "xaxis.ss", choices = p_mod$temp_choice)
      
    } else if(input$graph1 == "Histogram"){
      p_mod$temp_choice <- setdiff(colnames(p_mod$data_all1), input$colname_t)
      updateSelectInput(session, "xaxis.ss", choices = p_mod$temp_choice)
      
    } else if(input$graph1 == "Density Plot") {
      p_mod$temp_choice <- setdiff(names(dplyr::select_if(p_mod$data_all1,is.numeric)), input$colname_t)
      updateSelectInput(session, "xaxis.ss", choices = p_mod$temp_choice)
      
    } else if(input$graph1 == "Scatter Plot") {
      shinyjs::hide(id = "zaxis.ss")
      p_mod$temp_choice <- setdiff(names(dplyr::select_if(p_mod$data_all1,is.numeric)), input$colname_t)
      updateSelectInput(session, "xaxis.ss", choices = p_mod$temp_choice)
      updateSelectInput(session, "yaxis.ss", choices = p_mod$temp_choice)
      
    } else if(input$graph1 == "Bar Plot") {
      shinyjs::hide(id = "zaxis.ss")
      p_mod$temp_choice <- setdiff(names(dplyr::select_if(p_mod$data_all1,is.numeric)), input$colname_t)
      p_mod$temp_choice_cat <- setdiff(colnames(p_mod$data_all1), c(p_mod$temp_choice, input$colname_t))
      updateSelectInput(session, "xaxis.ss", choices = p_mod$temp_choice_cat)
      updateSelectInput(session, "yaxis.ss", choices = p_mod$temp_choice)
      
    } else if (input$graph1 == "Heat Map") {
      shinyjs::show(id = "zaxis.ss")
      p_mod$temp_choice <- setdiff(names(dplyr::select_if(p_mod$data_all1,is.numeric)), input$colname_t)
      p_mod$temp_choice_cat <- setdiff(colnames(p_mod$data_all1), c(p_mod$temp_choice, input$colname_t))
      updateSelectInput(session, "xaxis.ss", choices = p_mod$temp_choice_cat)
      updateSelectInput(session, "yaxis.ss", choices = p_mod$temp_choice_cat)
      updateSelectInput(session, "zaxis.ss", choices = c("Frequency", p_mod$temp_choice))
      
    } else if (input$graph1 == "Event Rate Plot") {
      shinyjs::hide(id = "zaxis.ss")
      p_mod$temp_choice <- setdiff(colnames(p_mod$data_all1), input$colname_t)
      updateSelectInput(session, "xaxis.ss", choices = p_mod$temp_choice)
      
    }
    
  })
  
  
  observeEvent(input$hyp_bool, {
    
    if(input$hyp_bool) {
      shinyjs::show(id = "hyparmt_1")
      shinyjs::show(id = "hyparmt_2")
      shinyjs::show(id = "hyparmt_3")
      shinyjs::show(id = "data_split_sb")
      
    } else {
      shinyjs::hide(id = "hyparmt_1")
      shinyjs::hide(id = "hyparmt_2")
      shinyjs::hide(id = "hyparmt_3")
      shinyjs::hide(id = "data_split_sb")
      
    }
    
    
  })
  
  
  
  # 
  # p_mod$evnt_cnt<- get_data_z1 %>%
  #   group_by(input$colname_t ) %>%
  #   summarize(tot_pop=n(), evnt=sum(input$colname_t) , evnt_rate=paste(round(100*(evnt/sum(tot_pop)),0),"%", sep=""))
  # 
  # 
  
  #
  # 
  output$obs_ss <- renderInfoBox({
    infoBox(width = 4, color = "blue",
            "Observations ", paste(p_mod$x)
    )
  })
  
  output$vars_ss <- renderInfoBox({
    infoBox(width = 4, color = "blue",
            "Total Variables ",paste(p_mod$y)
    )
  })
  
  output$numvars <- renderInfoBox({
    infoBox(width = 4, color = "blue",
            "Numeric Variables ",paste(p_mod$z1_mod1)
    )
  })
  
  output$charvars <- renderInfoBox({
    infoBox(width = 4, color = "blue",
            "Character Variables : ",paste(p_mod$z2_mod1)
    )
  })
  
  output$eventdist <- renderInfoBox({
    infoBox(width = 4, color = "blue",
            "Event Distribution  ",paste0(round(p_mod$ev_rate*100,2),"%")
    )
  })
  
  # varx1 <- reactive({
  #   varx1 <- input$xaxis.sd1
  # })
  #
  # vary1 <- reactive({
  #   vary1 <- input$yaxis.sd1
  # })
  
  
  
  
  
  observeEvent(input$action1, {
    
    
    if(input$analysis == "Bivariate") {
      
      if(input$graph1 == "Scatter Plot") {
        
        plot_fin1 <-plot_ly(alpha = 0.6, x= p_mod$data_all1[,input$xaxis.ss],y= p_mod$data_all1[,input$yaxis.ss]) %>% 
          layout(title = 'Scatter Plot',xaxis=list(title=input$xaxis.ss),
                 yaxis=list(title=input$yaxis.ss))
        
      } else if(input$graph1 == "Bar Plot") {
        
        temp_var <- c(input$xaxis.ss, input$yaxis.ss)
        temp_data <- p_mod$data_all1[, temp_var]
        
        temp_results <- temp_data %>%
          mutate(x_var = p_mod$data_all1[,input$xaxis.ss]) %>%
          mutate(y_var = p_mod$data_all1[,input$yaxis.ss]) %>%
          group_by(x_var) %>%
          summarise(med=median(y_var, na.rm = TRUE))
        
        
        plot_fin1 <-plot_ly(alpha = 0.6, x= temp_results$x_var,y= temp_results$med, type = "bar") %>% 
          layout(title = 'Median Bar Plot',xaxis=list(title=input$xaxis.ss),
                 yaxis=list(title=input$yaxis.ss))
        
        
      } else if(input$graph1 == "Heat Map") {
        
        if(input$zaxis.ss == "Frequency") {
          
          temp_var <- c(input$xaxis.ss, input$yaxis.ss)
          temp_data <- p_mod$data_all1[, temp_var]
          
          temp_results <- temp_data %>%
            mutate(x_var = p_mod$data_all1[,input$xaxis.ss]) %>%
            mutate(y_var = p_mod$data_all1[,input$yaxis.ss]) %>%
            group_by(x_var, y_var) %>%
            summarise(med = dplyr::n()) %>%
            spread(y_var, med) %>%
            ungroup
          
          temp_results2 <- temp_results
          row.names(temp_results) <- temp_results$x_var
          temp_results <- temp_results[,!(names(temp_results) %in% c("x_var"))]
          temp_mat <- as.matrix(temp_results)
          
          plot_fin1 <- plot_ly(x = temp_results2$x_var, y = colnames(temp_mat), z = temp_mat, 
                               type = "heatmap") %>%
            layout(title = 'Frequency', xaxis=list(title=input$xaxis.ss, showgrid = F, zeroline = F), 
                   yaxis=list(title=input$yaxis.ss, showgrid = F, zeroline = F))
          
          
        } else {
          
          temp_var <- c(input$xaxis.ss, input$yaxis.ss, input$zaxis.ss)
          temp_data <- p_mod$data_all1[, temp_var]
          
          temp_results <- temp_data %>%
            mutate(x_var = p_mod$data_all1[,input$xaxis.ss]) %>%
            mutate(y_var = p_mod$data_all1[,input$yaxis.ss]) %>%
            mutate(z_var = p_mod$data_all1[,input$zaxis.ss]) %>%
            group_by(x_var, y_var) %>%
            summarise(med = median(z_var, na.rm = TRUE)) %>%
            spread(y_var, med) %>%
            ungroup
          
          
          temp_results2 <- temp_results
          row.names(temp_results) <- temp_results$x_var
          temp_results <- temp_results[,!(names(temp_results) %in% c("x_var"))]
          temp_mat <- as.matrix(temp_results)
          
          plot_fin1 <- plot_ly(x = temp_results2$x_var, y = colnames(temp_mat), z = temp_mat, 
                               type = "heatmap") %>%
            layout(title = paste0("Median ",input$zaxis.ss), xaxis=list(title=input$xaxis.ss, showgrid = F, zeroline = F), 
                   yaxis=list(title=input$yaxis.ss, showgrid = F, zeroline = F))
          
        }
      }
      
    } else if(input$analysis == "Univariate") {
      
      if(input$graph1 == "Histogram") {
        plot_fin1 <-plot_ly(alpha = 0.6, x= p_mod$data_all1[,input$xaxis.ss], type = "histogram") %>% 
          layout(title = 'Histogram Plot',xaxis=list(title=input$xaxis.ss), 
                 yaxis=list(title='Frequency'))
        
      } else if(input$graph1 == "Box Plot") {
        plot_fin1 <-plot_ly(alpha = 0.6, boxpoints = 'all') %>% 
          add_boxplot(y= p_mod$data_all1[,input$xaxis.ss], name=paste0(input$xaxis.ss)
                      # marker = list(color = 'rgba(255, 182, 193, .9)'),
                      # line = list(color = 'rgba(255, 182, 193, .9)')
          ) %>% 
          layout(title = 'Box Plot')
        
      } else if(input$graph1 == "Density Plot"){
        density <- density(p_mod$data_all1[,input$xaxis.ss])
        
        plot_fin1 <- plot_ly(x = ~density$x, y = ~density$y, type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
          layout(title = ' Density Plot', xaxis = list(title = input$xaxis.ss),
                 yaxis = list(title = 'Density'))
        
      }
      
    } else if(input$analysis == "Event Rate") {
      
      if(input$graph1 == "Event Rate Plot") {
        
        if(is.numeric(p_mod$data_all1[,input$xaxis.ss])) {
          
          temp_results <- p_mod$data_all1 %>%
            mutate(Bins = cut(p_mod$data_all1[,input$xaxis.ss], breaks = 10)) %>%
            mutate(Event_Ratex = ifelse(p_mod$data_all1[,input$colname_t]==input$evt_val_ss, 1, 0)) %>%
            group_by(Bins) %>%
            summarise(Event_Rate = sum(Event_Ratex, na.rm = T)/dplyr::n())
          
          plot_fin1 <-plot_ly(alpha = 0.6, x= temp_results$Bins,y= temp_results$Event_Rate, type = "bar") %>% 
            layout(title = 'Event Rate Plot', xaxis=list(title=input$xaxis.ss, tickangle = 270),
                   yaxis=list(title="Event Rate"))
          
        } else {
          
          temp_results <- p_mod$data_all1 %>%
            mutate(Bins = p_mod$data_all1[,input$xaxis.ss]) %>%
            mutate(Event_Ratex = ifelse(p_mod$data_all1[,input$colname_t]==input$evt_val_ss, 1, 0)) %>%
            group_by(Bins) %>%
            summarise(Event_Rate = sum(Event_Ratex, na.rm = T)/dplyr::n())
          
          plot_fin1 <-plot_ly(alpha = 0.6, x= temp_results$Bins,y= temp_results$Event_Rate, type = "bar") %>% 
            layout(title = 'Event Rate Plot', xaxis=list(title=input$xaxis.ss, tickangle = 270),
                   yaxis=list(title="Event Rate"))
          
          
        }
        
        
      }
      
    }
    
    p_mod$plot_fin1 <- plot_fin1
    
  })
  
  
  
  output$myplot.ss <- renderPlotly({
    
    p_mod$plot_fin1
    
  })
  
  
  
  observeEvent(input$action3, {
    
    
    if(length(input$col_drp_ss)>0) {
      df <- p_mod$data_all1[, !(names(p_mod$data_all1) %in% input$col_drp_ss)]
    } else {
      df <- p_mod$data_all1
    }
    
    
    dfx <- df %>%
      mutate(Event_sb = df[,input$colname_t])  
    
    p_mod$model_df <- dfx
    
    
    # train test splitsplit
    set.seed(42)
    sample <- sample.int(n = nrow(p_mod$model_df), 
                         size = floor(input$data_split_sb*nrow(p_mod$model_df)/100), 
                         replace = F)
    train <- p_mod$model_df[sample, !(names(p_mod$model_df) %in% input$colname_t)]
    test  <- p_mod$model_df[-sample, !(names(p_mod$model_df) %in% input$colname_t)]
    
    
    
    if(input$modltyp == 2) {
      
      ############### Hyper Parameters ###################
      
      if(nchar(input$hyparmt_1) > 0) {
        maxdepth_i <- as.numeric(trimws(unlist(strsplit(input$hyparmt_1,","))))
        
      } else {
        maxdepth_i <- c(4, 6 ,8)
        
      }
      
      if(nchar(input$hyparmt_2) > 0) {
        minsplit_i <- as.numeric(trimws(unlist(strsplit(input$hyparmt_2,","))))
        
      } else {
        minsplit_i <- c(100, 200, 400)
        
      }
      
      if(nchar(input$hyparmt_3) > 0) {
        weights_i <- as.numeric(trimws(unlist(strsplit(input$hyparmt_3,","))))
        
      } else {
        weights_i <- c(0.4, 0.2, 0.6)
        
      }
      
      
      hyper_grid <- expand.grid(
        minsplit = minsplit_i,
        maxdepth = maxdepth_i,
        weights = weights_i
      )
      
      
      ######## Models ###########
      
      models <- list()
      for (i in 1:nrow(hyper_grid)) {
        
        # get minsplit, maxdepth values at row i
        minsplit <- hyper_grid$minsplit[i]
        maxdepth <- hyper_grid$maxdepth[i]
        weights  <- hyper_grid$weights[i]
        
        # train a model and store in the list
        models[[i]] <- rpart(
          formula = Event_sb ~ .,
          data    = train,
          method  = "class",
          parms = list(split = "information"),
          control = list(minsplit = minsplit, maxdepth = maxdepth, weights=weights)
        )
      }
      
      
      # function to get optimal cp
      get_cp <- function(x) {
        min    <- which.min(x$cptable[, "xerror"])
        cp <- x$cptable[min, "CP"]
      }
      
      # function to get minimum error
      get_min_error <- function(x) {
        min    <- which.min(x$cptable[, "xerror"])
        xerror <- x$cptable[min, "xerror"]
      }
      
      temp_1 <- hyper_grid %>%
        mutate(
          cp    = purrr::map_dbl(models, get_cp),
          error = purrr::map_dbl(models, get_min_error)
        ) %>%
        arrange(error) %>%
        top_n(-1, wt = error)
      temp_1 <- temp_1[1,]
      
      
      optimal_tree <- rpart(
        formula = Event_sb ~ .,
        data    = train,
        method  = "class",
        parms = list(split = "information"),
        control = list(minsplit = temp_1$minsplit, maxdepth = temp_1$maxdepth, 
                       cp = temp_1$cp, weights = temp_1$weights)
      )
      
      optimal_tree <- rpart(Event_sb ~ ., data = train)
      p_mod$optimal_tree <- optimal_tree
      
      # rpart.rules(optimal_tree, cover = TRUE)
      # class(rpart.rules(optimal_tree,cover=TRUE))
      
      
      if (length(unique(p_mod$model_df[,input$colname_t]))==3){
        
        # Predicting the classifier on train
        
        y_pred_train = predict(object=optimal_tree, newdata = train, type = 'class')
        train_t<-table(train$Event_sb,y_pred_train)
        result_train<-confusionMatrix(train_t)
        Confusion_metrc_train<-data.frame(result_train$byClass)
        Confusion_metrc_trainn <- as.data.frame(t(Confusion_metrc_train))
        Confusion_metrc_trainn$KPI <- rownames(Confusion_metrc_trainn)
        Confusion_metrc_trainn <- Confusion_metrc_trainn[c(4,1,2,3)]
        row.names(Confusion_metrc_trainn) <- seq(1:nrow(Confusion_metrc_trainn))
        names(Confusion_metrc_trainn)[2:4]<- c("Train_Yes", "Train_No", "Train_Indeterminate")
        
        
        #Predicting the Test set results
        
        y_pred = predict(object=optimal_tree, newdata = test, type = 'class')
        test_t <- table(test$Event_sb,y_pred)
        result_test<-confusionMatrix(test_t)
        Confusion_metrc_test<-data.frame(result_test$byClass)
        Confusion_metrc_testt <- as.data.frame(t(Confusion_metrc_test))
        Confusion_metrc_testt$KPI <- rownames(Confusion_metrc_testt)
        Confusion_metrc_testt <- Confusion_metrc_testt[c(4,1,2,3)]
        row.names(Confusion_metrc_testt) <- seq(1:nrow(Confusion_metrc_testt))
        names(Confusion_metrc_testt)[2:4]<- c("Test_Yes", "Test_No", "Test_Indeterminate")
        
        
        # Combining Predictions
        Confusion_matrix<-merge.data.frame(Confusion_metrc_trainn, Confusion_metrc_testt, bycol=KPI)
        
        
        round_df <- function(x, digits) {
          # round all numeric variables
          # x: data frame 
          # digits: number of digits to round
          numeric_columns <- sapply(x, mode) == 'numeric'
          x[numeric_columns] <-  round(x[numeric_columns], digits)
          
          return(x)
        }
        
        Confusion_matrix <- round_df(Confusion_matrix, 3)
        
        p_mod$model_conf_mat <- Confusion_matrix
        
        
        # Generating rules from train
        frm     <- optimal_tree$frame
        names   <- row.names(frm)
        ylevels <- attr(optimal_tree, "ylevels")
        ds.size <- optimal_tree$frame[1,]$n
        frm$outcome<- ylevels[frm[1:nrow(frm),]$yval]
        frm$coverage<-frm[1:nrow(frm),]$n
        # frm$coverage_per<- cat(sprintf("%.0f%%",round(100*frm[1:nrow(frm),]$n/ds.size)))
        frm$coverage_per<- round(100*frm[1:nrow(frm),]$n/ds.size,1)
        # frm$hit_rate<-frm$yval
        frm$hit_rate<-frm[1:nrow(frm),]$yval2[,5]
        frm$non_eventrt<-frm[1:nrow(frm),]$yval2[,6]
        frm$indetrt<-frm[1:nrow(frm),]$yval2[,7]
        frm$Event_vol<-round(frm$hit_rate*frm$coverage)
        frm$NonEvent_vol<-round(frm$non_eventrt*frm$coverage)
        frm$Indeterminate_vol<-round(frm$indetrt*frm$coverage)
        frm$terminalnode <- ifelse(frm$var=="<leaf>", "Terminal_node", "Other")
        frm1<-frm
        pth <- path.rpart(optimal_tree, nodes=as.numeric(names[1:nrow(frm)]), print.it=FALSE)
        zz<-list(pth)
        frm1$rule<-zz[[1]]
        final_table<-frm1
        final_table$rule <- gsub("\"|\\)|\\(","", final_table$rule)
        final_table$rule <- gsub("croot,","", final_table$rule)
        # final_table2 = data.frame(lapply(final_table, as.character), stringsAsFactors=FALSE)
        
        p_mod$model_final_table <- final_table
        
        
        Terminal_node_dt<-final_table %>%
          filter(terminalnode=="Terminal_node") %>%
          arrange(desc(hit_rate))
        
        Terminal_node_dt$cum_event<-cumsum(Terminal_node_dt$Event_vol)
        Terminal_node_dt$cum_nonevent<-cumsum(Terminal_node_dt$NonEvent_vol)
        Terminal_node_dt$cum_indeterminate<-cumsum(Terminal_node_dt$Indeterminate_vol)

        # Terminal_node_dt$tot_pop<-sum(Terminal_node_dt$Event_vol, Terminal_node_dt$NonEvent_vol,Terminal_node_dt$Indeterminate_vol)
        Terminal_node_dt$base_event_rt<-sum(Terminal_node_dt$Event_vol)/ ds.size
        Terminal_node_dt$gain= (Terminal_node_dt$hit_rate/Terminal_node_dt$base_event_rt)
        
        Terminal_node_dt<-Terminal_node_dt %>%
          mutate(event_perc=Event_vol/sum(Event_vol))%>%
          mutate(nonevent_perc=NonEvent_vol/sum(NonEvent_vol))%>%
          mutate(indeterminate_perc=Indeterminate_vol/sum(Indeterminate_vol))%>%
          mutate(cumm_event_perc=(cum_event/last(cum_event)))%>%
          mutate(cumm_nonevent_perc=(cum_nonevent/last(cum_nonevent))) %>%
          mutate(cumm_indeteminate_perc=(cum_indeterminate/last(cum_indeterminate))) %>%
          mutate(KS= (cumm_event_perc-cumm_nonevent_perc))
        
        p_mod$Terminal_node <- Terminal_node_dt
      
      } else if (length(unique(p_mod$model_df[,input$colname_t]))==2){
        
        # Predicting the classifier on train
        y_pred_train = predict(object=optimal_tree, newdata = train, type = 'class')
        train_t<-table(train$Event,y_pred_train)
        result_train<-confusionMatrix(train_t)
        Confusion_metrc_train<-data.frame(Value_train = result_train$byClass)
        Confusion_metrc_train$KPI <- rownames(Confusion_metrc_train)
        row.names(Confusion_metrc_train) <- seq(1:nrow(Confusion_metrc_train))
        Confusion_metrc_train <- Confusion_metrc_train[c(2,1)]
        
        
        #Predicting the Test set results
        y_pred = predict(object=optimal_tree, newdata = test, type = 'class')
        test_t <- table(test$Event,y_pred)
        result_test<-confusionMatrix(test_t)
        Confusion_metrc_test<-data.frame(Value_test = result_test$byClass)
        Confusion_metrc_test$KPI <- rownames(Confusion_metrc_test)
        row.names(Confusion_metrc_test) <- seq(1:nrow(Confusion_metrc_test))
        Confusion_metrc_test <- Confusion_metrc_test[c(2,1)]
        
        # Combining Predictions
        Confusion_matrix<-merge.data.frame(Confusion_metrc_train, Confusion_metrc_test, bycol=KPI)
        
        round_df <- function(x, digits) {
          # round all numeric variables
          # x: data frame 
          # digits: number of digits to round
          numeric_columns <- sapply(x, mode) == 'numeric'
          x[numeric_columns] <-  round(x[numeric_columns], digits)
          
          return(x)
        }
        
        Confusion_matrix <- round_df(Confusion_matrix, 3)
        
        p_mod$model_conf_mat <- Confusion_matrix
        
        
        
        # Generating rules from train
        frm     <- optimal_tree$frame
        names   <- row.names(frm)
        ylevels <- attr(optimal_tree, "ylevels")
        ds.size <- optimal_tree$frame[1,]$n
        frm$outcome<- ylevels[frm[1:nrow(frm),]$yval]
        frm$coverage<-frm[1:nrow(frm),]$n
        # frm$coverage_per<- cat(sprintf("%.0f%%",round(100*frm[1:nrow(frm),]$n/ds.size)))
        frm$coverage_per<- round(100*frm[1:nrow(frm),]$n/ds.size)
        # frm$hit_rate<-frm$yval
        frm$hit_rate<-frm[1:nrow(frm),]$yval2[,5]
        frm$Event_vol<-round(frm$hit_rate*frm$coverage)
        frm$NonEvent_vol<-frm$coverage-frm$Event_vol
        frm$terminalnode <- ifelse(frm$var=="<leaf>", "Terminal_node", "Other")
        frm1<-frm
        pth <- path.rpart(optimal_tree, nodes=as.numeric(names[1:nrow(frm)]), print.it=FALSE)
        zz<-list(pth)
        frm1$rule<-zz[[1]]
        final_table<-frm1
        final_table$rule <- gsub("\"|\\)|\\(","", final_table$rule)
        final_table$rule <- gsub("croot,","", final_table$rule)
        
        p_mod$model_final_table <- final_table
        
        Terminal_node_dt<-final_table %>%
          filter(terminalnode=="Terminal_node") %>%
          arrange(desc(hit_rate))
        
        Terminal_node_dt$cum_event<-cumsum(Terminal_node_dt$Event_vol)
        Terminal_node_dt$cum_nonevent<-cumsum(Terminal_node_dt$NonEvent_vol)
        # Terminal_node_dt$tot_pop<-sum(Terminal_node_dt$Event_vol, Terminal_node_dt$NonEvent_vol,Terminal_node_dt$Indeterminate_vol)
        Terminal_node_dt$base_event_rt<-sum(Terminal_node_dt$Event_vol)/ ds.size
        Terminal_node_dt$gain= (Terminal_node_dt$hit_rate/Terminal_node_dt$base_event_rt)
        
        Terminal_node_dt<-Terminal_node_dt %>%
          mutate(event_perc=Event_vol/sum(Event_vol))%>%
          mutate(nonevent_perc=NonEvent_vol/sum(NonEvent_vol))%>%
          mutate(cumm_event_perc=(cum_event/last(cum_event)))%>%
          mutate(cumm_nonevent_perc=(cum_nonevent/last(cum_nonevent))) %>%
          mutate(KS= (cumm_event_perc-cumm_nonevent_perc))
        
        p_mod$Terminal_node <- Terminal_node_dt
        
        
        
        
        }
      
      


      
    }
    
    
  })
  
  
  output$model_plot <- renderPlot({
    
    rpart.plot(p_mod$optimal_tree, type = 4, extra = 109, clip.right.labs = F, 
               roundint=F, under = T, tweak = 1.5, branch = 1)
    
  })
  
  
  
  output$conf_mat <- DT::renderDataTable({
    
    datatable(p_mod$model_conf_mat, rownames= FALSE)
    
  },
  
  options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    scrollY = TRUE,
    #columnDefs = list(list(width = '50%', targets = list(3))),
    pageLength = 10,
    dom = 'tip'
  ),
  
  rownames= FALSE)
  
  
  
  
  
  output$model_rules <- DT::renderDataTable({
    
    
    temp <- p_mod$Terminal_node[, c("rule", "coverage_per", "hit_rate", "event_perc","cumm_event_perc","KS","gain")]
    names(temp) <- c("Persona", "Coverage percent", "Event Rate", "Event Share","Cumulative Event Share","KS","Gain")
    
    round_df <- function(x, digits) {
      # round all numeric variables
      # x: data frame 
      # digits: number of digits to round
      numeric_columns <- sapply(x, mode) == 'numeric'
      x[numeric_columns] <-  round(x[numeric_columns], digits)
      
      return(x)
    }
    
    temp <- round_df(temp, 3)
    
    datatable(temp, rownames= FALSE)
    
  },
  
  options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    scrollY = TRUE,
    #columnDefs = list(list(width = '50%', targets = list(3))),
    pageLength = 10,
    dom = 'tip'
  ),
  
  rownames= FALSE)
  
  
  
  
  
  
  
  
  ######################### Unsupervised Learning #########################
  
  
  observeEvent(input$drp_var_sb, {
    
    if(length(input$col_drp_sb)>0) {
      df <- p_mod$data_all1[, !(names(p_mod$data_all1) %in% input$col_drp_sb)]
    } else {
      df <- p_mod$data_all1
    }
    
    # df<-df[complete.cases(df),]
    # df<-na.omit(df)
    # df<-remove_empty(df, which = c("rows", "cols"))
    # df <- df[!apply(is.na(df) | df == " ", 1, all),]
    
    # samp_col_idxs  <- sample(ncol(df), 15)
    # df_samp<-df[,samp_col_idxs]
    # p_mod$clus_df <- df_samp
    p_mod$clus_df <- df
    p_mod$clus_df[sapply(p_mod$clus_df, is.character)] <- lapply(p_mod$clus_df[sapply(p_mod$clus_df, is.character)], as.factor)
    #' Compute Gower distance
    gower_dist <- daisy(p_mod$clus_df, metric = "gower")
    gower_mat <- as.matrix(gower_dist)
    
    #' Print most similar clients
    # df[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]
    
    #' Print most dissimilar clients
    # df[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]
    
    # Optimal number of clusters
    sil_width <- c(NA)
    for(i in 2:10){  
      pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
      sil_width[i] <- pam_fit$silinfo$avg.width  
    }
    
    p_mod$sil_width <- sil_width
    p_mod$sug_nclus <- which(sil_width == max(sil_width, na.rm = T))
    
    p_mod$gower_dist <- gower_dist
    
    updateSelectInput(session, "nclus_sb", choices=c(seq(2, 10)), selected = p_mod$sug_nclus)
    
  })
  
  
  output$silhot_sb <- renderPlot({
    
    if(!is.null(p_mod$clus_df)) {
      
      plot(1:10, p_mod$sil_width,
           main = "Silhoute Plot for Optimal Cluster Count",
           xlab = "Number of clusters",
           ylab = "Silhouette Width")
      lines(1:10, p_mod$sil_width)
    } else {
      return(NULL)
    }
    
  })
  
  
  
  
  observeEvent(input$clus_ac_sb, {
    
    # Summary of each cluster
    
    k <- input$nclus_sb
    set.seed(42)
    pam_fit <- pam(p_mod$gower_dist, diss = TRUE, k)
    
    p_mod$pam_fit <- pam_fit
    
    pam_results <- p_mod$clus_df %>%
      mutate(cluster = pam_fit$clustering) %>%
      group_by(cluster) %>%
      do(the_summary = summary(.))
    pam_results$the_summary
    
    
    
    # Cluster Rule Generation
    
    pam_result_all <- p_mod$clus_df %>%
      mutate(cluster = pam_fit$clustering) %>%
      group_by(cluster) %>%
      summarise(count=dplyr::n())
    
    # str(df)
    nums <- unlist(lapply(p_mod$clus_df, is.numeric)) 
    df_num<-p_mod$clus_df[ , nums]
    # str(df_num)
    df_num[1:ncol(df_num)] <- lapply(df_num[1:ncol(df_num)], as.numeric)
    
    
    pam_results_num <- df_num %>%
      mutate(cluster = pam_fit$clustering) %>%
      group_by(cluster) %>%
      summarise_all("median")
    
    
    is.fact <- sapply(p_mod$clus_df, is.factor)
    df_cat <- p_mod$clus_df[, is.fact]
    
    dmy <- dummyVars(" ~ .", data = df_cat)
    df1 <- data.frame(predict(dmy, newdata = df_cat))
    
    
    pam_results_cat <- df1 %>%
      mutate(cluster = pam_fit$clustering) %>%
      group_by(cluster) %>%
      summarise_all("sum")
    
    
    pam_cluster<-merge(pam_result_all,pam_results_num, bycol="cluster")
    pam_cluster<-merge(pam_cluster,pam_results_cat, bycol="cluster") 
    pam_cluster<-pam_cluster %>%
      mutate(cluster_percentage=round(100*count/sum(count),0))
    pam_cluster<- pam_cluster[, !(names(pam_cluster) %in% c("count"))]
    pam_cluster <- pam_cluster %>%
      select(cluster,cluster_percentage, everything())
    
    aa <- as.data.frame(t(pam_cluster))
    colnames(aa) <- paste0(rep("Cluster"), "-",1:ncol(aa))
    aa$KPI <- row.names(aa)
    aa1 <- aa[row.names(aa)!="cluster",c(ncol(aa), seq(1,ncol(aa)-1))]
    rownames(aa1) <- NULL
    
    p_mod$fin_clus_rule <- aa1
    
    
  })
  
  
  output$tsne_sb <- renderPlotly({
    
    pal_sb1 <- brewer.pal(n = 9, name = "Dark2")
    pal_sb2 <- brewer.pal(n = 12, name = "Set1")
    pal_sb <- c(pal_sb1, pal_sb2)
    
    # Visualising in a low dimentional spacesetPackageName(
    set.seed(42)
    tsne_obj <- Rtsne(p_mod$gower_dist, is_distance = TRUE)
    
    tsne_data <- tsne_obj$Y %>%
      data.frame() %>%
      setNames(c("X", "Y")) %>%
      mutate(cluster = paste0("Cluster-", factor(p_mod$pam_fit$clustering)))
    
    p <- plot_ly(x = tsne_data$X, y = tsne_data$Y, 
                 color = tsne_data$cluster,
                 colors = pal_sb) %>%
      layout(title = 'tSNE Plot', xaxis = list(title = "tSNE X-Component"),
             yaxis = list(title = 'tSNE Y-Component'))
    
    return(p)
    
  })
  
  
  
  output$clus_rules_sb <- DT::renderDataTable({
    
    datatable(p_mod$fin_clus_rule, rownames= FALSE)
    
  },
  
  options = list(
    autoWidth = FALSE,
    scrollX = TRUE,
    scrollY = TRUE,
    #columnDefs = list(list(width = '50%', targets = list(3))),
    pageLength = 10,
    dom = 'tip'
  ),
  
  rownames= FALSE)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}

shinyApp(ui,server)
