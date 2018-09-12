
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(plotly)
library(plyr)
library(dplyr)
library(shinyjs)
library(DT)
source("helper_functions.R")

# Define Database logic required to draw a histogram
server <- function(input, output,session) {
  #disable sidebar
  options(warn = -1)
  
  addClass(selector = "body", class = "sidebar-collapse")
  output$dropdown=renderMenu({dropdownMenu(type="notifications",icon=icon("question-circle", "fa-2x"),
                                           badgeStatus = NULL,headerText="Help",get_noti(),get_noti2())})
  observeEvent(input$linkClicked,{
   
    if(grepl("Tutorial",input$linkClicked)){
      updateTabItems(session,"tabs",selected = "tutorial")
      output$dropdown=renderMenu({dropdownMenu(type="notifications",icon=icon("question-circle", "fa-2x"),
                                               badgeStatus = NULL,headerText="Help",get_noti(),get_noti2())})
    }
    else{
      updateTabItems(session,"tabs",selected = "help")
      output$dropdown=renderMenu({dropdownMenu(type="notifications",icon=icon("question-circle", "fa-2x"),
                                               badgeStatus = NULL,headerText="Help",get_noti(),get_noti2())})
    }
  })
  
  ddc<-reactiveValues(
    department=NULL,
    discipline=NULL,
    course=NULL
  )
  
  counter <- reactiveValues(n = 0,rendered=0)#for dynamically adding day and time
  day_1_created<-reactiveVal(value=FALSE)
  day_2_created<-reactiveVal(value=FALSE)
  limit_semesters<-reactiveValues(available_semesters=c("Fall","Winter","Spring","Summer"),
                                 year_affected=paste0((strtoi(format(Sys.Date(), "%Y")))-1,"-",(strtoi(format(Sys.Date(), "%Y")))))
  
  
  #table reactive values
  enrollment_tab<-reactiveValues(
    age=NULL,
    gender=NULL,
    probation=NULL,
    ethnicity=NULL,
    student_groups=NULL,
    major=NULL
  )
  retention_tab<-reactiveValues(
    age=NULL,
    gender=NULL,
    probation=NULL,
    ethnicity=NULL,
    student_groups=NULL,
    major=NULL
  )
  success_tab<-reactiveValues(
   
  )
  
  
  #graphs reactive values
  head_count_graph<-reactiveValues(
     age=NULL,gender=NULL,probation=NULL,ethnicity=NULL,student_groups=NULL,major=NULL
  )
  
  ethnicity_graph_rv <- reactiveValues(
    enrollment = NULL,success=NULL,retention=NULL
  )
  age_graph_rv <- reactiveValues(
    enrollment = NULL,success=NULL,retention=NULL
  )
  gender_graph_rv <- reactiveValues(
    enrollment = NULL,success=NULL,retention=NULL
  )
  ap_graph_rv<-reactiveValues(
    enrollment = NULL,success=NULL,retention=NULL
  )
  student_groups_graph_rv<-reactiveValues(
    enrollment = NULL,success=NULL,retention=NULL
  )
  major_graph_rv<-reactiveValues(
    enrollment = NULL,success=NULL,retention=NULL
  )
  
  #---COMPARISON TAB---
  comparison_graph<-reactiveValues(
    enrollment = NULL,success=NULL,retention=NULL,equity=NULL
  )
  comparison_table<-reactiveValues(
    enrollment = NULL,success=NULL,retention=NULL,equity=NULL
  )
  #----Equity Tab-------
  equity_index_graph<-reactiveValues(
    age=NULL, gender=NULL, probation=NULL,
    ethnicity=NULL, student_groups=NULL,major=NULL
  )
  equity_index_graph_standard<-reactiveValues(
    age=NULL, gender=NULL, probation=NULL,
    ethnicity=NULL, student_groups=NULL,major=NULL
  )
  equity_index_table<-reactiveValues(
    age=NULL, gender=NULL, probation=NULL,
    ethnicity=NULL, student_groups=NULL,major=NULL
  )
  
  #data
  data<-reactiveVal(NULL)
  data_comparison<-reactiveVal(NULL)
  years_available<-reactiveVal(NULL)
  year_chosen<-reactiveVal(NULL)
  
  #used to clear all checkboxes with clear button under student tab
  observe(if (input$clear_button > 0) {
    if(input$category=="Ethnicity"){
      shinyjs::show("include_gender")    
     
      output$title_main<-renderUI(h3("Ethnicity"))
      updateCheckboxGroupInput(session=session,inputId = "category_dis", label = NULL,
                               c("Asian",
                                 "Native American",
                                 "White",
                                 "Hispanic/Latino", 
                                 "Black/African American",
                                 "Hawaiian/Pacific Islander",
                                 "Two or more races",
                                 "Unknown"))
      updateCheckboxGroupInput(session=session,inputId="gender_chkbx",label=NULL,choices=c("Female","Male"))
    }
    else if(input$category=="Age"){
     
      output$title_main<-renderUI(h3("Age Range"))
      updateCheckboxGroupInput(session=session,inputId = "category_dis", 
                               label=NULL,c("Under 18","18-19", "20-24","25-29", "30-39","40 & older"))
    }
    else if(input$category=="Gender"){
      
      output$title_main<-renderUI(h3("Gender"))
      updateCheckboxGroupInput(session=session,inputId = "category_dis", 
                               label=NULL,choices=c("Female","Male"))
    }
    else if(input$category=="Student Groups"){
     
      output$title_main<-renderUI(h3("Student Groups"))
      updateCheckboxGroupInput(session=session,inputId = "category_dis", 
                               label=NULL,choices=c("EOPS","DSPS","CalWorks",
                                                    "Veteran","Military","BOGG","Pell Grant"))
    }
    else if(input$category=="Major"){
     
      output$title_main<-renderUI(h3("Major"))
      updateCheckboxGroupInput(session=session,inputId = "category_dis", 
                               label=NULL,choices=c("Undeclared","Declared"))
    }
    else if(input$category=="Academic Probation"){
     
      output$title_main<-renderUI(h3("Probation"))
      updateCheckboxGroupInput(session=session,inputId = "category_dis", 
                               label=NULL,choices=c("Yes","No"))
    }
  })
  
  #used to clear all checkboxes with clear button under course tab
  observe(if (input$clear_button_c > 0) {
    if(input$dept!="ALL"){
      choices=isolate(update_course_tab(department=input$dept,discipline=input$dis,course=input$course,filter="class type"))
      
      #class_type update
      if(!is.null(choices) || length(choices)!=0){
        updateCheckboxGroupInput(session,inputId="class_type_chkbx",label=NULL,
                                 choices=choices)}
      else{
        updateCheckboxGroupInput(session,inputId="class_type_chkbx",label=NULL,
                                 choices=NULL)
        output$class_type<-renderText("No options available")}
    }
    
    #location update
    updateCheckboxGroupInput(session,inputId="location_chkbx",label=NULL,
                             choices=isolate(update_course_tab(department=input$dept,discipline=input$dis,course=input$course,filter="location")))
    
    #day update
    days_available=update_course_tab(department=input$dept,discipline=input$dis,course=input$course,filter="days")
    updateSelectInput(session,inputId="day_static",label="Day",choices=days_available)
    
    
    for( i in 1:counter$rendered){
      updateSelectInput(session,inputId=paste0("day_dynamic_",i),label="Day",
                        choices=days_available)
    }
    updateSelectInput(session,inputId="time_dynamic_1",label="Time",
                      choices=c("Select Time"))
    updateSelectInput(session,inputId="time_dynamic_2",label="Time",
                      choices=c("Select Time"))
    updateSelectInput(session,inputId="time1",label="Time",
                      choices=c("Select Time"))
  
    
    
  })
  
  observeEvent(input$go_main,{
    updateTabItems(session, "tabs","main")
  })
  observeEvent(input$tutorial_go_main,{
    updateTabItems(session, "tabs","main")
  })
  
  observeEvent(input$view,{
    if(input$view=="View Overall Trends (Enrollment, Retention, & Success)"){
      shinyjs::hide("ethnicity")
      shinyjs::hide("category")
    }else{
      shinyjs::show("ethnicity")
      shinyjs::show("category")
    }
  })

  #when a new department is chosen a new discipline dropdown will appear
  observe({

    if(isolate(input$year)==isolate(limit_semesters$year_affected) ){
      updateSelectInput(session,'dept',label=NULL,get_departments(year=input$year,semester=limit_semesters$available_semesters),selected="ALL")
    }
    else{

       updateSelectInput(session,'dept',label=NULL,choices = get_departments(input$year,input$semester),selected="ALL")
    }
    
    
  })
  
  #update discipline 
  observe({
    updateSelectInput(session, 'dis',label=NULL, choices=get_disciplines(input$dept),selected="ALL")
  })
  
  #when a new disciplinr is chosen new courses dropdown will appear
  observe({
    updateSelectInput(session, 'course',label=NULL, choices=get_courses(input$dis),selected="ALL")
  })
  
  #updating uption in Historical Comparison->Courses
  observe({
    
    if(input$stud_course=="Historical Comparison"){
           choices=update_course_tab(department=input$dept,discipline=input$dis,course=input$course,filter="class type")
      
          #class_type update
          if(!is.null(choices) || length(choices)!=0){
            updateCheckboxGroupInput(session,inputId="class_type_chkbx",label=NULL,
                                     choices=choices)}
          else{
            updateCheckboxGroupInput(session,inputId="class_type_chkbx",label=NULL,
                                     choices=NULL)
            output$class_type<-renderText("No options available")}
        
        
        #location update
        updateCheckboxGroupInput(session,inputId="location_chkbx",label=NULL,
                                 choices=update_course_tab(department=input$dept,discipline=input$dis,course=input$course,filter="location"))
         
        #day update
        days_available=update_course_tab(department=input$dept,discipline=input$dis,course=input$course,filter="days")
        updateSelectInput(session,inputId="day_static",label="Day",choices=days_available)
        
    }
    
  })
  
  observe(if(input$day_static!="Select Day"){
      choices=isolate(update_course_tab(department=input$dept,discipline=input$dis,course=input$course,filter="time",day=input$day_static))
        if(choices[1]=="Online"){
          showNotification("Class only availble online. Start Time & Day are omitted",type="warning",duration=10)
          updateSelectInput(session,inputId="time1",label="Time",
                            choices=c("Select Time"))
          shinyjs::hide("day_static")
          shinyjs::hide("time1")
          shinyjs::hide("add_btn")
          if(isolate(counter$n)>=1){
            shinyjs::hide("day_dynamic_1")
            shinyjs::hide("time_dynamic_1")
          }
          if(isolate(counter$n)>=2){
            shinyjs::hide("day_dynamic_2")
            shinyjs::hide("time_dynamic_2")
          }
        }else{#other options
          shinyjs::show("class_type_chkbx")
          shinyjs::show("day_static")
          shinyjs::show("time1")
          shinyjs::show("add_btn")
          if(isolate(counter$n)>=1){
            shinyjs::show("day_dynamic_1")
            shinyjs::show("time_dynamic_1")
          }
          if(isolate(counter$n)>=2){
            shinyjs::show("day_dynamic_2")
            shinyjs::show("time_dynamic_2")
          }
           updateSelectInput(session,inputId="time1",label="Time",
                        choices=choices)
          }
      shinyjs::disable("class_type_chkbx")
      shinyjs::disable("location_chkbx")
  }
  else{
    shinyjs::enable("class_type_chkbx")
    shinyjs::enable("location_chkbx")
    if((day_1_created() && !is.null(input$day_dynamic_1) && input$day_dynamic_1!="Select Day") ||
       (day_2_created() && !is.null(input$day_dynamic_2) && input$day_dynamic_2!="Select Day")){
      shinyjs::disable("class_type_chkbx")
      shinyjs::disable("location_chkbx")
    }
      })#update time1
  
  observe(if(day_1_created() && !is.null(input$day_dynamic_1) && input$day_dynamic_1!="Select Day"){
                updateSelectInput(session,inputId="time_dynamic_1",label="Time",
                              choices=update_course_tab(department=input$dept,discipline=input$dis,course=input$course,filter="time",day=input$day_dynamic_1))
    shinyjs::disable("class_type_chkbx")
    shinyjs::disable("location_chkbx")
  }
  else{
    shinyjs::enable("class_type_chkbx")
    shinyjs::enable("location_chkbx")
    
    if((!is.null(input$day_static) && input$day_static!="Select Day") ||(!is.null(input$time1) && input$time1!="Select Time")  ){
      shinyjs::disable("class_type_chkbx")
      shinyjs::disable("location_chkbx")
    } 

  })#update time2
  observe(if(day_2_created() && !is.null(input$day_dynamic_2) && input$day_dynamic_2!="Select Day"){
            updateSelectInput(session,inputId="time_dynamic_2",label="Time",
                              choices=update_course_tab(department=input$dept,discipline=input$dis,course=input$course,filter="time",day=input$day_dynamic_2))
    shinyjs::disable("class_type_chkbx")
    shinyjs::disable("location_chkbx")
  }
  else{
    shinyjs::enable("class_type_chkbx")
    shinyjs::enable("location_chkbx")
    if((!is.null(input$day_static) && input$day_static!="Select Day") ||(!is.null(input$time1) && input$time1!="Select Time")  ){
      shinyjs::disable("class_type_chkbx")
      shinyjs::disable("location_chkbx")
    } 
      })#update time3
  
  #end of update
  
  observe(if(input$submit_button_student>0){

    years_needed=isolate(input$year)
    
    
    
    filter_type=NULL
    filter_option=NULL
    render_vt<-T
    if(isolate(input$year_to_check_s)!="1 year"){
      start=as.numeric(substr(isolate(input$year),6,9))
      years=as.numeric(substr(isolate(input$year_to_check_s),1,1))-1
      years_needed<-paste0(c((start-1):(start-years-1)),"-",
                           c((start):(start-years)))
    }
    
    withProgress(message = 'Getting Comparison Graph', value = 0, {
      n<-2
      incProgress(1/n, detail = paste("Getting data from Database "))
      #get all the data :\
      #nothing was clicked
      if(is.null(isolate(input$category_dis)) && isolate(input$view)!="View Overall Trends (Enrollment, Retention, & Success)"){
          showNotification("No sub-groups for category were chosen",type="error")
          render_vt<-F
      }
      else if(isolate(input$category_dis)=="Ethnicity" && isolate(input$include_gender)=="Yes" && isolate(is.null(input$gender_chkbx))){
        showNotification("Gender was not specified",type="error")
        render_vt<-F
      }
      else{#have something to check
        if(isolate(input$year)==isolate(limit_semesters$year_affected) && isolate(is.null(input$semester))){
          data_comparison(isolate(get_default(years_needed,limit_semesters$semesters_available,input$dept,input$dis,input$course)))
        }else{
          data_comparison(isolate(get_default(years_needed,input$semester,input$dept,input$dis,input$course)))
        }
      #---------------OVERAL TRENDS----------------------------------------------------
        if(isolate(input$view)=="View Overall Trends (Enrollment, Retention, & Success)"){
          filter_type="General Enrollments"
          
          incProgress(2/n, detail = paste("Getting calculations "))
          result=isolate(get_general_historical(enrolled=data_comparison()[[1]],dropped=data_comparison()[[2]],
                                                passed=data_comparison()[[3]],years=years_needed))
          
          
          
          #----Enrolled------
          comparison_table$enrollment<-result[,c("Academic_Year","Enrolled")]
          comparison_graph$enrollment<-create_general_historical_graph(comparison_table$enrollment,"Enrolled")
          #-----Retention------
          
          comparison_table$retention<-result[,c("Academic_Year","Enrolled","Retained","Retention_Rate")]
          comparison_graph$retention<-create_general_historical_graph(comparison_table$retention,"Retention_Rate")
          #-----Success----------
          comparison_table$success<-result[,c("Academic_Year","Enrolled","Succeeded","Success_Rate")]
          comparison_graph$success<-create_general_historical_graph(comparison_table$success,"Success_Rate")
          
          comparison_table$equity<-NULL
          comparison_graph$equity<-NULL
        } 
        #----------------------------FILTER BY CATEGORY--------------------------------
        else{
          #ethnicity+Genger
          if(isolate(input$category)=="Ethnicity" &&  isolate(input$include_gender)=="Yes" && isolate(!is.null(input$gender_chkbx))){
            #Get the calculation
            filter_type="Ethnicity"
            filter_option=isolate(input$category_dis)

            
            incProgress(2/n, detail = paste("Getting calculations "))
            result=isolate(get_ethnicity_graph(enrolled=data_comparison()[[1]],dropped=data_comparison()[[2]],
                                               passed=data_comparison()[[3]],status="filter",ethnicity=input$category_dis,years=years_needed,
                                               gender=input$gender_chkbx))
            
            adjusted_data=result#adjust_data(data=result,type="Ethnicity")
            
            #----Enrolled------
            
            comparison_table$enrollment<-result[,c("Academic_Year","Ethnicity_Gender","Enrolled")]
            
            comparison_graph$enrollment<-isolate(create_line_comparison_graph( comparison_table$enrollment,years_needed,"Ethnicity_Gender","Enrolled"))
            #-----Retention------
            
            
            comparison_table$retention<-result[,c("Academic_Year","Ethnicity_Gender","Enrolled","Retained","Retention_Rate")]
            comparison_graph$retention<-isolate(create_line_comparison_graph( comparison_table$retention,years_needed,"Ethnicity_Gender","Retention"))
            #-----Success----------
            
            comparison_table$success<-result[,c("Academic_Year","Ethnicity_Gender","Enrolled","Succeeded","Success_Rate")]
            comparison_graph$success<-isolate(create_line_comparison_graph(comparison_table$success,years_needed,"Ethnicity_Gender","Success"))
            #-----Equity------
            
            
            comparison_table$equity<-result[,c("Academic_Year","Ethnicity_Gender","Enrolled","Succeeded","Equity_Index")]
            comparison_graph$equity<-isolate(create_line_comparison_graph( comparison_table$equity,years_needed,type="Ethnicity_Gender",status = "Equity"))
          }
          else if(isolate(input$category)=="Ethnicity"){
            #Get the calculation
            filter_type="Ethnicity"
            filter_option=isolate(input$category_dis)
            
            incProgress(2/n, detail = paste("Getting calculations "))
            result=isolate(get_ethnicity_graph(enrolled=data_comparison()[[1]],dropped=data_comparison()[[2]],
                                               passed=data_comparison()[[3]],status="filter",ethnicity=input$category_dis,years=years_needed))
           
            adjusted_data=result#adjust_data(data=result,type="Ethnicity")
            
            #----Enrolled------
            
            comparison_table$enrollment<-result[,c("Academic_Year","Ethnicity","Enrolled")]
            
            comparison_graph$enrollment<-isolate(create_line_comparison_graph( comparison_table$enrollment,years_needed,"Ethnicity","Enrolled"))
            #-----Retention------
           
            
            comparison_table$retention<-result[,c("Academic_Year","Ethnicity","Enrolled","Retained","Retention_Rate")]
            comparison_graph$retention<-isolate(create_line_comparison_graph( comparison_table$retention,years_needed,"Ethnicity","Retention"))
            #-----Success----------
            
            comparison_table$success<-result[,c("Academic_Year","Ethnicity","Enrolled","Succeeded","Success_Rate")]
            comparison_graph$success<-isolate(create_line_comparison_graph(comparison_table$success,years_needed,"Ethnicity","Success"))
            #-----Equity------
            
            
            comparison_table$equity<-result[,c("Academic_Year","Ethnicity","Enrolled","Succeeded","Equity_Index")]
            comparison_graph$equity<-isolate(create_line_comparison_graph( comparison_table$equity,years_needed,type="Ethnicity",status = "Equity"))
            
          }#Ethnicity only
          else if(isolate(input$category)=="Age"){
                #Get the calculation
                filter_type="Age Range"
                filter_option=isolate(input$category_dis)
                incProgress(2/n, detail = paste("Getting calculations "))
                result=isolate(get_age_range_graph(enrolled=data_comparison()[[1]],year=years_needed,status="filter",age_range=input$category_dis))
                
                
                #----Enrolled------
                comparison_table$enrollment<-result[,c("Academic_Year","Age","Enrolled")]
                comparison_graph$enrollment<-isolate(create_line_comparison_graph( comparison_table$enrollment,years_needed,"Age","Enrolled"))
                print("Enrolled:Passed")
                #-----Retention------
               
                comparison_table$retention<-result[,c("Academic_Year","Age","Enrolled","Retained","Retention_Rate")]
                comparison_graph$retention<-isolate(create_line_comparison_graph( comparison_table$retention,years_needed,"Age","Retention"))
                print("Retention:Passed")
                #-----Success----------
              
                comparison_table$success<-result[,c("Academic_Year","Age","Enrolled","Succeeded","Success_Rate")]
                comparison_graph$success<-isolate(create_line_comparison_graph( comparison_table$success,years_needed,"Age","Success"))
                print("Success:Passed")
                #-----Equity------
               
                comparison_table$equity<-result[,c("Academic_Year","Age","Enrolled","Succeeded","Equity_Index")]
                comparison_graph$equity<-isolate(create_line_comparison_graph(comparison_table$equity,years_needed,type="Age",status = "Equity"))
                print("Equity:Passed")
              }
          else if(isolate(input$category)=="Student Groups"){
            filter_type="Student Groups"
            filter_option=isolate(input$category_dis)
            #Get the calculation
            incProgress(2/n, detail = paste("Getting calculations "))
            result=isolate(get_student_groups(enrolled=data_comparison()[[1]],dropped=data_comparison()[[2]],
                                              passed=data_comparison()[[3]],status="filter",Groups_wanted=input$category_dis, years=years_needed))
            
            
            #----Enrolled------
            comparison_table$enrollment<-result[,c("Academic_Year","Groups","Enrolled")]
            comparison_graph$enrollment<-isolate(create_line_comparison_graph(comparison_table$enrollment,years_needed,"Groups","Enrolled"))
            #-----Retention------
            comparison_table$retention<-result[,c("Academic_Year","Groups","Enrolled","Retained","Retention_Rate")]
            comparison_graph$retention<-isolate(create_line_comparison_graph(comparison_table$retention,years_needed,"Groups","Retention"))
            #-----Success----------

            comparison_table$success<-result[,c("Academic_Year","Groups","Enrolled","Succeeded","Success_Rate")]
            comparison_graph$success<-isolate(create_line_comparison_graph( comparison_table$success,years_needed,"Groups","Success"))
            #-----Equity------

            comparison_table$equity<-result[,c("Academic_Year","Groups","Enrolled","Succeeded","Equity_Index")]
            comparison_graph$equity<-isolate(create_line_comparison_graph(comparison_table$equity,years_needed,type="Groups",status = "Equity"))
            
          }
          else if(isolate(input$category)=="Academic Probation"){
            filter_type="Academic Probation"
            filter_option=isolate(input$category_dis)
            incProgress(2/n, detail = paste("Getting calculations "))
            result=isolate(get_academic_probation_students_(enrolled=data_comparison()[[1]],dropped=data_comparison()[[2]],
                                                            passed=data_comparison()[[3]],status="filter", year=years_needed, type=input$category_dis))
  
            #----Enrolled------
            comparison_table$enrollment<-result[,c("Academic_Year","Status","Enrolled")]
            comparison_graph$enrollment<-isolate(create_line_comparison_graph(comparison_table$enrollment,years_needed,"Status","Enrolled"))
            #-----Retention------
            comparison_table$retention<-result[,c("Academic_Year","Status","Enrolled","Retained","Retention_Rate")]
            comparison_graph$retention<-isolate(create_line_comparison_graph(  comparison_table$retention,years_needed,"Status","Retention"))
            #-----Success----------
            comparison_table$success<-result[,c("Academic_Year","Status","Enrolled","Succeeded","Success_Rate")]
            isolate(comparison_graph$success<-create_line_comparison_graph(comparison_table$success,years_needed,"Status","Success"))
            #-----Equity------
          
            comparison_table$equity<-result[,c("Academic_Year","Status","Enrolled","Succeeded","Equity_Index")]
            comparison_graph$equity<-isolate(create_line_comparison_graph(comparison_table$equity,years_needed,type="Status",status = "Equity"))
          }
          else if(isolate(input$category)=="Gender"){
            filter_type="Gender"
            filter_option=isolate(input$category_dis)
            incProgress(2/n, detail = paste("Getting calculations "))
            result=isolate(get_gender_graph(enrolled=data_comparison()[[1]],dropped=data_comparison()[[2]],
                                            passed=data_comparison()[[3]],status="filter",Gender=input$category_dis,years=years_needed))
            
            #----Enrolled------
            comparison_table$enrollment<-result[,c("Academic_Year","Gender","Enrolled")]
            #sensor here
            comparison_graph$enrollment<-isolate(create_line_comparison_graph(comparison_table$enrollment,years_needed,"Gender","Enrolled"))
            #-----Retention------
            comparison_table$retention<-result[,c("Academic_Year","Gender","Enrolled","Retained","Retention_Rate")]
            comparison_graph$retention<-isolate(create_line_comparison_graph(comparison_table$retention,years_needed,"Gender","Retention"))
            #-----Success----------
            comparison_table$success<-result[,c("Academic_Year","Gender","Enrolled","Succeeded","Success_Rate")]
            comparison_graph$success<-isolate(create_line_comparison_graph( comparison_table$success,years_needed,"Gender","Success"))
            #-----Equity------
            comparison_table$equity<-result[,c("Academic_Year","Gender","Enrolled","Succeeded","Equity_Index")]
            comparison_graph$equity<-isolate(create_line_comparison_graph(comparison_table$equity,years_needed,type="Gender",status = "Equity"))
          }
          else if(isolate(input$category)=="Major"){
            filter_type="Majors"
            filter_option=isolate(input$category_dis)
            incProgress(2/n, detail = paste("Getting calculations "))
            result=isolate(get_major(enrolled=data_comparison()[[1]],dropped=data_comparison()[[2]],
                                     passed=data_comparison()[[3]],status="filter",majors=input$category_dis,year = years_needed))
            #----Enrolled------
            comparison_table$enrollment<-result[,c("Academic_Year","Major","Enrolled")]
            #sensor here
            comparison_graph$enrollment<-isolate(create_line_comparison_graph(comparison_table$enrollment,years_needed,"Major","Enrolled"))
            #-----Retention------
          
            comparison_table$retention<-result[,c("Academic_Year","Major","Enrolled","Retained","Retention_Rate")]
            comparison_graph$retention<-isolate(create_line_comparison_graph(comparison_table$retention,years_needed,"Major","Retention"))
            #-----Success----------
            comparison_table$success<-result[,c("Academic_Year","Major","Enrolled","Succeeded","Success_Rate")]
            comparison_graph$success<-isolate(create_line_comparison_graph(comparison_table$success,years_needed,"Major","Success"))
            #-----Equity------
            
            comparison_table$equity<-result[,c("Academic_Year","Major","Enrolled","Succeeded","Equity_Index")]
            comparison_graph$equity<-isolate(create_line_comparison_graph(comparison_table$equity,years_needed,type="Major",status = "Equity"))
          }
        }
      }#1st(else)done with getting data
    })
    
    if( render_vt==T){
    output$filter<-renderInfoBox(
      infoBox(title="Filter",value=filter_type, icon = icon("filter"),fill=TRUE,
              color = "orange")
    )
    output$department_hc<-renderInfoBox(
      infoBox(title="Department",width=1,value=isolate(input$dept), icon = icon("bar-chart"),fill=TRUE,
              color = "black")
    )
    output$discipline_hc<-renderInfoBox(
      infoBox(title="Discipline",width=1,value=isolate(input$dis), icon = icon("bar-chart"),fill=TRUE,
              color = "black")
    )
    output$course_hc<-renderInfoBox(
      infoBox(title="Course",width=1,value=isolate(input$course), icon = icon("bar-chart"),fill=TRUE,
              color = "black")
    )
    
    showNotification("Note:Categories with less than 10 students will be censored for privacy reaons",duration=10,type="warning")}
  })
  
  observe(if(input$submit_button_courses>0){
    #Algorithm
    #1) Get data for the amount of years chooses
    years_needed=isolate(input$year)
    if(isolate(input$year_to_check_c)!="1 year"){
      start=as.numeric(substr(isolate(input$year),6,9))
      years=as.numeric(substr(isolate(input$year_to_check_c),1,1))-1
      years_needed<-paste0(c((start-1):(start-years-1)),"-",
                           c((start):(start-years)))
    }
    render_vt<-T
    filter_type=NULL
    filter_option=NULL
    
    withProgress(message = 'Getting Comparison Graph', value = 0, {
      n<-2
      incProgress(1/n, detail = paste("Getting data from Database "))
      #get all the data :\
  
      if(is.null(isolate(input$class_type_chkbx)) && is.null(isolate(input$location_chkbx)) &&
         (isolate(input$day_static)=="Select Day" || isolate(input$time1)=="Select Time")){
        if((isolate(input$day_static)=="Select Day" || isolate(input$time1)=="Select Time")){
            showNotification("Error: Not all boxes in Time and Day were filled out",type="error")}
           else{
             showNotification("Error: No filters were chosen",type="error")
           }
            render_vt<-F
      }
      else if(is.null(isolate(input$class_type_chkbx)) && is.null(isolate(input$location_chkbx)) &&
              (isolate(counter$n)>=1 && (isolate(input$time_dynamic_1)=="Select Time" || isolate(input$day_dynamic_1)=="Select Day"))){
        showNotification("Error: Not all boxes in Time and Day were filled out",type="error")
        render_vt<-F
      } 
      else if(is.null(isolate(input$class_type_chkbx)) && is.null(isolate(input$location_chkbx)) &&
              (isolate(counter$n)>=2 && (isolate(input$time_dynamic_2)=="Select Time" || isolate(input$day_dynamic_2)=="Select Day"))){
        showNotification("Error: Not all boxes in Time and Day were filled out",type="error")
        render_vt<-F
      }
      else{
        data_comparison(isolate(get_default(years_needed,input$semester,input$dept,input$dis,input$course)))
        
        if(!is.null(isolate(input$class_type_chkbx))){
          filter_type="Class_Type"
          result=isolate(get_class_type(enrolled=data_comparison()[[1]],dropped=data_comparison()[[2]],
                                        passed=data_comparison()[[3]],class_type=input$class_type_chkbx,years=years_needed))
         
          
          incProgress(2/n, detail = paste("Getting calculations "))
          #----Enrolled------
         
          comparison_table$enrollment<-result[,c("Academic_Year","Class_Type","Enrolled")]
          comparison_graph$enrollment<-isolate(create_line_comparison_graph(comparison_table$enrollment,years_needed,"Class_Type","Enrolled"))
          #-----Retention------
        
          comparison_table$retention<-result[,c("Academic_Year","Class_Type","Enrolled","Retained","Retention_Rate")]
          comparison_graph$retention<-isolate(create_line_comparison_graph(comparison_table$retention,years_needed,"Class_Type","Retention"))
          #-----Success----------
         
          comparison_table$success<-result[,c("Academic_Year","Class_Type","Enrolled","Succeeded","Success_Rate")]
          comparison_graph$success<-isolate(create_line_comparison_graph(comparison_table$success,years_needed,"Class_Type","Success"))
          #-----Equity------

          comparison_table$equity<-result[,c("Academic_Year","Class_Type","Enrolled","Succeeded","Equity_Index")]
          comparison_graph$equity<-isolate(create_line_comparison_graph(comparison_table$equity,years_needed,type="Class_Type",status = "Equity"))
          
        }
        else if(!is.null(isolate(input$location_chkbx))){
          filter_type="Location"
          result=isolate(get_location(enrolled=data_comparison()[[1]],dropped=data_comparison()[[2]],
                                      passed=data_comparison()[[3]],location=input$location_chkbx,years=years_needed))
         
          incProgress(2/n, detail = paste("Getting calculations "))
          #----Enrolled------
         
          comparison_table$enrollment<-result[,c("Academic_Year","Location","Enrolled")]
          comparison_graph$enrollment<-isolate(create_line_comparison_graph(comparison_table$enrollment,years_needed,"Location","Enrolled"))
          #-----Retention------
          
          comparison_table$retention<-result[,c("Academic_Year","Location","Enrolled","Retained","Retention_Rate")]
          comparison_graph$retention<-isolate(create_line_comparison_graph( comparison_table$retention,years_needed,"Location","Retention"))
          #-----Success----------
         
          comparison_table$success<-result[,c("Academic_Year","Location","Enrolled","Succeeded","Success_Rate")]
          comparison_graph$success<-isolate(create_line_comparison_graph(comparison_table$success,years_needed,"Location","Success"))
          #-----Equity------

          comparison_table$equity<-result[,c("Academic_Year","Location","Enrolled","Succeeded","Equity_Index")]
          comparison_graph$equity<-isolate(create_line_comparison_graph( comparison_table$equity,years_needed,type="Location",status = "Equity"))
          
          
        }
        else if(isolate(input$day_static)!="Select Day" && isolate(input$time1)!="Select Time" ){
          altert<-F
          if(isolate(input$time1)=="Select Time"){alert<-T}
          filter_type="Time of Day"
          day=isolate(c(input$day_static))
          time=isolate(c(input$time1))
          
          if(isolate(counter$n)>=1){
            if(isolate(input$time_dynamic_1)=="Select Time" || isolate(input$day_dynamic_1)=="Select Day"){alert<-T}
            day[2]=isolate(input$day_dynamic_1)
            time[2]=isolate(input$time_dynamic_1)
          }
          
          if(isolate(counter$n)>=2){
            if(isolate(input$time_dynamic_2)=="Select Time" || isolate(input$day_dynamic_2)=="Select Day"){alert<-T}
            day[3]=isolate(input$day_dynamic_2)
            time[3]=isolate(input$time_dynamic_2)
          } 
          
          
            result=isolate(get_time(enrolled=data_comparison()[[1]],dropped=data_comparison()[[2]],
                                    passed=data_comparison()[[3]],day=day,time=time,days=counter$n,years=years_needed))
           
            incProgress(2/n, detail = paste("Getting calculations "))
            #----Enrolled------

            comparison_table$enrollment<-result[,c("Academic_Year","Day_Time","Enrolled")]
            comparison_graph$enrollment<-isolate(create_line_comparison_graph(comparison_table$enrollment,years_needed,"Day_Time","Enrolled"))
            #-----Retention------
            comparison_table$retention<-result[,c("Academic_Year","Day_Time","Enrolled","Retained","Retention_Rate")]
            comparison_graph$retention<-isolate(create_line_comparison_graph(comparison_table$retention,years_needed,"Day_Time","Retention"))
            #-----Success----------
            comparison_table$success<-result[,c("Academic_Year","Day_Time","Enrolled","Succeeded","Success_Rate")]
            comparison_graph$success<-isolate(create_line_comparison_graph(comparison_table$success,years_needed,"Day_Time","Success"))
            #-----Equity------
            comparison_table$equity<-result[,c("Academic_Year","Day_Time","Enrolled","Succeeded","Equity_Index")]
            comparison_graph$equity<-isolate(create_line_comparison_graph(comparison_table$equity,years_needed,type="Day_Time",status = "Equity"))}
        
      
        
        }
      
      
    })
    if(render_vt==T){
    output$filter<-renderInfoBox(
      infoBox(title="Filter",value=filter_type, icon = icon("filter"),fill=TRUE,
              color = "orange")
    )
    output$department_hc<-renderInfoBox(
      infoBox(title="Department",width=1,value=isolate(input$dept), icon = icon("bar-chart"),fill=TRUE,
              color = "black")
    )
    output$discipline_hc<-renderInfoBox(
      infoBox(title="Discipline",width=1,value=isolate(input$dis), icon = icon("bar-chart"),fill=TRUE,
              color = "black")
    )
    output$course_hc<-renderInfoBox(
      infoBox(title="Course",width=1,value=isolate(input$course), icon = icon("bar-chart"),fill=TRUE,
              color = "black")
    )
    
    showNotification("Note:Categories with less than 10 students will be censored for privacy reasons",duration=10,type="warning")}
    
    
    
  })
  
  
  #when main submit button is pressed 
  observe(  if(input$main_submit > 0){
   isolate( year_chosen(input$year))
    withProgress(message = 'Getting Data', value = 0, {
      n<-8
      incProgress(1/n, detail = paste("Accessing Database"))
      data(isolate(get_default(input$year,input$semester,input$dept,input$dis,input$course)))
      
    })
    #if thereis no data for inputed information then an error message will pop up
    if(is.null(data()) || nrow(data()[[1]])==0){
      showNotification(paste("There is no information for your current selection"), duration = 0,type="error")
      return()
    }
    
    fresh_colors<-c('rgba(23, 62, 67,1)','rgba(63, 133, 176,1)','rgba(63, 176, 172,1)',
                    'rgba(236, 250, 150)','rgba(250, 229, 150,1)','rgba(221, 223, 212,1)')
    
    print("Get data:Pass")
    
    
    ddc$department<-isolate(input$dept)
    ddc$discipline<-isolate(input$dis)
    ddc$course<-isolate(input$course)
    #<----------------------ENROLLMENT TAB---------------------------------------------------------------------|
    
    output$select_table_e<-renderUI(
      selectInput(inputId = "select_table_e",label="Choose graph to display",
                  choices = c("Age Range","Gender","Academic Probation","Ethnicity","Student Groups","Major"),selected="Age Range")
    )
    
    age_graph_rv$enrollment=NULL
    ap_graph_rv$enrollment=NULL
    gender_graph_rv$enrollment=NULL
    ethnicity_graph_rv$enrollment=NULL
    student_groups_graph_rv$enrollment=NULL
    major_graph_rv$enrollment=NULL
    
    
    #<----------------------------------RETENTION TAB  --------------------------------------------------------|
    
    
    output$select_table_r<-renderUI(
      selectInput(inputId = "select_table_r",label="Choose graph to display",
                  choices = c("Age Range","Gender","Academic Probation","Ethnicity","Student Groups","Major"),selected="Age Range")
    )
    
    age_graph_rv$retention=NULL
    ap_graph_rv$retention=NULL
    gender_graph_rv$retention=NULL
    ethnicity_graph_rv$retention=NULL
    student_groups_graph_rv$retention=NULL
    major_graph_rv$retention=NULL
    
    
    #<------------------------------SUCCESS TAB -------------------------------------------------------
    
    
    output$select_table_s<-renderUI(
      selectInput(inputId = "select_table_s",label="Choose graph to display",
                  choices = c("Age Range","Gender","Academic Probation","Ethnicity","Student Groups","Major"),selected="Age Range")
    )
    
    age_graph_rv$success=NULL
    ap_graph_rv$success=NULL
    gender_graph_rv$success=NULL
    ethnicity_graph_rv$success=NULL
    student_groups_graph_rv$success=NULL
    major_graph_rv$success=NULL
    #<-----------------------------EQUITY INDEX------------------------------------------------------------ 
    output$select_table_equity<-renderUI(
      selectInput(inputId = "select_table_equity",label="Choose graph to display",
                  choices = c("Age Range","Gender","Academic Probation","Ethnicity","Student Groups","Major"),selected="Age Range")
    )
    
    equity_index_graph$age=NULL
    equity_index_graph$gender=NULL
    equity_index_graph$probation=NULL
    equity_index_graph$ethnicty=NULL
    equity_index_graph$student_groups=NULL
    equity_index_graph$major=NULL
  })
  #Enrollment Tab Display graphs and charts 
  
  observeEvent(input$dept,{
    if(isolate(input$dept)=="Escalante Program" || isolate(input$dept)=="Non-Credit Program" ){
      showNotification(paste("Due to department containing only non-credit classes (no grades available) 
                             Retention, Success, Equity Index, and Historical Comparison options are unavailable"), duration = 15,type="warning")
      hide(selector = "#stud_course li a[data-value=Retention]")
      hide(selector = "#stud_course li a[data-value=Success]")
      hide(selector = "#stud_course li a[data-value='Equity Index (Success)']")
      hide(selector = "#stud_course li a[data-value='Historical Comparison']")
    }else{
      show(selector = "#stud_course li a[data-value=Retention]")
      show(selector = "#stud_course li a[data-value=Success]")
      show(selector = "#stud_course li a[data-value='Equity Index (Success)']")
      show(selector = "#stud_course li a[data-value='Historical Comparison']")
    }
    
  })
  
  observe(if(input$stud_course=="Enrollment"){
    shinyjs::show("main_submit")
    if(!is.null(input$select_table_e)){
      shinyjs::show("head_count")
      #color scheme for graphs
      colors <- c('rgba(255, 232, 0,1)','rgba(212, 255, 44,1)',
                  'rgba(82, 214, 136,1)', 'rgb(90, 221, 147,1)',
                  'rgba(89, 199, 182,1)', 'rgb(94, 156, 147)',
                  'rgb(37, 94, 101)','rgba(0,0,0,1)')
      
      withProgress(message = 'Getting Enrollment Data', value = 0, {
        n<-9
        incProgress(2/n, detail = paste("Rendering value boxes"))
        title=isolate(HTML(paste( year_chosen(),br(),input$semester)))
        if(isolate(is.null(input$semester))){
          title=isolate(HTML(paste( year_chosen(),br(),"Academic Year")))
        }
        
        output$year_semester<-renderInfoBox(
          isolate(infoBox(title="Time",width=2,value=title, icon = icon("clock-o"),fill=TRUE,
                  color = "light-blue"))
        )
        
        output$dept_box<-renderInfoBox(
          isolate(infoBox(title="Department",width=2,value=isolate(ddc$department), icon = icon("bar-chart"),fill=TRUE,
                  color = "black"))
        )
        output$dis_box<-renderInfoBox(
          isolate(infoBox(title="Discipline",width=2,value=isolate(ddc$discipline), icon = icon("bar-chart"),fill=TRUE,
                  color = "black"))
        )
        output$course_box<-renderInfoBox(
          isolate(infoBox(title="Course",width=2,value=isolate(ddc$course), icon = icon("bar-chart"),fill=TRUE,
                  color = "black"))
        )
        output$general_enrollment<-renderUI(
          isolate(box(title=paste( "Total Enrollment: ",total_enrollments," . . . Total Head Count: ",total_head_count," . . . Total Sections:",
                           isolate(sections_available(ddc$department,ddc$discipline,ddc$course,input$semester))),width=12,height=50,
                  status = "warning"))
        )
        
        shinyjs::show("head_count")
        
        #tabulate graphs
        incProgress(3/n, detail = paste("Calculating Age Range"))
        if(is.null(age_graph_rv$enrollment)){
          #Age pie graph
          temp<-isolate(get_age_range_graph(enrolled=data()[[1]],year=isolate( year_chosen()),status="enrolled"))

          enrollment_tab$age<-sensor_data(temp,1)
          temp<-sensor_data(temp,1,TRUE)
          enrollment_tab$age<-temp
          age_graph_rv$enrollment<-temp%>%
            plot_ly( labels = ~Age, values = ~Percent, type = 'pie',
                     text= ~paste(Age,"\n", Enrolled,"(enrolled)","\n",Percent,"%"),
                     hoverinfo='text',
                     textinfo="percent",
                     insidetextfont = list(color = '#000000'),
                     sort = FALSE,
                     marker = list(colors = colors,
                                   line = list(color = '#FFFFFF', width = 1))
                     #The 'pull' attribute can also be used to create space between the sectors
            ) %>%
            layout(title = 'Student Age Range',showlegend = T,legend = list(orientation = 'h'),margin=list(r=38),
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,categoryorder = "trace"),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE,  showticklabels = FALSE,categoryorder = "trace"))%>% config(displayModeBar = F)
          head_count_graph$age<-create_head_count(temp,type="Age",title="Student Age Range")
          print("Enrollment Age Graph:Pass")
          colnames(enrollment_tab$age)[6]="Enrolled Percent"
          enrollment_tab$age=enrollment_tab$age[c(1,2,3,6,4,5)]
          colnames(enrollment_tab$age)[5]="Headcount"
          colnames(enrollment_tab$age)[6]="Headcount Percent"
        } #age range graph
        
        incProgress(4/n, detail = paste("Calculating Academic Probation"))
        if(is.null(ap_graph_rv$enrollment)){
          #Academic Probation Pie graph
          temp<-isolate(get_academic_probation_students_(enrolled=data()[[1]],year= year_chosen()))
          enrollment_tab$probation<-sensor_data(temp,1)
          temp<-sensor_data(temp,1,TRUE)
          enrollment_tab$probation<-temp
          ap_graph_rv$enrollment <- plot_ly(temp, 
                                            labels = ~Status, values = ~Percent, type = 'pie',
                                            text= ~paste(Status,"\n", Enrolled,"(enrolled)","\n",Percent,"%"),
                                            textinfo = 'percent',
                                            insidetextfont = list(color = '#000000'),
                                            hoverinfo = 'text',
                                            marker = list(colors = colors,
                                                          line = list(color = '#FFFFFF', width = 1))
                                            #The 'pull' attribute can also be used to create space between the sectors
          ) %>%
            layout(title = 'Academic Probation',showlegend = TRUE,legend = list(orientation = 'h'),
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>% config(displayModeBar = F)
          head_count_graph$probation<-create_head_count(temp,type="Status",title="Academic Probation")
          colnames(enrollment_tab$probation)[6]="Enrolled Percent"
          enrollment_tab$probation=enrollment_tab$probation[c(1,2,3,6,4,5)]
          colnames(enrollment_tab$probation)[5]="Headcount"
          colnames(enrollment_tab$probation)[6]="Headcount Percent"
          print("Enrollment Probation Graph:Pass")
          
        } #academic probation graph
        
        incProgress(5/n, detail = paste("Calculating Gender"))
        if(is.null(gender_graph_rv$enrollment)){
          #Gender pie graph
          temp<-get_gender_graph(enrolled=data()[[1]])
          enrollment_tab$gender<-sensor_data(temp,1)
          temp<-sensor_data(temp,1,TRUE)
          enrollment_tab$gender<-temp
          gender_graph_rv$enrollment <- plot_ly(temp, labels = ~Gender, values = ~Percent, type = 'pie',
                                                textposition = 'inside',
                                                textinfo = 'label+percent',
                                                insidetextfont = list(color = '#000000'),
                                                hoverinfo = 'text',
                                                text = ~paste(Gender,"\n", Enrolled,"(enrolled)","\n",Percent,"%"),
                                                marker = list(colors = colors,
                                                              line = list(color = '#FFFFFF', width = 1),showlegend = FALSE)
                                                #The 'pull' attribute can also be used to create space between the sectors
          ) %>%
            layout(title = 'Gender', showlegend = TRUE,legend = list(orientation = 'h'),
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>% config(displayModeBar = F)
          head_count_graph$gender<-create_head_count(temp,type="Gender",title="Gender")
          colnames(enrollment_tab$gender)[6]="Enrolled Percent"
          enrollment_tab$gender=enrollment_tab$gender[c(1,2,3,6,4,5)]
          colnames(enrollment_tab$gender)[5]="Headcount"
          colnames(enrollment_tab$gender)[6]="Headcount Percent"
          print("Enrollment Gender Graph:Pass")
        } #gender graph
        
        incProgress(6/n, detail = paste("Calculating Ethnicity"))
        if(is.null(ethnicity_graph_rv$enrollment)){
          #Ethnicity pie graph that can be turned into a donut
          temp=get_ethnicity_graph(enrolled=data()[[1]]) 
          ethnicity_graph_rv$enrollment<-sensor_data(temp,1)
          temp= enrollment_tab$ethnicity<-sensor_data(temp,1,TRUE)
          enrollment_tab$ethnicity<-temp
          ethnicity_graph_rv$enrollment <-temp%>%
            plot_ly(labels = ~Ethnicity, values = ~Percent,
                    text= ~paste(Ethnicity,"\n", Enrolled,"(enrolled)","\n",Percent,"%"),
                    hoverinfo='text',
                    textinfo="percent",
                    insidetextfont = list(color = '#000000'),
                    marker = list(colors = colors,
                                  line = list(color = '#FFFFFF', width = 1))) %>%
            add_pie(hole = 0.5) %>%
            layout(title = "Ethnicity ",  showlegend = T,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>% config(displayModeBar = F)
          head_count_graph$ethnicity<-create_head_count(temp,type="Ethnicity",title="Ethnicity")
          colnames(enrollment_tab$ethnicity)[6]="Enrolled_Percent"
          enrollment_tab$ethnicity=enrollment_tab$ethnicity[c(1,2,3,6,4,5)]
          colnames(enrollment_tab$ethnicity)[5]="Headcount"
          colnames(enrollment_tab$ethnicity)[6]="Headcount Percent"
          print("Enrollment Ethnicity Graph:Pass")
          
        } #ethnicity graph
        
        incProgress(7/n, detail = paste("Calculating Student Groups"))
        if(is.null(student_groups_graph_rv$enrollment)){
          #Student groups bar graph
          temp<-isolate(get_student_groups(enrolled=data()[[1]],years= year_chosen()))
          enrollment_tab$student_groups<-sensor_data(temp,1)
          temp$Percent="--"
          temp$Enrolled_=temp$Enrolled
          for(i in 1:length(temp$Enrolled)){
            if(temp$Enrolled[i]!="--" && as.numeric(unlist(temp$Enrolled[i]))<10 && temp$Enrolled[i]>0){
              temp$Enrolled_[i]="--"
            }
            if(temp$Enrolled_[i]!="--"){
              temp$Percent[i]=round((temp$Enrolled[i]/nrow(data()[[1]]))*100,digits=1)
            }
            
          }
          enrollment_tab$student_groups=temp
          student_groups_graph_rv$enrollment <-temp %>%
            plot_ly(x = ~Groups, y = ~Enrolled,height=340,
                    text = ~paste(Groups, Enrolled_, ' students'),type="bar",
                    insidetextfont = list(color = '#000000'),
                    hoverinfo = 'text',
                    marker = list(color =  c('rgba(255, 232, 0,1)','rgba(212, 255, 44,1)',
                                             'rgba(93, 237, 133,1)', 'rgb(90, 221, 147,1)',
                                             'rgba(89, 199, 182,1)','rgba(89, 173, 199,1)'))) %>%
            layout(title = 'Student Groups',yaxis = list(showticklabels = FALSE),
                   annotations = list(x = ~Groups, y = ~Enrolled, text =~paste(Percent,"%") ,
                                      xanchor = 'center', yanchor = 'bottom',
                                      showarrow = FALSE))%>% config(displayModeBar = F)
          
          head_count_graph$student_groups<-create_head_count(temp,type="Groups",title="Student Groups",chart = "bar")
          colnames(enrollment_tab$student_groups)[7]="Enrolled Percent"
          enrollment_tab$student_groups[4]=enrollment_tab$student_groups[6]
          enrollment_tab$student_groups=enrollment_tab$student_groups[c(1,2,3,7,4,5)]
          colnames(enrollment_tab$student_groups)[5]="Headcount"
          colnames(enrollment_tab$student_groups)[6]="Headcount Percent"
          print("Enrollment Student Groups:Pass")
          
        } #student groups graph
        
        if(is.null(major_graph_rv$enrollment)){
          temp<-isolate(get_major(enrolled=data()[[1]],year= year_chosen()))
          enrollment_tab$major<-sensor_data(temp,1)
          temp<-sensor_data(temp,1,TRUE)
          enrollment_tab$major<-temp
          
          major_graph_rv$enrollment <- plot_ly(temp, labels = ~Major, values = ~Percent, type = 'pie',
                                               textposition = 'inside',
                                               text= ~paste(Major,"\n", Enrolled,"(enrolled)","\n",Percent,"%"),
                                               textinfo = 'percent',
                                               insidetextfont = list(color = '#000000'),
                                               hoverinfo = 'text',
                                               marker = list(colors = colors,
                                                             line = list(color = '#FFFFFF', width = 1))
                                               #The 'pull' attribute can also be used to create space between the sectors
          ) %>%
            layout(title = paste("Major "), showlegend = TRUE,legend = list(orientation = 'h'),
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>% config(displayModeBar = F)
          head_count_graph$major<-create_head_count(temp,type="Major",title="Major")
          colnames(enrollment_tab$major)[6]="Enrolled Percent"
          enrollment_tab$major=enrollment_tab$major[c(1,2,3,6,4,5)]
          colnames(enrollment_tab$major)[5]="Headcount"
          colnames(enrollment_tab$major)[6]="Headcount Percent"
          print("Enrollment Major:Pass")
          
        } #majors
        
        # Render graphs
        incProgress(8/n, detail = paste("Rendering Graphs"))
        output$Age_graph_e<-renderPlotly({
          if (is.null(age_graph_rv$enrollment)) return()
          if(isTRUE(input$head_count)){ return( head_count_graph$age)}
          age_graph_rv$enrollment
        })
        output$Probation_graph_e<-renderPlotly({
          if (is.null(ap_graph_rv$enrollment)) return()
          if(isTRUE(input$head_count)){ return( head_count_graph$probation)}
          
          ap_graph_rv$enrollment
        }) 
        output$Major_graph_e<-renderPlotly({
          if (is.null(major_graph_rv$enrollment)) return()
          if(isTRUE(input$head_count)){ return( head_count_graph$major)}
          major_graph_rv$enrollment
        }) 
        output$Gender_graph_e<-renderPlotly({
          if (is.null(gender_graph_rv$enrollment)) return()
          if(isTRUE(input$head_count)){ return( head_count_graph$gender)}
          gender_graph_rv$enrollment
        })
        output$Ethnicity_graph_e<-renderPlotly({
          if (is.null(ethnicity_graph_rv$enrollment)) return()
          if(isTRUE(input$head_count)){ return( head_count_graph$ethnicity)}
          ethnicity_graph_rv$enrollment
        })
        output$Student_Groups_e<-renderPlotly({
          if (is.null(student_groups_graph_rv$enrollment)) return()
          if(isTRUE(input$head_count)){ return( head_count_graph$student_groups)}
          student_groups_graph_rv$enrollment
        })
        
        incProgress(9/n, detail = paste("Rendering Charts"))
        #Render charts
        if(!is.null(input$select_table_e)){
          if(input$select_table_e=="Age Range"){
          
            output$chart_e<-DT::renderDataTable(enrollment_tab$age %>%mutate_all(as.character),rownames= FALSE)
          }
          else if(input$select_table_e=="Gender"){
            output$chart_e<-DT::renderDataTable(enrollment_tab$gender %>%mutate_all(as.character),rownames= FALSE)
          }
          else if(input$select_table_e=="Academic Probation"){
            output$chart_e<-DT::renderDataTable(enrollment_tab$probation %>%mutate_all(as.character),rownames= FALSE)
          }
          else if(input$select_table_e=="Ethnicity"){
            output$chart_e<-renderDataTable(enrollment_tab$ethnicity %>%mutate_all(as.character),rownames= FALSE)
          }
          else if(input$select_table_e=="Student Groups"){
            print(enrollment_tab$student_groups)
            enrollment_tab$student_groups=sensor(enrollment_tab$student_groups,"Enrolled")
            output$chart_e<-DT::renderDataTable(enrollment_tab$student_groups %>%mutate_all(as.character),rownames= FALSE)
          }
          else if(input$select_table_e=="Major"){
            output$chart_e<-DT::renderDataTable(enrollment_tab$major %>%mutate_all(as.character),rownames= FALSE)
          }
        }
      })  
      
    }
    else{
      shinyjs::hide("head_count")
    }
    
  })
  
  # Retention tab display graphs and charts
  observe(if(input$stud_course=="Retention"){
    #render charts
    shinyjs::show("main_submit")
    if(!is.null(input$select_table_r)){
      withProgress(message = 'Making plot', value = 0, {
        n<-8
        title=isolate(HTML(paste( year_chosen(),br(),input$semester)))
        if(isolate(is.null(input$semester))){
          title=isolate(HTML(paste( year_chosen(),br(),"Academic Year")))
        }
        incProgress(2/n, detail = paste("Rendering value boxes"))
        #render value boxes
        output$year_semester_r<-renderInfoBox(
          infoBox(title="Time",width=2,value=title, icon = icon("clock-o"),fill=TRUE,
                  color = "light-blue")
        )
        output$dept_box_r<-renderInfoBox(
          infoBox(title="Department",width=2,value=isolate(ddc$department), icon = icon("bar-chart"),fill=TRUE, color = "black"))
        
        output$dis_box_r<-renderInfoBox(
          infoBox(title="Discipline",width=2,value=isolate(ddc$discipline), icon = icon("bar-chart"),fill=TRUE,color = "black"))
        
        output$course_box_r<-renderInfoBox(
          infoBox(title="Course",width=2,value=isolate(ddc$course), icon = icon("bar-chart"),fill=TRUE,color = "black"))
        
        output$overall_retention<-renderUI(
          box(title=paste("Overall Retention Rate:",round(((nrow(data()[[1]])-nrow(data()[[2]]))/nrow(data()[[1]]))*100,digits=2),"%")
              ,width=12,height=40,status = "warning")
        )
        
        #tabulate graphs
        incProgress(3/n, detail = paste("Calculating Age Range"))
        if(is.null(age_graph_rv$retention)){
          #Age  graph
          retention_tab$age<-isolate(get_age_range_graph(data()[[1]],year=isolate( year_chosen()),status="retention"))
          age_graph_rv$retention<-plot_ly( retention_tab$age, 
                                           x=~Age,y=~Retention_Rate,type="bar",
                                           text = ~paste(Age,"\n", Enrolled,"(enrolled)","\n",
                                                         Retained,"(retained)","\n", Retention_Rate,"%"),
                                           insidetextfont = list(color = '#000000'),
                                           hoverinfo = 'text',
                                           marker = list(color =  c( 'rgb(94, 156, 147)','rgba(89, 199, 182,1)',
                                                                     'rgb(82, 214, 136,1)','rgba(93, 237, 133,1)',
                                                                     'rgba(212, 255, 44,1)','rgba(255, 232, 0,1)'))) %>%                                                      
            layout(title = 'Age',
                   yaxis =  list(title = "Retention Rate", showticklabels = FALSE,range=c(0,110)),
                   xaxis=list(linecolor = "black",linewidth = 0.5,mirror = T,categoryorder = "trace"),
                   annotations = list(x = ~Age, y = ~Retention_Rate, text =~paste(Retention_Rate,"%") ,
                                      xanchor = 'center', yanchor = 'bottom',
                                      showarrow = FALSE))%>% config(displayModeBar = F)
          
          print("Retention Age:Pass")
          
        } #age range graph
        incProgress(4/n, detail = paste("Calculating Academic Probation"))
        if(is.null(ap_graph_rv$retention)){
          #Academic Probation Pie graph
          retention_tab$probation<-isolate(get_academic_probation_students_(enrolled=data()[[1]],
                                                                            dropped=data()[[2]],year= year_chosen(),status="retention"))
          ap_graph_rv$retention <- plot_ly(retention_tab$probation,
                                           x=~Status,y=~Retention_Rate,type="bar",
                                           text = ~paste(Status,"\n", Enrolled,"(enrolled)","\n",
                                                         Retained,"(retained)","\n", Retention_Rate,"%"),
                                           insidetextfont = list(color = '#000000'),
                                           hoverinfo = 'text',
                                           marker = list(color =  c('rgb(212, 255, 44)',#yellow
                                                                    'rgb( 115, 231, 187 )'))) %>% #aqua blue
            
            layout(title = 'Academic Probation',
                   yaxis =  list(title = "Retention Rate",showticklabels = FALSE,range=c(0,110)), 
                   xaxis=list(linecolor = "black",linewidth = 0.5,mirror = T),
                   annotations = list(x = ~Status, y = ~Retention_Rate, text =~paste(Retention_Rate,"%") ,
                                      xanchor = 'center', yanchor = 'bottom',
                                      showarrow = FALSE))%>% config(displayModeBar = F)
          
          print("Retention Probation:Pass")
        } #academic probation graph
        incProgress(5/n, detail = paste("Calculating Gender Probation"))
        if(is.null(gender_graph_rv$retention)){
          #Gender pie graph
          retention_tab$gender<-get_gender_graph(enrolled=data()[[1]],dropped=data()[[2]],status="retention")
          gender_graph_rv$retention <- plot_ly(  retention_tab$gender, 
                                                 x=~Gender,y=~Retention_Rate,type="bar",
                                                 text = ~paste(Gender,"\n", Enrolled,"(enrolled)","\n",
                                                               Retained,"(retained)","\n", Retention_Rate,"%"),
                                                 insidetextfont = list(color = '#000000'),
                                                 hoverinfo = 'text',
                                                 marker = list(color =  c('rgb(89, 199, 182)',#lime green
                                                                          'rgb(37, 94, 101)' ))) %>%#ocean blue                                                     
            layout(title = 'Gender', 
                   yaxis =list(title = "Retention Rate",showticklabels = FALSE,range=c(0,110)),
                   xaxis=list(linecolor = "black",linewidth = 0.5,mirror = T),
                   annotations = list(x = ~Gender, y = ~Retention_Rate, text =~paste(Retention_Rate,"%") ,
                                      xanchor = 'center', yanchor = 'bottom',
                                      showarrow = FALSE))%>%config(displayModeBar = F)
          
          print("Retention Gender:Pass")
        } #gender graph
        incProgress(6/n, detail = paste("Calculating ethnicity graph"))
        if(is.null(ethnicity_graph_rv$retention)){
          #Ethnicity pie graph that can be turned into a donut
          retention_tab$ethnicity=get_ethnicity_graph(enrolled=data()[[1]],dropped=data()[[2]],status="retention")  
          ethnicity_graph_rv$retention <-plot_ly(retention_tab$ethnicity,x=~Retention_Rate,y=~Ethnicity,type="bar",orientation="h",
                                                 text = ~paste(Ethnicity,"\n", Enrolled,"(enrolled)","\n",
                                                               Retained,"(retained)","\n", Retention_Rate,"%"),
                                                 insidetextfont = list(color = '#000000'),
                                                 hoverinfo = 'text',
                                                 marker = list(color =  c('rgba(255, 232, 0,1)','rgba(212, 255, 44,1)',
                                                                          'rgba(82, 214, 136,1)', 'rgb(90, 221, 147,1)',
                                                                          'rgba(89, 199, 182,1)', 'rgb(94, 156, 147)',
                                                                          'rgb(37, 94, 101)','rgba(0,0,0,1)'))) %>%
            
            layout(title = 'Ethnicity',margin = list(l=180,r=-2),
                   yaxis =  list( title = "",linecolor = "black",linewidth = 0.5,mirror = F, ticks='inside'),
                   xaxis=list(range=c(0,110)),
                   annotations = list(y = ~Ethnicity, x = ~Retention_Rate, text =~paste(Retention_Rate,"%") ,
                                      xanchor = 'bottom', yanchor = 'right',xshift=32,
                                      showarrow = FALSE))%>% config(displayModeBar = F)
          
          print("Retention Ethnicity:Pass")
        } #ethnicity graph
        incProgress(7/n, detail = paste("calculating student groups graph"))
        if(is.null(student_groups_graph_rv$retention)){
          #Student groups bar graph
          retention_tab$student_groups<-isolate(get_student_groups(enrolled=data()[[1]],dropped=data()[[2]],status="retention",years= year_chosen()))
          student_groups_graph_rv$retention <-plot_ly(retention_tab$student_groups, 
                                                      y=~Groups,x=~Retention_Rate,type="bar",height=300,orientation="h",
                                                      text = ~paste(Groups,"\n", Enrolled,"(enrolled)","\n",
                                                                    Retained,"(retained)","\n", Retention_Rate,"%"),
                                                      insidetextfont = list(color = '#000000'),
                                                      hoverinfo = 'text',
                                                      marker = list(color =  c('rgba(255, 232, 0,1)','rgba(212, 255, 44,1)',
                                                                               'rgba(82, 214, 136,1)', 'rgb(90, 221, 147,1)',
                                                                               'rgba(89, 199, 182,1)', 'rgb(94, 156, 147)',
                                                                               'rgb(37, 94, 101)','rgba(0,0,0,1)'))) %>%
            layout(title = 'Student Groups',margin = list(l=70,r=-2),
                   yaxis =  list( title = "",linecolor = "black",linewidth = 0.5,mirror = F, ticks='inside',categoryorder = "trace"),
                   xaxis=list(range=c(0,110)),
                   annotations = list(y = ~Groups, x = ~Retention_Rate, text =~paste(Retention_Rate,"%") ,
                                      xanchor = 'bottom', yanchor = 'right',xshift=32,
                                      showarrow = FALSE))%>% config(displayModeBar = F)
          
          print("Retention Student Groups:Pass")
        } #student groups graph
        incProgress(8/n, detail = paste("calculating majors graph"))
        if(is.null(major_graph_rv$retention)){
          #Student groups bar graph
          retention_tab$major<-isolate(get_major(enrolled=data()[[1]],dropped=data()[[2]],year= year_chosen(),status = "retention"))
          major_graph_rv$retention<-plot_ly(retention_tab$major,x=~Major,y=~Retention_Rate,type="bar",
                                            text = ~paste(Retained,"\n", Enrolled,"(enrolled)","\n",
                                                          Retention_Rate,"%"),
                                            insidetextfont = list(color = '#000000'),
                                            hoverinfo = 'text',
                                            marker = list(color =  c('rgba(255, 232, 0,1)','rgba(212, 255, 44,1)',
                                                                     'rgba(82, 214, 136,1)'))
                                            ,showlegend=TRUE) %>%
            layout(title ="Majors",
                   xaxis = list(title = "",linecolor = "black",linewidth = 0.5,mirror = T,categoryorder = "trace"),
                   yaxis = list(side = 'left', title = "Retention Rate",showticklabels = FALSE,range=c(0,110)),
                   annotations = list(x =~Major , y = ~Retention_Rate, text =~paste(Retention_Rate,"%") ,
                                      xanchor = 'center', yanchor = 'bottom',
                                      showarrow = FALSE))%>% config(displayModeBar = F)
          print("Enrollment Major:Pass")
          
        } #majors
        
        
        
        # Render Retention Graphs
        incProgress(8/n, detail = paste("Rendering Graphs"))
        output$Age_graph_r<-renderPlotly({
          if (is.null(age_graph_rv$retention)) return()
          age_graph_rv$retention
        })
        output$Probation_graph_r<-renderPlotly({
          if (is.null(ap_graph_rv$retention)) return()
          ap_graph_rv$retention
        }) 
        output$Gender_graph_r<-renderPlotly({
          if (is.null(gender_graph_rv$retention)) return()
          gender_graph_rv$retention
        })
        output$Ethnicity_graph_r<-renderPlotly({
          if (is.null(ethnicity_graph_rv$retention)) return()
          ethnicity_graph_rv$retention
        })
        output$Student_Groups_r<-renderPlotly({
          if (is.null(student_groups_graph_rv$retention)) return()
          student_groups_graph_rv$retention
        })
        output$Major_graph_r<-renderPlotly({
          if (is.null(major_graph_rv$retention)) return()
          major_graph_rv$retention
        })
        
        #render retention chart
        incProgress(9/n, detail = paste("Rendering Charts"))
        if(input$select_table_r=="Age Range"){
          output$chart_r<-DT::renderDataTable(retention_tab$age %>%mutate_all(as.character),rownames= FALSE)
        }
        else if(input$select_table_r=="Gender"){
          output$chart_r<-DT::renderDataTable(retention_tab$gender %>%mutate_all(as.character),rownames= FALSE)
        }
        else if(input$select_table_r=="Academic Probation"){
          output$chart_r<-DT::renderDataTable(retention_tab$probation %>%mutate_all(as.character),rownames= FALSE)
        }
        else if(input$select_table_r=="Ethnicity"){
          output$chart_r<-DT::renderDataTable(retention_tab$ethnicity %>%mutate_all(as.character),rownames= FALSE)
        }
        else if(input$select_table_r=="Student Groups"){
          output$chart_r<-DT::renderDataTable(retention_tab$student_groups %>%mutate_all(as.character),rownames= FALSE)
        }
        else if(input$select_table_r=="Major"){
          output$chart_r<-DT::renderDataTable(retention_tab$major %>%mutate_all(as.character),rownames= FALSE)
        }
      })
    }
  })
  
  #Success tab display graphs and charts
  observe(if(input$stud_course=="Success"){
    shinyjs::show("main_submit")
    if(!is.null(input$select_table_s)){
      withProgress(message = 'Making plot', value = 0, {
        n<-9
        
        incProgress(2/n, detail = paste("Rendering value boxes"))
        #render value boxes
        title=isolate(HTML(paste( year_chosen(),br(),input$semester)))
        if(isolate(is.null(input$semester))){
          title=isolate(HTML(paste( year_chosen(),br(),"Academic Year")))
        }
        output$year_semester_s<-renderInfoBox(
          infoBox(title="Time",width=2,value=title, icon = icon("clock-o"),fill=TRUE,
                  color = "light-blue")
        )
        output$dept_box_s<-renderInfoBox(
          infoBox(title="Department",width=2,value=isolate(ddc$department), icon = icon("bar-chart"),fill=TRUE,
                  color = "black")
        )
        output$dis_box_s<-renderInfoBox(
          infoBox(title="Discipline",width=2,value=isolate(ddc$discipline), icon = icon("bar-chart"),fill=TRUE,
                  color = "black")
        )
        output$course_box_s<-renderInfoBox(
          infoBox(title="Course",width=2,value=isolate(ddc$course), icon = icon("bar-chart"),fill=TRUE,
                  color = "black")
        )
        
        output$overall_success<-renderUI(
          box(title=paste("Overall Success Rate:",round((nrow(data()[[3]])/nrow(data()[[1]]))*100,digits=2),"%")
              ,width=12,height=40,status = "warning")
        )
        
        
        #Tabulate graphs
        incProgress(3/n, detail = paste("Calculating Age Range"))
        if(is.null(age_graph_rv$success)){
          #Age  graph
          success_tab$age<-isolate(get_age_range_graph(data()[[1]],year=isolate( year_chosen()),status="success"))
          age_graph_rv$success<-plot_ly(success_tab$age, 
                                        x=~Age,y=~Success_Rate,type="bar",
                                        text = ~paste(Age,"\n", Enrolled,"(enrolled)","\n",
                                                      Succeeded,"(succeeded)","\n", Success_Rate,"%"),
                                        insidetextfont = list(color = '#000000'),
                                        hoverinfo = 'text',
                                        marker = list(color =  c('rgb(94, 156, 147)','rgba(89, 199, 182,1)',
                                                                 'rgb(82, 214, 136,1)','rgba(93, 237, 133,1)',
                                                                 'rgba(212, 255, 44,1)','rgba(255, 232, 0,1)'))) %>%
            layout(title = 'Age',
                   yaxis =  list(title = "Success Rate",showticklabels = FALSE, range=c(0,110) ),
                   xaxis=list(linecolor = "black",linewidth = 0.5,mirror = T,categoryorder = "trace"),
                   annotations = list(x = ~Age, y = ~Success_Rate, text =~paste(Success_Rate,"%") ,
                                      xanchor = 'center', yanchor = 'bottom',
                                      showarrow = FALSE))%>% config(displayModeBar = F)
          print("Success Age Range:Pass")
        }#age range graph
        incProgress(4/n, detail = paste("Calculating Academic Probation"))
        if(is.null(ap_graph_rv$success)){
          #Academic Probation Pie graph
          success_tab$probation<-isolate(get_academic_probation_students_(enrolled=data()[[1]],
                                                                          passed=data()[[3]],year= year_chosen(),status="success"))
          ap_graph_rv$success <- plot_ly(success_tab$probation, 
                                         x=~Status,y=~Success_Rate,type="bar",
                                         text = ~paste(Status,"\n", Enrolled,"(enrolled)","\n",
                                                       Succeeded,"(succeeded)","\n", Success_Rate,"%"),
                                         insidetextfont = list(color = '#000000'),
                                         hoverinfo = 'text',
                                         marker = list(color =  c('rgb(212, 255, 44)',#yellow
                                                                  'rgb( 115, 231, 187 )'))) %>% #aqua blue
            layout(title = 'Academic Probation',
                   yaxis =  list( title = "Success Rate",showticklabels = FALSE,range=c(0,110)),
                   xaxis=list(linecolor = "black",linewidth = 0.5,mirror = T),
                   annotations = list(x = ~Status, y = ~Success_Rate, text =~paste(Success_Rate,"%") ,
                                      xanchor = 'center', yanchor = 'bottom',
                                      showarrow = FALSE))%>% config(displayModeBar = F)
          print("Success Probation:Pass")
        }#academic probation graph
        incProgress(5/n, detail = paste("Calculating Gender "))
        if(is.null(gender_graph_rv$success)){
          #Gender pie graph
          success_tab$gender<-get_gender_graph(enrolled=data()[[1]],passed=data()[[3]],status="success")
          gender_graph_rv$success <- plot_ly(  success_tab$gender,
                                               x=~Gender,y=~Success_Rate,type="bar",
                                               text = ~paste(Gender,"\n", Enrolled,"(enrolled)","\n",
                                                             Succeeded,"(succeeded)","\n", Success_Rate,"%"),
                                               insidetextfont = list(color = '#000000'),
                                               hoverinfo = 'text',
                                               marker = list(color =  c('rgb(89, 199, 182)',#lime green
                                                                        'rgb(37, 94, 101)' ))) %>%#ocean blue
            
            layout(title = 'Gender',
                   yaxis =  list( title = "Success Rate",showticklabels = FALSE,range=c(0,110)),
                   xaxis=list(linecolor = "black",linewidth = 0.5,mirror = T),
                   annotations = list(x = ~Gender, y = ~Success_Rate, text =~paste(Success_Rate,"%") ,
                                      xanchor = 'center', yanchor = 'bottom',
                                      showarrow = FALSE))%>%config(displayModeBar = F)
          
          print("Success Gender:Pass")
        }#gender graph
        incProgress(6/n, detail = paste("Calculating ethnicity graph"))
        if(is.null(ethnicity_graph_rv$success)){
          #Ethnicity pie graph that can be turned into a donut
          success_tab$ethnicity=get_ethnicity_graph(enrolled=data()[[1]],passed=data()[[3]],status="success")
          ethnicity_graph_rv$success <-plot_ly( success_tab$ethnicity,x=~Success_Rate,y=~Ethnicity,type="bar",orientation="h",
                                                text = ~paste(Ethnicity,"\n", Enrolled,"(enrolled)","\n",
                                                              Succeeded,"(succeeded)","\n", Success_Rate,"%"),
                                                insidetextfont = list(color = '#000000'),
                                                hoverinfo = 'text',
                                                marker = list(color =  c('rgba(255, 232, 0,1)','rgba(212, 255, 44,1)',
                                                                         'rgba(82, 214, 136,1)', 'rgb(90, 221, 147,1)',
                                                                         'rgba(89, 199, 182,1)', 'rgb(94, 156, 147)',
                                                                         'rgb(37, 94, 101)','rgba(0,0,0,1)'))) %>%
            layout(title = 'Ethnicity',margin = list(l=180,r=-2),
                   yaxis =  list( title = "",linecolor = "black",linewidth = 0.5,mirror = F, ticks='inside',categoryorder = "array"),
                   xaxis=list(range=c(0,110)),
                   annotations = list(y = ~Ethnicity, x = ~Success_Rate, text =~paste(Success_Rate,"%") ,
                                      xanchor = 'bottom', yanchor = 'right',xshift=32,
                                      showarrow = FALSE))%>% config(displayModeBar = F)
          print("Success Ethnicity:Pass")
        }#ethnicity graph
        incProgress(7/n, detail = paste("Rendering student groups graph"))
        if(is.null(student_groups_graph_rv$success)){
          #Student groups bar graph
          success_tab$student_groups<-isolate(get_student_groups(enrolled=data()[[1]],passed=data()[[3]],status="success",year= year_chosen()))
          student_groups_graph_rv$success <-plot_ly(success_tab$student_groups, 
                                                    y=~Groups,x=~Success_Rate,type="bar",height=300,orientation="h",
                                                    text = ~paste(Groups,"\n", Enrolled,"(enrolled)","\n",
                                                                  Succeeded,"(succeeded)","\n", Success_Rate,"%"),
                                                    insidetextfont = list(color = '#000000'),
                                                    hoverinfo = 'text',
                                                    marker = list(color =  c('rgba(255, 232, 0,1)','rgba(212, 255, 44,1)',
                                                                             'rgba(82, 214, 136,1)', 'rgb(90, 221, 147,1)',
                                                                             'rgba(89, 199, 182,1)', 'rgb(94, 156, 147)',
                                                                             'rgb(37, 94, 101)','rgba(0,0,0,1)'))) %>%
            layout(title = 'Student Groups',margin = list(l=70,r=-2),
                   yaxis =  list( title = "",linecolor = "black",linewidth = 0.5,mirror = F, ticks='inside',categoryorder = "trace"),
                   xaxis=list(range=c(0,110)),
                   annotations = list(y = ~Groups, x = ~Success_Rate, text =~paste(Success_Rate,"%") ,
                                      xanchor = 'bottom', yanchor = 'right',xshift=32,
                                      showarrow = FALSE))%>% config(displayModeBar = F)
          print("Success:Pass")
        }#student groups graph
        incProgress(8/n, detail = paste("calculating majors graph"))
        if(is.null(major_graph_rv$success)){
          #Student groups bar graph
          success_tab$major<-isolate(get_major(enrolled=data()[[1]],passed=data()[[3]],year= year_chosen(),status = "success"))
          major_graph_rv$success<-plot_ly(success_tab$major,x=~Major,y=~Success_Rate,type="bar",
                                          text = ~paste(Succeeded,"\n", Enrolled,"(enrolled)","\n",
                                                        Success_Rate,"%"),
                                          insidetextfont = list(color = '#000000'),
                                          hoverinfo = 'text',
                                          marker = list(color = c('rgba(255, 232, 0,1)','rgba(212, 255, 44,1)',
                                                                  'rgba(82, 214, 136,1)')),showlegend=TRUE) %>%
            layout(title ="Majors",
                   xaxis = list(title = "",linecolor = "black",linewidth = 0.5,mirror = T,categoryorder = "trace"),
                   yaxis = list(side = 'left', title = "Success Rate", zeroline = FALSE,showticklabels = FALSE,range=c(0,110)),
                   annotations = list(x =~Major , y = ~Success_Rate, text =~paste0(Success_Rate,"%") ,
                                      xanchor = 'center', yanchor = 'bottom',
                                      showarrow = FALSE))%>% config(displayModeBar = F)
          
        }
        
        incProgress(8/n, detail = paste("Rendering Graphs"))
        #render graphs
        output$Age_graph_s<-renderPlotly({
          if (is.null(age_graph_rv$success)) return()
          age_graph_rv$success
        })
        output$Probation_graph_s<-renderPlotly({
          if (is.null(ap_graph_rv$success)) return()
          ap_graph_rv$success
        }) 
        output$Gender_graph_s<-renderPlotly({
          if (is.null(gender_graph_rv$success)) return()
          gender_graph_rv$success
        })
        output$Ethnicity_graph_s<-renderPlotly({
          if (is.null(ethnicity_graph_rv$success)) return()
          ethnicity_graph_rv$success
        })
        output$Student_Groups_s<-renderPlotly({
          if (is.null(student_groups_graph_rv$success)) return()
          student_groups_graph_rv$success
        })
        output$Major_graph_s<-renderPlotly({
          if (is.null(major_graph_rv$success)) return()
          major_graph_rv$success
        })
        
        incProgress(9/n, detail = paste("Rendering Charts"))
        #Render charts
        if(input$select_table_s=="Age Range"){
          output$chart_s<-DT::renderDataTable(success_tab$age %>%mutate_all(as.character),rownames= FALSE)
        }
        else if(input$select_table_s=="Gender"){
          output$chart_s<-DT::renderDataTable(success_tab$gender %>%mutate_all(as.character),rownames= FALSE)
        }
        else if(input$select_table_s=="Academic Probation"){
          output$chart_s<-DT::renderDataTable(success_tab$probation %>%mutate_all(as.character),rownames= FALSE)
        }
        else if(input$select_table_s=="Ethnicity"){
          output$chart_s<-DT::renderDataTable(success_tab$ethnicity %>%mutate_all(as.character),rownames= FALSE)
        }
        else if(input$select_table_s=="Student Groups"){
          output$chart_s<-DT::renderDataTable(success_tab$student_groups %>%mutate_all(as.character),rownames= FALSE)
        }
        else if(input$select_table_s=="Major"){
          output$chart_s<-DT::renderDataTable(success_tab$major %>%mutate_all(as.character),rownames= FALSE)
        }
      })
    }
    
    
    
    
  })
  
  observe(if(input$stud_course=="Equity Gaps (Success)"){
    shinyjs::show("main_submit")
    if(!is.null(input$select_table_equity)){
      shinyjs::show("standard_line")
      withProgress(message = 'Making plot', value = 0, {
        n<-4
        incProgress(2/n, detail = paste("Rendering value boxes"))
        #render value boxes
        title=isolate(HTML(paste( year_chosen(),br(),input$semester)))
        if(isolate(is.null(input$semester))){
          title=isolate(HTML(paste( year_chosen(),br(),"Academic Year")))
        }
        output$year_semester_equity<-renderInfoBox(
          infoBox(title="Time",width=2,value=title, icon = icon("clock-o"),fill=TRUE,
                  color = "light-blue")
        )
        output$dept_box_equity<-renderInfoBox(
          infoBox(title="Department",width=2,value=isolate(ddc$department), icon = icon("bar-chart"),fill=TRUE,
                  color = "black")
        )
        output$dis_box_equity<-renderInfoBox(
          infoBox(title="Discipline",width=2,value=isolate(ddc$discipline), icon = icon("bar-chart"),fill=TRUE,
                  color = "black")
        )
        output$course_box_equity<-renderInfoBox(
          infoBox(title="Course",width=2,value=isolate(ddc$course), icon = icon("bar-chart"),fill=TRUE,
                  color = "black")
        )
        
        
        incProgress(3/n, detail = paste("Calculating Age Range"))
        #------AGE GRAPH-----
        if(is.null(equity_index_graph$age)){
          equity_index_table$age<-isolate(get_age_range_graph(data()[[1]],year=isolate( year_chosen()),status="equity"))
          temp<-isolate(create_equity_graph(equity_index_table$age,type="Age",year= year_chosen()))
          equity_index_graph$age<-temp[[1]]
          equity_index_graph_standard$age<-temp[[2]]
          print("Equity Age Range:Pass")
        }#age range graph
        
        #-----Gender----------
        incProgress(4/n, detail = paste("Calculating Gender"))
        if(is.null(equity_index_graph$gender)){
          equity_index_table$gender<-isolate(get_gender_graph(enrolled=data()[[1]],passed=data()[[3]],status="equity"))
          temp<-isolate(create_equity_graph(equity_index_table$gender,type="Gender",year= year_chosen()))
          equity_index_graph$gender<-temp[[1]]
          equity_index_graph_standard$gender<-temp[[2]]
          print("Equity Gender Range:Pass")
        }
        #-----Probation------
        incProgress(4/n, detail = paste("Calculating Academic Probation"))
        if(is.null(equity_index_graph$probation)){
          #Academic Probation Pie graph
          equity_index_table$probation<-isolate(get_academic_probation_students_(enrolled=data()[[1]],
                                                                                 passed=data()[[3]],year= year_chosen(),status="equity"))
          temp<-isolate(create_equity_graph(equity_index_table$probation,type="Status",year= year_chosen()))
          equity_index_graph$probation<-temp[[1]]
          equity_index_graph_standard$probation<-temp[[2]]
          print("Success Probation:Pass")
        }
        
        #-----Ethnicity------
        if(is.null(equity_index_graph$ethnicity)){
          equity_index_table$ethnicity<-get_ethnicity_graph(enrolled=data()[[1]],passed=data()[[3]],status="equity")
          temp<-isolate(create_equity_graph(equity_index_table$ethnicity,type="Ethnicity",year= year_chosen(),orientation="h"))
          equity_index_graph$ethnicity<-temp[[1]]
          equity_index_graph_standard$ethnicity<-temp[[2]]
          print("Success Ethnicity:Pass")
        }
        #-----Student Groups------
        if(is.null(equity_index_graph$student_groups)){
          equity_index_table$student_groups<-isolate(get_student_groups(enrolled=data()[[1]],passed=data()[[3]],status="equity",year= year_chosen()))
          temp<-isolate(create_equity_graph(equity_index_table$student_groups,type="Groups",year= year_chosen(),orientation="h",height=300))
          equity_index_graph$student_groups<-temp[[1]]
          equity_index_graph_standard$student_groups<-temp[[2]]
          print("Success Student Groups:Pass")
        }
        #-----Major-------
        if(is.null(equity_index_graph$major)){
          equity_index_table$major<-isolate(get_major(enrolled=data()[[1]],passed=data()[[3]],status="equity",year= year_chosen()))
          temp<-isolate(create_equity_graph(equity_index_table$major,type="Major",year= year_chosen()))
          equity_index_graph$major<-temp[[1]]
          equity_index_graph_standard$major<-temp[[2]]
          print("Equity Major:Pass")
        }
        
        incProgress(6/n, detail = paste("Rendering Graphs"))
        #Render graphs
        output$Age_graph_equity<-renderPlotly({
          if (is.null(equity_index_graph$age)) return()
          if(!isTRUE(input$standard_line)){ return(equity_index_graph$age)}
          equity_index_graph_standard$age
        })
        output$Probation_graph_equity<-renderPlotly({
          if (is.null(equity_index_graph$probation)) return()
          if(!isTRUE(input$standard_line)){return(equity_index_graph$probation)}
          equity_index_graph_standard$probation
        }) 
        output$Gender_graph_equity<-renderPlotly({
          if (is.null(equity_index_graph$gender)) return()
          if(!isTRUE(input$standard_line)){return(equity_index_graph$gender)}
          equity_index_graph_standard$gender
        })
        output$Ethnicity_graph_equity<-renderPlotly({
          if (is.null(equity_index_graph$ethnicity)) return()
          if(!isTRUE(input$standard_line)){return(equity_index_graph$ethnicity)}
          equity_index_graph_standard$ethnicity
        })
        output$Student_Groups_equity<-renderPlotly({
          if (is.null(equity_index_graph$student_groups)) return()
          if(!isTRUE(input$standard_line)){return(equity_index_graph$student_groups)}
          equity_index_graph_standard$student_groups
        })
        output$Major_graph_equity<-renderPlotly({
          if (is.null(equity_index_graph$major)) return()
          if(!isTRUE(input$standard_line)){return(equity_index_graph$major)}
          equity_index_graph_standard$major
        })
        
        #Render charts
        if(input$select_table_equity=="Age Range"){
          output$chart_equity<-DT::renderDataTable(equity_index_table$age %>%mutate_all(as.character),rownames= FALSE)
        }
        else if(input$select_table_equity=="Gender"){
          output$chart_equity<-DT::renderDataTable(equity_index_table$gender %>%mutate_all(as.character),rownames= FALSE)
        }
        else if(input$select_table_equity=="Academic Probation"){
          output$chart_equity<-DT::renderDataTable(equity_index_table$probation %>%mutate_all(as.character),rownames= FALSE)
        }
        else if(input$select_table_equity=="Ethnicity"){
          output$chart_equity<-DT::renderDataTable(equity_index_table$ethnicity %>%mutate_all(as.character),rownames= FALSE)
        }
        else if(input$select_table_equity=="Student Groups"){
          output$chart_equity<-DT::renderDataTable(equity_index_table$student_groups %>%mutate_all(as.character),rownames= FALSE)
        }
        else if(input$select_table_equity=="Major"){
          output$chart_equity<-DT::renderDataTable(equity_index_table$major %>%mutate_all(as.character),rownames= FALSE)
        }
      })
      
      
    }else{
      shinyjs::hide("standard_line")
    }
    
    
  })
  
  observe(if(input$stud_course=="Historical Comparison"){
    #Render Graphs

    
    shinyjs::hide("main_submit")
    output$enrollment_comparison_graph<-renderPlotly({
      if (is.null(comparison_graph$enrollment)) return()
      comparison_graph$enrollment
    })
    output$retention_comparison_graph<-renderPlotly({
      if (is.null(comparison_graph$retention)) return()
      comparison_graph$retention
    })
    output$success_comparison_graph<-renderPlotly({
      if (is.null(comparison_graph$success)) return()
      comparison_graph$success
    })
    output$equity_index_comparison_graph<-renderPlotly({
      if (is.null(comparison_graph$equity)) return()
      comparison_graph$equity
    })
    
    
    #Render Charts
    if(!is.null(comparison_table$enrollment)){
      comparison_table$enrollment<-sensor_comparison(comparison_table$enrollment)
      comparison_table$enrollment<- fix_years(comparison_table$enrollment)
      output$enrollment_comparison_table<-DT::renderDataTable( comparison_table$enrollment)
    }
    if(!is.null(comparison_table$retention)){
      comparison_table$retention<-sensor_comparison(comparison_table$retention,"retention")
      comparison_table$retention<- fix_years(comparison_table$retention)
      output$retention_comparison_table<-DT::renderDataTable( comparison_table$retention,options=list(autoWidth = FALSE,scrollX=TRUE))
    }
    if(!is.null(comparison_table$success)){
      comparison_table$success<-sensor_comparison(comparison_table$success,"success")
      comparison_table$success<- fix_years(comparison_table$success)
      output$success_comparison_table<-DT::renderDataTable( comparison_table$success,options=list(autoWidth = TRUE,scrollX=TRUE))
    }
    if(!is.null(comparison_table$equity)){
      comparison_table$equity<-sensor_comparison(comparison_table$equity,"equity")
      comparison_table$equity<- fix_years(comparison_table$equity)
      output$equity_index_comparison_table<-DT::renderDataTable( comparison_table$equity,options=list(autoWidth = TRUE,scrollX=TRUE))
    }
  })

  observe(if(input$include_gender=="No"){
      shinyjs::hide("gender")
    }else{
      shinyjs::show("gender")
    })
  observe(if(input$stud_course=="Historical Comparison" && input$dissaggregate=="Student Demographics"){
    if(input$category=="Ethnicity"){
      shinyjs::show("include_gender")    
     
     output$title_main<-renderUI(h3("Ethnicity"))
      updateCheckboxGroupInput(session=session,inputId = "category_dis", label = NULL,
                         c("Asian",
                           "Native American",
                           "White",
                           "Hispanic/Latino", 
                           "Black/African American",
                           "Hawaiian/Pacific Islander",
                           "Two or more races",
                           "Unknown"))
      
    }
    else if(input$category=="Age"){
      
      output$title_main<-renderUI(h3("Age Range"))
      updateCheckboxGroupInput(session=session,inputId = "category_dis", 
                                label=NULL,c("Under 18","18-19", "20-24","25-29", "30-39","40 & older"))
      shinyjs::hide("gender")                        
      shinyjs::hide("include_gender")                                 
    }
    else if(input$category=="Gender"){
     
      output$title_main<-renderUI(h3("Gender"))
      updateCheckboxGroupInput(session=session,inputId = "category_dis", 
                               label=NULL,choices=c("Female","Male"))
      shinyjs::hide("gender")                        
      shinyjs::hide("include_gender") 
    }
    else if(input$category=="Student Groups"){
     
      output$title_main<-renderUI(h3("Student Groups"))
      updateCheckboxGroupInput(session=session,inputId = "category_dis", 
                               label=NULL,choices=c("EOPS","DSPS","CalWorks",
                                                            "Veteran","Military","BOGG","Pell Grant"))
      shinyjs::hide("gender")                        
      shinyjs::hide("include_gender") 
    }
    else if(input$category=="Major"){
     
      output$title_main<-renderUI(h3("Major"))
      updateCheckboxGroupInput(session=session,inputId = "category_dis", 
                               label=NULL,choices=c("Undeclared","Declared"))
      shinyjs::hide("gender")                        
      shinyjs::hide("include_gender") 
    }
    else if(input$category=="Academic Probation"){
     
      output$title_main<-renderUI(h3("Probation"))
      updateCheckboxGroupInput(session=session,inputId = "category_dis", 
                               label=NULL,choices=c("Yes","No"))
      shinyjs::hide("gender")                        
      shinyjs::hide("include_gender") 
    }
    
  })  
  observe(if(input$stud_course=="Historical Comparison" && input$year==years_available()[1]){
      showNotification(paste("Warning: Limited data for ",years_available()[1]," year. Will skew enrollment graph."),duration=15,type="warning", id="missing_data")
    })
  #disable checkboxes if one box is chosen in historical comparison->students
 
  #disable checkboxes if one box is chonsen in historical comparison->courses
  observe(if(!is.null(input$class_type_chkbx)){
    
    shinyjs::disable("location_chkbx")
    shinyjs::disable("day_static")
    shinyjs::disable("time1")
    shinyjs::disable("add_btn")
    if(isolate(day_1_created())){
      shinyjs::disable("day_dynamic_1")
      shinyjs::disable("time_dynamic_1")
    }
    if(isolate(day_2_created())){
      shinyjs::disable("day_dynamic_2")
      shinyjs::disable("time_dynamic_2")
    }
  }
  else{
    shinyjs::enable("location_chkbx")
    shinyjs::enable("add_btn")
    if(update_course_tab(department=input$dept,discipline=input$dis,course=input$course,filter="time",day=input$day_static)!="Online"){
        shinyjs::enable("day_static")
        shinyjs::enable("time1")
        if(isolate(counter$n)>=1){
          shinyjs::enable("day_dynamic_1")
          shinyjs::enable("time_dynamic_1")
        }
        if(isolate(counter$n)>=2){
          shinyjs::enable("day_dynamic_2")
          shinyjs::enable("time_dynamic_2")
        }
    }
   
  })
  
  observe(if(!is.null(input$location_chkbx)){
    
    shinyjs::disable("class_type_chkbx")
    shinyjs::disable("day_static")
    shinyjs::disable("time1")
    shinyjs::disable("add_btn")
    if(isolate(counter$n)>=1){
      shinyjs::disable("day_dynamic_1")
      shinyjs::disable("time_dynamic_1")
    }
    if(isolate(counter$n)>=2){
      shinyjs::disable("day_dynamic_2")
      shinyjs::disable("time_dynamic_2")
    }
  }
  else{
    shinyjs::enable("class_type_chkbx")
    shinyjs::enable("add_btn")
    if(update_course_tab(department=input$dept,discipline=input$dis,course=input$course,filter="time",day=input$day_static)!="Online"){
        shinyjs::enable("day_static")
        shinyjs::enable("time1")
        if(isolate(counter$n)>=1){
          shinyjs::enable("day_dynamic_1")
          shinyjs::enable("time_dynamic_1")
        }
        if(isolate(counter$n)>=2){
          shinyjs::enable("day_dynamic_2")
          shinyjs::enable("time_dynamic_2")
        }
      }
  
  })
  
  observe(if((!is.null(input$day_dynamic_2) && input$day_dynamic_2!="Select Day") || (!is.null(input$time_dynamic_2) && input$time_dynamic_2!="Select Time") ){
    
    shinyjs::disable("class_type_chkbx")
    shinyjs::disable("location_chkbx")
  }
  else{
    shinyjs::enable("class_type_chkbx")
    shinyjs::enable("location_chkbx")
  })
  
  observe(if(!is.null(comparison_table$enrollment) && nrow(comparison_table$enrollment[comparison_table$enrollment$Enrolled==0,])!=0){
    showNotification("Warning: No data for some categories selected",duration=15,type="error", id="missing_data")
  })
  
  #limit several checkboxes
  observe(if(length(input$semester)>=1){
    
   if(input$year!=limit_semesters$year_affected){
     choices=c("Fall","Winter","Spring","Summer")
     subElement <- paste0("#semester .checkbox:nth-child(", match(setdiff(choices,input$semester),choices),") label")
     #disable single checkbox of group
     shinyjs::disable(selector=subElement)
     }
    
  }
  else{
    
    
  shinyjs::enable("semester")
    if(input$year==limit_semesters$year_affected){
      
     
      choices=c("Fall","Winter","Spring","Summer")
      subElement <- paste0("#semester .checkbox:nth-child(", match(setdiff(choices,limit_semesters$available_semesters),choices),") label")
      #disable single checkbox of group
      shinyjs::disable(selector=subElement)
    }
    
  })#limit semester to 1
  
  #dynammically add select  input time and day in historical tab
  observeEvent(input$add_btn,{
    counter$n <- counter$n + 1
    
    if(counter$n==1 && counter$rendered==0){
      day_1_created(TRUE)

      output$day_dynamic_1<-renderUI({selectInput(inputId = "day_dynamic_1",
                  label = "Day", choices =update_course_tab(department=input$dept,discipline=input$dis,course=input$course,filter="days"))})
      output$time_dynamic_1<-renderUI({selectInput(inputId = "time_dynamic_1",
                                                   label = "Time",choices=update_course_tab(department=input$dept,discipline=input$dis,
                                                                                            course=input$course,filter="time",day="Select Day") )})
                                                                                      
    }
    else if(counter$n==1 && counter$rendered>=1){
      shinyjs::show("day_dynamic_1")
      shinyjs::show("time_dynamic_1")
    }
    
    if(counter$n==2 && counter$rendered==1 ){
      day_2_created(T)
      output$day_dynamic_2<-renderUI({selectInput(inputId = "day_dynamic_2",
                                                  label = "Day", choices =update_course_tab(department=input$dept,discipline=input$dis,
                                                                                            course=input$course,filter="days"))})
      output$time_dynamic_2<-renderUI({selectInput(inputId = "time_dynamic_2",
                                                   label = "Time",choices=update_course_tab(department=input$dept,discipline=input$dis,
                                                                                            course=input$course,
                                                                                            filter="time",day="Select Day") )})
    }
    else if(counter$n==2 && counter$rendered>=2){
      shinyjs::show("day_dynamic_2")
      shinyjs::show("time_dynamic_2")
    }
    if(counter$rendered<2 && counter$rendered==(counter$n-1)){
      counter$rendered=counter$rendered+1
    }

  })
  observe(if(counter$n>=2){
    shinyjs::disable("add_btn")
  }
  else if(counter$n==0){
    shinyjs::disable("rm_btn")
  }
  else{
    shinyjs::enable("add_btn")
    shinyjs::enable("rm_btn")
  })
  observeEvent(input$rm_btn, {
    if(counter$n==1){
      shinyjs::hide("day_dynamic_1")
      shinyjs::hide("time_dynamic_1")
    }
    if(counter$n==2){
      shinyjs::hide("day_dynamic_2")
      shinyjs::hide("time_dynamic_2")
    }
    if (counter$n > 0) counter$n <- counter$n - 1
    
  })
  
  #----------------------THIS IS FOR CHANGING WHEN DATA BECOOMES AVAILABLE---------------------------------
  
  observeEvent(strtoi(format(Sys.Date())),{
    
    #October-January
    #Semester available: Summer
    current_year=strtoi(format(Sys.Date(), "%Y"))
    if((as.integer(format(Sys.Date(),"%m"))>=10 && as.integer(format(Sys.Date(),"%m"))<=12) || as.integer(format(Sys.Date(),"%m"))==1){
      limit_semesters$available_semesters<-c("Summer")
      
      #Academic_Year: 201(current year)- 201(current year +1)
      limit_semesters$year_affected<- paste0((strtoi(format(Sys.Date(), "%Y"))),"-",(strtoi(format(Sys.Date(), "%Y")))+1)
      
      current_year=strtoi(format(Sys.Date(), "%Y"))
      if( as.integer(format(Sys.Date(),"%m"))==1){
        
        #Academic_Year: 201(current year -1)- 201(current year) 
        current_year=strtoi(format(Sys.Date(), "%Y"))-1
        limit_semesters$year_affected<- paste0((strtoi(format(Sys.Date(), "%Y")))-1,"-",(strtoi(format(Sys.Date(), "%Y"))))
      }
      years_available(paste0((current_year):(current_year-3),"-",
                             ( current_year+1):(current_year-2)))
      
      updateSelectInput(session=session,inputId = "year",label="Academic Year",
                        choices=paste0((current_year):(current_year-3),"-",
                                       ( current_year+1):(current_year-2)))
      
      
      choices=c("Fall","Winter","Spring","Summer") 
      subElement <- paste0("#semester .checkbox:nth-child(", match(setdiff(choices,"Summer"),choices),") label")
      #disable single checkbox of group
      shinyjs::disable(selector=subElement)
      showNotification("Fall data will not be available until grades are fully submitted (February)",type="message")
      
    }
    # February-March
    #Fall data available in February
    #Semester available: Summer,Fall
    else if (as.integer(format(Sys.Date(),"%m"))>=2 && as.integer(format(Sys.Date(),"%m"))<=3){
      #Academic_Year: 201(current year -1)- 201(current year)
      current_year=strtoi(format(Sys.Date(), "%Y"))
      limit_semesters$available_semesters<-c("Summer","Fall")
      limit_semesters$year_affected<- paste0((strtoi(format(Sys.Date(), "%Y")))-1,"-",(strtoi(format(Sys.Date(), "%Y"))))
      years_available(paste0(( current_year-1):(current_year-4),"-",
                             (current_year):(current_year-3)))
      
      updateSelectInput(session=session,inputId = "year",label="Academic Year",
                        choices=paste0(( current_year-1):(current_year-4),"-",
                                       (current_year):(current_year-3)))
      
      
      choices=c("Fall","Winter","Spring","Summer") 
      subElement <- paste0("#semester .checkbox:nth-child(", match(setdiff(choices,limit_semesters$available_semesters),choices),") label")
      #disable single checkbox of group
      shinyjs::disable(selector=subElement)
      showNotification("Winter data will not be available until grades are fully submitted (April)",type="message")
    }
    # April-July
    #Winter data available in April
    #Semester available: Summer,Fall,Winter
    else if (as.integer(format(Sys.Date(),"%m"))>=4 && as.integer(format(Sys.Date(),"%m"))<=7){
      
      #Academic_Year: 201(current year)- 201(current year +1)
      current_year=strtoi(format(Sys.Date(), "%Y"))
      
      limit_semesters$available_semesters<-c("Summer","Fall","Winter")
      limit_semesters$year_affected<- paste0((strtoi(format(Sys.Date(), "%Y")))-1,"-",(strtoi(format(Sys.Date(), "%Y"))))
      years_available(paste0((current_year):(current_year-3),"-",
                             ( current_year+1):(current_year-2)))
      updateSelectInput(session=session,inputId = "year",label="Academic Year",
                        choices=paste0((current_year):(current_year-3),"-",
                                       ( current_year+1):(current_year-2)))

      
      choices=c("Fall","Winter","Spring","Summer") 
      subElement <- paste0("#semester .checkbox:nth-child(", match(setdiff(choices, limit_semesters$available_semesters),choices),") label")
      #disable single checkbox of group
      shinyjs::disable(selector=subElement)
      showNotification("Spring data will not be available until grades are fully submitted (August)",type="message")
    }
    #August -September
    #Spring data becomes available in October
    #Options_Available: Summer,Fall,Winter,Spring
    else{
      limit_semesters$available_semesters<-c("Summer","Fall","Winter","Spring")
      
      #Academic_Year: 201(current year-1)- 201(current year)
      limit_semesters$year_affected<- paste0((strtoi(format(Sys.Date(), "%Y")))-1,"-",(strtoi(format(Sys.Date(), "%Y"))))
      years_available(paste0(( current_year-1):(current_year-5),"-",
                             (current_year):(current_year-4)))


      updateSelectInput(session=session,inputId = "year",label="Academic Year",
                        choices=years_available())
      showNotification("Summer data will not be available until grades are fully submitted (October)",type="message")
    }
    
  })
  
  observeEvent(input$year,{
    if(input$year!=limit_semesters$year_affected && input$year!=" " ){
      shinyjs::enable("semester")
    }
  })

  
}