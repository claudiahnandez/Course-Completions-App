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



###THIS IS WITH SIDEBAR AND PICTURE OF ELAC ON THE BOTTOM OF IT

# Define UI for application that draws a histogram
#shinytheme sets theme to a black background
#title is for the css file will change font


ui <- fluidPage(includeCSS("www/theme.css"),
                
dashboardPage(skin="black",#changes the dashboard bar on top to green

dashboardHeader(title = "ELAC",dropdownMenuOutput('dropdown')),
dashboardSidebar(br(),
       sidebarMenu(
         id = "tabs",
         menuItem("Completions App", tabName = "main"),
         menuItem("Help", tabName = "help"),
         menuItem("Tutorial",tabName = "tutorial")
       )
),#end of sidebar

dashboardBody(shinyjs::useShinyjs(),tags$script(HTML("function clickFunction(link){ 
                                           Shiny.onInputChange('linkClicked',link);}")),#script used to link the notification status to help tab
    
    tabItems(tabItem(tabName = "main",fluidRow(
      #box for everything
      box(title=NULL,width=12,
          #time frame
          div(class="boxx_",box(title = "Time Frame",width=12,status = "danger",br(),
                                column(width=3,div(id="year_",selectInput(inputId = "year",label="Academic Year",selected = "2016-2017",
                                                                          choices=NULL),width="1%")),
                                column(width=3,offset=1,checkboxGroupInput(inputId="semester",label="Semester",
                                                                  choices=c("Fall","Winter","Spring","Summer") ))
                                #column(width=2,offset=3,div(id="elac_img",img(src="www/elac_building.jpg",width=190, heigth=130)))
          )),
          
          
          #box for departments
          box(title=NULL,width=6,status = "primary",solidHeader = TRUE,
              div(class="top_box",selectInput(inputId="dept",label=NULL,choices=c("ALL"),selected="ALL")),
              h3("Department"),height="90%"),
          
          #box for disciplines
          box(title=NULL,width=3,status = "primary",solidHeader = TRUE,
              div(class="top_box",
                  selectInput(inputId = "dis",label=NULL,choices=get_disciplines("All"))),
              h3("Discipline"),height="90%"),
          
          #box for courses
          box(title=NULL,width=3,status="primary",solidHeader=TRUE,
              div(class="top_box",
                  selectInput(inputId = "course",label=NULL,choices=get_courses("All"))),
              h3("Course"),height="90%"),
          
          
          actionButton(inputId="main_submit",label="Submit",width=100)
          
          
      )#box where everything is 
    ),#fluidrOW
    fluidRow(
      tabsetPanel(id="stud_course",
                  tabPanel("Enrollment",br(),textOutput(outputId = "waiting"),
                           div(class="valueBoxes",infoBoxOutput("year_semester",width=3),infoBoxOutput("dept_box",width=3)
                               ,infoBoxOutput("dis_box",width=3),infoBoxOutput("course_box",width=3)),
                           div(class="stats",column(width=12,uiOutput("general_enrollment"))),
                           column(12,offset=10,materialSwitch(inputId = "head_count",status="primary",label ="Show Headcount")),
                           column(3,div(class="Plotly_Graph",plotlyOutput("Age_graph_e",width="105%",height="400px"))),
                           column(3,div(class="Plotly_Graph",plotlyOutput("Gender_graph_e",width="95%",height="400px"))),
                           column(3,div(class="Plotly_Graph",plotlyOutput("Probation_graph_e",width="95%",height="400px"))),
                           column(3,div(class="Plotly_Graph",plotlyOutput("Major_graph_e",width="95%",height="400px"))),
                           column(6,div(class="Plotly_Graph",plotlyOutput("Ethnicity_graph_e",height="420px"))),
                           column(6,div(class="Plotly_Graph",plotlyOutput("Student_Groups_e",height="420px"))),
                           
                           div(class="select_graph",uiOutput('select_table_e')),
                           div(class="table",DT::dataTableOutput("chart_e"))),#end of "enrollment" tab
                  tabPanel(title="Retention",br(),
                           div(class="valueBoxes",infoBoxOutput("year_semester_r",width=3),infoBoxOutput("dept_box_r",width=3),
                               infoBoxOutput("dis_box_r",width=3),infoBoxOutput("course_box_r",width=3)),
                           div(class="stats",column(width=12, uiOutput("overall_retention"))),
                           column(4,div(class="Plotly_Graph",plotlyOutput("Age_graph_r",height="350px"))),
                           column(2,div(class="Plotly_Graph",plotlyOutput("Gender_graph_r",height="350px"))),
                           column(3,div(class="Plotly_Graph",plotlyOutput("Probation_graph_r",height="350px"))),
                           column(3,div(class="Plotly_Graph",plotlyOutput("Major_graph_r",height="350px"))),
                           column(6,div(class="Plotly_Graph",plotlyOutput("Ethnicity_graph_r",height="300px"))),
                           column(6,div(class="Plotly_Graph",plotlyOutput("Student_Groups_r",height="350px"))),
                      
                           div(class="select_graph",uiOutput('select_table_r')),
                           div(class="table",DT::dataTableOutput("chart_r"))),
                  tabPanel(title="Success",br(),
                           div(class="valueBoxes",infoBoxOutput("year_semester_s",width=3),infoBoxOutput("dept_box_s",width=3),
                               infoBoxOutput("dis_box_s",width=3),infoBoxOutput("course_box_s",width=3)),
                           div(class="stats",column(width=12, uiOutput("overall_success"))),
                           column(4,div(class="Plotly_Graph",plotlyOutput("Age_graph_s",height="350px"))),
                           column(2,div(class="Plotly_Graph",plotlyOutput("Gender_graph_s",height="350px"))),
                           column(3,div(class="Plotly_Graph",plotlyOutput("Probation_graph_s",height="350px"))),
                           column(3,div(class="Plotly_Graph",plotlyOutput("Major_graph_s",height="350px"))),
                           column(6,div(class="Plotly_Graph",plotlyOutput("Ethnicity_graph_s",height="300px"))),
                           column(6, div(class="Plotly_Graph",plotlyOutput("Student_Groups_s",height="350px"))),
                           
                           div(class="select_graph",uiOutput('select_table_s')),
                           div(class="table",DT::dataTableOutput("chart_s"))),
                  tabPanel(title="Equity Gaps (Success)",br(),
                           div(class="valueBoxes",infoBoxOutput("year_semester_equity",width=3),infoBoxOutput("dept_box_equity",width=3),
                               infoBoxOutput("dis_box_equity",width=3),infoBoxOutput("course_box_equity",width=3)),
                           column(12,offset=10,materialSwitch(inputId = "standard_line",status="primary",label ="Standard line")),
                           column(4,div(class="Plotly_Graph",plotlyOutput("Age_graph_equity",height="350px"))),
                           column(2,div(class="Plotly_Graph",plotlyOutput("Gender_graph_equity",height="350px"))),
                           column(3,div(class="Plotly_Graph",plotlyOutput("Probation_graph_equity",height="350px"))),
                           column(3,div(class="Plotly_Graph",plotlyOutput("Major_graph_equity",height="350px"))),
                           column(6,div(class="Plotly_Graph",plotlyOutput("Ethnicity_graph_equity",height="300px"))),
                           column(6, div(class="Plotly_Graph",plotlyOutput("Student_Groups_equity",height="300px"))),
                           
                           div(class="select_graph",uiOutput('select_table_equity')),
                           div(class="table",DT::dataTableOutput("chart_equity"))),
                  tabPanel(title="Historical Comparison",br(),
                           #collapsible box DISAGGREGATE
                           div(class="advanced",
                               box(title="Disaggregate",solidHeader = TRUE,status="danger",collapsible=TRUE,width=12,
                                   
                                   tabsetPanel(id="dissaggregate",
                                     tabPanel(title = "Student Demographics", br(),
                                              div(class="box_",box(title=NULL,width=12,status="primary",solidHeader = TRUE,
                                                                   selectInput(inputId = "year_to_check_s",label="How far do you want to compare?",
                                                                               choices=c("2 years", "3 years","4 years","5 years"), width="40%"),
                                                                  radioButtons(inputId="view",label="Mode",
                                                                               choices=c("View Overall Trends (Enrollment, Retention, & Success)","Filter by Categrory"),
                                                                               selected="Filter by Categrory") 
                                                                  )),
                                              
                                              
                                              column(3,selectInput(inputId="category",label="Choose a Category",
                                                                   choices=c("Ethnicity","Age","Student Groups","Major","Academic Probation","Gender"))),
                                              #ethnicity box          
                                              div(id="ethnicity",class="box_s",box(title = NULL,uiOutput("title_main"),
                                                                                  column(5,checkboxGroupInput(inputId = "category_dis", label = NULL,
                                                                                                              choices=NULL)),
                                                                                  column(7,radioButtons(inputId="include_gender",label="Disaggregate by Gender?",
                                                                                                        choices=c("No","Yes"),inline=TRUE)),
                                                                                  #gender box
                                                                                  column(width=7, div(id="gender",class="box_",box(title = "Gender",width=10,
                                                                                                                                   checkboxGroupInput(inputId="gender_chkbx",label=NULL,choices=c("Female","Male"))))),
                                                                                  width=6,status="primary" )),
                                              
                                              
                                              
                                              #clear and submit button
                                              column(width=1,offset=10,actionButton(inputId = "clear_button",label="Clear",width=85)),
                                              column(width=1,actionButton(inputId = "submit_button_student",label="Filter",width=85))
                                     ),
                                     
                                     ##COURSES SECTION<----------------------------------------        
                                     tabPanel(title="Course Attributes",br(),
                                              div(class="box_",box(title=NULL,width=12,status="primary",solidHeader = TRUE,
                                                                   selectInput(inputId = "year_to_check_c",label="How far do you want to compare?",
                                                                               choices=c("2 years", "3 years","4 years","5 years"), width="40%"))),
                                              #Class type  
                                              div(class="box_",box(
                                                                   title = "Class Type", width=3,height=300,
                                                                   textOutput("class_type"),
                                                                   checkboxGroupInput(inputId="class_type_chkbx",label=NULL,
                                                                                      choices=c("CTE","Basic Skills","Deggree Applicable")))),
                                              
                                              
                                              #Location  box
                                              div(class="box_",box(title = "Location",
                                                                   textOutput("location"),
                                                                   checkboxGroupInput(inputId="location_chkbx",label=NULL
                                                                                      ,choices=c("Main Campus","SGEC","Online","Offsite","Hybrid")),
                                                                   width=3,height="300")),                      
                                              #time and day box
                                              div(class="box_",box(title="Start Time & Day",
                                                                   width=6,color="purple",br(),
                                                                   div(class="course_day_time",column(width=4,selectInput(inputId = "day_static",label="Day",
                                                                                                                          c("Select Day","Monday","Tuesday"
                                                                                                                            ,"Wednesday","Thursday","Friday",
                                                                                                                            "Saturday","Sunday"))),
                                                                       column(width=8,selectInput(inputId = "time1",label="Time",
                                                                                                  choices=c("Select Time","Early Morning (Before 10:00AM)"
                                                                                                            ,"Morning (10:00AM-12:00PM)",
                                                                                                            "College Hour(12:00PM-1:30AM)",
                                                                                                            "Early Afternoon (1:30PM-3:30PM)",
                                                                                                            "Afternoon (3:30PM-6:00PM)",
                                                                                                            "Evening (After 6:00PM)")))),
                                                                   column(width=4,uiOutput("day_dynamic_1")),
                                                                   column(width=8,uiOutput("time_dynamic_1")),
                                                                   column(width=4,uiOutput("day_dynamic_2")),
                                                                   column(width=8,uiOutput("time_dynamic_2")),
                                                                   actionButton(inputId = "add_btn",label="Add Day", icon=icon('plus')),
                                                                   actionButton("rm_btn", label="Remove Day", icon=icon('minus'))
                                              )
                                              ),br(),br(),
                                              
                                              #clear and submit button
                                              column(width=1,offset=10,actionButton(inputId = "clear_button_c",label="Clear",width=85)),
                                              column(width=1,actionButton(inputId = "submit_button_courses",label="Filter",width=85))
                                              
                                     ))
                               )),#end of collapsible box DISSAGREGATE
                           #comparison graphs and tables
                           infoBoxOutput("filter",width=3),infoBoxOutput("department_hc",width=3),infoBoxOutput("discipline_hc",width=3),
                           infoBoxOutput("course_hc",width=3),
                           column(width=6,div(class="Plotly_Graph",plotlyOutput("enrollment_comparison_graph",height="500px"))),
                           column(width=6,div(class="hc_table",DT::dataTableOutput("enrollment_comparison_table",width="90%", height="500px"))),
                           column(width=6,div(class="Plotly_Graph",plotlyOutput("retention_comparison_graph",height="500px"))),
                           column(width=6,div(class="hc_table", DT::dataTableOutput("retention_comparison_table",width="100%",height="500px"))),
                           column(width=6,div(class="Plotly_Graph",plotlyOutput("success_comparison_graph",height="500px"))),
                           column(width=6,div(class="hc_table",DT::dataTableOutput("success_comparison_table",width="100%",height="500px"))),
                           column(width=6,div(class="Plotly_Graph",plotlyOutput("equity_index_comparison_graph",height="500px"))),
                           column(width=6,div(class="hc_table",DT::dataTableOutput("equity_index_comparison_table",width="100%",height="500px")))           
                  )#end of "Historical Comparison" tab
                  
      )#tabSETpanel end
    )#end of fluid row
    
    )#End of main tab item
    ,tabItem(tabName = "help",
             
             actionBttn(inputId = "go_main",label="Go Back",style="stretch",color="primary",icon=icon("arrow-left"),size="sm",no_outline = FALSE),
             includeHTML("faq.txt")),
    tabItem(tabName = "tutorial",
            actionBttn(inputId = "tutorial_go_main",label="Go Back",style="stretch",color="primary",icon=icon("arrow-left"),size="sm",no_outline = FALSE),
            h2("Tutorial"),
            box(title="How to use the app?",width=12,solidHeader = TRUE,collapsible = T,collapsed=T,status="success",
                includeHTML("main_page.txt")
                
            ),#end of box of General tutorial
            box(title="How to use Historical Comparison tab?",width=12,solidHeader = TRUE,collapsible = T,collapsed=T,status="success",
                HTML('
                     <h2>Using Historical Comparison Tab...</h2>
                     <figure>
                     <img src="simple_hc.jpg" width=1000 heigth=350  hspace="30" >
                     <figcaption>Data in the Historical Comparison tab may be disaggregated by either <b>Student</b> or 
                                  <b>Course</b> attributes
                    </figcaption>
                     </figure></br>'),
                
                box(title="How to disaggregate by Student attributes?",width=12,solidHeader = TRUE,collapsible = T,collapsed=T, status="info",
                    includeHTML("hc-students.txt")),#STUDENT SUB-TAB
                box(title="How to disaggregate by Course attributes?",width=12,solidHeader = TRUE,collapsible = T,collapsed=T, status="info",
                    includeHTML("hc-course.txt"))#COURSE SUB-TAB
            ),#HISTORICAL COMPARISON BOX
            box(title="How to interpret the graphs?",width=12,solidHeader = TRUE,collapsible = T,collapsed=T,status="success",
                includeHTML("graphs.txt"))
    ))#tabItems
)#end of body


                                            ))