library(readr)
library(dplyr)
library(stringr)
library(RODBC)
library(plotly)
library(shiny)

uid = "ommited to github"
pwd = "ommited to github"

options(stringsAsFactors = FALSE)
courses =NULL
student_info=NULL

total_enrollments=0
total_head_count=0

#get available courses for that year
get_server_data_courses<-function(year,semester=NULL){
  year=str_replace_all(year,"-","")

  connection = odbcConnect("OIEA_SERVER", uid, pwd)

  query=paste0 ("select sections2.Department,sections2.Discipline,sections2.Course,sections2.Section_Number, courses.CTE, courses.Basic_Skills,
                  courses.Degree_Applicable,Start_Period, Meeting_Days,SGEC, Online, Hybrid, Offsite, Main_Campus,sections2.Year_Semester
                  from sections2
                 left join courses on sections2.Curriculum_Id= courses.Curriculum_Id and sections2.Curriculum_Version = courses.Curriculum_Version and 
                 sections2.College = courses.College
                 where sections2.College='E' and sections2.Status='Open' and sections2.PSA = 0 and sections2.Academic_Year='",year,"'")
  

  #use sqlQuery here
  sem=NULL
  if(!is.null(semester)){
    for(i in 1:length(semester)){
      if(semester[i]=="Fall"){
        sem=3
      }else if(semester[i]=="Spring"){
        sem=1
      }else if(semester[i]=="Winter"){
        sem=0
      }else if(semester[i]=="Summer"){
        sem=2
      }
    }
  }

  
  if(!is.null(semester)){
    query=paste0(query," and sections2.Semester in (",paste0(paste0("'",sem,"'"), collapse = ","),")")
  }
  #write.table(query, "mydata.txt")#used for exporting text file
    courses<<- sqlQuery(connection, query, stringsAsFactors = FALSE)
  
  odbcClose(connection)
 
  return (NULL)
}

get_server_data_student_info<-function(year,semester,department,discipline,course){

  for(i in 1:length(year)){
    year[i]=str_replace_all(year[i],"-","")
  }

  year=paste0(paste0("'",year,"'"), collapse = ",")
  sem=NULL
  if(!is.null(semester)){
    for(i in 1:length(semester)){
      if(semester[i]=="Fall"){
        sem=3
      }else if(semester[i]=="Spring"){
        sem=1
        sem=0
      }else if(semester[i]=="Summer"){
        sem=2
      }
    }
  }
 
query="select enrollments.Student_Id, Ethnicity, Gender, sections2.Academic_Year, enrollments.Year_Semester, sections2.Course, Grade,
case when Grade in ('A','B','C','P') then 1 else 0 end as Success,
case when Grade != 'W' then 1 else 0 end as Retained, sections2.Department, Discipline_Name, 
Start_Period, Meeting_Days,SGEC, Online, Hybrid, Offsite, Main_Campus, BOGG, Pell_Grant, Military,
Veteran, EOPS, DSPS, CalWorks,Term_Age,Academic_Probation,courses.CTE, courses.Basic_Skills,courses.Degree_Applicable,
Major_Declared,sections2.Year_Semester
from enrollments
left join sections2 on enrollments.Section_Number = sections2.Section_Number and
enrollments.Year_Semester = sections2.Year_Semester and enrollments.College = sections2.College 
left join students on enrollments.Student_Id = students.Student_Id
left join student_profiles on enrollments.Student_Id = student_profiles.Student_Id and 
enrollments.Year_Semester = student_profiles.Year_Semester and
enrollments.College = student_profiles.College
left join courses on sections2.Curriculum_Id= courses.Curriculum_Id and
sections2.Curriculum_Version = courses.Curriculum_Version and 
sections2.College = courses.College
and enrollments.College = sections2.College 
where sections2.College = 'E' and sections2.Status='Open' and sections2.PSA = 0"
  
query=paste0(query," and sections2.Academic_Year in (", year,")")

if(!is.null(semester)){
  query=paste0(query," and sections2.Semester in ('",sem,"')")
}
if(department!="ALL"){
  query=paste0(query," and sections2.Department='",department,"'")
}
if(department=="Non-Credit Program" || department=="Escalante Program" ){
  query=paste0(query," and enrollments.Status='Active'")
}else{
  query=paste0(query," and Grade!='E'")
}
if(discipline!="ALL"){
  query=paste0(query," and courses.Discipline='",discipline,"'")
}
if(course!="ALL"){
  query=paste0(query," and courses.Course='",course,"'")
}


query=str_replace_all(query, "[\r\n]" , " ")
print("Trying to write query")
write(query, file = "student_info_completion_app.txt")

connection = odbcConnect("OIEA_SERVER", uid, pwd)
student_info<<-sqlQuery(connection, query,stringsAsFactors = FALSE)
odbcClose(connection)

student_info<<-student_info[complete.cases(student_info),]

temp<-setdiff(student_info,student_info[complete.cases(student_info$Major_Declared),])
if(nrow(temp)!=0){
temp$Major_Declared=0
student_info<<-rbind(student_info[complete.cases(student_info),],temp[complete.cases(temp),])}

total_enrollments<<-nrow(student_info)
total_head_count<<-length(unique(student_info$Student_Id))

return (student_info) 
}

#get departments
get_departments = function(year, semester=NULL)
{

  get_server_data_courses(year,semester)
  courses<<-courses[courses$Department!='TUTORING',]
  courses[, 1] <<- sapply(courses[, 1], as.character)
  return(c("ALL",sort(unique(courses[,1]))))
}

#get disciplines based on departments
get_disciplines=function(department="ALL"){
  if(department=="ALL"){
    return("ALL")
  }
  else{
    courses[, 2] <<- sapply(courses[, 2], as.character)
    dept=courses[courses$Department==department,]

    return(c("ALL",sort(unique(dept[,2]))))
  }
  
  
}

#get courses based on departments and discipline
get_courses=function(discipline="ALL"){
  if(discipline=="ALL"){
   return("ALL")
  }
  else{
    courses[, 3] <<- sapply(courses[, 3], as.character)
    course=courses[courses$Discipline==discipline,]
    return(c("ALL",sort(unique(course[,3]))))
  }
   
  
}

get_default<-function(year,semester,department,discipline,course){
  

  get_server_data_student_info(year,semester,department,discipline,course)
  if(is.null(student_info)){
    return(NULL)
  }
  #change the semester to a number

  result=student_info[student_info$Academic_Year==year,]
  
  result=student_info
  
  #get students who passed
  passed=result[result$Grade=="A" | result$Grade=="B" | result$Grade=="C" | result$Grade=="P",]
  dropped=result[result$Grade=="W",]
  result=list(result,dropped,passed)

  return(result)
}

#check for current year and possibly semester to check if data is up
student_profiles_is_working<-function(year,semester){
  if(!is.null(semester))
    check_available<-c(get_year_semester(year,semester))
  else{
    check_available<-c(get_year_semester(year,"Summer"),get_year_semester(year,"Fall"),
                       get_year_semester(year,"Winter"),get_year_semester(year,"Spring"))
  }
  
  connection = odbcConnect("OIEA_SERVER", uid, pwd)
  for(i in 1:length(check_available)){
    query<-paste0("Select top 1 * from student_profiles where College='E' and Year_Semester=",check_available[i])
    data_check<- sqlQuery(connection, query, stringsAsFactors = FALSE)
    
    if(is.null(data_check) || nrow(data_check)==0){
      odbcClose(connection)
      return(F)
    }
  }
  
  odbcClose(connection)
  return(T)
    
}

update_course_tab<-function(department,discipline,course,filter,time=NULL,day=NULL){
 if(!is.null(department) && !is.null(discipline) && !is.null(course) && !is.null(filter)  ){
if(department=="ALL"){
  course="ALL"
  discipline="ALL"
}

if(discipline=="ALL"){
  course="ALL"
}
  
  if(course!="ALL"){
    temp=courses[courses$Department==department & courses$Discipline==discipline & courses$Course==course,]
  }
  else if(discipline!="ALL"){
    temp=courses[courses$Department==department & courses$Discipline==discipline,]
  }
  else if(department!="ALL"){
    temp=courses[courses$Department==department,]
  }
  else{
    if(filter=="days"){
      return(c("Select Day",unique(courses[courses$Meeting_Days!="Online" & courses!="TBA",]$Meeting_Days)))
    }
    if(filter=="time"){
      return(c("Select Time","Early Morning (Before 10:00AM)","Morning (10:00AM-12:00PM)",
             "College Hour(12:00PM-1:30AM)","Early Afternoon (1:30PM-3:30PM)",
             "Afternoon (3:30PM-6:00PM)","Evening (After 6:00PM)"))
    }
    if(filter=="class type"){
      return(c("CTE","Basic Skills","Deg.Appl"))}
    return(NULL)
  }
   result=vector(mode="character",length=0)
   
   #narrow class type
   if(filter=="class type"){
    if(nrow(temp[temp$CTE==1,])!=0){
      result[length(result)+1]="CTE"
    }
    if(nrow(temp[temp$Basic_Skills==1,])!=0){
      result[length(result)+1]="Basic Skills"
    }
    if(nrow(temp[temp$Degree_Applicable==1,])!=0){
      result[length(result)+1]="Degree Applicable"
    }
   }
   #narrow location
   else if(filter=="location"){
    if(nrow(temp[temp$Main_Campus==1,])!=0){
      result[length(result)+1]="Main Campus"
    }
    if(nrow(temp[temp$SGEC==1,])!=0){
      result[length(result)+1]="SGEC"
    }
    if(nrow(temp[temp$Online==1,])!=0){
      result[length(result)+1]="Online"
    }
    if(nrow(temp[temp$Hybrid==1,])!=0){
      result[length(result)+1]="Hybrid"
    }
    if(nrow(temp[temp$Offsite==1,])!=0){
      result[length(result)+1]="Offsite"
    }
    
   }
   #narrow time and day
   else if(filter =="days"){
      result=c("Select Day",unique(temp[temp$Meeting_Days!="Online" & temp$Meeting_Days!="TBA",]$Meeting_Days))
      
   }
   else if(filter =="time"){
     if(!is.null(day) && day=="Select Day"){

      x<-unique(temp[temp$Meeting_Days!="Online" & temp$Meeting_Days!="TBA",]$Start_Period)
     if(length(x)==0){
        return(c("Online"))
     }
      
      for(i in 1:length(x)){
        if(x[i]=="Afternoon"){
          x[i]="Afternoon (3:30PM-6:00PM)"
        }
        if(x[i]=="Early Afternoon" ){
          x[i]= "Early Afternoon (1:30PM-3:30PM)"
        }
        if(x[i]=="College Hour"){
          x[i]= "College Hour(12:00PM-1:30AM)"
        }
        if(x[i]=="Morning"){
          x[i]= "Morning (10:00AM-12:00PM)"
        }
        if(x[i]=="Early Morning"){
          x[i]="Early Morning (Before 10:00AM)" 
        }
        if(x[i]=="Evening"){
          x[i]= "Evening (After 6:00PM)"
        }
      }
       return(c("Select Time",x))
     }

     result[length(result)+1]="Select Time"
     if(nrow(temp[temp$Start_Period=="Afternoon" & temp$Meeting_Days %in% day,])!=0){
       result[length(result)+1]="Afternoon (3:30PM-6:00PM)"
     }
     if(nrow(temp[temp$Start_Period=="Evening" & temp$Meeting_Days %in% day,])!=0){
       result[length(result)+1]="Evening (After 6:00PM)"
     }
     if(nrow(temp[temp$Start_Period=="College Hour" & temp$Meeting_Days %in% day,])!=0){
       result[length(result)+1]="College Hour(12:00PM-1:30AM)"
     }
     if(nrow(temp[temp$Start_Period=="Morning" & temp$Meeting_Days %in% day,])!=0){
       result[length(result)+1]="Morning (10:00AM-12:00PM)"
     }
     if(nrow(temp[temp$Start_Period=="Early Morning" & temp$Meeting_Days %in% day,])!=0){
       result[length(result)+1]="Early Morning (Before 10:00AM)"
     }
     if(nrow(temp[temp$Start_Period=="Early Afternoon" & temp$Meeting_Days %in% day,])!=0){
       result[length(result)+1]="Early Afternoon (1:30PM-3:30PM)"
     }
     
   }

  return(result)
 }
  else{
    return(NULL)
  }
}

get_academic_probation_students_<-function(enrolled,dropped=NULL,passed=NULL,year,status="enrolled",type=NULL){
  
  year=str_replace_all(year,"-","")
  
  #get all the academic probation people
  temp_=enrolled[enrolled$Academic_Probation!="Good standing" & 
                  enrolled$Academic_Probation!="New or returning student - does not apply",]
  #not sure if relevant at this point
  result=data.frame(Academic_Year=year,Enrolled=0,Status="Probation")

  if(nrow(temp_)!=0){
    result=ddply(temp_, .(Academic_Year), nrow) #count all students by year, will result in all studens in probation
    colnames(result)[2]="Enrolled"
    result$Status="Probation"} #replace count variable V1 with sstudents
 
  #get all good standing
    temp=ddply(enrolled[enrolled$Academic_Probation=="Good standing" | 
                            enrolled$Academic_Probation=="New or returning student - does not apply",]
                 , .(Academic_Year), nrow) #count all students by year, will result in all studens in probation
    colnames(temp)[2]="Enrolled"#replace count variable V1 with Enrolled
    temp$Status="Good Standing" 

  result=rbind(result,temp)
   result=result[c(1,3,2)]#reorder columns to have enrolled after status
   
   if(status=="enrolled"){
     total_enrollment=length(unique(enrolled$Student_Id))
     result$Head_Count=0
     result$Head_Percent<-0
     temp=enrolled[enrolled$Academic_Probation=="Good standing" | 
                     enrolled$Academic_Probation=="New or returning student - does not apply",]
     x<-data.frame(Status=c("Probation","Good Standing"),Head_Count=c(length(unique(temp_$Student_Id)),length(unique(temp$Student_Id))))
     
     for(i in 1:nrow(result)){
       result$Head_Count[i]=x[x$Status == result$Status[i],]$Head_Count
     }
     
     result$Head_Percent<-round((result$Head_Count/total_enrollment)*100,digits=1)

     result<-sensor(data=result,column="Head_Count")
     
   }

  if(status=="filter"){
    for(i in 1:length(type)){
      if(type[i]=="No"){
        type[i]="Good Standing"
      }
      else{
        type[i]="Probation"
      }
    }

    result_f=data.frame(Academic_Year=numeric(),Status=character(), Enrolled=numeric(),stringsAsFactors=FALSE)
    for(i in 1:length(type)){
      result_f=rbind(result_f, data.frame(Academic_Year=year,Status=type[i], Enrolled=0,stringsAsFactors=FALSE))
   
    }

    for(i in 1:nrow(result_f)){
      if(nrow(result[result$Academic_Year==result_f$Academic_Year[i] & result$Status==result_f$Status[i],])!=0)
        result_f$Enrolled[i]=result[result$Academic_Year==result_f$Academic_Year[i] & result$Status==result_f$Status[i],]$Enrolled
    }

    result<-result_f

  }



  #<---------RETENTION-------------------------|
  if(status=="retention" || status=="filter"){
    ap_students_w=ddply(dropped[dropped$Academic_Probation!="Good standing" & 
                             dropped$Academic_Probation!="New or returning student - does not apply",], 
                        .(Academic_Year), nrow)#get students in probation with W's
    good_standing_students_w=ddply(dropped[dropped$Academic_Probation=="Good standing" | 
                                       dropped$Academic_Probation=="New or returning student - does not apply",],
                                   .(Academic_Year), nrow)#get good standing students with W's

    #add retainted number of students
    result$Retained=result$Enrolled

    for(i in 1:nrow(result)){
      if(result$Status[i]=="Probation" && nrow(ap_students_w[ap_students_w$Academic_Year==result$Academic_Year[i],])!=0){
        result$Retained[i]=result$Enrolled[i]-ap_students_w[ap_students_w$Academic_Year==result$Academic_Year[i],]$V1
      }
      else if(result$Status[i]=="Good Standing" && nrow(good_standing_students_w[good_standing_students_w$Academic_Year==result$Academic_Year[i],])!=0 ){
        result$Retained[i]=result$Enrolled[i]-good_standing_students_w[good_standing_students_w$Academic_Year==result$Academic_Year[i],]$V1
      }
    }

    #add retention rate
    result$Retention_Rate=round((result$Retained/result$Enrolled)*100,digits=1)
    for(i in 1:length(result$Retention_Rate)){
      if(is.nan(result$Retention_Rate[i])){
        result$Retention_Rate[i]=0
      }
    }

    #<----------------SUCCESS------------------|
  }
  if(status=="success" || status=="filter" || status=="equity"){

    ap_student_sucess= ddply(passed[passed$Academic_Probation!="Good standing" & 
                                    passed$Academic_Probation!="New or returning student - does not apply",],
                            .(Academic_Year), nrow)
    good_standing_success= ddply(passed[passed$Academic_Probation=="Good standing" | 
                                         passed$Academic_Probation=="New or returning student - does not apply",],
                                .(Academic_Year), nrow)
     result$Succeeded=0
    for(i in 1:nrow(result)){
      if(result$Status[i]=="Probation" && nrow(ap_student_sucess[ap_student_sucess$Academic_Year==result$Academic_Year[i],])!=0){
        result$Succeeded[i]=ap_student_sucess[ap_student_sucess$Academic_Year==result$Academic_Year[i],]$V1
      }else if(result$Status[i]=="Good Standing" &&  nrow(good_standing_success[good_standing_success$Academic_Year==result$Academic_Year[i],])!=0){
        result$Succeeded[i]=good_standing_success[good_standing_success$Academic_Year==result$Academic_Year[i],]$V1
      }
    }
 
    result$Success_Rate=round((result$Succeeded/result$Enrolled)*100,digits=1)
    for(i in 1:length(result$Success_Rate)){
      if(is.nan(result$Success_Rate[i])){
        result$Success_Rate[i]=0
      }
    }
 
  }

   if(status!="filter"){
       rows_to_delete<-result[result$Enrolled==0,]
       result=setdiff(result,rows_to_delete)
   }

  
  if(status=="equity" || status=="filter"){
    if(status!="filter"){ 
      result$Success_Rate<-NULL
    total_succeeded=sum(result$Succeeded)
    total_enrolled=sum(result$Enrolled)
    result$Equity_Index=round((result$Succeeded/total_succeeded)/(result$Enrolled/total_enrolled),digits=2)
    }else{
      total_enrolled=ddply(enrolled,.(Academic_Year),nrow)
      total_succeeded=ddply(passed,.(Academic_Year),nrow)
      result$Equity_Index=0
      for(i in 1:nrow(result)){
        if(any(total_succeeded[total_succeeded$Academic_Year==result$Academic_Year[i],]$V1) &&
           any(total_enrolled[total_enrolled$Academic_Year==result$Academic_Year[i],]$V1))
        result$Equity_Index[i]=round((result$Succeeded[i]/total_succeeded[total_succeeded$Academic_Year==result$Academic_Year[i],]$V1)
                                     /(result$Enrolled[i]/total_enrolled[total_enrolled$Academic_Year==result$Academic_Year[i],]$V1)
                                     ,digits=2) 
      }
      
    }
   
  }

  #sensor data
  if(status=="retention" || status=="success" ){
    result=sensor_data(result,2)
  }
  else if(status=="enrolled"){
    result$Status=paste(result$Status,"     ")
  }else if( status=="equity"){
    result=sensor_comparison(result,type="success")
  }

    academic_year=as.character(unique(result$Academic_Year))
    result$Academic_Year=paste( substr(academic_year,1,4),"-",substr(academic_year,5,8))
  


  return(result)
}

get_gender_graph<-function(enrolled,dropped=NULL,passed=NULL, status="enrolled",Gender=NULL, years=NULL){

  if(status=="filter"){
    years=str_replace_all(years,"-","")
    for(i in 1:length(Gender))
        Gender[i]=substr(Gender[i],1,1)
      
    
    result=data.frame(Academic_Year=numeric(),Gender=character(), Enrolled=numeric(),stringsAsFactors=FALSE)
    for(i in 1:length(Gender)){
      result=rbind(result, data.frame(Academic_Year=years,Gender=Gender[i], Enrolled=0,stringsAsFactors=FALSE))
    }
    
    for(i in 1:nrow(result)){
      result$Enrolled[i]=nrow(enrolled[enrolled$Academic_Year==result$Academic_Year[i] & enrolled$Gender==result$Gender[i],])
    }
  }
  else{
    result<-ddply(enrolled, .(Gender, Academic_Year), nrow)
    colnames(result)[3]<-"Enrolled"
    #result[, 2] <- sapply(result[, 1], as.character)
    result=result[c(2,1,3)]
    if(status=="enrolled"){
      total_enrollment=length(unique(enrolled$Student_Id))
      result$Head_Count=0
      result$Head_Percent<-0
      result$Head_Count<-c(length(unique(enrolled[enrolled$Gender=="F",]$Student_Id)),length(unique(enrolled[enrolled$Gender=="M",]$Student_Id)))
      result$Head_Percent<-round((result$Head_Count/total_enrollment)*100,digits=1)
      result<-sensor(data=result,column="Head_Count")
      
    }
  }
  
  
  #<---------RETENTION-------------------------|
  if(status=="retention" || status=="filter"){
    retained=ddply(dropped, .(Gender, Academic_Year), nrow)
    result$Retained=result$Enrolled

    for(i in 1:nrow(result)){
      if(nrow(retained[retained$Academic_Year==result$Academic_Year[i] & retained$Gender==result$Gender[i],])!=0){                 
         result$Retained[i]=result$Enrolled[i]-retained[retained$Academic_Year==result$Academic_Year[i] 
                                                        & retained$Gender==result$Gender[i],]$V1}
      }
    
    result$Retention_Rate= round((result$Retained/result$Enrolled)*100,digits=1)
    for(i in 1:length(result$Retention_Rate)){
      if(is.nan(result$Retention_Rate[i])){
        result$Retention_Rate[i]=0
      }
    }
    
  #<----------------SUCCESS------------------|
  }
  if(status=="success" || status=="filter" || status=="equity"){
    temp=ddply(passed,.(Gender,Academic_Year),nrow)
    
    result$Succeeded=0
    for(i in 1:nrow(result)){
      if(nrow(temp[temp$Academic_Year==result$Academic_Year[i] & temp$Gender==result$Gender[i],])!=0){                 
        result$Succeeded[i]=temp[temp$Academic_Year==result$Academic_Year[i]  & temp$Gender==result$Gender[i],]$V1}
                                                     
    }
    result$Success_Rate=round((result$Succeeded/result$Enrolled)*100,digits=1)
    for(i in 1:length(result$Success_Rate)){
      if(is.nan(result$Success_Rate[i])){
        result$Success_Rate[i]=0
      }
    }
  }

  for(i in 1:nrow(result)){
    if(result$Gender[i]=='F'){
      result$Gender[i]='Female'
    }else{
      result$Gender[i]='Male'
    }
  }
  
  
  if(status=="equity" || status=="filter"){
    if(status!="filter"){ 
      result$Success_Rate<-NULL
    total_succeeded=sum(result$Succeeded)
    total_enrolled=sum(result$Enrolled)
    result$Equity_Index=round((result$Succeeded/total_succeeded)/(result$Enrolled/total_enrolled),digits=2)
    }
    else{
      total_enrolled=ddply(enrolled,.(Academic_Year),nrow)
      total_succeeded=ddply(passed,.(Academic_Year),nrow)
      result$Equity_Index=0
      for(i in 1:nrow(result)){
        if(any(total_succeeded[total_succeeded$Academic_Year==result$Academic_Year[i],]$V1 &&
               any(total_enrolled[total_enrolled$Academic_Year==result$Academic_Year[i],]$V1))){
           result$Equity_Index[i]=round((result$Succeeded[i]/total_succeeded[total_succeeded$Academic_Year==result$Academic_Year[i],]$V1)
                                     /(result$Enrolled[i]/total_enrolled[total_enrolled$Academic_Year==result$Academic_Year[i],]$V1)
                                     ,digits=2) 
        }
       
      }
      
    }

  }
  

  #sensor data
  if(status=="retention" || status=="success" ){
    result=sensor_data(result,2)
  }else if( status=="equity"){
    result=sensor_comparison(result,type="success")
  }

    academic_year=as.character(unique(result$Academic_Year))
    result$Academic_Year=paste( substr(academic_year,1,4),"-",substr(academic_year,5,8))
  return(result)
}

get_ethnicity_graph<-function(enrolled,dropped=NULL,passed=NULL,status="enrolled",ethnicity=NULL,years=NULL,gender=NULL){
  #enrolled=ddply(enrolled, .(Student_Id), head, n = 1) 
  if(!is.null(gender)){
    Gender=gender
    for(i in 1:length(gender)){
      Gender[i]=substr(gender[i],1,1)
    }
      
  }

  result=NULL
  if(status=="filter"){
    years=str_replace_all(years,"-","")
    if(!is.null(gender)){
      result=data.frame(Academic_Year=numeric(),Ethnicity=character(),Gender=character(), Enrolled=numeric(),stringsAsFactors = FALSE)
    }
    else{
       result=data.frame(Academic_Year=numeric(),Ethnicity=character(), Enrolled=numeric())
    }
   
    
    for(i in 1:length(ethnicity)){
      if(!is.null(gender)){#if gender is needed
        for(j in 1:length(gender))
          result=rbind(result, data.frame(Academic_Year=years,Ethnicity_Gender=paste0(ethnicity[i],"-",gender[j]),
                                          Ethnicity=ethnicity[i],Gender=Gender[j], Enrolled=0))
      }
      else{
        result=rbind(result, data.frame(Academic_Year=years,Ethnicity=ethnicity[i], Enrolled=0))
      }
      
    }
    for(i in 1:nrow(result)){
      if(!is.null(gender)){#if gender is needed
        result$Enrolled[i]=nrow(enrolled[enrolled$Academic_Year==result$Academic_Year[i] & enrolled$Ethnicity==result$Ethnicity[i]
                                         & enrolled$Gender==result$Gender[i],])
      }else{
        result$Enrolled[i]=nrow(enrolled[enrolled$Academic_Year==result$Academic_Year[i] & enrolled$Ethnicity==result$Ethnicity[i],])
      }
      
    }
  }
  else{
    result<-ddply(enrolled, .( Academic_Year,Ethnicity), nrow)
    colnames(result)[3]<-"Enrolled"
    if(status=="enrolled"){
      total_enrollment=length(unique(enrolled$Student_Id))
      temp<-ddply(enrolled, .( Student_Id), head,n=1)
      temp<-ddply(temp, .( Academic_Year,Ethnicity), nrow)
      result$Head_Count=0
      for(i in 1:nrow(result)){
        result$Head_Count[i]=temp[temp$Ethnicity==result$Ethnicity[i],]$V1
      }
      result$Head_Percent<-round((result$Head_Count/total_enrollment)*100,digits=1)
      result<-sensor(data=result,column="Head_Count")
    }
  }
  #---------RETENTION-------------------------|
  if(status=="retention" || status=="filter"){
    if(!is.null(gender)){
      temp<-ddply(dropped, .(Ethnicity, Academic_Year,Gender), nrow)
    }
    else{
      temp<-ddply(dropped, .(Ethnicity, Academic_Year), nrow)
    }
      
      if(status=="filter"){
        temp=temp[temp$Ethnicity %in% ethnicity,]
      }
      
      retained=vector(mode="numeric",length=nrow(result))
    
      
      for(i in 1:nrow(result)){
        if(!is.null(gender)){
          ws=temp[temp$Ethnicity==result$Ethnicity[i] & temp$Academic_Year==result$Academic_Year[i] & 
                    temp$Gender==result$Gender[i],]
        }
        else{
          ws=temp[temp$Ethnicity==result$Ethnicity[i] & temp$Academic_Year==result$Academic_Year[i],]
        }
        
        if(nrow(ws)!=0){
          retained[i]=as.numeric(result$Enrolled[i])-as.numeric(ws$V1)
        }
        else{
          retained[i]=as.numeric(result$Enrolled[i])
        }
        
      }
      
      result$Retained=retained
      result$Retention_Rate=round((result$Retained/result$Enrolled)*100,digits=1)
      for(i in 1:length(result$Retention_Rate)){
        if(is.nan(result$Retention_Rate[i])){
          result$Retention_Rate[i]=0
        }
      }

    #<----------------SUCCESS------------------|
  }

  if(status=="success" || status=="filter" || status=="equity"){

    if(!is.null(gender)){
      temp<-ddply(passed, .(Ethnicity, Academic_Year,Gender), nrow)
    }
    else{
      temp<-ddply(passed, .(Ethnicity, Academic_Year), nrow)
    }
    
    if(status=="filter"){
      temp=temp[temp$Ethnicity %in% ethnicity,]
    }
  
    succeeded=vector(mode="numeric",length=nrow(result))

    for(i in 1:nrow(result)){
      if(!is.null(gender)){
        ws=temp[temp$Ethnicity==result$Ethnicity[i] & temp$Academic_Year==result$Academic_Year[i]
                & temp$Gender==result$Gender[i],]
      }
      else{
        ws=temp[temp$Ethnicity==result$Ethnicity[i] & temp$Academic_Year==result$Academic_Year[i],]
      }
      
      if(nrow(ws)!=0){
        succeeded[i]=as.numeric(ws$V1)
      }
      else{
        succeeded[i]=0
      }
    }
    
    result$Succeeded=succeeded
   
    result$Success_Rate=round((result$Succeeded/result$Enrolled)*100,digits=1)
    for(i in 1:length(result$Success_Rate)){
      if(is.nan(result$Success_Rate[i])){
        result$Success_Rate[i]=0
      }
    }
  }

  #sensor data
  if(status=="equity" ||status=="filter"){
    if(status!="filter"){
      result$Success_Rate<-NULL
      
    total_succeeded=sum(result$Succeeded)
    total_enrolled=sum(result$Enrolled)
   
    result$Equity_Index=round((result$Succeeded/total_succeeded)/(result$Enrolled/total_enrolled),digits=2)
  
    }else{
      total_enrolled=ddply(enrolled,.(Academic_Year),nrow)
      total_succeeded=ddply(passed,.(Academic_Year),nrow)
      result$Equity_Index=0
      for(i in 1:nrow(result)){
        if(any(total_succeeded[total_succeeded$Academic_Year==result$Academic_Year[i],]$V1) &&
               any(total_enrolled[total_enrolled$Academic_Year==result$Academic_Year[i],]$V1)){
          result$Equity_Index[i]=round((result$Succeeded[i]/total_succeeded[total_succeeded$Academic_Year==result$Academic_Year[i],]$V1)
                                       /(result$Enrolled[i]/total_enrolled[total_enrolled$Academic_Year==result$Academic_Year[i],]$V1)
                                       ,digits=2) 
        }
        
      }
      
    }

  }
  if(status=="retention" || status=="success" ){
    result=sensor_data(result,2)
  }
  else if(status=="enrolled"){
    result$Ethnicity=paste(result$Ethnicity,"    ")
  }else if( status=="equity"){
    result=sensor_comparison(result,type="success")
  }
  
  
    academic_year=as.character(unique(result$Academic_Year))
    result$Academic_Year=paste( substr(academic_year,1,4),"-",substr(academic_year,5,8))
  if(status=="filter" && !is.null(gender)){
    result$Ethnicity=NULL
    result$Gender=NULL
  }

  return(result)
}

get_age_range_graph<-function(enrolled,year=NULL,status="enrolled",age_range=null){
  year=str_replace_all(year,"-","")
 
  Age=c("Under 18","18-19","20-24","25-29","30-39","40 & older")
  range18=enrolled[enrolled$Term_Age<18,]
  range1819=enrolled[enrolled$Term_Age>=18 & enrolled$Term_Age<=19,]
  range2024=enrolled[enrolled$Term_Age>=20 & enrolled$Term_Age<=24,]
  range2529=enrolled[enrolled$Term_Age>=25 & enrolled$Term_Age<=29,]
  range3039=enrolled[enrolled$Term_Age>=30 & enrolled$Term_Age<=39,]
  range40=enrolled[enrolled$Term_Age>=40,]
  temp<-list(range18,range1819,range2024,range2529,range3039,range40)
  ages<-data.frame(Academic_Year=numeric(),Enrolled=numeric(),Age=character(),stringsAsFactors=FALSE)
  for(i in 1:length(Age)){
    x<-ddply(temp[[i]], .(Academic_Year), nrow)
    if(nrow(x)!=0){
      names(x)[2]<-"Enrolled"
      x$Age<-Age[i]
    }
    else{
    
      x=data.frame(Academic_Year=as.numeric(year),Enrolled=0,Age=Age[i])
      
    }
    if(length(setdiff(year,x$Academic_Year))!=0){
      x=rbind(x,data.frame(Academic_Year=as.numeric(setdiff(year,x$Academic_Year)),Enrolled=0,Age=Age[i]))
    }
    x=x[c(1,3,2)]
    ages=rbind(ages,x)

  }
 
  if(status=="filter"){
    ages=ages[ages$Age %in% age_range,]
  }

  if(status=="enrolled"){
    total_enrollment=length(unique(enrolled$Student_Id))
    ages$Head_Count=0
    ages$Head_Percent<-0
    ages$Head_Count<-c(length(unique(range18$Student_Id)),length(unique(range1819$Student_Id)),length(unique(range2024$Student_Id)),
               length(unique(range2529$Student_Id)),length(unique(range3039$Student_Id)),length(unique(range40$Student_Id)))
    ages$Head_Percent<-round((ages$Head_Count/total_enrollment)*100,digits=1)
    ages<-sensor(data=ages,column="Head_Count")
  }
  #<---------RETENTION-------------------------
  if(status=="retention" || status=="filter"){
    
    ws=data.frame(Academic_Year=numeric(),Retained<-numeric(),Age=character())
    for(i in 1:length(Age)){
      x<-ddply(temp[[i]][temp[[i]]$Grade=="W",], .(Academic_Year), nrow)
      if(nrow(x)!=0){
        names(x)[2]<-"Retained"
        x$Age<-Age[i]
      }
      else{
        x=data.frame(Academic_Year=as.numeric(year),Retained=0,Age=Age[i])
      }
      if(length(setdiff(year,x$Academic_Year))!=0){
        x=rbind(x,data.frame(Academic_Year=as.numeric(setdiff(year,x$Academic_Year)),Retained=0,Age=Age[i]))
      }
      x=x[c(1,3,2)]
      ws=rbind(ws,x)
    }
    
    if(status=="filter"){
      ws=ws[ws$Age %in% age_range,]
    }
    ages$Retained=0
    for(i in 1:nrow(ages)){
      ages$Retained[i]=ages$Enrolled[i]-ws[ws$Academic_Year==ages$Academic_Year[i] & ws$Age==ages$Age[i],]$Retained
    }
    
    ages$Retention_Rate=round((ages$Retained/ages$Enrolled)*100,digits=1)
    for(i in 1:length(ages$Retention_Rate)){
      if(is.nan(ages$Retention_Rate[i])){
        ages$Retention_Rate[i]=0
      }
    }
    

    
  }#<----------------SUCCESS------------------
  if(status=="success" || status=="filter" || status=="equity"){
   
    success=data.frame(Academic_Year=numeric(),Succeeded<-numeric(),Age=character())
    for(i in 1:length(Age)){
      x<-ddply(temp[[i]][temp[[i]]$Grade=="A" | temp[[i]]$Grade=="B" | temp[[i]]$Grade=="C" | temp[[i]]$Grade=="P",], .(Academic_Year), nrow)
      if(nrow(x)!=0){
        names(x)[2]<-"Succeeded"
        x$Age<-Age[i]
      }
      else{
        x=data.frame(Academic_Year=as.numeric(year),Succeeded=0,Age=Age[i])
      }
      if(length(setdiff(year,x$Academic_Year))!=0){
        x=rbind(x,data.frame(Academic_Year=as.numeric(setdiff(year,x$Academic_Year)),Succeeded=0,Age=Age[i]))
      }
      x=x[c(1,3,2)]
      success=rbind(success,x)
    }
   
    ages$Succeeded=0
    for(i in 1:nrow(ages)){
      ages$Succeeded[i]=success[success$Academic_Year==ages$Academic_Year[i] & success$Age==ages$Age[i],]$Succeeded
    }
    ages$Success_Rate=round((ages$Succeeded/ages$Enrolled)*100,digits=1)
    for(i in 1:length(ages$Success_Rate)){
      if(is.nan(ages$Success_Rate[i])){
        ages$Success_Rate[i]=0
      }
    }
  }
  
  if(status=="equity" || status=="filter"){
    if(status!="filter"){
      ages$Success_Rate<-NULL
    total_succeeded=sum(ages$Succeeded)
    total_enrolled=sum(ages$Enrolled)
    ages$Equity_Index=round((ages$Succeeded/total_succeeded)/(ages$Enrolled/total_enrolled),digits=2)
    }else{
      passed=enrolled[enrolled$Grade=="A" | enrolled$Grade=="B" | enrolled$Grade=="C" | enrolled$Grade=="P",]
      total_enrolled=ddply(enrolled,.(Academic_Year),nrow)
      total_succeeded=ddply(passed,.(Academic_Year),nrow)
      ages$Equity_Index=0
      for(i in 1:nrow(ages)){
        if(any(total_succeeded[total_succeeded$Academic_Year==ages$Academic_Year[i],]$V1) &&
           any(total_enrolled[total_enrolled$Academic_Year==ages$Academic_Year[i],]$V1)){
          
           ages$Equity_Index[i]=round((ages$Succeeded[i]/total_succeeded[total_succeeded$Academic_Year==ages$Academic_Year[i],]$V1)
                                     /(ages$Enrolled[i]/total_enrolled[total_enrolled$Academic_Year==ages$Academic_Year[i],]$V1)
                                     ,digits=2) 
        }
       
      }
    }
    
    for(i in 1:length(ages$Equity_Index)){
      if(is.nan(ages$Equity_Index[i])){
        ages$Equity_Index[i]=0
      }
    }
    
  }
  
  
  if(status!="filter")
  {rows_to_delete<-ages[ages$Enrolled==0,]
  ages=setdiff(ages,rows_to_delete)}
  
  
  #sensor data
  if(status=="retention" || status=="success"){
    ages=sensor_data(ages,2)
  }else if( status=="equity"){
    ages=sensor_comparison(ages,type="success")
  }
  

    academic_year=as.character(unique(ages$Academic_Year))
    ages$Academic_Year=paste( substr(academic_year,1,4),"-",substr(academic_year,5,8))
  
  
    ages=ages[order(ages$Academic_Year),]
  return(ages)
}
 
get_student_groups<-function(enrolled,dropped=NULL,passed=NULL,status="enrolled", Groups_wanted=NULL, years=NULL){
  years=str_replace_all(years,"-","")
  
  Groups=c("EOPS","DSPS","CalWorks","Veteran","Military","BOGG","Pell Grant")
  EOPS<-ddply(enrolled,.(EOPS,Academic_Year),nrow)
  DSPS<-ddply(enrolled,.(DSPS,Academic_Year),nrow)
  CalWorks<-ddply(enrolled,.(CalWorks,Academic_Year),nrow)
  Veteran<-ddply(enrolled,.(Veteran,Academic_Year),nrow)
  Military<-ddply(enrolled,.(Military,Academic_Year),nrow)
  BOGG<-ddply(enrolled,.(BOGG,Academic_Year),nrow)
  Pell_Grant<-ddply(enrolled,.(Pell_Grant,Academic_Year),nrow)
  temp=list(EOPS,DSPS,CalWorks,Veteran,Military,BOGG,Pell_Grant)

  result=data.frame(Academic_Year=numeric(),Enrolled=numeric(),Groups=character())
  for(i in 1:length(temp)){
    names(temp[[i]])[3]<-"Enrolled"
    if(nrow(temp[[i]][temp[[i]][1]==1 ,])!=0){ 
      
      temp[[i]]=temp[[i]][temp[[i]][1]==1,]
      temp[[i]]$Groups=Groups[i]
      temp[[i]][1]<-NULL
      
    }
    else{
      temp[[i]]=data.frame(Academic_Year=years,Enrolled=0,Groups=Groups[i])
    }
    if(length(setdiff(years,temp[[i]]$Academic_Year))!=0){
      temp[[i]]=rbind(temp[[i]],data.frame(Academic_Year=as.numeric(setdiff(years,temp[[i]]$Academic_Year)),Enrolled=0,Groups=Groups[i]))
    }
    
    result=rbind(result,temp[[i]])
  }
  
  if(status=="filter"){
    result=result[result$Groups %in% Groups_wanted,]
  }
 
  result=result[c(1,3,2)]
  
  if(status=="enrolled"){
    total_enrollment=length(unique(enrolled$Student_Id))
    result$Head_Count=0
    result$Head_Percent<-0
    result$Head_Count<-c(length(unique(enrolled[enrolled$EOPS==1,]$Student_Id)),
                         length(unique(enrolled[enrolled$DSPS==1,]$Student_Id)),
                         length(unique(enrolled[enrolled$CalWorks==1,]$Student_Id)),
                         length(unique(enrolled[enrolled$Veteran==1,]$Student_Id)),
                         length(unique(enrolled[enrolled$Military==1,]$Student_Id)),
                         length(unique(enrolled[enrolled$BOGG==1,]$Student_Id)),
                         length(unique(enrolled[enrolled$Pell_Grant==1,]$Student_Id)))

    result$Head_Percent<-round((result$Head_Count/total_enrollment)*100,digits=1)
    result<-sensor(data=result,column="Head_Count",affected="Head_Percent",add_column = T)
  }
  #result<-result[complete.cases(result), ]#will get rid of NA values
  #<---------RETENTION-------------------------|
  if(status=="retention" || status=="filter"){
    ws=data.frame(Academic_Year=numeric(), Retained=numeric(), Groups=character())
    temp=list(ddply(dropped,.(EOPS,Academic_Year),nrow),
         ddply(dropped,.(DSPS,Academic_Year),nrow),
         ddply(dropped,.(CalWorks,Academic_Year),nrow),
         ddply(dropped,.(Veteran,Academic_Year),nrow),
         ddply(dropped,.(Military,Academic_Year),nrow),
         ddply(dropped,.(BOGG,Academic_Year),nrow),
         ddply(dropped,.(Pell_Grant,Academic_Year),nrow))

    for(i in 1:length(temp)){
      if(nrow(temp[[i]])!=0){
        #temp[[i]]=temp[[i]][temp[[i]]]
        names(temp[[i]])[3]<-"Retained"

        if(nrow(temp[[i]][temp[[i]][1]==1,])!=0){
          temp[[i]]=temp[[i]][temp[[i]][1]==1,]
          temp[[i]]$Groups=Groups[i]
          temp[[i]][1]<-NULL
        }else{
          temp[[i]]=data.frame(Academic_Year=years,Retained=0,Groups=Groups[i])
        }
        if(length(setdiff(years,temp[[i]]$Academic_Year))!=0){
          temp[[i]]=rbind(temp[[i]],data.frame(Academic_Year=as.numeric(setdiff(years,temp[[i]]$Academic_Year)),Retained=0,Groups=Groups[i]))
         
        }
        
        #temp[[i]]=temp[[i]][complete.cases(temp[[i]]),]#will get rid of NA values
        ws=rbind(ws,temp[[i]])}
        
    }
    
  result$Retained=result$Enrolled
  result$Retention_Rate=0
    for(i in 1:nrow(result)){
      if(nrow(ws[ws$Groups==result$Groups[i] & ws$Academic_Year==result$Academic_Year[i],])!=0){
      result$Retained[i]=result$Enrolled[i]-ws[ws$Groups==result$Groups[i] & ws$Academic_Year==result$Academic_Year[i],]$Retained}
      result$Retention_Rate[i]=round((result$Retained[i]/result$Enrolled[i])*100,digits=1)
    }
  for(i in 1:length(result$Retention_Rate)){
    if(is.nan(result$Retention_Rate[i])){
      result$Retention_Rate[i]=0
    }
  }

    #<----------------SUCCESS------------------|
  }
  if(status=="success" || status=="filter" || status=="equity"){
    ws=data.frame(Academic_Year=numeric(), Retained=numeric(), Groups=character())
    temp=list(  ddply(passed,.(EOPS,Academic_Year),nrow),
         ddply(passed,.(DSPS,Academic_Year),nrow),
         ddply(passed,.(CalWorks,Academic_Year),nrow),
         ddply(passed,.(Veteran,Academic_Year),nrow),
         ddply(passed,.(Military,Academic_Year),nrow),
         ddply(passed,.(BOGG,Academic_Year),nrow),
         ddply(passed,.(Pell_Grant,Academic_Year),nrow))
       
    for(i in 1:length(temp)){
      
      if(nrow(temp[[i]])!=0 && nrow(temp[[i]][temp[[i]][1]==1,])!=0){
        names(temp[[i]])[3]<-"Succeeded"
        temp[[i]]=temp[[i]][temp[[i]][1]==1,]
        temp[[i]]$Groups=Groups[i]
        temp[[i]][1]<-NULL
      }else{
        temp[[i]]=data.frame(Academic_Year=years,Succeeded=0,Groups=Groups[i])
      }
      if(length(setdiff(years,temp[[i]]$Academic_Year))!=0){
        temp[[i]]=rbind(temp[[i]],data.frame(Academic_Year=as.numeric(setdiff(years,temp[[i]]$Academic_Year)),Succeeded=0,Groups=Groups[i]))
      }
      #temp[[i]]=temp[[i]][complete.cases(temp[[i]]),]#will get rid of NA values
      ws=rbind(ws,temp[[i]])
    }
 
 
    result$Succeeded=result$Enrolled
    result$Success_Rate=0
    for(i in 1:nrow(result)){
      result$Succeeded[i]=ws[ws$Groups==result$Groups[i] & ws$Academic_Year==result$Academic_Year[i],]$Succeeded
      result$Success_Rate[i]=round((result$Succeeded[i]/result$Enrolled[i])*100,digits=1)
    }
    for(i in 1:length(result$Success_Rate)){
      if(is.nan(result$Success_Rate[i])){
        result$Success_Rate[i]=0
      }
    }

    
  }

  if(status=="equity" || status=="filter"){
    if(status!="filter"){ 
     result$Success_Rate<-NULL
    total_succeeded=nrow(passed)
    total_enrolled=nrow(enrolled)
    result$Equity_Index=round((result$Succeeded/total_succeeded)/(result$Enrolled/total_enrolled),digits=2)
    }else{
      total_enrolled=ddply(enrolled,.(Academic_Year),nrow)
      total_succeeded=ddply(passed,.(Academic_Year),nrow)
      result$Equity_Index=0
      for(i in 1:nrow(result)){
        if(any(total_succeeded[total_succeeded$Academic_Year==result$Academic_Year[i],]$V1 && 
               any(total_enrolled[total_enrolled$Academic_Year==result$Academic_Year[i],]$V1))){
           result$Equity_Index[i]=round((result$Succeeded[i]/total_succeeded[total_succeeded$Academic_Year==result$Academic_Year[i],]$V1)
                                    /(result$Enrolled[i]/total_enrolled[total_enrolled$Academic_Year==result$Academic_Year[i],]$V1)
                                    ,digits=2) 
        }
      }
      
    }

  }
 
  #deletedgroups that have no one in them
  if(status!="filter" && !(length(unique(result$Enrolled))==1 && unique(result$Enrolled)==0) ){
    rows_to_delete<-result[result$Enrolled==0,]
    result=setdiff(result,rows_to_delete)
    result<-droplevels(result)}
  #sensor data
  if(status=="retention" || status=="success"){
    result=sensor_data(result,2)
  }else if( status=="equity"){
    result=sensor_comparison(result,type="success")
  }

    academic_year=as.character(unique(result$Academic_Year))
    result$Academic_Year=paste( substr(academic_year,1,4),"-",substr(academic_year,5,8))
  
  result=result[order(result$Academic_Year),]
  return(result)
  
}

get_major<-function(enrolled,dropped,passed, year,status="enrolled",majors=NULL){
  result=data.frame(Academic_Year=numeric,Major=character(),Enrolled=numeric())
  year=str_replace_all(year,"-","")
  
  result=NULL
  if(status=="filter"){
    result=data.frame(Academic_Year=numeric(),Major=character(), Enrolled=numeric())
    for(i in 1:length(majors)){
      result=rbind(result, data.frame(Academic_Year=year,Major=majors[i], Enrolled=0))
    }
    for(i in 1:nrow(result)){
      if(result$Major[i]=="Declared"){
       result$Enrolled[i]=nrow(enrolled[enrolled$Academic_Year==result$Academic_Year[i] & enrolled$Major_Declared==1,])}
      else{
        result$Enrolled[i]=nrow(enrolled[enrolled$Academic_Year==result$Academic_Year[i] & enrolled$Major_Declared==0,])
      }
    }
    
  }else{
    result<-ddply(enrolled, .( Academic_Year,Major_Declared), nrow)
    for(i in 1:nrow(result)){
      if(as.numeric(result$Major_Declared[i])==0){
        result$Major_Declared[i]="Undeclared"
      }else if(as.numeric(result$Major_Declared[i])==1){
        result$Major_Declared[i]="Declared"
      }
    }

    colnames(result)[2]<-"Major"
    colnames(result)[3]<-"Enrolled"
    if(status=="enrolled"){
      total_enrollment=length(unique(enrolled$Student_Id))
      result$Head_Count=0
      result$Head_Percent<-0
      temp<-data.frame(Major=c("Undeclared","Declared"),Head_Count=c(length(unique(enrolled[enrolled$Major_Declared==0,]$Student_Id)),
                           length(unique(enrolled[enrolled$Major_Declared==1,]$Student_Id))))
      for(i in 1:nrow(result)){
        result$Head_Count[i]=temp[temp$Major==result$Major[i],]$Head_Count
      }

      result$Head_Percent<-round((result$Head_Count/total_enrollment)*100,digits=1)
      result<-sensor(data=result,column="Head_Count")
    }
  }
  
  #<---------RETENTION-------------------------|
  if(status=="retention" || status=="filter"){
    result$Retained=result$Enrolled
    
    temp<-ddply(dropped, .(Academic_Year, Major_Declared), nrow)
 
    if(nrow(temp)!=0){
      for(i in 1:nrow(temp)){
        if(temp$Major_Declared[i]==0){
          temp$Major_Declared[i]="Undeclared"
        }else if(temp$Major_Declared[i]==1){
          temp$Major_Declared[i]="Declared"
        }
      }
      colnames(result)[2]<-"Major"
      retained=vector(mode="numeric",length=nrow(result))
      
      
      for(i in 1:nrow(result)){
        ws=temp[temp$Major==result$Major[i] & temp$Academic_Year==result$Academic_Year[i],]
        if(nrow(ws)!=0){
          result$Retained[i]=as.numeric(result$Enrolled[i])-as.numeric(ws$V1)
        }
  
      }
    }
    
    result$Retention_Rate=round((result$Retained/result$Enrolled)*100,digits=1)
    for(i in 1:length(result$Retention_Rate)){
      if(is.nan(result$Retention_Rate[i])){
        result$Retention_Rate[i]=0
      }
    }

    #<----------------SUCCESS------------------|
  }
  
  if(status=="success" || status=="filter" || status=="equity"){
    
    temp<-ddply(passed, .(Academic_Year,Major_Declared), nrow)
    for(i in 1:nrow(temp)){
      if(temp$Major_Declared[i]==0){
        temp$Major_Declared[i]="Undeclared"
      }else if(temp$Major_Declared[i]==1){
        temp$Major_Declared[i]="Declared"
      }
    }
    colnames(result)[2]<-"Major"

    succeeded=vector(mode="numeric",length=nrow(result))
    result$Succeeded=0
    for(i in 1:nrow(result)){
      ws=temp[temp$Major==result$Major[i] & temp$Academic_Year==result$Academic_Year[i],]
      if(nrow(ws)!=0){
        result$Succeeded[i]=as.numeric(ws$V1)
      }
    }
    
    
    result$Success_Rate=round((result$Succeeded/result$Enrolled)*100,digits=1)
    for(i in 1:length(result$Success_Rate)){
      if(is.nan(result$Success_Rate[i])){
        result$Success_Rate[i]=0
      }
    }
  }
  
  #sensor data
  if(status=="equity" ||status=="filter"){
    if(status!="filter"){
      result$Success_Rate<-NULL
    
    total_succeeded=sum(result$Succeeded)
    total_enrolled=sum(result$Enrolled)
    result$Equity_Index=round((result$Succeeded/total_succeeded)/(result$Enrolled/total_enrolled),digits=2)
    }else{
      result$Equity_Index=round((result$Succeeded/nrow(passed))/(result$Enrolled/nrow(enrolled)),digits=2)
    }
    for(i in 1:length(result$Equity_Index)){
      if(is.nan(result$Equity_Index[i])){
        result$Equity_Index[i]=0
      }
    }
  }
  
  if(status=="retention" || status=="success" ){
    result=sensor_data(result,2)
  }
  else if(status=="enrolled"){
    result$Major=paste(result$Major,"    ")
  }else if( status=="equity"){
    result=sensor_comparison(result,type="success")
  }
  

    academic_year=as.character(unique(result$Academic_Year))
    result$Academic_Year=paste( substr(academic_year,1,4),"-",substr(academic_year,5,8))
  
  
  
  return(result)
  
  
}

get_class_type<-function(enrolled,dropped,passed,class_type,years){
 
  for(i in 1:length(class_type)){
    if(class_type[i]=="Deg.Appl")
      class_type[i]="Degree Applicable"
  }
  
  type=c("CTE","Basic Skills", "Degree Applicable")
  temp=list(ddply(enrolled,.(CTE,Academic_Year),nrow),
            ddply(enrolled,.(Basic_Skills,Academic_Year),nrow),
            ddply(enrolled,.(Degree_Applicable,Academic_Year),nrow))
 
  result=data.frame(Academic_Year=numeric(),Enrolled=numeric(),Class_Type=character())
  
  for(i in 1:length(temp)){
    if(nrow(temp[[i]][temp[[i]][1]==1,])!=0){
        names(temp[[i]])[3]="Enrolled"
        temp[[i]]=temp[[i]][temp[[i]][1]==1,]#isolate the row needed
        temp[[i]][1]=NULL#remove first row
        temp[[i]]$Class_Type=type[i]
        #check for missing years
        if(length(setdiff(as.numeric(years),unique(temp[[i]]$Academic_Year)))!=0){
          #find the missing years
          missing_years=setdiff(as.numeric(years),unique(temp[[i]]$Academic_Year))
          temp[[i]]=rbind(temp[[i]],data.frame(Academic_Year=missing_years,Enrolled=0,Class_Type=type[i]))
        }
        result=rbind(result,temp[[i]])
    }
  }
  result=result[c(1,3,2)]
  result=result[result$Class_Type %in% class_type,]
  result=result[complete.cases(result),]

  #-----------Retention-----------------
  temp=data.frame(Academic_Year=numeric(),Retained=numeric(),Class_Type=character())
  x=list(ddply(dropped,.(CTE,Academic_Year),nrow),
                  ddply(dropped,.(Basic_Skills,Academic_Year),nrow),
                  ddply(dropped,.(Degree_Applicable,Academic_Year),nrow))
  
  for(i in 1:length(x)){
   if(nrow(x[[i]][x[[i]][1]==1,])!=0){
      names(x[[i]])[3]="Retained"
      x[[i]]=x[[i]][x[[i]][1]==1,]#isolate the row needed
      x[[i]][1]=NULL#remove first row
      x[[i]]$Class_Type=type[i]
      temp=rbind(temp,x[[i]])}
  }
  
  result$Retained=0
  for(i in 1:nrow(result)){
    result$Retained[i]=result$Enrolled[i]-temp[temp$Class_Type==result$Class_Type[i] & temp$Academic_Year==result$Academic_Year[i],]$Retained
  }
  
  result$Retention_Rate=round((result$Retained/result$Enrolled)*100,digits=1)
  result=result[complete.cases(result),]
  #-------------Success-------------------------------------------
  temp=data.frame(Academic_Year=numeric(),Succeeded=numeric(),Class_Type=character())
  x=list(ddply(passed,.(CTE,Academic_Year),nrow),
         ddply(passed,.(Basic_Skills,Academic_Year),nrow),
         ddply(passed,.(Degree_Applicable,Academic_Year),nrow))
  
  for(i in 1:length(x)){
    if(nrow(x[[i]][x[[i]][1]==1,])!=0){
        names(x[[i]])[3]="Succeeded"
        x[[i]]=x[[i]][x[[i]][1]==1,]#isolate the row needed
        x[[i]][1]=NULL#remove first row
        x[[i]]$Class_Type=type[i]
        temp=rbind(temp,x[[i]])}
  }
  
  result$Succeeded=0
  for(i in 1:nrow(result)){
    result$Succeeded[i]=temp[temp$Class_Type==result$Class_Type[i] & temp$Academic_Year==result$Academic_Year[i],]$Succeeded
  }
  
  result$Success_Rate=round((result$Succeeded/result$Enrolled)*100,digits=1)

  
  
  #Equity Graph
  total_enrolled=ddply(enrolled,.(Academic_Year),nrow)
  total_succeeded=ddply(passed,.(Academic_Year),nrow)
  result$Equity_Index=0
  for(i in 1:nrow(result)){
    result$Equity_Index[i]=round((result$Succeeded[i]/total_succeeded[total_succeeded$Academic_Year==result$Academic_Year[i],]$V1)
                                 /(result$Enrolled[i]/total_enrolled[total_enrolled$Academic_Year==result$Academic_Year[i],]$V1)
                                 ,digits=2) 
  }
    
  for(i in 1:length(result$Equity_Index)){
    if(is.nan(result$Equity_Index[i])){
      result$Equity_Index[i]=0
    }
  }
  
  academic_year=as.character(unique(result$Academic_Year))
  result$Academic_Year=paste( substr(academic_year,1,4),"-",substr(academic_year,5,8))
  result=result[order(result$Academic_Year),]
  result=result[complete.cases(result),]
  return(result)
}

get_location<-function(enrolled,dropped,passed,location,years){
  #------------Enrolled--------------
  years=str_replace_all(years,"-","")
  type<-c("Main Campus","Hybrid","Offsite","SGEC","Online")
  temp<-list(ddply(enrolled,.(Main_Campus,Academic_Year),nrow),
             ddply(enrolled,.(Hybrid,Academic_Year),nrow),
             ddply(enrolled,.(Offsite,Academic_Year),nrow),
             ddply(enrolled,.(SGEC,Academic_Year),nrow),
             ddply(enrolled,.(Online,Academic_Year),nrow))
  result=data.frame(Academic_Year=numeric(),Enrolled=numeric(),Location=character())
  
  for(i in 1:length(temp)){
    if(nrow(temp[[i]][temp[[i]][1]==1,])!=0){
      names(temp[[i]])[3]="Enrolled"
      temp[[i]]=temp[[i]][temp[[i]][1]==1,]#isolate the row needed
      temp[[i]][1]=NULL#remove first row
      temp[[i]]$Location=type[i]
      
      #check for missing years
      if(length(setdiff(as.numeric(years),unique(temp[[i]]$Academic_Year)))!=0){
        #find the missing years
        missing_years=setdiff(as.numeric(years),unique(temp[[i]]$Academic_Year))
        temp[[i]]=rbind(temp[[i]],data.frame(Academic_Year=missing_years,Enrolled=0,Location=type[i]))
      }
      result=rbind(result,temp[[i]])
    }
  }
  result=result[c(1,3,2)]
  result=result[result$Location %in% location,]

  #------------Retention----------
  x<-list(ddply(dropped,.(Main_Campus,Academic_Year),nrow),
             ddply(dropped,.(Hybrid,Academic_Year),nrow),
             ddply(dropped,.(Offsite,Academic_Year),nrow),
             ddply(dropped,.(SGEC,Academic_Year),nrow),
             ddply(dropped,.(Online,Academic_Year),nrow))
  temp=data.frame(Academic_Year=numeric(),Retained=numeric(),Location=character())

  for(i in 1:length(x)){
    if(nrow(x[[i]][x[[i]][1]==1,])!=0){
      names(x[[i]])[3]="Retained"
      x[[i]]=x[[i]][x[[i]][1]==1,]#isolate the row needed
      x[[i]][1]=NULL#remove first row
      x[[i]]$Location=type[i]
      #check for missing years
      if(length(setdiff(as.numeric(years),unique(x[[i]]$Academic_Year)))!=0){
        #find the missing years
        missing_years=setdiff(as.numeric(years),unique(x[[i]]$Academic_Year))
        x[[i]]=rbind(x[[i]],data.frame(Academic_Year=missing_years,Retained=0,Location=type[i]))

      }
      temp=rbind(temp,x[[i]])}
  }
  
  result$Retained=0
  for(i in 1:nrow(result)){
    result$Retained[i]=result$Enrolled[i]-temp[temp$Location==result$Location[i] & temp$Academic_Year==result$Academic_Year[i],]$Retained
  }
  
  result$Retention_Rate=round((result$Retained/result$Enrolled)*100,digits=1)
  for(i in 1:length(result$Retention_Rate)){
    if(is.nan(result$Retention_Rate[i])){
      result$Retention_Rate[i]=0
    }
  }

  #-----------Success---------------------
  x<-list(ddply(passed,.(Main_Campus,Academic_Year),nrow),
          ddply(passed,.(Hybrid,Academic_Year),nrow),
          ddply(passed,.(Offsite,Academic_Year),nrow),
          ddply(passed,.(SGEC,Academic_Year),nrow),
          ddply(passed,.(Online,Academic_Year),nrow))
  temp=data.frame(Academic_Year=numeric(),Succeeded=numeric(),Location=character())
  
  
  for(i in 1:length(x)){
    if(nrow(x[[i]][x[[i]][1]==1,])!=0){
      names(x[[i]])[3]="Succeeded"
      x[[i]]=x[[i]][x[[i]][1]==1,]#isolate the row needed
      x[[i]][1]=NULL#remove first row
      x[[i]]$Location=type[i]
      if(length(setdiff(as.numeric(years),unique(x[[i]]$Academic_Year)))!=0){
        #find the missing years
        missing_years=setdiff(as.numeric(years),unique(x[[i]]$Academic_Year))
        x[[i]]=rbind(x[[i]],data.frame(Academic_Year=missing_years,Succeeded=0,Location=type[i]))
        
      }      
      temp=rbind(temp,x[[i]])}
  }
  
  result$Succeeded=0
  for(i in 1:nrow(result)){
    result$Succeeded[i]=temp[temp$Location==result$Location[i] & temp$Academic_Year==result$Academic_Year[i],]$Succeeded
  }
  
  result$Success_Rate=round((result$Succeeded/result$Enrolled)*100,digits=1)
  for(i in 1:length(result$Success_Rate)){
    if(is.nan(result$Success_Rate[i])){
      result$Success_Rate[i]=0
    }
  }
 
  
  #Equity Graph
  total_enrolled=ddply(enrolled,.(Academic_Year),nrow)
  total_succeeded=ddply(passed,.(Academic_Year),nrow)
  result$Equity_Index=0
  for(i in 1:nrow(result)){
    if(any(total_succeeded[total_succeeded$Academic_Year==result$Academic_Year[i],]$V1) &&
       any(total_enrolled[total_enrolled$Academic_Year==result$Academic_Year[i],]$V1)){
       result$Equity_Index[i]=round((result$Succeeded[i]/total_succeeded[total_succeeded$Academic_Year==result$Academic_Year[i],]$V1)
                                 /(result$Enrolled[i]/total_enrolled[total_enrolled$Academic_Year==result$Academic_Year[i],]$V1)
                                 ,digits=2) 
    }
   
  }
  
  for(i in 1:length(result$Equity_Index)){
    if(is.nan(result$Equity_Index[i])){
      result$Equity_Index[i]=0
    }
  }
  
  academic_year=as.character(unique(result$Academic_Year))
  result$Academic_Year=paste( substr(academic_year,1,4),"-",substr(academic_year,5,8))
  result=result[order(result$Academic_Year),]
return(result)
}

get_time<-function(enrolled,dropped,passed,day,time,days=1, years){
  years=str_replace_all(years,"-","")

  for(i in 1:length(time)){
    if(time[i]=="Afternoon (3:30PM-6:00PM)"){
      time[i]="Afternoon"
    }
    if(time[i]=="Early Afternoon (1:30PM-3:30PM)"){
      time[i]="Early Afternoon"
    }
    if(time[i]=="College Hour(12:00PM-1:30AM)"){
      time[i]="College Hour"
    }
    if(time[i]=="Morning (10:00AM-12:00PM)"){
      time[i]="Morning"
    }
    if(time[i]=="Early Morning (Before 10:00AM)"){
      time[i]="Early Morning"
    }
    if(time[i]=="Evening (After 6:00PM)"){
      time[i]="Evening"
    }
  }
  
  result=data.frame(Academic_Year=numeric(),Day_Time=character(),Day=character(),Time=character(),Enrolled=numeric())
  temp=data.frame(Academic_Year=numeric(),Day_Time=character(),Day=character(),Time=character(),Enrolled=numeric())
  for(j in 1:length(time)){
    for(i in 1:length(years)){
      temp=rbind(temp,data.frame(Academic_Year=years[i],Day_Time=c(paste(day[j],"/",time[j])),
                                     Day=day[j],Time=time[j],
                           Enrolled=c(nrow(enrolled[enrolled$Start_Period==time[j] & enrolled$Meeting_Days==day[j] & enrolled$Academic_Year==years[i],]))))
  }

  }

result=temp
#-------Retention-------
 
  result$Retained=0
  result$Retention_Rate=0
  for(i in 1:nrow(result)){
    result$Retained[i]=result$Enrolled[i]-nrow(dropped[dropped$Start_Period==result$Time[i] & dropped$Meeting_Days==result$Day[i] 
                             & dropped$Academic_Year==result$Academic_Year[i],])
    if(!is.nan(result$Retained[i]/result$Enrolled[i])){
      result$Retention_Rate[i]=round((result$Retained[i]/result$Enrolled[i])*100,digits=1)
    }
    
  }
  

#-------Success----------
 
  result$Succeeded=0
  result$Success_Rate=0
  for(i in 1:nrow(result)){
    result$Succeeded[i]=nrow(passed[passed$Start_Period==result$Time[i] & passed$Meeting_Days==result$Day[i] 
                                                       & passed$Academic_Year==result$Academic_Year[i],])
    if(!is.nan(result$Succeeded[i]/result$Enrolled[i])){
      result$Success_Rate[i]=round((result$Succeeded[i]/result$Enrolled[i])*100,digits=1)
    }
  
  }
  
  result$Day<-NULL
  result$Time<-NULL
  
  #Equity Graph
  total_enrolled=ddply(enrolled,.(Academic_Year),nrow)
  total_succeeded=ddply(passed,.(Academic_Year),nrow)
  result$Equity_Index=0
  for(i in 1:nrow(result)){
    result$Equity_Index[i]=round((result$Succeeded[i]/total_succeeded[total_succeeded$Academic_Year==result$Academic_Year[i],]$V1)
                                 /(result$Enrolled[i]/total_enrolled[total_enrolled$Academic_Year==result$Academic_Year[i],]$V1)
                                 ,digits=2) 
  }
  
  for(i in 1:length(result$Equity_Index)){
    if(is.nan(result$Equity_Index[i])){
      result$Equity_Index[i]=0
    }
  }
  
  academic_year=as.character(unique(result$Academic_Year))
  result$Academic_Year=paste( substr(academic_year,1,4),"-",substr(academic_year,5,8))
  result=result[order(result$Academic_Year),]
  return(result)  

}


get_general_historical<-function(enrolled,dropped,passed,years){
  
  result=ddply(enrolled,.(Academic_Year),nrow)
  colnames(result)[2]<-"Enrolled"
  

  #check for missing years
  if(length(setdiff(as.numeric(years),unique(result$Academic_Year)))!=0){
    #find the missing years
    missing_years=setdiff(as.numeric(years),unique(result$Academic_Year))
    result=rbind(result,data.frame(Academic_Year=missing_years,Enrolled=0))
  }
  result<-result[complete.cases(result),]

#---------------Retention-----------------------------------
  
  temp<-ddply(dropped,.(Academic_Year),nrow)
  result$Retained=result$Enrolled

  for( i in 1:nrow(result)){
    if(nrow(temp[temp$Academic_Year==result$Academic_Year[i],])!=0){
      result$Retained[i]=result$Enrolled[i]-temp[temp$Academic_Year==result$Academic_Year[i],]$V1
    }
  }

  result$Retention_Rate=round((result$Retained/result$Enrolled)*100,digits=2)
#----------------------------Success---------------------------
  
  temp<-ddply(passed,.(Academic_Year),nrow)
  result$Succeeded=0

  for( i in 1:nrow(result)){
    if(nrow(temp[temp$Academic_Year==result$Academic_Year[i],])!=0){
      result$Succeeded[i]=temp[temp$Academic_Year==result$Academic_Year[i],]$V1
    }
  }

  result$Success_Rate=round((result$Succeeded/result$Enrolled)*100,digits=2)
  academic_year=as.character(unique(result$Academic_Year))
  result$Academic_Year=paste( substr(academic_year,1,4),"-",substr(academic_year,5,8))
  return(result)
  
}

sensor_data<-function(data,check=1,sensor=FALSE){

  if(sensor==TRUE){
    total=sum(as.numeric(data$Enrolled))
    data$Percent=100

    for(i in 1:nrow(data)){
         data$Percent[i]=round((as.numeric(data$Enrolled[i])/total)*100,digits=1)
    }
  }
  
  for(i in 1:nrow(data)){

    if(as.integer(data[i,3])< as.integer(10)){#Enrollment sensor the data
      data[i,3]="--"
    }
  
    if(as.integer(check)==as.integer(2) && as.integer(data[i,4])<as.integer(10)){#success or retention
      data[i,4]="--"
      data[i,3]="--"
    }
  }
  return(data)
}

#will sensor anything include column name, and column names of other column affected 
sensor<-function(data,column,affected=NULL,add_column=F){
  if(add_column){
    data[[paste0(column,"_")]]=data[[column]]
  }
  data=data[complete.cases(data),]
  for(i in 1:nrow(data)){

    if(data[[column]][i]!="--" && as.integer(data[[column]][i])< as.integer(10) && as.integer(data[[column]][i])!=as.integer(0)){
      if(add_column){
        data[[paste0(column,"_")]][i]="--"
      }
      else{
        data[[column]][i]="--"
      }
      
      if(!is.null(affected)){
        for(j in 1:length(affected)){
          data[[affected[j]]][i]="--"
        }
      }
    }
  }
  return (data)
}

sensor_comparison<-function(data,type="enrolled"){
 
  data=data[complete.cases(data),]
  for(i in 1:length(data$Enrolled)){
    if(data$Enrolled[i]!="--" && as.numeric(unlist(data$Enrolled[i]))<10 && data$Enrolled[i]>0){
      data$Enrolled[i]="--"
 
    }
  }
      for(i in 1:nrow(data)){
        if(type =="retention" ){
          if(as.numeric(unlist(data$Retained[i]))<10 && data$Retained[i]>0){
            data$Retained[i]="--"
            data$Enrolled[i]="--"

            }
        }
        else if(type =="success" || type=="equity" ){
        
          if((as.numeric(unlist(data$Succeeded[i]))<10 && data$Succeeded[i]>0) || is.nan(as.numeric(unlist(data$Succeeded[i])))){
            data$Succeeded[i]="--"
            data$Enrolled[i]="--"
          }
        }
       
      }
  
  return (data)
}

adjust_data<-function(data,type){
  #get the years
  years=unique(data$Academic_Year)
  #to trace must have column for specific year not row so changing column names
  names=type
  for(i in 1:length(years)){
    names[(length(names)+1):(length(names)+5)]=paste0(c("Enrolled_", "Retained_","Retention_Rate_",
                                                                "Succeeded_", "Success_Rate_","Equity_Index_"),years[i])
  }
  result=setNames(data.frame(matrix(ncol = (length(years)*5)+1, nrow = 0)),names)
 
  #rearrange data 
  result[1:length(unique(data[[type]])),]<-NA
  result[[type]]=unique(data[[type]])
 
  
  for(j in 1:length( result[[type]])){
      for(i in 1:length(years)){#number of rows=years
        year_data=data[data[[type]]==result[[type]][j] & data$Academic_Year==years[i],]

          result[[paste0("Enrolled_",years[i])]][j]=year_data$Enrolled
          result[[paste0("Retained_",years[i])]][j]=year_data$Retained
          result[[paste0("Retention_Rate_",years[i])]][j]=year_data$Retention_Rate
          result[[paste0("Succeeded_",years[i])]][j]=year_data$Succeeded
          result[[paste0("Success_Rate_",years[i])]][j]=year_data$Success_Rate
          result[[paste0("Equity_Index_",years[i])]][j]=year_data$Equity_Index
      }
  }
  
  #Algorithm
  df_names=c("Enrolled","Retained","Retention_Rate","Succeeded","Success_Rate")
  for(i in 1:length(colnames(result))){
  
    #1)Cannot pick and choose which ones to add to graph or not so must make double column for everything :\
      #2) Make a copy of that column
    if(1 %in% pmatch(df_names,names(result[i])) )
      {
       
        result$New<-0
        names(result)[names(result) == 'New'] <-paste0(names(result[i]),'_')

        #3)Sensor the data
        for(j in 1:length(result[[paste0(names(result[i]),'_')]])){
          if(as.numeric(result[[paste0(names(result[i]))]][j])<as.numeric(10)){
            result[[paste0(names(result[i]),'_')]][j]="--"
          }
          else{
            result[[paste0(names(result[i]),'_')]][j]= result[[paste0(names(result[i]))]][j]
          }
        }
      }
    
    
  }
  
  return(result)
}

create_comparison_graph<-function(data,years_needed,type,status){
  color_graph =  c( 'rgb(90, 221, 147,1)',
                    'rgba(89, 199, 182,1)', 'rgb(94, 156, 147)',
  
                                      'rgb(37, 94, 101)','rgba(0,0,0,1)')
  
  if(status=="Enrolled" ){

  p <-plot_ly(data,
              hoverinfo = 'text', marker = list(color = color_graph[1]), type = 'bar')%>%
    layout(title = paste(type, status) ,margin = list(b = 100,r=-10), yaxis = list( showticklabels = FALSE),
           barmode = 'group',
            showlegend = TRUE)%>% config(displayModeBar = F)
  }
  else if(status=="Equity"){
    p <-plot_ly(data,
                hoverinfo = 'text', marker = list(color = color_graph[1]), type = 'bar')%>%
      layout(title = paste(type, status," (Success)") ,margin = list(b = 100,r=-10),
             xaxis = list(title = "",linecolor = "black",linewidth = 0.5,categoryorder = "trace"),
             yaxis = list(side = 'left', title = "Equity Index/Success", zeroline = FALSE),
             barmode = 'group',
             showlegend = TRUE)%>% config(displayModeBar = F) 
    }
  else{
    p <-plot_ly(data,
                hoverinfo = 'text', marker = list(color = color_graph[1]), type = 'bar')%>%
      layout(title = paste(type, status) ,margin = list(b = 100,r=-10),# xaxis = list(tickangle = 25),
             xaxis = list(title = "",linecolor = "black",linewidth = 0.5,categoryorder = "trace"),
             yaxis = list(side = 'left', title = paste0(status," Rate"), zeroline = FALSE,range=c(0,105) ),
             barmode = 'group',
             showlegend = TRUE)%>% config(displayModeBar = F)
    } 

                                         
                                         
  if(status=="Enrolled"){
    
    for(i in length(years_needed):1){
      
      if(!is.null(data[[paste0("Enrolled_",str_replace_all(years_needed[i],"-",""))]])){
        p <-add_trace(p,x=data[[type]],  y=data[[paste0("Enrolled_",str_replace_all(years_needed[i],"-",""))]],
                      text=paste(data[[paste0("Enrolled_",str_replace_all(years_needed[i],"-",""),"_")]],"students"),
                      name =paste(years_needed[i],"  "), marker = list(color = color_graph[i]),evaluate=TRUE)}
    } 

  }else if(status=="Retention"){
    
    for(i in length(years_needed):1){
      if(!is.null(data[[paste0("Retention_Rate_",str_replace_all(years_needed[i],"-",""))]])){
        p <-add_trace(p,x=data[[type]],  y=data[[paste0("Retention_Rate_",str_replace_all(years_needed[i],"-",""))]],
                      text=paste(years_needed[i],"\n",
                                 "Enrolled ",data[[paste0("Enrolled_",str_replace_all(years_needed[i],"-",""),"_")]],"\n",
                                 "Retained ",data[[paste0("Retained_",str_replace_all(years_needed[i],"-",""),"_")]],"\n",
                                 "Retention Rate",data[[paste0("Retention_Rate_",str_replace_all(years_needed[i],"-",""),"_")]],"%"),
                      name =paste(years_needed[i],"  "), marker = list(color = color_graph[i]),evaluate=TRUE)}
    }                                      
    
  }else if(status=="Success"){

    for(i in length(years_needed):1){
      if(!is.null(data[[paste0("Success_Rate_",str_replace_all(years_needed[i],"-",""))]])){
        p <-add_trace(p,x=data[[type]],  y=data[[paste0("Success_Rate_",str_replace_all(years_needed[i],"-",""))]],
                      text=paste(years_needed[i],"\n",
                                 "Enrolled ",data[[paste0("Enrolled_",str_replace_all(years_needed[i],"-",""),"_")]],"\n",
                                 "Succeeded ",data[[paste0("Succeeded_",str_replace_all(years_needed[i],"-",""),"_")]],"\n",
                                 "Success Rate",data[[paste0("Success_Rate_",str_replace_all(years_needed[i],"-",""),"_")]],"%"),
                      name =paste(years_needed[i],"  "), marker = list(color = color_graph[i]),evaluate=TRUE)}
    } 
  }else if(status=="Equity"){
    
    for(i in length(years_needed):1){
      if(!is.null(data[[paste0("Equity_Index_",str_replace_all(years_needed[i],"-",""))]])){
        p <-add_trace(p,x=data[[type]],  y=data[[paste0("Equity_Index_",str_replace_all(years_needed[i],"-",""))]],
                      text=paste(years_needed[i],"\n",
                                 "Enrolled ",data[[paste0("Enrolled_",str_replace_all(years_needed[i],"-",""),"_")]],"\n",
                                 "Equity Index",data[[paste0("Equity_Index_",str_replace_all(years_needed[i],"-",""))]]),
                      name =paste(years_needed[i],"  "), marker = list(color = color_graph[i]),evaluate=TRUE)}
    } 
     p<-p%>% add_trace(x=data[[type]], y=1, type = 'scatter', mode = 'lines',line = list(color = '#000000'),showlegend=FALSE) 
  }
  
  return(p)
  
  
}

create_line_comparison_graph<-function(data,years_needed,type,status){
# colors <- c('rgba(255, 232, 0,1)','rgba(212, 255, 44,1)', 'rgba(82, 214, 136,1)', 'rgb(90, 221, 147,1)',
#           'rgba(89, 199, 182,1)', 'rgb(94, 156, 147)','rgb(37, 94, 101)','rgba(0,0,0,1)')

 
  colors =  c( 'rgba(255, 232, 0,1)','rgb(90, 221, 147,1)',
                    'rgba(89, 199, 182,1)', 
                    'rgb(94, 156, 147)',
                    'rgb(37, 94, 101)',
                    'rgba(0,0,0,1)')

  options=unique(data[[type]])
#create some sensor for graph
  
  
  if(status=="Enrolled"){
    data<-sensor(data=data,column="Enrolled",add_column = T)
    p<-plot_ly( type="scatter", mode="markers+lines",  hoverinfo = 'text',sort=FALSE)%>%
      layout(title = paste(type ," Enrollment"),legend = list(orientation = 'h'),margin=list(r=20,b=30),
             xaxis=list( type = "category", categoryorder = "array",categoryarray = sort(unique(data$Academic_Year)))
             )%>% config(displayModeBar = F)
  
             

    for(i in 1:length(options)){
      temp=data[data[[type]]==options[i],]
      p<-p %>% add_trace(y=temp$Enrolled, x= temp$Academic_Year , type="scatter", mode="markers+lines" ,name=options[i],
                   line = list(color = colors[i]),text=paste(type,":",temp[[type]],"\n",temp$Enrolled_," students"),evaluate=TRUE)

    }
    p<- p 
    data$Enrolled_=NULL   
   
  }
  else if(status=="Retention"){
    data<-sensor(data=data,column="Retained",affected=c("Enrolled"))
    p<-plot_ly( type="scatter", mode="markers+lines",  hoverinfo = 'text')%>%
      layout(title = paste("Retention Rate"),legend = list(orientation = 'h'),margin=list(r=40,b=30),
             yaxis = list(side = 'left', title = paste0(status," Rate"),range=c(0,105)), 
             xaxis=list( type = "category", categoryorder = "array",categoryarray = sort(unique(data$Academic_Year)))
             )%>%  config(plot_ly(), displaylogo = FALSE,collaborate =FALSE,
                          modeBarButtonsToRemove = list('sendDataToCloud','pan2d','lasso2d','zoom2d','select2d',
                                                        'zoomIn2d','zoomOut2d','hoverClosestCartesian','hoverCompareCartesian',
                                                       'toImage','toggleSpikelines' ))
    
    
    for(i in 1:length(options)){
      temp=data[data[[type]]==options[i],]
      p<-p %>% add_trace( y=temp$Retention_Rate, x=temp$Academic_Year , type="scatter", mode="markers+lines" ,name=options[i],
                   line = list(color = colors[i]),text=paste(type,":",temp[[type]],"\n Enrolled:",temp$Enrolled,"\n Retained:",
                                                             temp$Retained,"\n Retention Rate:",temp$Retention_Rate,"%"))
    }
  }
  else if(status=="Success"){
    data<-sensor(data=data,column="Succeeded",affected=c("Enrolled"))
    p<-plot_ly( type="scatter", mode="markers+lines",  hoverinfo = 'text')%>%
      layout(title = paste("Success Rate"),legend = list(orientation = 'h'),margin=list(r=40,b=30),
             yaxis = list(side = 'left', title = paste0(status," Rate"),range=c(0,105)),
             xaxis=list( type = "category", categoryorder = "array",categoryarray = sort(unique(data$Academic_Year)))
             )%>%  config(plot_ly(), displaylogo = FALSE,collaborate =FALSE,
                          modeBarButtonsToRemove = list('sendDataToCloud','pan2d','lasso2d','zoom2d','select2d',
                                                        'zoomIn2d','zoomOut2d','hoverClosestCartesian','hoverCompareCartesian',
                                                        'toImage','toggleSpikelines' ))
    for(i in 1:length(options)){
      temp=data[data[[type]]==options[i],]
      p<-p %>% add_trace( y=temp$Success_Rate, x=temp$Academic_Year , type="scatter", mode="markers+lines" ,name=options[i],
                   line = list(color = colors[i]),text=paste(type,":",temp[[type]],"\n Enrolled:",temp$Enrolled,"\n Succeeded:",
                                                             temp$Succeeded,"\n Success Rate:",temp$Success_Rate,"%"))
    }
  }
  else{
    data<-sensor(data=data,column="Succeeded",affected=c("Enrolled"))
    p<-plot_ly( type="scatter", mode="markers+lines",  hoverinfo = 'text')%>%
      layout(title = paste("Equity Index (Success)"),legend = list(orientation = 'h'),margin=list(r=40,b=30),
             yaxis = list(side = 'left', title = paste0("Equity Index (Success)"),range=c(0,max(data$Equity_IndeX)+.1)),
             xaxis=list( type = "category", categoryorder = "array",categoryarray = sort(unique(data$Academic_Year)))
      )%>%  config(plot_ly(), displaylogo = FALSE,collaborate =FALSE,
                   modeBarButtonsToRemove = list('sendDataToCloud','pan2d','lasso2d','zoom2d','select2d',
                                                 'zoomIn2d','zoomOut2d','hoverClosestCartesian','hoverCompareCartesian',
                                                 'toImage','toggleSpikelines' ))
    for(i in 1:length(options)){
      temp=data[data[[type]]==options[i],]
      p<-p %>% add_trace( y=temp$Equity_Index, x=temp$Academic_Year , type="scatter", mode="markers+lines" ,name=options[i],
                   line = list(color = colors[i]),text=paste(type,":",temp[[type]],"\n Enrolled:",temp$Enrolled,"\n Succeeded:",
                                                             temp$Succeeded,"\n Equity Index:",temp$Equity_Index))
    }
 
  }
  return (p)
  
}

create_general_historical_graph<-function(data,type){
  color_graph =  c( 'rgb(90, 221, 147,1)',
                    'rgba(89, 199, 182,1)', 'rgb(94, 156, 147)',
                    'rgb(37, 94, 101)','rgba(0,0,0,1)')
  graph=NULL
  if(type=="Enrolled")
  {
   
    graph<-plot_ly(data,x=~Academic_Year,y=~Enrolled,type="bar",
                   text = ~paste(Academic_Year,"\n",Enrolled,"(enrolled)"),
                   insidetextfont = list(color = '#000000'),
                   hoverinfo = 'text',
                   marker = list(color =   color_graph)) %>%                                                      
      layout(title = paste("General Enrollment"),height="95%",
             yaxis =  list(title = "Enrollment"),
             xaxis=list(linecolor = "black",linewidth = 0.5,mirror = T,categoryorder = "trace"))%>%
      config(displayModeBar = F)

    
    }
  else if(type=="Retention_Rate"){
    graph<-plot_ly(data,x = ~Academic_Year, y = ~Retention_Rate,type="bar",
                   text = ~paste(Enrolled,"(enrolled)","\n",
                                Retained,"(retained)","\n",
                                Retention_Rate,"% "),
                   insidetextfont = list(color = '#000000'),
                   hoverinfo = 'text',
                   marker = list(color = color_graph)) %>%                                                      
      layout(title = paste("General Retention Rate"),
             yaxis =  list(title = "Retention Rate", range=c(0,110)), barmode = 'group',height="95%",
             xaxis=list(label=NULL,linecolor = "black",linewidth = 0.5,mirror = T))%>% config(displayModeBar = F)
  }
  else{
    graph<-plot_ly(data,x = ~Academic_Year, y = ~Success_Rate,type="bar",
                   text = ~paste(Enrolled,"(enrolled)","\n",
                                 Succeeded,"(succeeded)","\n",
                                 Success_Rate,"% "),
                   insidetextfont = list(color = '#000000'),
                   hoverinfo = 'text',
                   marker = list(color = color_graph)) %>%                                                      
      layout(title = paste("General Success Rate"), barmode = 'group',
             yaxis =  list(title = "Success Rate", range=c(0,110)),
             xaxis=list(label=NULL,linecolor = "black",linewidth = 0.5,mirror = T))%>% config(displayModeBar = F)
  }
  return(graph)
  
}

create_equity_graph<-function(data,type,orientation="v", year,height=0){
  color=NULL
  if(type=="Age"){
    color =  c('rgb(94, 156, 147)','rgba(89, 199, 182,1)',
               'rgb(82, 214, 136,1)','rgba(93, 237, 133,1)',
               'rgba(212, 255, 44,1)','rgba(255, 232, 0,1)')
  }
  else if(type=="Gender"){
    color =  c('rgb(89, 199, 182)',#lime green
               'rgb(37, 94, 101)' )
  }
  else if(type== "Status"){
    color =  c('rgb(212, 255, 44)',#yellow
               'rgb( 115, 231, 187 )')
  }
  else if(type=="Ethnicity" || type=="Groups"){
    color =  c('rgba(255, 232, 0,1)','rgba(212, 255, 44,1)',
               'rgba(82, 214, 136,1)', 'rgb(90, 221, 147,1)',
               'rgba(89, 199, 182,1)', 'rgb(94, 156, 147)',
               'rgb(37, 94, 101)','rgba(0,0,0,1)')
  }
  else if(type=="Major"){
    color = c('rgba(255, 232, 0,1)','rgba(212, 255, 44,1)',
              'rgba(82, 214, 136,1)')
  }
  
  
  result=vector(mode="list",length=2)
  
  if(orientation=="v"){
   p<-plot_ly(data)%>%
          add_trace(x=data[[type]],y=~Equity_Index,type="bar",
          text = paste(data[[type]],"\n", data$Enrolled,"(enrolled)","\n",
                        data$Equity_Index),
          insidetextfont = list(color = '#000000'),
          hoverinfo = 'text',
          marker = list(color =  color),showlegend=TRUE) %>%
    layout(title = paste0(type),
           xaxis = list(title = "",linecolor = "black",linewidth = 0.5,mirror = T,categoryorder = "trace"),
           yaxis = list(side = 'left', title = "Equity Index/Success", zeroline = FALSE),
           annotations = list(x =data[[type]] , y = ~Equity_Index, text =~paste(Equity_Index) ,
                              xanchor = 'center', yanchor = 'bottom',
                              showarrow = FALSE))%>% config(displayModeBar = F)
   
     result[[1]]<-p 
     result[[2]]<-p%>%add_trace(x=data[[type]], y=1, type = 'scatter', mode = 'lines',line = list(color = '#45171D'),showlegend=FALSE)
     }
  else if(height!=0){
    p<-plot_ly(data)%>%
      add_trace(y=data[[type]],x=~Equity_Index,type="bar",height=height,orientation="h",
                text = paste(data[[type]],"\n", data$Enrolled,"(enrolled)","\n", data$Equity_Index),
                insidetextfont = list(color = '#000000'),
                hoverinfo = 'text',
                marker = list(color =  c('rgba(255, 232, 0,1)','rgba(212, 255, 44,1)',
                                         'rgba(82, 214, 136,1)', 'rgb(90, 221, 147,1)',
                                         'rgba(89, 199, 182,1)', 'rgb(94, 156, 147)',
                                         'rgb(37, 94, 101)','rgba(0,0,0,1)'))) %>%
      layout(title = paste0(type),margin = list(l=70,r=-2),
             yaxis =  list( title = " " ,linecolor = "black",linewidth = 0.5,mirror = F, ticks='inside',categoryorder = "array"),
             xaxis = list(title = "Equity Index/Success"),
             annotations = list(y = data[[type]], x = ~Equity_Index, text =~paste(Equity_Index) ,
                                xanchor = 'bottom', yanchor = 'right',xshift=32,
                                showarrow = FALSE))%>% config(displayModeBar = F)
    result[[1]]<-p
     result[[2]]=p%>%add_trace(x=1, y=data[[type]], type = 'scatter', mode = 'lines',line = list(color = '#45171D'),showlegend=FALSE)
     }
      
  else{
    p<-plot_ly(data)%>%
      add_trace(x=~Equity_Index,y=data[[type]],type="bar",orientation="h",
                text =paste(data[[type]],"\n", data$Enrolled,"(enrolled)","\n",
                             data$Equity_Index, (" Equity Index")),
                insidetextfont = list(color = '#000000'),
                hoverinfo = 'text',
                marker = list(color =  c('rgba(255, 232, 0,1)','rgba(212, 255, 44,1)',
                                         'rgba(82, 214, 136,1)', 'rgb(90, 221, 147,1)',
                                         'rgba(89, 199, 182,1)', 'rgb(94, 156, 147)',
                                         'rgb(37, 94, 101)','rgba(0,0,0,1)'))) %>%
      layout(title =isolate(paste(type)),margin = list(l=180,r=-2),
             yaxis =  list( title = "",linecolor = "black",linewidth = 0.5,mirror = F, ticks='inside',categoryorder = "array"),
             xaxis = list(title = "Equity Index/Success"),
             annotations = list(y = data[[type]], x = ~Equity_Index, text =~paste(Equity_Index) ,
                                xanchor = 'bottom', yanchor = 'right',xshift=32,
                                showarrow = FALSE))%>% config(displayModeBar = F)
    result[[1]]<-p
     result[[2]]<-p%>%add_trace(x=1, y=data[[type]], type = 'scatter', mode = 'lines',line = list(color = '#45171D'),showlegend=FALSE)}
      
 
  
 
  
  #used to add annotation
  #annotations = list(y = data[[type]], x = ~Equity_Index, text =~paste(Equity_Index) ,
                    # xanchor = 'bottom', yanchor = 'right',xshift=32,
                     #showarrow = FALSE)
  return(result)
    
}

create_head_count<-function(data,type,chart="pie",title, height=NULL){
  colors <- c('rgba(255, 232, 0,1)','rgba(212, 255, 44,1)',
              'rgba(82, 214, 136,1)', 'rgb(90, 221, 147,1)',
              'rgba(89, 199, 182,1)', 'rgb(94, 156, 147)',
              'rgb(37, 94, 101)','rgba(0,0,0,1)')
  
  if(type=="Gender"){
    writting_on_graph='label+percent'
  }
  else{
    writting_on_graph='percent'
  }
  
  if(chart=="pie"){
    if(type=="Age"){
    p<-plot_ly(data,labels = data[[type]], values =  ~Head_Percent, type = 'pie',
               text= ~paste(data[[type]],"\n", Head_Count,"(headcount)","\n",Head_Percent,"%"),
               hoverinfo='text',
               textinfo="percent",
               insidetextfont = list(color = '#000000'),
               sort = FALSE,
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1))
               #The 'pull' attribute can also be used to create space between the sectors
      ) %>%
      layout(title = title,showlegend = T,legend = list(orientation = 'h'),margin=list(r=38),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,categoryorder = "trace"),
             yaxis = list(showgrid = FALSE, zeroline = FALSE,  showticklabels = FALSE,categoryorder = "trace"))%>% config(displayModeBar = F)}
    
    else if(type=="Ethnicity"){
     p<- plot_ly(data,labels = data[[type]], values =  ~Head_Percent,
              text= ~paste(data[[type]],"\n", Head_Count,"(headcount)","\n",Head_Percent,"%"),
              hoverinfo='text',
              textinfo=writting_on_graph,
              insidetextfont = list(color = '#000000'),
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1))) %>%
        add_pie(hole = 0.5) %>%
        layout(title = title,  showlegend = T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(tshowgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>% config(displayModeBar = F)
    }
#===================================================================================================================================================
  else{
    p<-plot_ly(data, labels = data[[type]], values = ~Head_Percent, type = 'pie',
            text= ~paste(data[[type]],"\n", Head_Count,"(headcount)","\n",Head_Percent,"%"),
            textinfo = writting_on_graph,
            insidetextfont = list(color = '#000000'),
            hoverinfo = 'text',
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1))
            #The 'pull' attribute can also be used to create space between the sectors
    ) %>%
      layout(title = title,showlegend = TRUE,legend = list(orientation = 'h'),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>% config(displayModeBar = F)

    }
    
    
    
    
    
 
  }
  if(chart=="bar"){
    
   p<- plot_ly(data,x = data[[type]], y = ~Head_Count,height=340,
            text = ~paste(data[[type]], Head_Count_, ' students'),type="bar",
            insidetextfont = list(color = '#000000'),
            hoverinfo = 'text',
            marker = list(color =  c('rgba(255, 232, 0,1)','rgba(212, 255, 44,1)',
                                     'rgba(93, 237, 133,1)', 'rgb(90, 221, 147,1)',
                                     'rgba(89, 199, 182,1)','rgba(89, 173, 199,1)'))) %>%
      layout(title = title,yaxis = list(title="Headcount",showticklabels = FALSE),
             annotations = list(x = data[[type]], y = ~Head_Count, text =~paste(Head_Percent,"%") ,
                                xanchor = 'center', yanchor = 'bottom',
                                showarrow = FALSE))%>% config(displayModeBar = F)
    
    
  }

  
  return (p)
    
}

get_noti=function(){
  notification <- notificationItem(icon = icon("question"), status = "info", paste0("Frequently Asked Questions"))
  notification$children[[1]] <- a(href="#shiny-tab-main","onclick"=paste0("clickFunction('",
                                                                          paste0(substr(as.character(runif(1, 0, 1)),1,6),
                                                                                 "Frequently Asked Questions"),"'); return false;"),
                                  list(notification$children[[1]]$children))
  return(notification)
}

get_noti2=function(){
  notification <- notificationItem(icon = icon("info-circle"), status = "info", paste0("Tutorial"))
  notification$children[[1]] <- a(href="#shiny-tab-main","onclick"=paste0("clickFunction('",
                                                                          paste0(substr(as.character(runif(1, 0, 1)),1,6),
                                                                                 "Tutorial"),"'); return false;"),
                                  list(notification$children[[1]]$children))
  return(notification)
}

sections_available<-function(department,discipline,course, semester){
  temp=NULL

  if(department!="ALL"){
    temp=courses[courses$Department==department,]
    
  }else{
    if(!is.null(semester)){
      return(length(unique(courses$Section_Number)))
    }else{
      
      temp=ddply(courses,.(Year_Semester),summarize,Sections=length(unique(Section_Number)))
      return(sum(temp$Sections))
    }
  }

  if(discipline!="ALL"){
    temp=temp[temp$Discipline==discipline,]
  }
  
  if(course!="ALL"){
    temp=temp[temp$Course==course,]
  }
  
  if(is.null(semester)){
    temp=ddply(temp,.(Year_Semester),summarize,Sections=length(unique(Section_Number)))
    return(sum(temp$Sections))
  }
  else{
    return(length(unique(temp$Section_Number)))
  }
}

fix_years<-function(data){
  result=data
  if(!grepl("-",result$Academic_Year)){
    result$Academic_Year=as.character(result$Academic_Year)
    for(i in 1:length(result$Academic_Year)){
      result$Academic_Year[i]=paste( substr(result$Academic_Year[i],1,4),"-",substr(result$Academic_Year[i],5,8))
    }
  }
  return(result)
}

get_year_semester<-function(year,semester){
  year=str_replace_all(year,"-","")
  
  if(semester==0 || semester=="Winter"){
    return(paste0(substr(year,5,8),0))
  }else if(semester==1 || semester=="Spring"){
    return(paste0(substr(year,5,8),1))
  }else if(semester==2 || semester=="Summer"){
    return(paste0(substr(year,1,4),2))
  }else if(semester==3 || semester=="Fall"){
    return(paste0(substr(year,1,4),3))
  }
}

