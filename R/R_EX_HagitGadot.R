
library(dplyr)

library(DBI)
con <- dbConnect(odbc::odbc(), "COLLEGE", timeout = 10)
Students<-dbReadTable(con,"students")
Classrooms<-dbReadTable(con,"Classrooms")
Teachers=dbReadTable(con,"Teachers")
Courses=dbReadTable(con,"courses")
Department=dbReadTable(con,"departments")


############################################################################
#---- 2a. Number of students by Department

course_class <-inner_join(Courses, classrooms,by="CourseId")
stu_by_course<-inner_join(course_class,Department,by=c("DepartmentID"=="DepartmentId"))
uniq<-stu_by_course%>%
  group_by(DepartmentName,StudentId)%>%
  summarise(d=mean(degree),count=n())
a2<-uniq%>%
    group_by(DepartmentName)%>%
    summarise(count=n())
a2

############################################################################
#2b. How many studenst has the English teacher by course and in total?

course_class <-inner_join(Courses, classrooms,by="CourseId")

BB<-course_class%>%
  filter(DepartmentID == "1") %>%
  group_by(CourseName)%>% tally()

Total<-BB%>%
  group_by()%>%
  summarise(total_eng=sum(BB$n ))

b2<-BB%>%
    add_row(CourseName="Total", n=Total$total_eng)
b2

#####################################################################################
#---- 2c. How many small (<22) and bigger (>=22) Classrooms has the Science Department?

CC<-course_class%>%
  filter(DepartmentID == "2") %>%
  group_by(CourseName)%>% tally()

C1<-CC%>%
  filter(CC$n>22)
BigClass<-C1%>%group_by%>%summarise(count=n())

C2<-CC%>%
  filter(CC$n<=22)
SmallClass<-C2%>%group_by%>%summarise(count=n())

a<-c("Small classes","Big classes")
b<-c(SmallClass$count,BigClass$count)
df2c<-data.frame("In_the_Science_Department"=a,"Classrooms"=b)
df2c

##################################################################################
##2d. How many students are by Gender?



d2<- Students%>%
  group_by(Gender)%>%
  summarise(count=n())
d2
 
 ##################################################################################
 #----- 2e. In which courses the percentage of males / females are higher than 70% ?  


course_class <-inner_join(classrooms, Courses,by="CourseId")
Stu_class <-inner_join(course_class, Students ,by="StudentId")
coures_gender<- Stu_class%>%group_by(CourseName,Gender)%>%tally()
All_stu<-Stu_class%>%group_by(CourseName)%>%tally()
Homogenic<-inner_join(coures_gender,All_stu,by="CourseName")
Homogenic$percent=(Homogenic$n.x/Homogenic$n.y)*100
e2<-Homogenic%>% select(Gender,percent) %>%filter(percent>70) 
e2

##################################################################################
#----- ----- 2f. How many students (n and %) have a degree of 80+ by Department?  

e1<-inner_join(Stu_class,Department,by=c("DepartmentID"="DepartmentId"))

e_all<-e1%>%
  group_by(DepartmentName,StudentId)%>%
  summarise(student_Degree=max(degree))

e_top<-e_all%>%filter(student_Degree>80)

e_top<-e_top%>%group_by(DepartmentName)%>%summarise(count=n())
e_all<-e_all%>%group_by(DepartmentName)%>%summarise(count=n())
e<-inner_join(e_top,e_all,by="DepartmentName")
e$Top_Ration=(e$count.x/e$count.y)*100
f2<-e%>% select(DepartmentName,all_student=count.y,Top_students=count.x,Top_Ration)
f2

##################################################################################
##2g. How many students (n and %) have a degree lower than 60 by Department?


f_all<-e1%>%
  group_by(DepartmentName,StudentId)%>%
  summarise(student_Degree=min(degree))

f_low<-f_all%>%filter(student_Degree<60)

f_low<-f_low%>%group_by(DepartmentName)%>%summarise(count=n())
f_all<-f_all%>%group_by(DepartmentName)%>%summarise(count=n())
f<-inner_join(f_low,f_all,by="DepartmentName")
f$Low_Ration=(f$count.x/f$count.y)*100

g2<-f%>% select(DepartmentName,"Improvment need students"=count.x,all_student=count.y,Low_Ration)
g2

##############################################################################
##### 2h Rate in descending order the teachers by their student's mean degree.

a<-inner_join(Courses,Teachers, by="TeacherId")
a$TeacherName=paste(a$FirstName," ",a$LastName)
b=inner_join(a,classrooms,by="CourseId")
h2<-b%>%group_by(TeacherName)%>%summarise(Teacher_mean=mean(degree))
h2<-h2[order(-h2$Teacher_mean),]
h2

##############################################################################
##3a. shows the courses, departments, teachers and number of students on each

Teachers_courses<-inner_join(inner_join(Teachers,Courses,by="TeacherId"),Department ,by=c("DepartmentID"="DepartmentId"))
C_S<-inner_join(Classrooms,Students,by= "StudentId")
Stu_in_cou<-C_S%>%
  group_by(CourseId)%>%summarise(count=n())
s<-left_join(Teachers_courses,Stu_in_cou,by="CourseId")
s[is.na(s)]=0

s<-s[order(-s$count),]
a3<-s%>%select(DepartmentName,CourseName,FirstName,LastName,"Student_num"=count)
a3

##############################################################################
#3b. Create a view that shows each student, the number of courses taken,their mean degree by department and the total degree mean.

co_dep<-left_join(Courses,Department, by=c("DepartmentID"="DepartmentId"))
cla_st<-left_join(left_join(students,Classrooms, by="StudentId"),co_dep,"CourseId")
all<-cla_st%>%group_by(StudentId)%>%summarise(Dep_degree=mean(degree),count=n())


english<-cla_st%>%filter(DepartmentID==1)
science<-cla_st%>%filter(DepartmentID==2)
arts<-cla_st%>%filter(DepartmentID==3)
sport<-cla_st%>%filter(DepartmentID==4)


e1<-english%>%group_by(StudentId)%>%summarise("English"=mean(degree))
e2<-science%>%group_by(StudentId)%>%summarise("Science"=mean(degree))
e3<-science%>%group_by(StudentId)%>%summarise("Art"=mean(degree))
e4<-sport%>%group_by(StudentId)%>%summarise("Sport"=mean(degree))

e<-full_join(full_join(full_join(full_join(all,e1,by="StudentId"),e2,by="StudentId"),e3,by="StudentId"),e4,by="StudentId")
e_all<-left_join(e,students,by="StudentId")

b3<-e_all%>%select(StudentId,FirstName,LastName,"No of courses"=count,Science,English,Art, Sport,Dep_degree)
b3


