<<<<<<< HEAD
﻿--a.	מנהל המכללה ביקש לדעת כמה סטודנטים יש לפי יחידה -מחלקה.
-------------------------------------------------------------------------

SELECT  Departments.DepartmentName,count(Students.StudentId) as studentsNUM
			from Classrooms
			LEFT OUTER JOIN Students
			ON Students.StudentId=Classrooms.StudentId
			inner join Courses as C
			on c.CourseId =Classrooms.CourseId
			full outer join Departments
			on Departments.DepartmentId=C.DepartmentID
			group by Departments.DepartmentName


--	b.	המורה באנגלית צריך להתארגן וביקש לדעת כמה סטודנטים יש לו לפי כל קורס שהוא מעביר וסה"כ התלמידים בכל הקורסים שלו.
-----------------------------------------------------------------------------------------------------------------------


SELECT  C.CourseName, count (Students.StudentId) as studentsNUM
			from Classrooms
			inner JOIN Students
			ON Students.StudentId=Classrooms.StudentId
			inner join Courses as C
			on c.CourseId =Classrooms.CourseId
			inner join Departments
			on Departments.DepartmentId=C.DepartmentID
			where Departments.DepartmentName='English'
			group by Departments.DepartmentName,c.CourseName
		union all
			SELECT  'ALL_STUDENTDS' as [CourseName] ,count (Students.StudentId) as studentsNUM
			from Classrooms
			inner JOIN Students
			ON Students.StudentId=Classrooms.StudentId
			inner join Courses as C
			on c.CourseId =Classrooms.CourseId
			inner join Departments
			on Departments.DepartmentId=C.DepartmentID
			where Departments.DepartmentName='English'
		

			
--c.	המרכז למדעים רוצה להבין כמה כיתות קטנות (מתחת ל-22) וכמה גדולות צריך עבור קורסים ביחידה (מחלקה) שלו.
-----------------------------------------------------------------------------------------------

select Class_Size.Class_TYPE,count(Class_Size.NUM)as Students_Num
 from 
(	select Science_Classes.CourseName, Science_Classes.NUM ,
	case when NUM>=22 then 'Large' 
	else 'Small'
	end as Class_TYPE
	from(
		select count(studentid) as NUM, c.CourseName
		from Classrooms inner join Courses as C
			on c.CourseId =Classrooms.CourseId
			where (C.DepartmentID=2)
			group by CourseName
		)as Science_Classes) as Class_Size 
group by Class_TYPE




----d.	סטודנטית שהיא פעילה פמיניסטית טוענת שהמכללה מעדיפה לקבל יותר גברים מאשר נשים. 
---תבדקו האם הטענה מוצדקת (מבחינת כמותית, לא סטטיסטית).
	
select Students.Gender,
	count( [StudentId]) as Student_count
from 	[dbo].[Students]
group by [Gender]


--e.	באיזה קורסים אחוז הגברים / הנשים הינה מעל 70%?
---------------------------------------------------------------



		select distinct CGC.CourseName, 
			FIRST_VALUE(class_size) over (partition by courseName  order by courseName) as Male,
			case when (lead(class_size) over (partition by courseName order by courseName))is null then Class_Size
			else  
			lead(class_size) over (partition by courseName order by courseName)
			end  as Female
-----creating table to count students in each class by gender
	into #class_gender
		from (
			select count (Classrooms.studentId)as Class_Size,(Students.Gender), Courses.CourseName
			from Classrooms left join Students 
			on  Classrooms.StudentId=Students.StudentId
			inner join Courses on Classrooms.CourseId=Courses.CourseId 
			group by Courses.CourseName, students.gender)as CGC

	select #class_gender.CourseName from #class_gender
	where (10*#class_gender.male)>7*(#class_gender.Female+#class_gender.Male)
		or(10*#class_gender.Female)>7*(#class_gender.Female+#class_gender.Male)

drop table #class_gender

----f.	בכל אחד מהיחידות (מחלקות), כמה סטודנטים (מספר ואחוזים) עברו עם ציון מעל 80?
------------------------------------------------------------------------------------
 

select departments.DepartmentName, count (Classrooms.StudentId)as all_st, avg(top_table.top_st)as top_students
	into #count_top_class
 from Departments inner join Courses on Departments.DepartmentId=Courses.DepartmentID 
   inner join Classrooms on courses.CourseID=classrooms.CourseId
	inner join
		(select Departments.DepartmentName, count(Classrooms.StudentId) as top_st
		from Departments inner join Courses on Departments.DepartmentId=Courses.DepartmentID
				INNER JOIN classrooms ON courses.CourseID=classrooms.CourseId
		where classrooms.degree>80
		group by Departments.DepartmentName)as top_table on top_table.DepartmentName=Departments.DepartmentName 
group by Departments.DepartmentName

		select  #count_top_class.DepartmentName,#count_top_class.top_students,
		#count_top_class.all_st as all_students,
		(#count_top_class.top_students *1.00/#count_top_class.all_st*1.00)*100 as top_students_precent
		 
 from #count_top_class

drop table #count_top_class


----g.	בכל אחד מהיחידות (מחלקות), כמה סטודנטים (מספר ואחוזים) לא עברו (ציון מתחת ל-60) ?
-------------------------------------------------------------------------------------------



select departments.DepartmentName, count (Classrooms.StudentId)as all_students, avg(low_table.low_students)as low_students
	into #count_class
 from Departments inner join Courses on Departments.DepartmentId=Courses.DepartmentID 
   inner join Classrooms on courses.CourseID=classrooms.CourseId
	inner join
		(select Departments.DepartmentName, count(Classrooms.StudentId) as low_students

		from Departments inner join Courses on Departments.DepartmentId=Courses.DepartmentID
				INNER JOIN classrooms ON courses.CourseID=classrooms.CourseId
			where classrooms.degree<60
			group by Departments.DepartmentName)as low_table on low_table.DepartmentName =Departments.DepartmentName 
		group by Departments.DepartmentName

		select  #count_class.DepartmentName,#count_class.low_students,#count_class.all_students as all_students,
		(#count_class.low_students*1.0/#count_class.all_students *1.0)*100 as improvment_nead_precent
		 
 from #count_class

drop table #count_class

select * from #count_class

----h.	תדרגו את המורים לפי ממוצע הציון של הסטודנטים מהגבוהה לנמוך.
----------------------------------------------------------------------	


select FirstName, LastName,students_mean from(
	select Courses.TeacherId, count(Classrooms.degree) as students_num , (avg(degree)*1.0) as students_mean
	FROM courses
	INNER JOIN classrooms ON courses.CourseID=classrooms.CourseId
	group by Courses.TeacherId) as Temp_table 
	inner join Teachers on Teachers.TeacherId=Temp_table.TeacherId
	order by students_mean desc

	------------------------

--------------------------------------------------------------------------
-------------------------------------------------------------------------


--a.	תייצרו VIEW 
----המראה את הקורסים, היחידות (מחלקות) עליהם משויכים, המרצה בכל קורס ומספר התלמידים רשומים בקורס
-----------------------------------------------------------------------------------------


create view BIU_students_v as (
select Departments.DepartmentName,teachers.FirstName, Teachers.LastName ,
	courses.CourseName, count(Students.StudentId) as student_NUM
from Departments inner join Courses on Departments.DepartmentId=Courses.DepartmentID
	inner join Teachers on Teachers.TeacherId=Courses.TeacherId
	inner join Classrooms on Classrooms.CourseId=Courses.CourseId
	inner join Students on Students.StudentId=Classrooms.StudentId
group by teachers.TeacherId, courses.CourseName,
	teachers.FirstName, Teachers.LastName,Departments.DepartmentName
	)



-----------------------------------------------
----c.	VIEW המראה את התלמידים,
--------------מס' הקורסים שהם לוקחים,הממוצע של הציונים לפי יחידה (מחלקה)
--- והממוצע הכולל שלהם

create view Graduation_report as (
	select Students.FirstName, Students.LastName,Classrooms.StudentId,
	Departments.DepartmentName,
	count(Classrooms.degree) aS courses_count,
	avg(Classrooms.degree)*1.0 as course_avg,
	student_avg_t.student_avg

	from Classrooms
	inner join Courses on Classrooms.CourseId=Courses.CourseId
	inner join Departments on Departments.DepartmentId=Courses.DepartmentID
	inner join Students on Students.StudentId=Classrooms.StudentId
	inner join 
	---------ממוצע ציון  לסטודנט
	(
		select avg(Classrooms.degree )*1.0 as student_avg, StudentId
		from 
		Classrooms inner join Courses on Classrooms.CourseId=Courses.CourseId	
		group by StudentId) as student_avg_T
		on student_avg_T.StudentId=Students.StudentId	
	group by Departments.DepartmentName, Classrooms.StudentId,Students.FirstName,
	 Students.LastName, student_avg_T.student_avg
)
=======
﻿--a.	מנהל המכללה ביקש לדעת כמה סטודנטים יש לפי יחידה -מחלקה.
-------------------------------------------------------------------------

SELECT  Departments.DepartmentName,count(Students.StudentId) as studentsNUM
			from Classrooms
			LEFT OUTER JOIN Students
			ON Students.StudentId=Classrooms.StudentId
			inner join Courses as C
			on c.CourseId =Classrooms.CourseId
			full outer join Departments
			on Departments.DepartmentId=C.DepartmentID
			group by Departments.DepartmentName


--	b.	המורה באנגלית צריך להתארגן וביקש לדעת כמה סטודנטים יש לו לפי כל קורס שהוא מעביר וסה"כ התלמידים בכל הקורסים שלו.
-----------------------------------------------------------------------------------------------------------------------


SELECT  C.CourseName, count (Students.StudentId) as studentsNUM
			from Classrooms
			inner JOIN Students
			ON Students.StudentId=Classrooms.StudentId
			inner join Courses as C
			on c.CourseId =Classrooms.CourseId
			inner join Departments
			on Departments.DepartmentId=C.DepartmentID
			where Departments.DepartmentName='English'
			group by Departments.DepartmentName,c.CourseName
		union all
			SELECT  'ALL_STUDENTDS' as [CourseName] ,count (Students.StudentId) as studentsNUM
			from Classrooms
			inner JOIN Students
			ON Students.StudentId=Classrooms.StudentId
			inner join Courses as C
			on c.CourseId =Classrooms.CourseId
			inner join Departments
			on Departments.DepartmentId=C.DepartmentID
			where Departments.DepartmentName='English'
		

			
--c.	המרכז למדעים רוצה להבין כמה כיתות קטנות (מתחת ל-22) וכמה גדולות צריך עבור קורסים ביחידה (מחלקה) שלו.
-----------------------------------------------------------------------------------------------

select Class_Size.Class_TYPE,count(Class_Size.NUM)as Students_Num
 from 
(	select Science_Classes.CourseName, Science_Classes.NUM ,
	case when NUM>=22 then 'Large' 
	else 'Small'
	end as Class_TYPE
	from(
		select count(studentid) as NUM, c.CourseName
		from Classrooms inner join Courses as C
			on c.CourseId =Classrooms.CourseId
			where (C.DepartmentID=2)
			group by CourseName
		)as Science_Classes) as Class_Size 
group by Class_TYPE




----d.	סטודנטית שהיא פעילה פמיניסטית טוענת שהמכללה מעדיפה לקבל יותר גברים מאשר נשים. 
---תבדקו האם הטענה מוצדקת (מבחינת כמותית, לא סטטיסטית).
	
select Students.Gender,
	count( [StudentId]) as Student_count
from 	[dbo].[Students]
group by [Gender]


--e.	באיזה קורסים אחוז הגברים / הנשים הינה מעל 70%?
---------------------------------------------------------------



		select distinct CGC.CourseName, 
			FIRST_VALUE(class_size) over (partition by courseName  order by courseName) as Male,
			case when (lead(class_size) over (partition by courseName order by courseName))is null then Class_Size
			else  
			lead(class_size) over (partition by courseName order by courseName)
			end  as Female
-----creating table to count students in each class by gender
	into #class_gender
		from (
			select count (Classrooms.studentId)as Class_Size,(Students.Gender), Courses.CourseName
			from Classrooms left join Students 
			on  Classrooms.StudentId=Students.StudentId
			inner join Courses on Classrooms.CourseId=Courses.CourseId 
			group by Courses.CourseName, students.gender)as CGC

	select #class_gender.CourseName from #class_gender
	where (10*#class_gender.male)>7*(#class_gender.Female+#class_gender.Male)
		or(10*#class_gender.Female)>7*(#class_gender.Female+#class_gender.Male)

drop table #class_gender

----f.	בכל אחד מהיחידות (מחלקות), כמה סטודנטים (מספר ואחוזים) עברו עם ציון מעל 80?
------------------------------------------------------------------------------------
 

select departments.DepartmentName, count (Classrooms.StudentId)as all_st, avg(top_table.top_st)as top_students
	into #count_top_class
 from Departments inner join Courses on Departments.DepartmentId=Courses.DepartmentID 
   inner join Classrooms on courses.CourseID=classrooms.CourseId
	inner join
		(select Departments.DepartmentName, count(Classrooms.StudentId) as top_st
		from Departments inner join Courses on Departments.DepartmentId=Courses.DepartmentID
				INNER JOIN classrooms ON courses.CourseID=classrooms.CourseId
		where classrooms.degree>80
		group by Departments.DepartmentName)as top_table on top_table.DepartmentName=Departments.DepartmentName 
group by Departments.DepartmentName

		select  #count_top_class.DepartmentName,#count_top_class.top_students,
		#count_top_class.all_st as all_students,
		(#count_top_class.top_students *1.00/#count_top_class.all_st*1.00)*100 as top_students_precent
		 
 from #count_top_class

drop table #count_top_class


----g.	בכל אחד מהיחידות (מחלקות), כמה סטודנטים (מספר ואחוזים) לא עברו (ציון מתחת ל-60) ?
-------------------------------------------------------------------------------------------



select departments.DepartmentName, count (Classrooms.StudentId)as all_students, avg(low_table.low_students)as low_students
	into #count_class
 from Departments inner join Courses on Departments.DepartmentId=Courses.DepartmentID 
   inner join Classrooms on courses.CourseID=classrooms.CourseId
	inner join
		(select Departments.DepartmentName, count(Classrooms.StudentId) as low_students

		from Departments inner join Courses on Departments.DepartmentId=Courses.DepartmentID
				INNER JOIN classrooms ON courses.CourseID=classrooms.CourseId
			where classrooms.degree<60
			group by Departments.DepartmentName)as low_table on low_table.DepartmentName =Departments.DepartmentName 
		group by Departments.DepartmentName

		select  #count_class.DepartmentName,#count_class.low_students,#count_class.all_students as all_students,
		(#count_class.low_students*1.0/#count_class.all_students *1.0)*100 as improvment_nead_precent
		 
 from #count_class

drop table #count_class

select * from #count_class

----h.	תדרגו את המורים לפי ממוצע הציון של הסטודנטים מהגבוהה לנמוך.
----------------------------------------------------------------------	


select FirstName, LastName,students_mean from(
	select Courses.TeacherId, count(Classrooms.degree) as students_num , (avg(degree)*1.0) as students_mean
	FROM courses
	INNER JOIN classrooms ON courses.CourseID=classrooms.CourseId
	group by Courses.TeacherId) as Temp_table 
	inner join Teachers on Teachers.TeacherId=Temp_table.TeacherId
	order by students_mean desc

	------------------------

--------------------------------------------------------------------------
-------------------------------------------------------------------------


--a.	תייצרו VIEW 
----המראה את הקורסים, היחידות (מחלקות) עליהם משויכים, המרצה בכל קורס ומספר התלמידים רשומים בקורס
-----------------------------------------------------------------------------------------


create view BIU_students_v as (
select Departments.DepartmentName,teachers.FirstName, Teachers.LastName ,
	courses.CourseName, count(Students.StudentId) as student_NUM
from Departments inner join Courses on Departments.DepartmentId=Courses.DepartmentID
	inner join Teachers on Teachers.TeacherId=Courses.TeacherId
	inner join Classrooms on Classrooms.CourseId=Courses.CourseId
	inner join Students on Students.StudentId=Classrooms.StudentId
group by teachers.TeacherId, courses.CourseName,
	teachers.FirstName, Teachers.LastName,Departments.DepartmentName
	)



-----------------------------------------------
----c.	VIEW המראה את התלמידים,
--------------מס' הקורסים שהם לוקחים,הממוצע של הציונים לפי יחידה (מחלקה)
--- והממוצע הכולל שלהם

create view Graduation_report as (
	select Students.FirstName, Students.LastName,Classrooms.StudentId,
	Departments.DepartmentName,
	count(Classrooms.degree) aS courses_count,
	avg(Classrooms.degree)*1.0 as course_avg,
	student_avg_t.student_avg

	from Classrooms
	inner join Courses on Classrooms.CourseId=Courses.CourseId
	inner join Departments on Departments.DepartmentId=Courses.DepartmentID
	inner join Students on Students.StudentId=Classrooms.StudentId
	inner join 
	---------ממוצע ציון  לסטודנט
	(
		select avg(Classrooms.degree )*1.0 as student_avg, StudentId
		from 
		Classrooms inner join Courses on Classrooms.CourseId=Courses.CourseId	
		group by StudentId) as student_avg_T
		on student_avg_T.StudentId=Students.StudentId	
	group by Departments.DepartmentName, Classrooms.StudentId,Students.FirstName,
	 Students.LastName, student_avg_T.student_avg
)
>>>>>>> 09e97300c08ed82afbc3b1ff6711a93f409a91c5
