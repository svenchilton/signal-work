/* 
Solutions to company.sql homework exercises
Sven Chilton
Signal Data Science Cohort 3
July 18, 2016
*/

.read company.sql

select '';
select 'The following tables are contained in company.sql:';
.tables

select '';
select 'Table "records" contains the following columns:';
pragma table_info(records);

select '';
select 'Table "records" contains '||count(*)||' rows' 
from records; 

select '';
select 'Table "meetings" contains the following columns:';
pragma table_info(meetings);

select '';
select 'Table "meetings" contains '||count(*)||' rows' 
from meetings; 

select '';
select 'All the data in the "records" table:';
select * from records;

select '';
select 'Oliver Warbucks manages the following employees directly:';
select Name from records where Supervisor = 'Oliver Warbucks';

select '';
select 'All information about self-supervising employees:';
select * from records where Name = Supervisor;

select '';
select 'All employees with salary greater than 50000, in alphabetical order';
select Name from records where Salary > 50000 order by Name;

select '';
select 'Employee, salary, supervisor and supervisor’s salary, containing ';
select 'all supervisors who earn more than twice as much as the employee:';
select x.Name as Employee, 
       x.Salary, 
       x.Supervisor, 
       y.Salary as Supervisor_Salary
from records x
     join records y on x.Supervisor = y.Name
where Supervisor_Salary > 2*x.Salary;

select '';
select 'Each employee whose manager is in a different division:';
select x.Name
from records x
     join records y on x.Supervisor = y.Name
where x.Division != y.Division;

select '';
select 'Meeting days and times of all employees directly supervised ';
select 'by Oliver Warbucks:';
select x.Name, y.Day, y.Time
from records x
     join meetings y on x.Division = y.Division
where x.Supervisor = 'Oliver Warbucks';

select '';
select 'The following employees are middle managers; they each ';
select 'supervise someone and a different someone supervises them:';
select distinct y.Name
from records x
     join records y on x.Supervisor = y.Name
     join records z on y.Supervisor = z.Name
where x.Name != y.Name and y.Name != z.Name;

select '';
select 'The following employees meet on the same day as their supervisor, ';
select 'not including Oliver Warbucks, who is his own supervisor:';
select distinct x.Name
from records x
     join records y on x.Supervisor = y.Name
     join meetings xx on x.Division = xx.Division
     join meetings yy on y.Division = yy.Division
where xx.Day = yy.Day and x.Name != x.Supervisor;

select '';
select 'Each supervisor and the some of the salaries of the employees ';
select 'whom s/he supervises directly, including self-supervisors:';
select Supervisor, sum(Salary)
from records
group by Supervisor;

select '';
select 'All salaries which appear more than once in "records" table:';
select Salary 
from records 
group by Salary 
having count(Salary) > 1;

