/* 
Solutions to dogs.sql homework exercises
Sven Chilton
Signal Data Science Cohort 3
July 18, 2016
*/

.read dogs.sql

select '';
select 'The following tables are contained in dogs.sql:';
.tables

select '';
select 'Table "dogs" contains the following columns:';
pragma table_info(dogs);

select '';
select 'Table "dogs" contains '||count(*)||' rows' 
from dogs; 

select '';
select 'Table "parents" contains the following columns:';
pragma table_info(parents);

select '';
select 'Table "parents" contains '||count(*)||' rows' 
from parents; 

select '';
select 'Table "sizes" contains the following columns:';
pragma table_info(sizes);

select '';
select 'Table "sizes" contains '||count(*)||' rows' 
from sizes;

select '';
select 'Practice with creating and extracting information ';
select 'from local tables:';

select '';
select 'Integers 0 through 10, inclusive, and their factorials:';
with factorials(n, f) as (
     select 0, 1 union
     select n+1, f*(n+1) from factorials where n < 10
)
select * from factorials;

select '';
select '3-number-long sequences for integers 0 through 14, inclusive:';
with seq3(x, y, z) as (
     select 0, 1, 2 union
     select x+3, y+3, z+3 from seq3 where x < 12
)
select * from seq3;

select '';
select 'All dogs that have a parent, ordered by descending parent height:';
select x.name
from dogs x
     join parents y on x.name = y.child
     join dogs z on y.parent = z.name
order by z.height desc;

select '';
select 'Each pair of siblings in the same size category:';
with sibling_pairs(sib1, sib2) as (
     select x.child as sib1, y.child as sib2
     from parents x
          join parents y on x.parent = y.parent
     where sib1 < sib2
)
select sib1 || ' and ' || sib2 || ' are ' || s1.size || ' siblings'
from sibling_pairs
     join dogs d1 on sib1 = d1.name
     join dogs d2 on sib2 = d2.name
     join sizes s1 on (d1.height > s1.min) and (d1.height <= s1.max)
     join sizes s2 on (d2.height > s2.min) and (d2.height <= s2.max)
     where s1.size = s2.size;

select '';
select 'Each stack of dogs, consisting of 4 distinct dogs, with a stack ';
select 'height of at least 170, arranged by ascending dog and stack height:'
select a.name || ', ' || 
       b.name || ', ' || 
       c.name || ', ' ||
       d.name, 
       a.height + b.height + c.height + d.height as stack_height
from dogs a
     join dogs b on a.height < b.height
     join dogs c on b.height < c.height
     join dogs d on c.height < d.height
where stack_height > 170
order by stack_height;

select '';
select 'Height and name of every dog that shares the 10s digit of its ';
select 'height with at least one other dog and has the highest 1s digit ';
select 'of all dogs with heights of the same 10s digit:';
select height, name
from dogs
group by (height/10)
having (height%10) = max(height%10)
and count(height/10) > 1;

select '';
select 'All pairs of dogs related as grandparents/children or great-';
select 'grandparents/children, ordered by descending height difference:';
with gparents(gparent, gchild) as (
     select x.parent as gparent, y.child as gchild
     from parents x
          join parents y on x.child = y.parent
),  
ggparents(ggparent, ggchild) as (
     select x.parent as ggparent, z.child as ggchild
     from parents x
          join parents y on x.child = y.parent
          join parents z on y.child = z.parent
)
select x.name, y.name
from dogs x
     join dogs y
     left join gparents gcx on x.name = gcx.gchild
     left join gparents gcy on y.name = gcy.gchild
     left join ggparents ggcx on x.name = ggcx.ggchild
     left join ggparents ggcy on y.name = ggcy.ggchild
where x.height <= y.height
      and gcx.gparent = y.name
      or gcy.gparent = x.name
      or ggcx.ggparent = y.name
      or ggcy.ggparent = x.name 
order by (y.height - x.height) desc;
