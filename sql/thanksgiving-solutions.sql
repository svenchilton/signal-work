/* 
Solutions to thanksgiving.sql homework exercises
Sven Chilton
Signal Data Science Cohort 3
July 30, 2016
*/

.read thanksgiving.sql

select '';
select 'The following tables are contained in thanksgiving.sql:';
.tables

select '';
select 'Model flight price increases in November given the following:';
select 'The respective prices on the 1st, 2nd, and 3rd are $20, $30, and $40.';
select 'On an day after the 3rd, the price of a ticket is equal to the average '; 
select 'of the previous two days, which is then added to 5 times the value of ';
select 'the numerical date (i.e., 5th, 6th, etc.) mod 7.';
with threedayprices(d, p1, p2, p3) as (
     select 3, 20, 30, 40 union
     select d+1, p2, p3, (p2+p3)/2 + 5*((d+1)%7)
     from threedayprices 
     where d < 25
),
prices(dy, price) as (
     select 1, 20 union
     select 2, 30 union
     select d, p3 from threedayprices
)
select * from prices;

select '';
select 'Find the cheapest flights from SFO to PDX with at most 1 transfer';
select 'Any values of "000" in the transfer column denote direct flights';
with direct_flights(departure, arrival, price) as (
     select * from flights f
     where f.departure = 'SFO' and f.arrival = 'PDX'
),
one_stop_flights(departure, transfer, arrival, price) as (
     select f1.departure, f1.arrival, f2.arrival, 
            f1.price + f2.price
     from flights f1 
          join flights f2 on f1.arrival = f2.departure
     where f1.departure = 'SFO' and f2.arrival = 'PDX'
)
select departure, '000' as transfer, arrival, price
from direct_flights 
union
select * from one_stop_flights 
order by price asc;

-----------------------------------------------------------
/*
select a.item || ', ' ||
       b.item || ', ' ||
       c.item || ', ' ||
       d.item || ', ' ||
       e.item || ', ' ||
       f.item || ', ' ||
       g.item || ', ' ||
       h.item,
*/
select *, 
       60 - 
       (a.price + b.price + c.price + d.price + 
        e.price + f.price + g.price + h.price) 
       as surplus
from supermarket a
     left join supermarket b on a.price <= b.price
     left join supermarket c on b.price <= c.price
     left join supermarket d on c.price <= d.price
     left join supermarket e on d.price <= e.price
     left join supermarket f on e.price <= f.price
     left join supermarket g on f.price <= g.price
     left join supermarket h on g.price <= h.price
--where surplus >= 0;     

/*
WITH cart(list, list_len, mod_list_len, last, budget) AS (
  SELECT item, LENGTH(item), LENGTH(item), price, 60 - price FROM supermarket WHERE price <= 60 UNION
  SELECT list || ", " || item, 
         LENGTH(list),
         CASE WHEN instr(list,item) = 1
                THEN LENGTH(item)
              WHEN instr(list,item) > 1
                THEN LENGTH(substr(list,1,instr(list,item)-3) || ', ' || item)
              ELSE LENGTH(list) END,
         price, 
         budget - price 
  FROM cart, supermarket
    WHERE price <= budget AND price >= last AND list_len <= mod_list_len
)
SELECT list, budget FROM cart ORDER BY budget, list;
*/


WITH cart(list, last, budget, sublist) AS (
     SELECT item, price, 60 - price, item 
     FROM supermarket 
     WHERE price <= 60 UNION
     SELECT list || ", " || item, 
            price, 
            budget - price, 
            CASE WHEN instr(list,item) = 1 
                      THEN substr(list,2*length(item)+5,length(list))
                 WHEN instr(list,item) > 1
                      THEN substr(list,1,length(list)-2*length(item)-6)
                 ELSE list
            END
     FROM cart, supermarket
     WHERE price <= budget AND 
           price >= last AND 
           instr(sublist,item) = 0
)
SELECT list, budget FROM cart ORDER BY budget, list;
--SELECT * FROM cart;


with cart(list, last, budget) as (
),
cart2()


case when instr('cranberries, cranberries, potatoes, potatoes','potatoes') > 0 
     then 

/*
with dummy(list, item) as (
     select 'cranberries, pumpkin pie, pumpkin pie, pumpkin pie', 'cranberries'
)
select instr(list,item), 
       case when instr(list,item) = 1
                 then length(item) 
            when instr(list,item) > 1
                 then length(substr(list,1,instr(list,item)-3) || ', ' || item) 
            else length(list)
            end as mod_list_len,
       length(item),
       length(list)
from dummy;
*/


with dummy(list, item) as (
     --select 'pumpkin pie, pumpkin pie, pumpkin pie, cranberries', 'pumpkin pie'
     select 'potatoes, potatoes, potatoes, turkey', 'turkey'
), 
dummy2(list, item, instr_ind, sub_list) as (
     select list, item, instr(list,item),
            case when instr(list,item) = 1 
                 then substr(list,2*length(item)+5,length(list))
            when instr(list,item) > 1 
                 then substr(list,1,length(list)-2*length(item)-6)
            else list
            end
     from dummy
)
select *, instr(sub_list,item)
from dummy2;

with dummy(list, item) as (
     --select 'pumpkin pie, pumpkin pie, pumpkin pie, cranberries', 'pumpkin pie'
     select 'potatoes, potatoes, potatoes, turkey', 'turkey'
) 
select instr(list,item) from dummy;






-----------------------------------------------------------

select '';
select 'There are '||count(distinct meat)||' types of meat in the main_course table.'
from main_course;

select '';
select 'Count the possible "full" Thanksgiving meals consisting of 1 type ';
select 'each of meat, side dish, and pie, containing strictly fewer than ';
select '2500 calories:';
select 'There are ' ||count(mc.calories + p.calories)||
       ' possible "full" Thanksgiving meals containing fewer than 2500 calories.'
from main_course mc join pies p
where mc.calories + p.calories < 2500;

select '';
select 'Output the caloric content of the healthiest full meal ';
select '(containing meat, a side dish, and pie) involving each type of meat.';
select 'Eliminate any meals containing more than 3000 calories.';
with full_meal(meat, side, pie, calories) as (
     select mc.meat, mc.side, p.pie, mc.calories + p.calories
     from main_course mc join pies p
     where mc.calories + p.calories <= 3000
)
select meat, min(calories) from full_meal
group by meat;

select '';
select 'List the product categories and the average price of items in each category:';
select category, avg(MSRP) from products
group by category;

select '';
select 'Create a table lowest_prices which lists: ';
select '(1) each item in the inventory, ';
select '(2) the store selling the item at the lowest price, and ';
select '(3) the lowest price of the item.';
create table lowest_prices
(
item varchar(255),
store varchar(255),
lowest_price float
);
insert into lowest_prices
select item, store, min(price) from inventory
group by item;
select 'Display the lowest_prices table:';
select * from lowest_prices;

select '';
select 'Create a table shopping_list which lists: ';
select '(1) each product category, ';
select '(2) the item with the smallest MSRP/rating, and ';
select '(3) the store which sells the item at the lowest price.';
create table shopping_list
(
category varchar(255),
item varchar(255),
store varchar(255)
);
insert into shopping_list
with product_deals(category, name, MSRP_over_rating) as (
     select category, name, MSRP/rating from products
)
select category, name, store
from product_deals join lowest_prices on name = item
group by category
having min(MSRP_over_rating);
select 'Display the shopping_list table:';
select * from shopping_list;

select '';
select 'Calculate the total amount of bandwidth needed to'; 
select 'get everything in your shopping list.';
select 'This will require the shopping_list and stores tables.';
select 'It takes '||sum(MiBs)||' MiBs to purchase everything on the shopping list.'
from stores s join shopping_list sl on s.store = sl.store;























