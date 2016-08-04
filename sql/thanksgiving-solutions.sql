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

select a.item || ', ' ||
       b.item || ', ' ||
       c.item || ', ' ||
       d.item || ', ' ||
       e.item || ', ' ||
       f.item || ', ' ||
       g.item || ', ' ||
       h.item,
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
where surplus >= 0;     



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























