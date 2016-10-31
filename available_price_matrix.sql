drop table if exists iowa.__discrete_price_matrix_0001;
create table iowa.__discrete_price_matrix_0001 as
select distinct date, reduced_item_code, 
case when reduced_item_code = 902836770 and date = '2013-04-08' then 26.22
     when reduced_item_code = 641361756 and date < '2015-01-15' and date > '2014-12-01' then 13.86
     else state_cost end as state_cost, 
case when reduced_item_code = 902836770 and date = '2013-04-08' then 39.33
     when reduced_item_code = 641361756 and date < '2015-01-15' and date > '2014-12-01' then 20.79
     else bottle_price end as bottle_price,
date - cast('2011-12-31' as date) as time
     from iowa.discrete_data_0001
where reduced_item_code != 9999
order by reduced_item_code, date;

select * from iowa.__discrete_price_matrix_0001
where reduced_item_code = 9369673634 and date > '2013-11-30' and date < '2014-01-01'
order by date;

drop table if exists iowa.__discrete_price_matrix_monthly_0001;
create table iowa.__discrete_price_matrix_monthly_0001 as
select distinct year, month, min(state_cost) as state_cost, min(bottle_price) as bottle_price, reduced_item_code 
from (select distinct extract(month from date) as month, extract(year from date) as year, 
      state_cost, bottle_price, reduced_item_code from iowa.__discrete_price_matrix_0001
      order by reduced_item_code, year, month) d
group by reduced_item_code, year, month
order by reduced_item_code, year, month;

drop table if exists iowa._discrete_price_matrix_monthly_0001;
select transpose('iowa.__discrete_price_matrix_monthly_0001', 'iowa._discrete_price_matrix_monthly_0001', 'horizontally', 
                 'year, month', 'reduced_item_code', 
                 'bottle_price');

drop table if exists iowa._discrete_price_matrix_0001;
select transpose('iowa.__discrete_price_matrix_0001', 'iowa._discrete_price_matrix_0001', 'horizontally', 
                 'date', 'reduced_item_code', 
                 'bottle_price');

                 
-- write data to csv file
copy iowa._discrete_price_matrix_0001
to 'C:\Users\JW\Documents\Iowa papers\discrete_price_matrix_0001.csv'
delimiter ','
csv
header;

copy iowa._discrete_price_matrix_monthly_0001
to 'C:\Users\JW\Documents\Iowa papers\discrete_price_matrix_monthly_0001.csv'
delimiter ','
csv
header;

copy iowa.discrete_data_0001_products
to 'C:\Users\JW\Documents\Iowa papers\discrete_data_0001_products.csv'
delimiter ','
csv
header;

-- select reduced_item_code, date, avg(state_cost), avg(bottle_price), count(*)
-- from (select distinct date, reduced_item_code, state_cost, bottle_price from iowa.discrete_data_0001
-- where reduced_item_code != 9999
-- order by reduced_item_code, date) d
-- group by reduced_item_code, date
-- having count(*) > 1
-- 
-- select cast('2015-02-01' as date) - cast('2015-01-01' as date)
-- 
-- select distinct time from iowa.discrete_price_matrix_0001
-- order by time