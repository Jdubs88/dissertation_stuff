select distinct store_num, extract(year from date), extract(month from date), extract(week from date) from iowa.discrete_data_0001

drop table if exists iowa.___temp;
create table iowa.___temp as 
select *, extract(year from date) as year, extract(week from date) as week, extract(month from date) as month from iowa.discrete_data_0001;
drop table if exists iowa.discrete_data_0001_alt;
select transpose('iowa.___temp', 'iowa.discrete_data_0001_alt', 'horizontally', 
                 'year, week, month, store_num', 'reduced_item_code', 
                 'volume_liters');

select count(*) from iowa.discrete_data_0001_alt

drop table if exists iowa.discrete_data_0001_monthly_long;
create table iowa.discrete_data_0001_monthly_long as
select extract(month from date) as month, extract (year from date) as year, reduced_item_code, store_num,
       sum(pack_volume * volume_liters)/sum(volume_liters) as pack_volume, 
       sum(bottle_volume * volume_liters)/sum(volume_liters) as bottle_volume,
       sum(state_cost * volume_liters)/sum(volume_liters) as state_cost, 
       sum(quantity_ordered) as quantity_ordered, 
       sum(total_cost) as total_cost, 
       sum(volume_liters) as volume_liters, 
       city, zip, store_type, premise_type, store_first_date, store_last_date, square_footage_whole, square_footage_retail,
       reduced_product_name, reduced_category, reduced_cat_num, reduced_vendor, reduced_vendor_num
from iowa.discrete_data_0001
group by store_num, month, year, reduced_item_code,  zip, store_type, premise_type, store_first_date, store_last_date, 
       reduced_product_name, reduced_category, reduced_cat_num, square_footage_whole, square_footage_retail,
       reduced_vendor, reduced_vendor_num, city
order by store_num, month, year;

drop table if exists iowa.discrete_data_maxvolcol;
create table iowa.discrete_data_maxvolcol as
select store_num,
       sum(volume_liters) as max_volume
from iowa.discrete_data_0001
group by store_num 
order by store_num;

drop table if exists iowa.discrete_data_0001_monthly_maxvol;
create table iowa.discrete_data_0001_monthly_maxvol as 
select m.*, v.max_volume
from iowa.discrete_data_0001_monthly_long m
left join iowa.discrete_data_maxvolcol v
on m.store_num = v.store_num;



drop table if exists iowa.___temp;
create table iowa.___temp as 
select *, extract(year from date) as year, extract(week from date) as week, extract(month from date) as month from iowa.discrete_data_0001;
drop table if exists iowa.discrete_data_0001_month;
select transpose('iowa.___temp', 'iowa.discrete_data_0001_month', 'horizontally', 
                 'year, month, store_num', 'reduced_item_code', 
                 'volume_liters');

drop table if exists iowa.___temp;
create table iowa.___temp as 
select *, extract(year from date) as year, extract(week from date) as week, extract(month from date) as month from iowa.discrete_data_0001;
drop table if exists iowa.discrete_data_0001_month;
select transpose('iowa.___temp', 'iowa.discrete_data_0001_storewide', 'horizontally', 
                 'year, month, reduced_item_code', 'store_num', 
                 'volume_liters');

drop table if exists iowa.___temp;

drop table if exists iowa.discrete_data_0001_month_maxvol;
create table iowa.discrete_data_0001_month_maxvol as
select m.*, v.max_volume
from iowa.discrete_data_0001_month m
left join iowa.discrete_data_maxvolcol v
on m.store_num = v.store_num; 


drop table if exists iowa.discrete_data_0001_month_maxvol_sorted;
create table iowa.discrete_data_0001_month_maxvol_sorted as
select * from iowa.discrete_data_0001_month_maxvol
order by store_num, year, month;


-- Make a long datset with filled-in zeroes
drop table if exists iowa.discrete_data_0001_month_long;
drop table if exists iowa.___temp;
select transpose('iowa.discrete_data_0001_month', 'iowa.___temp','vertically',
		 'year, month, store_num','reduced_item_code',
		 'volume_liters');
create table iowa.discrete_data_0001_month_long as 
select year, month, store_num, cast(substring(reduced_item_code,15,100) as bigint) as reduced_item_code, case when cast(volume_liters as numeric) is null then 0 else cast(volume_liters as numeric) end as volume_liters
from iowa.___temp
order by store_num, year, month;

