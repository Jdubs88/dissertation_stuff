-- creates alternative lookup table
-- schema and file location will need to be specified appropriately
-- table columns may also change based on project

-- setup alternatives table
drop table if exists iowa.__raw_data;
create table iowa.__raw_data
(invoice text,
 date date,
 store_num integer,
 store text,
 address text,
 city_improper text,
 zip text,
 store_location text,
 county_num integer,
 county text,
 category_num integer,
 category text,
 vendor_num integer,
 vendor text,
 item_num integer,
 product_name text,
 pack_size text,
 bottle_volume text,
 state_cost text,
 bottle_price text,
 quantity_ordered integer,
 total_cost text,
 volume_liters numeric,
 volume_gallons numeric);

-- read in data from a csv
copy iowa.__raw_data
from 'C:\Users\JW\Documents\Iowa papers\Iowa_Liquor_Sales_2.csv'
delimiter ','
csv
header;

drop table if exists iowa._raw_data;
create table iowa._raw_data as
select *, cast(replace(state_cost,'$','') as numeric) as state_cost_num,
          cast(replace(bottle_price,'$','') as numeric) as bottle_price_num,
          cast(replace(total_cost,'$','') as numeric) as total_cost_num,
          case when city_improper = 'ARNOLD''S PARK' then 'ARNOLDS PARK'
               when city_improper = 'Carroll' then 'CARROLL'
               when city_improper = 'Davenport' then 'DAVENPORT'
               when city_improper = 'Des Moines' then 'DES MOINES'
               when city_improper = 'Dubuque' then 'DUBUQUE'
               when city_improper = 'FT. ATKINSON' then 'FORT ATKINSON'
               when city_improper = 'GUTTENBURG' then 'GUTTENBERG'
               when city_improper = 'Holstein' then 'HOLSTEIN'
               when city_improper = 'Inwood' then 'INWOOD'
               when city_improper = 'KELLOG' then 'KELLOGG'
               when city_improper = 'Keosauqua' then 'KEOSAUQUA'
               when city_improper = 'LECLAIRE' then 'LE CLAIRE'
               when city_improper = 'LEMARS' then 'LE MARS'
               when city_improper = 'Monona' then 'MONONA'
               when city_improper = 'MT PLEASANT' then 'MOUNT PLEASANT'
               when city_improper = 'MT VERNON' then 'MOUNT VERNON'
               when city_improper = 'Northwood' then 'NORTHWOOD'
               when city_improper = 'OTTUWMA' then 'OTTUMWA'
               when city_improper = 'Urbandale' then 'URBANDALE'
               when city_improper = 'Windsor Heights' then 'WINDSOR HEIGHTS'
               else city_improper end as city
               
from iowa.__raw_data;

drop table if exists iowa._store_info;
create table iowa._store_info
(store_num_abd integer,
store_name_abd text,
address_abd text,
city_abd text,
owner_name text,
dba text,
address_line_1 text,
address_line_2 text,
city_abd_a text,
state_abd text,
zip_code text,
county_abd text,
license_number text,
business_phone text,
insurance_company text,
sub_license_type text,
license_type text,
premise_type text,
status text,
effective_date date,
expiration_date date,
local_authority text,
license_cost numeric,
length_of_license text,
contact_name text,
email_address text,
contact_phone text,
mailing_address_line_1 text,
mailing_address_city text,
mailing_address_state text,
mailing_address_zip text,
status_of_business text,
capacity text,
sell_gasoline text,
operation_hours text,
delivery_hours text,
flexible text,
issue_date date,
account_number integer,
infusion text,
square_footage_whole integer,
square_footage_retail integer,
current_issue_date date,
store_type text,
address_fixed text);

copy iowa._store_info
from 'C:\Users\JW\Documents\Iowa papers\fulldata_store_data.csv'
--from 'C:\Users\JW\Documents\Iowa papers\cutdata_store_data.csv'
delimiter ','
csv
header;

drop table if exists iowa.item_list;
create table iowa.item_list
(item_num integer, category_num integer, category text, true_item_code bigint);
copy iowa.item_list
from 'C:\Users\JW\Documents\Iowa papers\updated_product_info.csv'
delimiter ','
csv
header;

drop table if exists iowa.raw_data;
create table iowa.raw_data as 
select r.*, i.category_num as true_cat_num, i.category as true_category, i.true_item_code
from iowa._raw_data r
left join iowa.item_list i
on i.item_num = r.item_num;

drop table if exists iowa.__raw_data;
drop table if exists iowa._raw_data;

drop table if exists iowa.store_info;
create table iowa.store_info as 
select *,
          case when city_abd = 'ARNOLD''S PARK' then 'ARNOLDS PARK'
               when city_abd = 'Carroll' then 'CARROLL'
               when city_abd = 'Davenport' then 'DAVENPORT'
               when city_abd = 'Des Moines' then 'DES MOINES'
               when city_abd = 'Dubuque' then 'DUBUQUE'
               when city_abd = 'FT. ATKINSON' then 'FORT ATKINSON'
               when city_abd = 'GUTTENBURG' then 'GUTTENBERG'
               when city_abd = 'Holstein' then 'HOLSTEIN'
               when city_abd = 'Inwood' then 'INWOOD'
               when city_abd = 'KELLOG' then 'KELLOGG'
               when city_abd = 'Keosauqua' then 'KEOSAUQUA'
               when city_abd = 'LECLAIRE' then 'LE CLAIRE'
               when city_abd = 'LEMARS' then 'LE MARS'
               when city_abd = 'Monona' then 'MONONA'
               when city_abd = 'MT PLEASANT' then 'MOUNT PLEASANT'
               when city_abd = 'MT VERNON' then 'MOUNT VERNON'
               when city_abd = 'Northwood' then 'NORTHWOOD'
               when city_abd = 'OTTUWMA' then 'OTTUMWA'
               when city_abd = 'Urbandale' then 'URBANDALE'
               when city_abd = 'Windsor Heights' then 'WINDSOR HEIGHTS'
               else city_abd end as city_abd_proper
from iowa._store_info;

drop table if exists iowa._store_info;

drop table if exists iowa._cut_data;
create table iowa._cut_data as 
select c.*, d.operation_time, d.last_date
from iowa.raw_data c
	 left join
         iowa.store_count d
         on c.store_num = d.store_num;
         --where operation_time >= 180 AND extract(year from last_date) >= 2014;

drop table if exists iowa.cut_data;
create table iowa.cut_data as 
select c.*, d.* from 
iowa._cut_data c
	left join
	iowa.store_info d
	on c.store_num = d.store_num_abd;


drop table if exists iowa.store_count;
create table iowa.store_count as select a.store_num, a.store, count(a.date) as date_count, max(a.date) - min(a.date) as operation_time, max(a.date) as last_date, min(a.date) as first_date, a.store_location as location, a.city as city, (max(a.date) - min(a.date))*avg(a.square_footage_whole) as foot_days, avg(a.square_footage_whole) as square_footage from
	(select distinct store_num, store, store_location, city, date, square_footage_whole
	 from iowa.cut_data) a
group by store_num, store, store_location, city
order by store_num;

drop table if exists iowa.item_summary;
create table iowa.item_summary as 
select true_item_code, stddev(state_cost_num) as sd_state_cost,
		 avg(state_cost_num) as avg_state_cost,
                 stddev(bottle_price_num) as sd_bottle_price,
                 avg(bottle_price_num) as bottle_price,
                 sum(volume_liters) as total_volume,
                 count(true_item_code) as number_of_orders       
from iowa.raw_data
group by true_item_code;

drop table if exists iowa.order_by_date;
create table iowa.order_by_date as
select date, sum(volume_liters) as total_volume, count(true_item_code) as number_of_orders
from iowa.raw_data
group by date
order by date;

drop table if exists iowa.product_info;
create table iowa.product_info as select s.*, t.max_date, t.min_date, t.order_count, t.volume_liters
from (select distinct true_item_code, product_name, pack_size, bottle_volume, true_cat_num, true_category, vendor_num, vendor
from iowa.raw_data) s 
left join (select true_item_code, max(date) as max_date, min(date) as min_date, count(*) as order_count, sum(volume_liters) as volume_liters from iowa.raw_data
		group by true_item_code) t
on t.true_item_code = s.true_item_code
order by true_item_code;


drop table if exists iowa._different_prices;
create table iowa._different_prices as
select true_item_code, bottle_price_num, count(*) as price_appearances
from iowa.raw_data
group by true_item_code, bottle_price_num
order by true_item_code, bottle_price_num;

drop table if exists iowa.different_prices;
create table iowa.different_prices as
select d.*, a.price_count from 
	(select true_item_code, count(true_item_code) as price_count from iowa._different_prices group by true_item_code) a
	left join iowa._different_prices d
		on a.true_item_code = d.true_item_code
	order by true_item_code, bottle_price_num;

drop table if exists iowa.prices_over_time;
create table iowa.prices_over_time as 
select date, true_item_code, avg(bottle_price_num) as bottle_price, avg(state_cost_num) as state_cost
from iowa.raw_data
group by date, true_item_code
order by true_item_code, date;

drop table if exists iowa.dates;
create table iowa.dates as select distinct date
from iowa.raw_data;

drop table if exists iowa._different_prices;

drop table if exists iowa.caseys_glidden;
create table iowa.caseys_glidden as select * from iowa.raw_data
where store_num = 4546;

drop table if exists iowa.sales_by_store;

drop table if exists iowa.store_addresses;
create table iowa.store_addresses as 
select distinct store_num, store, address, city_improper, county, zip, store_location
from iowa.cut_data;