drop table if exists iowa._discrete_data_0001;
create table iowa._discrete_data_0001 as
select r.*, p.min_date as product_min_date, p.max_date as product_max_date, p.order_count as total_product_orders, p.volume_liters as total_product_liters,
       case when p.volume_liters < 7660 then 9999 else r.true_item_code end as reduced_item_code,
       case when p.volume_liters < 7660 then 'Other' else r.true_category end as reduced_category,
       case when p.volume_liters < 7660 then 'Other' else r.product_name end as reduced_product_name,
       case when p.volume_liters < 7660 then 9999 else r.true_cat_num end as reduced_cat_num,
       case when p.volume_liters < 7660 then 'Other' else r.vendor end as reduced_vendor,
       case when p.volume_liters < 7660 then 9999 else r.vendor_num end as reduced_vendor_num,
       case when p.volume_liters < 7660 then '1/1/2012' else p.min_date end as reduced_min_date,
       case when p.volume_liters < 7660 then '12/31/2015' else p.max_date end as reduced_max_date,
       cast(r.pack_size as numeric) * r.volume_liters as packvol_weight,
       cast(r.bottle_volume as numeric) * r.volume_liters as bottlevol_weight,
       r.state_cost_num * r.volume_liters as statecost_weight,
       r.bottle_price_num * r.volume_liters as bottleprice_weight
       
from
iowa.raw_data r left join
iowa.product_info p on
r.true_item_code = p.true_item_code;

drop table if exists iowa.__discrete_data_0001;
create table iowa.__discrete_data_0001 as 
select date, store_num, city, zip, county_num, county, reduced_min_date, reduced_max_date, reduced_item_code,
	     reduced_product_name, reduced_vendor, reduced_vendor_num,
             case when sum(volume_liters) = 0 then 0 else sum(packvol_weight)/sum(volume_liters) end as pack_volume,  
             case when sum(volume_liters) = 0 then 0 else sum(bottlevol_weight)/sum(volume_liters) end as bottle_volume,  
             case when sum(volume_liters) = 0 then 0 else sum(statecost_weight)/sum(volume_liters) end as state_cost,  
             case when sum(volume_liters) = 0 then 0 else sum(bottleprice_weight)/sum(volume_liters) end as bottle_price, 
             case when reduced_item_code = 9046891337 then 'IMPORTED VODKA'
		  when reduced_item_code = 774724901 then 'WHISKEY LIQUEUR' 
		  else reduced_category end as reduced_category,
             case when reduced_item_code = 9046891337 then 1032080 
                  when reduced_item_code = 774724901 then 1081600 
                  else reduced_cat_num end as reduced_cat_num,
             sum(quantity_ordered) as quantity_ordered,
             sum(total_cost_num) as total_cost,
             sum(volume_liters) as volume_liters
from iowa._discrete_data_0001
group by store_num, date, city, zip, county_num, county, reduced_min_date, reduced_max_date, reduced_item_code, reduced_category, 
	     reduced_product_name, reduced_cat_num, reduced_vendor, reduced_vendor_num
order by store_num, date;


drop table if exists iowa.discrete_data_0001_a_products;
create table iowa.discrete_data_0001_a_products as
select distinct reduced_item_code, reduced_min_date, reduced_max_date, reduced_category, reduced_product_name, reduced_cat_num, reduced_vendor,
		reduced_vendor_num
from iowa.__discrete_data_0001
order by reduced_item_code;


drop table if exists iowa.true_addresses;
create table iowa.true_addresses
(store_num integer, address text, city text);
copy iowa.true_addresses
from 'C:\Users\JW\Documents\Iowa papers\store_addresses.csv'
delimiter ','
csv
header;

drop table if exists iowa.discrete_data_0001_a_stores;
create table iowa.discrete_data_0001_a_stores as
select distinct d.store_num, d.city, d.zip, d.county_num, d.county,
		s.store_name_abd as store_name, s.premise_type, s.store_type, s.license_cost, s.square_footage_whole, s.square_footage_retail,
		ss.store_first_date, ss.store_last_date, ss.store_operation_time,
		a.address
from iowa.__discrete_data_0001 d
left join iowa.store_info s
	on d.store_num = s.store_num_abd
left join (select store_num, max(date) as store_last_date, min(date) as store_first_date, max(date) - min(date) as store_operation_time
		from iowa.discrete_data_0001_a
		group by store_num) ss
	on d.store_num = ss.store_num
left join iowa.true_addresses a
	on d.store_num = a.store_num
order by store_num;

drop table if exists iowa.discrete_data_0001_a;
create table iowa.discrete_data_0001_a as
select d.date, d.store_num, d.reduced_item_code, d.pack_volume, d.bottle_volume, d.state_cost, d.bottle_price, d.quantity_ordered, d.total_cost, d.volume_liters, 
       s.city, s.zip, s.county_num, s.county, s.store_name, s.premise_type, s.store_type, s.license_cost, s.square_footage_whole, s.square_footage_retail, s.store_first_date, s.store_last_date, s.store_operation_time, s.address,
       p.reduced_product_name, p.reduced_min_date, p.reduced_max_date, p.reduced_category, p.reduced_cat_num, p.reduced_vendor, p.reduced_vendor_num
from iowa.__discrete_data_0001 d
left join iowa.discrete_data_0001_a_stores s
	on d.store_num = s.store_num
left join iowa.discrete_data_0001_a_products p 
	on d.reduced_item_code = p.reduced_item_code
order by store_num, date, reduced_item_code;

drop table if exists iowa._discrete_data_0001;
drop table if exists iowa.__discrete_data_0001;

-- Make a new RD dataset based on the discrete data's categorizations
drop table if exists iowa.__rd_data_new;
create table iowa.__rd_data_new as 
select city, count(distinct address) as address_count, count(distinct store_num) as store_num_count, sum(total_cost) as total_sales, sum(volume_liters) as total_volume,
       count(date) as product_orders, count(distinct reduced_item_code) as item_variety, sum(total_cost)/sum(volume_liters) as average_cost_per_liter
from iowa.discrete_data_0001_a
group by city;

drop table if exists iowa._rd_data_new;
create table iowa._rd_data_new as
select d.*, c.store_orders from
iowa.__rd_data_new d
left join (select cc.city, count(cc.date) as store_orders from (select distinct city, store_num, date
	   from iowa.discrete_data_0001_a) cc
	   group by cc.city) c
	   on d.city = c.city;

drop table if exists iowa.rd_data_new;
create table iowa.rd_data_new as
select d.*, t.operation_time from
iowa._rd_data_new d	   
left join (select tt.city, sum(tt.store_operation_time) as operation_time from (select distinct city, store_num, store_operation_time
	   from iowa.discrete_data_0001_a) tt
	   group by tt.city) t
	   on d.city = t.city
order by d.city;

drop table if exists iowa.__rd_data_new;
drop table if exists iowa._rd_data_new;

--Construct the actual discrete choice dataset
drop table if exists iowa.discrete_data_001;
create table iowa.discrete_data_001 as 
select date, store_num, reduced_item_code, 
