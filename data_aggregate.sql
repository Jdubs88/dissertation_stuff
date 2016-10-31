drop table if exists iowa.discrete_data_001_monthly_long;
create table iowa.discrete_data_001_monthly_long as
select month, year, reduced_item_code_001, store_num, city, zip, store_type, premise_type, store_first_date, store_last_date, square_footage_whole, square_footage_retail, 
       reduced_product_name_001, reduced_category_001, reduced_cat_num_001, reduced_vendor_001, reduced_vendor_num_001,
       sum(pack_volume * volume_liters)/sum(volume_liters) as pack_volume,
       sum(bottle_volume * volume_liters)/sum(volume_liters) as bottle_volume,
       sum(state_cost * volume_liters)/sum(volume_liters) as state_cost,
       sum(quantity_ordered) as quantity_ordered,
       sum(total_cost) as total_cost,
       sum(volume_liters) as volume_liters
from   
(select d.*, 
case when d.reduced_item_code = 9999 then 9999 else c.reduced_item_code end as reduced_item_code_001,
case when c.reduced_item_code = 9999 then 'Other' else reduced_product_name end as reduced_product_name_001,
case when c.reduced_item_code = 9999 then 'Other' else reduced_category end as reduced_category_001,
case when c.reduced_item_code = 9999 then 9999 else reduced_cat_num end as reduced_cat_num_001,
case when c.reduced_item_code = 9999 then 'Other' else reduced_vendor end as reduced_vendor_001,
case when c.reduced_item_code = 9999 then 9999 else reduced_vendor_num end as reduced_vendor_num_001
from
iowa.discrete_data_0001_monthly_long d
left join
(select distinct r.true_item_code, 
case when p.volume_liters < 76600 then 9999 else r.true_item_code end as reduced_item_code
from
iowa.raw_data r left join
iowa.product_info p on
r.true_item_code = p.true_item_code) c
on c.true_item_code = d.reduced_item_code) a
group by month, year, reduced_item_code_001, store_num, city, zip, store_type, premise_type, store_first_date, store_last_date, square_footage_whole, square_footage_retail, 
       reduced_product_name_001, reduced_category_001, reduced_cat_num_001, reduced_vendor_001, reduced_vendor_num_001;

drop table if exists iowa.discrete_data_001_monthly_wide;
select transpose('iowa.discrete_data_001_monthly_long', 'iowa.discrete_data_001_monthly_wide', 'horizontally', 
                 'year, month, store_num', 'reduced_item_code_001', 
                 'volume_liters');

drop table if exists iowa.discrete_data_001_monthly_wide_maxvol;
create table iowa.discrete_data_001_monthly_wide_maxvol as 
select m.*, v.max_volume
from iowa.discrete_data_001_monthly_wide m
left join iowa.discrete_data_maxvolcol v
on m.store_num = v.store_num;

copy (select *
      from iowa.discrete_data_001_monthly_wide_maxvol
      order by store_num, year, month)
to 'C:\Users\JW\Documents\Iowa papers\discrete_data_001_monthly_wide_maxvol.csv'
delimiter ','
csv
header;