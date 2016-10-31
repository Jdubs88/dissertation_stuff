select distinct store_num, extract(year from date), extract(month from date), extract(week from date) from iowa.discrete_data_0001


create table iowa.___temp as 
select *, extract(year from date) as year, extract(week from date) as week, extract(month from date) as month from iowa.discrete_data_0001;
drop table if exists iowa.discrete_data_0001_alt;
select transpose('iowa.___temp', 'iowa.discrete_data_0001_alt', 'horizontally', 
                 'year, week, month, store_num', 'reduced_item_code', 
                 'volume_liters');

select count(*) from iowa.discrete_data_0001_alt
