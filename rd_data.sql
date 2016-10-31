drop table if exists iowa.rd_info;
create table iowa.rd_info as
select s.city, s.population, d.total_liters, d.order_count, d.sales, c.store_count, f.store_count_address, gg.operation_time, gg.square_footage, gg.foot_days
from iowa.cities s
left join 
(select city, sum(total_cost_num) as sales, sum(volume_liters) as total_liters, count(store_num) as order_count 
 from iowa.cut_data
 group by city) d
on s.city = d.city
left join
(select city, sum(operation_time) as operation_time, sum(square_footage) as square_footage, sum(foot_days) as foot_days from iowa.store_count group by city) gg
on gg.city = s.city
left join
(select cc.city, count(cc.store_num) as store_count from
	(select distinct city, store_num from iowa.cut_data) cc
	group by city) c
on c.city = s.city
left join
(select ff.city, count(ff.address_fixed) as store_count_address from
	(select distinct city, address_fixed from iowa.cut_data) ff
	group by city) f
on f.city = s.city
order by population;

select count(*) from iowa.raw_data;
select count(*) from iowa.cut_data;