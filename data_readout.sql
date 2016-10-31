copy (select s.*, p.population from 
      iowa.store_info s
      left join iowa.cities p
      on p.city = s.city_abd_proper)
to 'C:\Users\JW\Documents\Iowa papers\store_type_count_data.csv'
delimiter ','
csv
header;

copy (select *
      from iowa.discrete_data_0001
      order by store_num, date)
to 'C:\Users\JW\Documents\Iowa papers\discrete_data_0001.csv'
delimiter ','
csv
header;

copy (select *
      from iowa.store_info
      order by store_num_abd)
to 'C:\Users\JW\Documents\Iowa papers\store_info.csv'
delimiter ','
csv
header;

copy (select *
      from iowa.discrete_data_0001_alt
      order by store_num, year, week)
to 'C:\Users\JW\Documents\Iowa papers\discrete_data_0001_alt.csv'
delimiter ','
csv
header;

copy (select *
      from iowa.discrete_data_0001_month_sorted
      order by store_num, year, month)
to 'C:\Users\JW\Documents\Iowa papers\discrete_data_0001_month.csv'
delimiter ','
csv
header;

copy (select *
      from iowa.discrete_data_0001_month_maxvol_sorted
      order by store_num, year, month)
to 'C:\Users\JW\Documents\Iowa papers\discrete_data_0001_month_maxvol.csv'
delimiter ','
csv
header;

copy (select *
      from iowa.discrete_data_0001_monthly_long
      order by store_num, year, month)
to 'C:\Users\JW\Documents\Iowa papers\discrete_data_0001_monthly_long.csv'
delimiter ','
csv
header;

copy (select *
      from iowa.discrete_data_0001_month_long
      order by store_num, year, month)
to 'C:\Users\JW\Documents\Iowa papers\discrete_data_0001_month_long_for_rf.csv'
delimiter ','
csv
header;

copy (select *
      from iowa.discrete_data_0001_storewide
      order by reduced_item_code, year, month)
to 'C:\Users\JW\Documents\Iowa papers\discrete_data_0001_storewide.csv'
delimiter ','
csv
header;