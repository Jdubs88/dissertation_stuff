select count(*), sell_gasoline, population_under_1500, population_1500_3000, square_footage_under_1500, square_footage_1500_5000, square_footage_gt_5000
       from (
       select s.*, c.population,
       case when c.population < 1500 then 1 else 0 end as population_under_1500,
       case when c.population >= 1500 and c.population <3001 then 1 else 0 end as population_1500_3000,
       case when s.square_footage_retail < 1500 then 1 else 0 end as square_footage_under_1500,
       case when s.square_footage_retail >= 1500 and s.square_footage_retail < 5000 then 1 else 0 end as square_footage_1500_5000,
       case when s.square_footage_retail >= 5000 then 1 else 0 end as square_footage_gt_5000
       from iowa.store_info s
       left join iowa.cities c
       on c.city = s.city_abd) a
where population < 3001
group by sell_gasoline, population_under_1500, population_1500_3000, sell_gasoline, population_under_1500, population_1500_3000, square_footage_under_1500, square_footage_1500_5000, square_footage_gt_5000
order by sell_gasoline, population_under_1500, population_1500_3000, sell_gasoline, population_under_1500, population_1500_3000, square_footage_under_1500, square_footage_1500_5000, square_footage_gt_5000 