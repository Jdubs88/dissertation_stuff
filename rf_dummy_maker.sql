SELECT DISTINCT 'case when cast(substring(reduced_item_code,15,10) as bigint) = ' || 
    reduced_item_code|| 
    ' then 1 else 0 end as item_'||
    reduced_item_code ||
    ','
    from iowa.rf_dataset_covariates

SELECT DISTINCT 'case when s.store_type = ' || 
    store_type|| 
    ' then 1 else 0 end as store_type_'||
    store_type ||
    ','
    from iowa.store_info

SELECT DISTINCT 'case when s.sell_gasoline = ' || 
    sell_gasoline|| 
    ' then 1 else 0 end as sell_gasoline_'||
    sell_gasoline ||
    ','
    from iowa.store_info

SELECT DISTINCT 'case when s.city_abd_proper = ' || 
    city_abd_proper || 
    ' then 1 else 0 end as city_'||
    city_abd_proper ||
    ','
    from iowa.store_info

SELECT DISTINCT 'case when s.county_abd = ' || 
    county_abd || 
    ' then 1 else 0 end as county_'||
    county_abd ||
    ','
    from iowa.store_info

SELECT DISTINCT 'case when r.vendor_num = ' || 
    vendor_num || 
    ' then 1 else 0 end as vendor_'||
    vendor_num ||
    ','
    from iowa.product_info
    where vendor_num = 85
or vendor_num = 297
or vendor_num = 55
or vendor_num = 421
or vendor_num = 434
or vendor_num = 380
or vendor_num = 260
or vendor_num = 115
or vendor_num = 259



SELECT DISTINCT 'case when r.true_cat_num = ' || 
    true_cat_num || 
    ' then 1 else 0 end as category_'||
    true_cat_num ||
    ','
    from iowa.product_info
where true_cat_num = 1031080
or true_cat_num = 1012100
or true_cat_num = 1011100
or true_cat_num = 1011300
or true_cat_num = 1081600
or true_cat_num = 1062310