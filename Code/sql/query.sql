select concat(year(time_utc), '-', lpad(month(time_utc), 2, '0'), '-', lpad(day(time_utc), 2, '0')) as Date,
    count(*) as Observations
    from data
    group by Date
    order by Date asc;

select concat(year(time_utc), '-', lpad(month(time_utc), 2, '0'), '-', lpad(day(time_utc), 2, '0')) as Date,
    max(time) - min(time) as Length
    from data
    group by Date
    order by Date asc;

