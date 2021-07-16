# Big Data Hackathon - whatshouldireadnext SQL queries

#how many users?
select count(*) from `bx-users`;
# how many books?
select count(*) from `bx-books`;
# how many ratings?
select count(*) from `bx-book-ratings`;

#histogram of user-ratings <table(num ratings, num users)> (how many users have rated N times? <number>)
create view temp as select count(*) as num_ratings
from `bx-users` natural join `bx-book-ratings`
group by `User-ID`
order by count(*);

select  num_ratings,count(*) as bin_size
from temp
group by num_ratings
order by num_ratings;


#histogram of book-ratings <table(num ratings, num users)> (how many books have been rated N times? <number>)

create view booksHistogram as select count(*) as num_ratings
from `bx-book-ratings`
group by `ISBN`
order by count(*);

select num_ratings, count(*) as bin_size
from booksHistogram
group by num_ratings
order by num_ratings;


# top ten most rated books
select `Book-Title`, count(`Book-Rating`)
from `bx-books` natural join `bx-book-ratings`
group by ISBN
order by count(`Book-Rating`) desc
limit 10;


#top ten most active users
select rating.`User-ID` as name, count(`User-ID`) as N
from `bx-book-ratings` rating
group by rating.`User-ID`
order by count(*) desc limit 0,10;
