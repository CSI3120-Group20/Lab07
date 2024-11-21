% Task C

% Question 1
:- dynamic book/4.

% book('The Great Gatsby', 'F. Scott Fitzgerald', 1925, 'Novel').
% book('1984', 'George Orwell', 1949, 'Dystopian').
% book('To Kill a Mockingbird', 'Harper Lee', 1960, 'Novel').
% book('Brave New World', 'Aldous Huxley', 1932, 'Dystopian').

% Question 2
add_book(Title, Author, Year, Genre) :- not(book(Title, Author, Year, Genre)), assertz(book(Title, Author, Year, Genre)).

% Question 3
remove_book(Title, Author, Year, Genre) :- book(Title, Author, Year, Genre), retract(book(Title, Author, Year, Genre)).

% Question 4
is_available(Title, Author, Year, Genre) :- book(Title, Author, Year, Genre), not(borrowed(Title, Author, Year, Genre)).

% Question 5
:- dynamic borrowed/4.
borrow_book(Title, Author, Year, Genre) :- book(Title, Author, Year, Genre), assertz(borrowed(Title, Author, Year, Genre)).

% Question 6
return_book(Title, Author, Year, Genre) :- retract(borrowed(Title, Author, Year, Genre)).

% Question 7
find_by_author(Author, Books) :- findall(Name, book(Name, Author, _, _), Books).
find_by_genre(Genre, Books) :- findall(Name, book(Name, _, _, Genre), Books).
find_by_year(Year, Books):- findall(Name, book(Name, _, Year, _), Books).

% Question 8
recommend_by_genre(Genre, Books) :- findall(Name, is_available(Name, _, _, Genre), Books).
recommend_by_author(Author, Books) :- findall(Name, is_available(Name, Author, _, _), Books).