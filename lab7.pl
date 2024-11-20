% Task A
% Question 1: Right-Angled Triangle Pattern on Console
% Predicate to print a right-angled triangle on the console
right_angle_triangle_console :-
    write('Enter the height of the right-angled triangle: '), % print the query on the console
    read(Height), % Waits for the user input and assigns it to Height
    Height > 0, % Ensure the input is a positive integer greater than 0
    nl,% outputs a newline to the console
    print_triangle(1, Height). % calls the print_triangle predicate to print the triangle

% Auxiliary predicate for printing the right-angled triangle
% Base case: Stop when the current row exceeds the height
print_triangle(Current, Height) :-
    Current > Height, !. % cuts further backtracking when Current > Height

% Recursive case: Print the current row and move to the next
print_triangle(Current, Height) :-
    print_row(Current), % print the current row of the triangle
    nl, 
    Next is Current + 1, % Increment the row number by 1 and assign it to Next
    print_triangle(Next, Height).

% Print a single row with a given number of '#'
% Base case: when N is 0, meaning no more # characters needs to be printed
print_row(0) :- !. % cuts further backtracking when based case is reached
% recursive case: when N is greater than 0, meaning further '#' needs to be printed
print_row(N) :-
    write('#'),
    Remaining is N - 1, % update the number of '#' that needs to be printed
    print_row(Remaining).


% Question 2
% Predicate to write an isosceles triangle to a file
isosceles_triangle_pattern_file(Height,File) :- 
    open(File,write,Stream), % open the file for write operation
    print_isosoles(Height,1,Stream), % Generate the isosceles triangle 
    close(Stream), % close the file after writing
    write('Isosceles triangle pattern written to file:'),write(File),nl.
    
%Auxiliary predicate for printing the isosoles triangle
%Base case: stop when the curren row exceeds the height
print_isosoles(Height,Row,_) :-
    Row > Height,!.
%Recursive case:Write the current row including spaces to the file and move on to the next tow
print_isosoles(Height,Row,Stream) :- 
    Space is Height - Row, % number of spaces needed before each line
    Star is 2 * Row - 1, % number of stars in the row
    print_spaces(Space,Stream), % write spaces to the file
    print_stars(Star,Stream), % write stars to the file 
    nl(Stream), % add a newline to the file
    Next is Row + 1, % Move on to the next row
    print_isosoles(Height,Next,Stream).

%Auxiliary predicate to write spaces
%Base case: stops backing tracking when the remaining number of spaces reaches 0
print_spaces(0,_) :-
    !.
%Recursive case: write the spaces before each line
print_spaces(N,Stream) :- 
    write(Stream,' '),
    Tmp is N - 1,
    print_spaces(Tmp,Stream).

%Auxiliary predicate to write stars
%Base case: stops backtracking when the number of stars reaches 0
print_stars(0,_) :-
    !.
%Recursive case: write stars 
print_stars(N,Stream) :-
    write(Stream,'*'),
    Remaining is N - 1,
    print_stars(Remaining,Stream).
    
% Task B



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