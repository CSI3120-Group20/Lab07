% Task B

% Define a DCG (definite clause grammar) for Character Descriptions
% DCG rules process the input list in order, consuming one item in the input list at a time from left to right.

% Main rule
% The main rule `character_description` expands into the sequence of sub-rules
character_description -->
    character_type(Type),
    subtype(Type, Subtype),
    sequence_number(PositiveNumber),
    movement_direction(Type, Movement, Weapon),
    health_level(HealthLevel),
    weapon_possession(Type, Weapon),
    movement_style(Style).


% Rules for Character Type
% character_type(e) is nonterminal, which represents the left-hand side of the rule
% `e` and `h` are placeholders or constants
%
% It used character type `e` to match the terminal `enemy` in the input list. By defining this mapping, we
% ensure that `e` always corresponds to `enemy`.
%
character_type(e) --> [enemy].
character_type(h) --> [hero].


% Rules for Character Subtype
% `dark_wizard` is a placeholder
% When the character type is `e` and subtype is `dark_wizard`, match the terminal `darkwizard` in the input list
subtype(e, dark_wizard) --> [darkwizard].
subtype(e, demon) --> [demon].
subtype(e, basilisk) --> [basilisk].
subtype(h, wizard) --> [wizard].
subtype(h, mage) --> [mage].
subtype(h, elf) --> [elf].


% Rules for Sequence
sequence_number(number) -->
    [Number],
    { integer(Number), Number > 0 }.
    % A grammar goal is written in curly brackets as { Goal }. It acts as a constraint or condition.
    % Integer(Number): Ensures that `Number` is an integer
    % Number > 0: Ensures that `Number` is greater than 0


% Rules for Movement Direction
movement_direction(e, move_toward, _) --> [towards].
movement_direction(h, move_toward, has_weapon) --> [towards].
movement_direction(h, move_away, no_weapon) --> [away].


% Rules for Health Levels
health_level(veryWeak) --> [very_weak].
health_level(weak) --> [weak].
health_level(normal) --> [normal].
health_level(strong) --> [strong].
health_level(very_strong) --> [very_strong].


% Rules for Weapon possession
weapon_possession(e, no_weapon) --> [no_weapon].
weapon_possession(h, no_weapon) --> [no_weapon].
weapon_possession(h, has_weapon) --> [has_weapon].


% Rules for Movement styles
movement_style(move_jerky) --> [jerky].
movement_style(move_stealthy) --> [stealthy].
movement_style(move_smoothly) --> [smoothly].