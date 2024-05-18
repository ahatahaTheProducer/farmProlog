% mustafa taha soylemez
% 2021400081
% compiling: yes
% complete: yes


:- ['cmpefarm.pro'].
:- init_from_map.


% 1- agents_distance(+Agent1, +Agent2, -Distance)
agents_distance(Agent1, Agent2, Distance) :-
    get_dict(x, Agent1, X1),
    get_dict(y, Agent1, Y1),
    % now we have the coordinates of the first agents
    get_dict(x, Agent2, X2),
    get_dict(y, Agent2, Y2),
    % now we can give the output as the Distance
    Distance is abs(X1 - X2) + abs(Y1 - Y2).


% 2- number_of_agents(+State, -NumberOfAgents)
number_of_agents(State, NumberOfAgents) :-
    State = [Agents, _, _, _],
    dict_pairs(Agents, _, Pairs),
    the_length(Pairs, NumberOfAgents).

the_length([], Len) :-
    Len is 0.

the_length([_| Tail], Len) :-
    the_length(Tail, RemLen),
    Len is RemLen + 1.


% 3- value_of_farm(+State, -Value)
value_of_farm(State, Value) :-
    State = [Agents, Objects, _, _],
    dict_pairs(Agents, _, Pairs),
    dict_pairs(Objects, _, Pairs2),
    concat(Pairs, Pairs2, List),
    calculate_values(List, Value).
% like concatanating two lists, i will use a recursive method in order to iterate over the elements of the list.
calculate_values([], Value) :-
    Value is 0.

calculate_values([Head | Tail], Value) :-
    calculate_values(Tail, SubValue),
    second_one_of_the_pair(Head, Dict),
    get_dict(subtype, Dict, Genes),
    (Genes = wolf ->
        CurValue = 0
        ;
        value(Genes, CurValue)
    ),
    Value is SubValue + CurValue.
    
% this one gives me the second term of the Key-Value pair when i need it.
second_one_of_the_pair(_-Value, Value).

% with a recursive call i will concatanate two lists
concat([], L2, L2).

concat([Head | Tail], L2, [Head | ResultList]) :-
    concat(Tail, L2, ResultList).

% 4- find_food_coordinates(+State, +AgentId, -Coordinates)
find_food_coordinates(State, AgentId, Coordinates) :-
    % i am going to iterate over all the objects and if consumable i will concatanate it to an existing list with concat predicate i wrote
    % first i need to know what animal i am facing
    State = [Agents, Objects, _, _],
    Animal = Agents.AgentId.subtype,
    dict_pairs(Objects, _, FoodList),
    dict_pairs(Agents, _, AgentsList), % this is actually for wolves
    concat(FoodList, AgentsList, AllList),
    edibles(Animal, AllList, Coordinates),
    (Coordinates = [] ->
        fail
        ;
        true
    ),
    !.

edibles(_, [], CoordinatesList) :-
    CoordinatesList = [].
edibles(Animal, [Head | Tail], CoordinatesList) :-
    edibles(Animal, Tail, RemList),
    second_one_of_the_pair(Head, CurObject),
    FoodType = CurObject.subtype,
    (can_eat(Animal,FoodType) ->
        CurCoord = [[CurObject.x, CurObject.y]]
        ;
        CurCoord = []
    ),
    concat(RemList, CurCoord, CoordinatesList).

% 5- find_nearest_agent(+State, +AgentId, -Coordinates, -NearestAgent)
%here we use setof since it sorts the elements that it found.

%so the first element in the Sorted pairs will be the closest one (since we are eliminating the Agent itself)
find_nearest_agent(State, AgentId, Coordinates, NearestAgent) :-
    % first i need to find where my base point is
    State = [Agents, _, _, _],
    coordinate_finder(Agents, AgentId, BaseCoord),
    dict_pairs(Agents, _, AgentsList),
    setof(Distance-AgentDict, AgentPair^TempId^AgentId^X^Y^(
        member(AgentPair, AgentsList),
        AgentPair = TempId-AgentDict,
        TempId \= AgentId,
        get_dict(x, AgentDict, X),
        get_dict(y, AgentDict, Y),
        manhattan_distance(X, Y, BaseCoord, Distance)
    ), SortedPairs),
    SortedPairs = [NearestAgentPair | _],
    NearestAgentPair = _-NearestAgent,
    Coordinates = [NearestAgent.x, NearestAgent.y].

first_one_of_the_pair(Key-_, Key).
second_in_the_list([_ | Tail], Second) :-
    Tail = [Second | _].

manhattan_distance(X, Y, BaseCoord, Distance) :-
    BaseCoord = [BaseX, BaseY],
    Distance is abs(X - BaseX) + abs(Y - BaseY).    
coordinate_finder(Agents, AgentId, BaseCoord) :-
    NowAgent = Agents.AgentId,
    get_dict(x, NowAgent, BaseX),
    get_dict(y, NowAgent, BaseY),
    BaseCoord = [BaseX, BaseY].


% 6- find_nearest_food(+State, +AgentId, -Coordinates, -FoodType, -Distance)
%this predicate follows the same logic as the previous one
find_nearest_food(State, AgentId, Coordinates, FoodType, Distance) :-
    % first i will create a list containing all consumable foods in the area
    State = [Agents, Objects, _, _],
    Animal = Agents.AgentId.subtype,
    BaseCoord = [Agents.AgentId.x, Agents.AgentId.y],
    dict_pairs(Objects, _, FoodList),
    dict_pairs(Agents, _, AgentsList), % this is actually for wolves
    concat(FoodList, AgentsList, AllList),
    edibles_list(Animal, AllList, FoodsList),
    (FoodsList = [] ->
        fail
        ;
        true
    ),
    !,
    setof(Distance-AgentDict, X^Y^(
        member(AgentDict, FoodsList),
        get_dict(x, AgentDict, X),
        get_dict(y, AgentDict, Y),
        manhattan_distance(X, Y, BaseCoord, Distance)
    ), SortedFoods),
    SortedFoods = [NearestFoodPair | _],
    NearestFoodPair = Distance-TheFood,
    Coordinates = [TheFood.x, TheFood.y],
    FoodType = TheFood.subtype,
    !.

edibles_list(_, [], FoodsList) :-
    FoodsList = [].
edibles_list(Animal, [Head | Tail], FoodsList) :-
    edibles_list(Animal, Tail, RemList),
    second_one_of_the_pair(Head, CurObject),
    FoodType = CurObject.subtype,
    (can_eat(Animal,FoodType) ->
        CurObjList = [CurObject]
        ;
        CurObjList = []
    ),
    concat(RemList, CurObjList, FoodsList).



% 7- move_to_coordinate(+State, +AgentId, +X, +Y, -ActionList, +DepthLimit)

%this one is the triciest one for me. it uses a breadth first search approach to find the way to the destination in the shortest
%possible way. 
move_to_coordinate(State, AgentId, X, Y, ActionList, DepthLimit) :-
    % now we have three possibilities
    % first we can have either a wolf, a chicken or a cow
    % i will write seperate predicates for all of them since they move in various ways
    animal_mover(State, AgentId, X, Y, ActionList, DepthLimit).



animal_mover(State, AgentId, X, Y, ActionList, DepthLimit) :-
    % State = [Agents, _, _, _],
    % lets see initially where we area
    % BaseCoord = [Agents.AgentId.x, Agents.AgentId.y],
    % now we have the predicate move_rep giving the possible moves for each state i want to create another predicate that
    % will make a breadth first search in order to get to the coordinate X,Y
    bfs([[State, [], 0]], [], AgentId, X, Y, DepthLimit, ActionList),!.

my_move_rep(Agents, Objects, AgentId, ActionList):-
    % AllPossibleDirections = [move_down, move_down_left, move_left, move_up_left, move_up, move_up_right, move_right, move_down_right],
    State = [Agents, Objects, _, _],
    findall(
        SomeAction, 
        (
            get_dict(AgentId, Agents, Agent),
            can_move(Agent.subtype, SomeAction),
            move(State, AgentId, SomeAction, _)),
        ActionList
        ),!.
bfs([[State, Actions, Depth] | RestQ], Visited, AgentId, X, Y, DepthLimit, ResultActions) :-
    State = [Agents, Objects, _, _],
    CurPos = [Agents.AgentId.x, Agents.AgentId.y],

    (CurPos = [X, Y] -> 
        ResultActions = Actions
    ;
    \+ member(CurPos, Visited),
    Depth < DepthLimit ->
        NewVisited = [CurPos | Visited],
        my_move_rep(Agents, Objects, AgentId, PosMovs),
        list_expander(Visited, State, Actions, NewRestQ, Depth, PosMovs, AgentId),
        concat(RestQ, NewRestQ, NewQ),
        bfs(NewQ, NewVisited, AgentId, X, Y, DepthLimit, ResultActions)
    ;
        bfs(RestQ, Visited, AgentId, X, Y, DepthLimit, ResultActions)
    ).

% the list_expander adds new list elemnets for bfs to searth through so that bfs will go on for all possible directions
% ofcourse we are considering visited coordinates since we do not have infinite time, energy and memory
list_expander(Visited, State, Actions, NewRestQ, Depth, PosMovs, AgentId) :-
    findall([TmpState, TmpActions, NewDepth], (
        member(TmpMov, PosMovs),
        move(State, AgentId, TmpMov, TmpState),
        TmpState = [TmpAgents, _, _, _],
        \+ member([TmpAgents.AgentId.x, TmpAgents.AgentId.y], Visited),
        concat(Actions, [TmpMov], TmpActions),
        NewDepth is Depth + 1
    ),
    NewRestQ), !.
    
% 8- move_to_nearest_food(+State, +AgentId, -ActionList, +DepthLimit)
move_to_nearest_food(State, AgentId, ActionList, DepthLimit) :-
    find_nearest_food(State, AgentId, Coordinates, _, _),
    Coordinates = [X, Y],
    move_to_coordinate(State, AgentId, X, Y, ActionList, DepthLimit).
% 9- consume_all(+State, +AgentId, -NumberOfMoves, -Value, NumberOfChildren +DepthLimit)
% this one is actually pretty simple since it uses the previous predicates.
% if not exceeded the depth limit then it finds the nearest consumable food and goes there, eat it and with the newstate it
%calls the predicate again,
%whenever it cannot find any more food, or it reaches the depth limit then it gives the current information.
consume_all(State, AgentId, NumberOfMoves, Value, NumberOfChildren, DepthLimit) :-
    my_consume_all(State, AgentId, NumberOfMoves, 0, Value, NumberOfChildren, DepthLimit), !.    
my_consume_all(State, AgentId, NumberOfMoves, CurrentNumber, Value, NumberOfChildren, DepthLimit) :-
    (DepthLimit =< 0 -> 
        value_of_farm(State, Value),
        State = [Agents, _, _, _],
        NumberOfChildren = Agents.AgentId.children,
        NumberOfMoves = CurrentNumber
        ;
     find_nearest_consumable_food(State, AgentId, Coordinates, _, _, DepthLimit) ->
        Coordinates = [X, Y],
        mover(State, AgentId, X, Y, DepthLimit, NewLim, NewState, TmpNumberOfMoves),
        NextTmpNumberOfMoves is TmpNumberOfMoves + CurrentNumber,
        eat(NewState, AgentId, EatenState),
        my_consume_all(EatenState, AgentId, NumberOfMoves, NextTmpNumberOfMoves, Value, NumberOfChildren, NewLim)
        ;
    value_of_farm(State, Value),
    State = [Agents, _, _, _],
    NumberOfChildren = Agents.AgentId.children,
    NumberOfMoves = CurrentNumber
    ), !.
mover(State, AgentId, X, Y, DepthLimit, NewLim, NewState, NumberOfMovements) :-
    move_to_coordinate(State, AgentId, X, Y, ActionList, DepthLimit),
    recur_mover(State, AgentId, ActionList, 0, NumberOfMovements, NewState),
    NewLim is DepthLimit - NumberOfMovements.



recur_mover(State, _, [], CurNum, ResultNum, NewState) :-
    NewState = State,
    ResultNum = CurNum.

recur_mover(State, AgentId, [Action | RestActions], CurNum, ResultNum, NewState) :-
    Var is CurNum + 1,
    move(State, AgentId, Action, TempState),
    recur_mover(TempState, AgentId, RestActions, Var, ResultNum, NewState),!.
   
% this one is used because we want consumable nearest food, it additionally checks whether the agent can move to the destionation or not
find_nearest_consumable_food(State, AgentId, Coordinates, FoodType, Distance, DepthLimit) :-
    % first i will create a list containing all consumable foods in the area
    State = [Agents, Objects, _, _],
    Animal = Agents.AgentId.subtype,
    BaseCoord = [Agents.AgentId.x, Agents.AgentId.y],
    dict_pairs(Objects, _, FoodList),
    dict_pairs(Agents, _, AgentsList), % this is actually for wolves
    concat(FoodList, AgentsList, AllList),
    edibles_list(Animal, AllList, FoodsList),
    (FoodsList = [] ->
        fail
        ;
        true
    ),
    !,
    setof(Distance-AgentDict, X^Y^(
        member(AgentDict, FoodsList),
        get_dict(x, AgentDict, X),
        get_dict(y, AgentDict, Y),
        move_to_coordinate(State, AgentId, X, Y, _, DepthLimit),
        manhattan_distance(X, Y, BaseCoord, Distance)
    ), SortedFoods),
    (SortedFoods = [] -> 
        Distance = 1234,
        Coordinates = [],
        FoodType = 0
        ;
        SortedFoods = [NearestFoodPair | _],
        NearestFoodPair = Distance-TheFood,
        Coordinates = [TheFood.x, TheFood.y],
        FoodType = TheFood.subtype
    ),
    !.

