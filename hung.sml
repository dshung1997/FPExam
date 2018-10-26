structure Reversi_AI =
struct
    (* ALL your code goes here, inside the structure *)
    (* datatype player = Black | White
    datatype move = Pass | Move of int *)
    type field = player option;
    val size = 8;
    val game_tree_height = 5;
    type board = field list;
    type T = player * board;         
    val author = "Hung Phan";
    val nickname = "Hung Phan";

    (* player_of position
    TYPE: T -> player
    PRE:  true
    POST: player in T
    SIDE EFFECTS: side-effects free
    EXAMPLES: player_of (p, f) = p
    *)    
    fun player_of ((p, _) : T) = p;
    
    (* board_of position
    TYPE: T -> board
    PRE:  true
    POST: board in T
    SIDE EFFECTS: side-effects free
    EXAMPLES: player_of (p, f) = f
    *)    
    fun board_of ((_, b): T) = b;

    (* get_field position i
    TYPE: T -> int -> field
    PRE:  0 <= i <= 63
    POST: ith field in board of position
    SIDE EFFECTS: raise Subscript if i < 0 or i > 63
    EXAMPLES: get_field (init Black) = None
    *)    
    fun get_field (position: T) i = List.nth (board_of position, i);

    (* opponent p
    TYPE: player -> player
    PRE:  true
    POST: opponent of p
    SIDE EFFECTS: side-effects free
    EXAMPLES: opponent Black = White
    *)  
    fun opponent Black = White
        | opponent White = Black;

    val board_list = [  0, 0, 0, 0, 0, 0, 0, 0, 
                        0, 0, 0, 0, 0, 0, 0, 0, 
                        0, 0, 0, 0, 0, 0, 0, 0, 
                        0, 0, 0, ~1, 1, 0, 0, 0, 
                        0, 0, 0, 1, ~1, 0, 0, 0, 
                        0, 0, 0, 0, 0, 0, 0, 0, 
                        0, 0, 0, 0, 0, 0, 0, 0, 
                        0, 0, 0, 0, 0, 0, 0, 0];

    val marks = [
                    99, ~8, 8, 6, 6, 8, ~8, 99, 
                    ~8, ~24, ~4, ~3, ~3, ~4, ~24, ~8, 
                    8, ~4, 7, 4, 4, 7, ~4, 8, 
                    6, ~3, 4, 0, 0, 4, ~3, 6, 
                    6, ~3, 4, 0, 0, 4, ~3, 6, 
                    8, ~4, 7, 4, 4, 7, ~4, 8, 
                    ~8, ~24, ~4, ~3, ~3, ~4, ~24, ~8, 
                    99, ~8, 8, 6, 6, 8, ~8, 99
                ];

    (* mark_of p
    TYPE: T -> int
    PRE:  true
    POST: difference between the point of player in position and opponent
    SIDE EFFECTS: side-effects free
    EXAMPLES: mark_of (init Black) = 0
    *) 
    fun mark_of position = 
            (* mark_of' i
            TYPE: int -> int
            PRE:  i = 0
            POST: difference between the point of player in position and opponent
            SIDE EFFECTS: side-effects free
            EXAMPLES: mark_of (init Black) = 0
            *)
            (* INVARIANT: 63-i *)
        let
            fun mark_of' i = if i > 63 then
                                0
                            else case (get_field position i) of
                                SOME(p) => if p = (player_of position) then
                                                (List.nth (marks, i)) + (mark_of' (i+1))
                                            else
                                                ~(List.nth (marks, i)) + (mark_of' (i+1))
                                                
                                | NONE => mark_of' (i+1)
        in
            mark_of' 0
        end;

    (* to_board x
    TYPE: int list -> board
    PRE:  elements in x have value in (~1, 0, 1)
    POST: board with corresponding field is None if value = 0, SOME(Black) if value = 1, SOME(White) if value = ~1
    SIDE EFFECTS: side-effects free
    EXAMPLES: to_board [~1, 1 , 0] = [SOME(White), SOME(Black), NONE]
    *) 
    (* INVARIANT: length of x *)
    fun to_board [] = []
        | to_board (x::xs) = if x = 1 then
                                SOME(Black)::(to_board xs)
                            else if x = ~1 then
                                SOME(White)::(to_board xs)
                            else 
                                NONE::(to_board xs);

    (* init p
    TYPE: player -> T
    PRE:  true
    POST: T with player = p, board is initial board of reversi game
    SIDE EFFECTS: side-effects free
    EXAMPLES: to_board Black = (Black, [    NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, 
                                            NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, 
                                            NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, 
                                            NONE, NONE, NONE, SOME(White), SOME(Black), NONE, NONE, NONE, 
                                            NONE, NONE, NONE, SOME(Black), SOME(White), NONE, NONE, NONE, 
                                            NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, 
                                            NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, 
                                            NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE])
    *) 
    fun init player : T =
        let
          val my_board = to_board board_list
        in
          (player, my_board)
        end; 

    (* is_valid_up_move p i active_player
    TYPE: T -> int -> player -> bool
    PRE:  0 <= i <= 63
    POST: true if i is valid move of active_player, which flips opponent's discs in up side, false otherwise
    SIDE EFFECTS: side-effects free
    EXAMPLES:   is_valid_up_move (init Black) 0 Black = false
                is_valid_up_move (init Black) 44 Black = true
    *) 
    fun is_valid_up_move (position: T) i active_player =
        (* is_valid_up_move' j c
        TYPE: int -> int -> bool
        PRE:  j=i-8, c=0
        POST: true if i is valid move of active_player, which flips opponent's discs in up side, false otherwise
        SIDE EFFECTS: side-effects free
        EXAMPLES:   is_valid_up_move' 36 0 = true (when position = init Black, active_player = Black) 
        *)
        (*INVARIANT: j *)
                let
                    fun is_valid_up_move' j c = if j < 0 then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = active_player then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_up_move' (j-8) (c+1)
                                                     | NONE => false  
                in
                    is_valid_up_move' (i-8) 0
                end;

    (* is_valid_down_move p i active_player
    TYPE: T -> int -> player -> bool
    PRE:  0 <= i <= 63
    POST: true if i is valid move of active_player, which flips opponent's discs in down side, false otherwise
    SIDE EFFECTS: side-effects free
    EXAMPLES:   is_valid_down_move (init Black) 19 Black = true
    *) 
    fun is_valid_down_move (position: T) i  active_player =
        (* is_valid_down_move' j c
        TYPE: int -> int -> bool
        PRE:  j=i+8, c=0
        POST: true if i is valid move of active_player, which flips opponent's discs in down side, false otherwise
        SIDE EFFECTS: side-effects free
        EXAMPLES:   is_valid_down_move' 28 0 = true (when position = init Black, active_player = Black) 
        *)
        (*INVARIANT: j *)
                let
                    fun is_valid_down_move' j c = if j > 63  then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = active_player then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_down_move' (j+8) (c+1)
                                                    | NONE => false  
                in
                    is_valid_down_move' (i+8) 0
                end;

    (* is_valid_left_move p i active_player
    TYPE: T -> int -> player -> bool
    PRE:  0 <= i <= 63
    POST: true if i is valid move of active_player, which flips opponent's discs in left side, false otherwise
    SIDE EFFECTS: side-effects free
    EXAMPLES:   is_valid_left_move (init Black) 37 Black = true
    *) 
    fun is_valid_left_move (position: T) i active_player =
        (* is_valid_left_move' j c
        TYPE: int -> int -> bool
        PRE:  j=i-1, c=0
        POST: true if i is valid move of active_player, which flips opponent's discs in left side, false otherwise
        SIDE EFFECTS: side-effects free
        EXAMPLES:   is_valid_left_move' 36 0 = true (when position = init Black, active_player = Black) 
        *)
        (*INVARIANT: j *)
                let
                    fun is_valid_left_move' j c = if (i-j) > (i mod size)  then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = active_player then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_left_move' (j-1) (c+1)
                                                    | NONE => false
                in
                    is_valid_left_move' (i-1) 0
                end;

    (* is_valid_right_move p i active_player
    TYPE: T -> int -> player -> bool
    PRE:  0 <= i <= 63
    POST: true if i is valid move of active_player, which flips opponent's discs in right side, false otherwise
    SIDE EFFECTS: side-effects free
    EXAMPLES:   is_valid_right_move (init Black) 26 Black = true
    *) 
    fun is_valid_right_move (position: T) i active_player =
        (* is_valid_right_move' j c
        TYPE: int -> int -> bool
        PRE:  j=i+1, c=0
        POST: true if i is valid move of active_player, which flips opponent's discs in right side, false otherwise
        SIDE EFFECTS: side-effects free
        EXAMPLES:   is_valid_right_move' 27 0 = true (when position = init Black, active_player = Black) 
        *)
        (*INVARIANT: j *)
                let
                    fun is_valid_right_move' j c = if (j-i + (i mod size)) >= size  then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = active_player then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_right_move' (j+1) (c+1)
                                                    | NONE => false
                in
                    is_valid_right_move' (i+1) 0
                end;

    (* is_valid_up_left_move p i active_player
    TYPE: T -> int -> player -> bool
    PRE:  0 <= i <= 63
    POST: true if i is valid move of active_player, which flips opponent's discs in up-left side, false otherwise
    SIDE EFFECTS: side-effects free
    EXAMPLES:   is_valid_up_left_move (init Black) 26 Black = false
    *) 
    fun is_valid_up_left_move (position: T) i active_player =
        (* is_valid_up_left_move' j c c'
        TYPE: int -> int -> int -> bool
        PRE:  j=i-9, c=0, c'=1
        POST: true if i is valid move of active_player, which flips opponent's discs in up-left side, false otherwise
        SIDE EFFECTS: side-effects free
        EXAMPLES:   is_valid_up_left_move' 27 0 = false (when position = init Black, active_player = Black) 
        *)
        (*INVARIANT: j *)
                let
                    fun is_valid_up_left_move' j c c' = if j < 0 orelse c' > (i mod size) then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = active_player then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_up_left_move' (j-9) (c+1) (c'+1)
                                                    | NONE => false
                in
                    is_valid_up_left_move' (i-9) 0 1
                end;

    (* is_valid_down_left_move p i active_player
    TYPE: T -> int -> player -> bool
    PRE:  0 <= i <= 63
    POST: true if i is valid move of active_player, which flips opponent's discs in down-left side, false otherwise
    SIDE EFFECTS: side-effects free
    EXAMPLES:   is_valid_down_left_move (init Black) 26 Black = false
    *) 
    fun is_valid_down_left_move (position: T) i active_player =
        (* is_valid_down_left_move' j c c'
        TYPE: int -> int -> int -> bool
        PRE:  j=i+7, c=0, c'=1
        POST: true if i is valid move of active_player, which flips opponent's discs in down-left side, false otherwise
        SIDE EFFECTS: side-effects free
        EXAMPLES:   is_valid_down_left_move' 27 0 = false (when position = init Black, active_player = Black) 
        *)
        (*INVARIANT: j *)
                let
                    fun is_valid_down_left_move' j c c' = if j > 63 orelse c' > (i mod size) then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = active_player then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_down_left_move' (j+7) (c+1) (c'+1)
                                                    | NONE => false
                in
                    is_valid_down_left_move' (i+7) 0 1
                end;      

    (* is_valid_up_right_move p i active_player
    TYPE: T -> int -> player -> bool
    PRE:  0 <= i <= 63
    POST: true if i is valid move of active_player, which flips opponent's discs in up_right side, false otherwise
    SIDE EFFECTS: side-effects free
    EXAMPLES:   is_valid_up_right_move (init Black) 26 Black = false
    *) 
    fun is_valid_up_right_move (position: T) i active_player =
        (* is_valid_up_right_move' j c c'
        TYPE: int -> int -> int -> bool
        PRE:  j=i-7, c=0, c'=1
        POST: true if i is valid move of active_player, which flips opponent's discs in up-right side, false otherwise
        SIDE EFFECTS: side-effects free
        EXAMPLES:   is_valid_up_right_move' 27 0 = false (when position = init Black, active_player = Black) 
        *)
        (*INVARIANT: j *)
                let
                    fun is_valid_up_right_move' j c c' = if j < 0 orelse c' > (size - 1 - (i mod size)) then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = active_player then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_up_right_move' (j-7) (c+1) (c'+1)
                                                    | NONE => false
                in
                    is_valid_up_right_move' (i-7) 0 1
                end;     

    (* is_valid_down_right_move p i active_player
    TYPE: T -> int -> player -> bool
    PRE:  0 <= i <= 63
    POST: true if i is valid move of active_player, which flips opponent's discs in down_right side, false otherwise
    SIDE EFFECTS: side-effects free
    EXAMPLES:   is_valid_down_right_move (init Black) 26 Black = false
    *) 
    fun is_valid_down_right_move (position: T) i active_player =
        (* is_valid_down_right_move' j c c'
        TYPE: int -> int -> int -> bool
        PRE:  j=i+9, c=0, c'=1
        POST: true if i is valid move of active_player, which flips opponent's discs in down-right side, false otherwise
        SIDE EFFECTS: side-effects free
        EXAMPLES:   is_valid_down_right_move' 27 0 = false (when position = init Black, active_player = Black) 
        *)
        (*INVARIANT: j *)
                let
                    fun is_valid_down_right_move' j c c' = if j > 63 orelse c' > (size - 1 - (i mod size)) then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = active_player then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_down_right_move' (j+9) (c+1) (c'+1)
                                                    | NONE => false
                in
                    is_valid_down_right_move' (i+9) 0 1
                end;            
    
    (* get_valid_moves p player_arg
    TYPE: T -> player -> move list
    PRE:  true
    POST: list of valid moves of player_arg in p
    SIDE EFFECTS: side-effects free
    EXAMPLES:   get_valid_moves (init Black) Black = [Move(44), Move(19), Move(37), Move(26)]
    *)     
    fun get_valid_moves (position: T) player_arg =
        (* get_valid_moves' i
        TYPE: int -> move list
        PRE:  i = 0
        POST: list of valid moves of player_arg in p
        SIDE EFFECTS: side-effects free
        EXAMPLES:   get_valid_moves' 0 = [Move(44), Move(19), Move(37), Move(26)] (when position = init Black, player_arg = Black)
        *)  
        (* INVARIANT: 63-i *)
        let
            fun get_valid_moves' i = if i > 63 then
                                        []
                                    else if (get_field position i) = NONE andalso ((is_valid_up_move position i player_arg) 
                                                                    orelse (is_valid_down_move position i player_arg) 
                                                                    orelse (is_valid_left_move position i player_arg) 
                                                                    orelse (is_valid_right_move position i player_arg)
                                                                    orelse (is_valid_down_left_move position i player_arg)
                                                                    orelse (is_valid_up_left_move position i player_arg)
                                                                    orelse (is_valid_up_right_move position i player_arg)
                                                                    orelse (is_valid_down_right_move position i player_arg)) then
                                        Move(i)::(get_valid_moves' (i+1))
                                    else
                                        get_valid_moves' (i+1)
        in
            get_valid_moves' 0
        end; 

    (* make_move p active_player m
    TYPE: T -> player -> move -> T
    PRE:  m is valid move of active_player
    POST: position after active_player make move m
    SIDE EFFECTS: side-effects free
    *)   
    fun make_move (position: T) active_player m : T =
            (* make_move' i j b
            TYPE: int -> int -> board -> board
            PRE:  j = 0
            POST: mark ith field with value = SOME(active_player) in b
            SIDE EFFECTS: side-effects free
            *)             
            (* INVARIANT: length of b *)
        let
            fun make_move' _ _ [] _ = []
                | make_move' i j (f::fs) active_player = if i = j then 
                                                            (SOME(active_player))::fs
                                                        else 
                                                            f::(make_move' i (j+1) fs active_player)
            exception FLIPNONE

            (* flip_disc_up position i
            TYPE: T -> int -> T
            PRE:  0 <= i <= 63
            POST: if is_valid_up_move position i, then flip discs of active_player's opponent in the up side till reaching the first disc of active_player
            SIDE EFFECTS: side-effects free
            *)             
            fun flip_disc_up (position: T) i = 
                (* flip_disc_up' j position
                TYPE: int -> T -> T
                PRE:  j=i-8
                POST: then flip discs of active_player's opponent in the up side till reaching the first disc of active_player
                SIDE EFFECTS: side-effects free
                *)
                (* INVARIANT: j *)
                let
                    fun flip_disc_up' j (position: T) = case (get_field position j) of
                                                            SOME(p) => if p = active_player then 
                                                                            position
                                                                        else
                                                                            flip_disc_up' (j-8) (player_of position, make_move' j 0 (board_of position) active_player)
                                                            | NONE => raise FLIPNONE              
                in
                    if is_valid_up_move position i active_player then
                        flip_disc_up' (i-8) position
                    else position
                end;

            (* flip_disc_down position i
            TYPE: T -> int -> T
            PRE:  0 <= i <= 63
            POST: if is_valid_down_move position i, then flip discs of active_player's opponent in the down side till reaching the first disc of active_player
            SIDE EFFECTS: side-effects free
            *) 
            fun flip_disc_down (position: T) i =
                (* flip_disc_down' j position
                TYPE: int -> T -> T
                PRE:  j=i+8
                POST: then flip discs of active_player's opponent in the down side till reaching the first disc of active_player
                SIDE EFFECTS: side-effects free
                *)
                (* INVARIANT: j *)
                let
                    fun flip_disc_down' j (position: T) = case (get_field position j) of
                                                                SOME(p) => if p = active_player then 
                                                                                position
                                                                            else
                                                                                flip_disc_down' (j+8) (player_of position, make_move' j 0 (board_of position) active_player)
                                                                | NONE => raise FLIPNONE             
                in
                    if is_valid_down_move position i active_player then
                        flip_disc_down' (i+8) position
                    else position
                end                                                           

            (* flip_disc_left position i
            TYPE: T -> int -> T
            PRE:  0 <= i <= 63
            POST: if is_valid_left_move position i, then flip discs of active_player's opponent in the left side till reaching the first disc of active_player
            SIDE EFFECTS: side-effects free
            *) 
            fun flip_disc_left (position: T) i =
                (* flip_disc_left' j position
                TYPE: int -> T -> T
                PRE:  j=i-1
                POST: then flip discs of active_player's opponent in the left side till reaching the first disc of active_player
                SIDE EFFECTS: side-effects free
                *)
                (* INVARIANT: j *)
                let
                    fun flip_disc_left' j (position: T) = case (get_field position j) of
                                                                SOME(p) => if p = active_player then 
                                                                                position
                                                                            else
                                                                                flip_disc_left' (j-1) (player_of position, make_move' j 0 (board_of position) active_player)
                                                                | NONE => raise FLIPNONE               
                in
                    if is_valid_left_move position i active_player then
                        flip_disc_left' (i-1) position
                    else position                
                end;

            (* flip_disc_right position i
            TYPE: T -> int -> T
            PRE:  0 <= i <= 63
            POST: if is_valid_right_move position i, then flip discs of active_player's opponent in the right side till reaching the first disc of active_player
            SIDE EFFECTS: side-effects free
            *) 
            fun flip_disc_right (position: T) i =
                (* flip_disc_right' j position
                TYPE: int -> T -> T
                PRE:  j=i+1
                POST: then flip discs of active_player's opponent in the right side till reaching the first disc of active_player
                SIDE EFFECTS: side-effects free
                *)
                (* INVARIANT: j *)
                let 
                    fun flip_disc_right' j (position: T) = case (get_field position j) of
                                                                SOME(p) => if p = active_player then 
                                                                                position
                                                                            else
                                                                                flip_disc_right' (j+1) (player_of position, make_move' j 0 (board_of position) active_player)
                                                                | NONE => raise FLIPNONE                
                in
                    if is_valid_right_move position i active_player then
                        flip_disc_right' (i+1) position
                    else position                
                end;

            (* flip_disc_up_left position i
            TYPE: T -> int -> T
            PRE:  0 <= i <= 63
            POST: if is_valid_up_left_move position i, then flip discs of active_player's opponent in the up-left side till reaching the first disc of active_player
            SIDE EFFECTS: side-effects free
            *) 
            fun flip_disc_up_left (position: T) i =
                (* flip_disc_up_left' j position
                TYPE: int -> T -> T
                PRE:  j=i-9
                POST: then flip discs of active_player's opponent in the up-left side till reaching the first disc of active_player
                SIDE EFFECTS: side-effects free
                *)
                (* INVARIANT: j *)
                let
                    fun flip_disc_up_left' j (position: T) = case (get_field position j) of 
                                                                SOME(p) => if p = active_player then
                                                                                position
                                                                            else flip_disc_up_left' (j-9) (player_of position, make_move' j 0 (board_of position) active_player)
                                                                | NONE => raise FLIPNONE
                in
                    if is_valid_up_left_move position i active_player then
                        flip_disc_up_left' (i-9) position
                    else position
                end;

            (* flip_disc_up_right position i
            TYPE: T -> int -> T
            PRE:  0 <= i <= 63
            POST: if is_valid_up_right_move position i, then flip discs of active_player's opponent in the up-right side till reaching the first disc of active_player
            SIDE EFFECTS: side-effects free
            *) 
            fun flip_disc_up_right (position: T) i =
                (* flip_disc_up_right' j position
                TYPE: int -> T -> T
                PRE:  j=i-7
                POST: then flip discs of active_player's opponent in the up-right side till reaching the first disc of active_player
                SIDE EFFECTS: side-effects free
                *)
                (* INVARIANT: j *)
                let
                    fun flip_disc_up_right' j (position: T) = case (get_field position j) of 
                                                                SOME(p) => if p = active_player then
                                                                                position
                                                                            else flip_disc_up_right' (j-7) (player_of position, make_move' j 0 (board_of position) active_player)
                                                                | NONE => raise FLIPNONE
                in
                    if is_valid_up_right_move position i active_player then
                        flip_disc_up_right' (i-7) position
                    else position
                end;    

            (* flip_disc_down_right position i
            TYPE: T -> int -> T
            PRE:  0 <= i <= 63
            POST: if is_valid_down_right_move position i, then flip discs of active_player's opponent in the down-right side till reaching the first disc of active_player
            SIDE EFFECTS: side-effects free
            *) 
            fun flip_disc_down_right (position: T) i =
                (* flip_disc_down_right' j position
                TYPE: int -> T -> T
                PRE:  j=i+9
                POST: then flip discs of active_player's opponent in the down-right side till reaching the first disc of active_player
                SIDE EFFECTS: side-effects free
                *)
                (* INVARIANT: j *)
                let
                    fun flip_disc_down_right' j (position: T) = case (get_field position j) of 
                                                                    SOME(p) => if p = active_player then
                                                                                    position
                                                                                else flip_disc_down_right' (j+9) (player_of position, make_move' j 0 (board_of position) active_player)
                                                                    | NONE => raise FLIPNONE
                in
                    if is_valid_down_right_move position i active_player then
                        flip_disc_down_right' (i+9) position
                    else position
                end;    

            (* flip_disc_down_left position i
            TYPE: T -> int -> T
            PRE:  0 <= i <= 63
            POST: if is_valid_down_left_move position i, then flip discs of active_player's opponent in the down-left side till reaching the first disc of active_player
            SIDE EFFECTS: side-effects free
            *) 
            fun flip_disc_down_left (position: T) i =
                (* flip_disc_down_left' j position
                TYPE: int -> T -> T
                PRE:  j=i+7
                POST: then flip discs of active_player's opponent in the down-left side till reaching the first disc of active_player
                SIDE EFFECTS: side-effects free
                *)
                (* INVARIANT: j *)
                let
                    fun flip_disc_down_left' j (position: T) = case (get_field position j) of 
                                                                SOME(p) => if p = active_player then
                                                                                position
                                                                            else flip_disc_down_left' (j+7) (player_of position, make_move' j 0 (board_of position) active_player)
                                                                | NONE => raise FLIPNONE
                in
                    if is_valid_down_left_move position i active_player then
                        flip_disc_down_left' (i+7) position
                    else position
                end; 
                                                                       
            (* final_position position i
            TYPE: T -> int -> T
            PRE:  0 <= i <= 63
            POST: final position after active_player making Move(i)
            SIDE EFFECTS: side-effects free
            *) 
            fun final_position (position: T) i = flip_disc_up_left (
                                                flip_disc_up_right (
                                                flip_disc_down_right (
                                                flip_disc_down_left (
                                                flip_disc_right (
                                                flip_disc_left (
                                                flip_disc_down (
                                                flip_disc_up (player_of position, make_move' i 0 (board_of position) active_player) i) i) i) i
                                                ) i) i) i) i;
                            
        in
            case m of
            Pass => position
            | Move(i) => final_position position i
        end; 

                                                                       
    (* minimax position
    TYPE: T -> move * int
    PRE:  true
    POST: next move with highest mark after searching tree game with height = tree_game_height
    SIDE EFFECTS: side-effects free
    *)
    fun minimax position =
        (* minimax' i position
        TYPE: int -> T -> move * int
        PRE:  i = 1
        POST: next move with highest mark after searching tree game with height = game_tree_height
        SIDE EFFECTS: side-effects free
        *)
        (* INVARIANT: game_tree_height - i *)
        let
            fun minimax' i position =
                let
                    fun extremum cmp [] = (Pass, mark_of position)
                    | extremum cmp [(m, v)] = (m, v)
                    | extremum cmp ((m1, v1)::(m2, v2)::xs) =
                        if Int.compare (v1, v2) = cmp then
                            extremum cmp ((m1, v1)::xs)
                        else
                            extremum cmp ((m2, v2)::xs)
                    val max = extremum GREATER
                    val min = extremum LESS
                in
                    if i > game_tree_height then
                        (Pass, mark_of position)
                    else if i mod 2 = 1 then
                        let
                            val moves = get_valid_moves position (player_of position)
                            val positions = map (make_move position (player_of position)) moves
                            val moves_and_values = map (minimax' (i+1)) positions
                            val values = map (fn (_, v) => v) moves_and_values
                        in
                            max (ListPair.zipEq (moves, values))
                        end
                    else
                        let
                            val moves = get_valid_moves position (opponent (player_of position))
                            val positions = map (make_move position (opponent (player_of position))) moves
                            val moves_and_values = map (minimax' (i+1)) positions
                            val values = map (fn (_, v) => v) moves_and_values
                        in
                            min (ListPair.zipEq (moves, values))
                        end
                end
        in 
            minimax' 1 position
        end;

    (* next_move position
    TYPE: T -> move
    PRE:  true
    POST: next move after using minimax
    SIDE EFFECTS: side-effects free
    *)
     fun next_move (position: T) =
        let
            val valid_moves = get_valid_moves position (player_of position)
            val next_m = minimax position
        in
            #1 next_m
        end;

    (* think position m t
    TYPE: T * move * 'a -> move * T
    PRE:  true
    POST: pair of next move and next position
    SIDE EFFECTS: side-effects free
    *)
    fun think (position: T, m, t) = 
        let
            val current_position = make_move position (opponent (player_of position)) m
            val next_m = next_move current_position
            val next_position = make_move current_position (player_of position) next_m 
        in
            (next_m, next_position)
        end;

end;

(* REFERENCE: http://www.samsoft.org.uk/reversi/strategy.htm *)