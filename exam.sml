(*

SUBMISSION FOR THE EXAM OF FUNCTIONAL PROGRAMMING I - UPPSALA UNIVERSITY 2018 HT

Author: Sy-Hung Doan
Email : Syhung.Doan.2877@student.uu.se | dshung1997@gmail.com
Date  : 00:49:26 26/10/2018


I dedicate this code to all of my best friends in Vietnam for their understanding when I spent time on writing the code instead of being with them :-)


First, to simplify the problem, I denote (in the game board) 1 for Black, ~1 for White and 0 for an empty square.

Then, when it is my turn, I do as follows:
    1. Update the board following the previous move of the opponent (function: update)
    2. Find the next move (function: getNextMove)
    3. Update the board with the new move that I shall make



What about finding the next move ?

1. Function isValidMove returns a tuple of a boolean (that position is valid or not) and a list of furthest positions to be updated
    ->  I declare 8 fixed directions. (val directions)
    ->  For each direction, I get all the positions along that direction starting from the current position being examined. (function: getNodesByDirection)
    ->  Then, I retrieve the values at those positions. (function: getValuesFromNodes)
    ->  If I find a pattern of values so that I can flip at least one of the opponent's disc, (function: checkValues)
    ->  I return true and the furthest position along that direction that I can flip.
    ->  I do the same for the remaining directions (function: checkAllDirections)

2. I obtain a list of valid moves of the current board via (function: getAllValidMoves)

3. I use alpha-beta pruning algorithm for my AI as I initially want to examine till 8th depth (https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning)
    ->  This algorithm is basically minimax
    ->  When the board, which is currently being examined, is at the last depth, I return its heuristic value (I will mention this shortly)

4. About the heuristic value:
    ->  I was inspired by the technique of this Github user : https://github.com/rishabh-shukla/AI-Reversi
    ->  My heuristic value includes 2 elements: (The original technique has 3 elements, but I found it very time-consuming to calculate the 3rd element)
            ->  The first one is how advantageous this board can be for the player. 
                Following his utility matrix (which is a matrix contains each square's strength), I count the sum of all values in the board based on the current player's view point.

            ->  The second one is the difference between the number of Black nodes and the number of White nodes on the board

            -> To find the best weights for the 2 elements, I let my AI play with a stupid bot (makes random moves) couples of times to see which weights give me the highest score.


In conclusion, 
->  I refered to :
        1. https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning for understanding Alpha-beta pruning algorithm
        2. https://github.com/rishabh-shukla/AI-Reversi for how to have a good heuristic value
        3. Book 'AI - a modern approach' also for understading Alpha-beta pruning algorithm

->  I let my AI play against that stupid bot 100 times and below is my result:
    
    [34, 32, 28, 28, 6, 16, 32, 40, 34, 26, 16, 56, 48, 48, 50, 18, 32, 12, 12, 38, 28, 30, 18, 30, 26, 42, 30, 40, 38, 52, 42, 14, 24, 40, 32, 52, 32, 44, 20, 18, 22, 30, 20, 8, 28, 58, 36, 32, 30, 36, 22, 22, 24, 40, 40, 42, 36, ~6, 52, 38, 48, 26, 50, 42, 28, 48, 32, 38, 14, 22, 24, 24, 30, 60, 34, 42, 32, 48, 44, 18, 46, 38, 44, 38, 44, 18, 18, 32, 48, 16, 38, 40, 36, 44, 60, 24, 32, 32, 42, 28]

    Mean: 607.38
    Min of 20 consecutive games : 555
    Max of 20 consecutive games : 661

*)
(************************************************************************************************************************************************)
structure Reversi_AI =
struct
    type T = int * (int list)

    val MAX_REAL =  1000000.0
    val MIN_REAL = ~1000000.0 

    val MAX_DEPTH = 6

    val cellUtility =   
                    [ 
                        25, ~5, 14, 10, 10, 14, ~5, 25,
                        ~5, ~7, ~4,  1,  1, ~4, ~7, ~5,
                        14, ~4,  3,  2,  2,  3, ~4, 14,
                        10,  1,  2, ~6, ~6,  2,  1, 10,
                        10,  1,  2, ~6, ~6,  2,  1, 10,
                        14, ~4,  3,  2,  2,  3, ~4, 14,
                        ~5, ~7, ~4,  1,  1, ~4, ~7, ~5,
                        25, ~5, 14, 10, 10, 14, ~5, 25
                    ]

    val originalBoard = 
                    [   0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0,~1, 1, 0, 0, 0,
                        0, 0, 0, 1,~1, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0 
                    ]

    val author = "Sy-Hung Doan"
    val nickname = "rakkoon"


    (* init  p
    TYPE: player -> int * (int list)
    PRE:  true
    POST: a tuple of the player (integer) and the initial board
    *)
    fun init (p: player) =
            case p of 
                Black => (1, originalBoard)
                | White =>  (~1, originalBoard)
    

    (* think ((plr, board), (lastMove: move), (t: Time.time))
    TYPE: (int * (int list)) * move * Time.time -> move * (int * (int list))
    PRE:  true
    POST: a tuple of next move and a tuple containing information of the player and the board
    *)
    fun think ((plr, board), (lastMove: move), (t: Time.time)) = 
        let
            (* isValidMove board index player
            TYPE: (int list) -> int -> int -> (bool * (int list))
            PRE:  true
            POST: return a tuple of a boolean indicating the index is valid or not, and a list of furthest positions to be updated
            *)
            fun isValidMove (board: int list) (idx: int) (plr: int) = 
                let
                    val directions = [(~1, ~1), (~1, 0), (~1, 1), (0, ~1), (0, 1), (1, ~1), (1, 0), (1, 1)]


                    (* getNodesByDirection index direction_y direction_x
                    TYPE: int * int * int -> int list
                    PRE:  true
                    POST: return a list of positions along that direction from the index
                    *)
                    fun getNodesByDirection (idx: int) (dy: int) (dx: int) =
                        let
                            val y = idx div 8
                            val x = idx mod 8

                            (* isInbound y x
                            TYPE: int * int -> bool
                            PRE:  true
                            POST: whether or not this position is inbound
                            *)
                            fun isInbound (y: int) (x: int) = (y>=0 andalso y<=7) andalso (x>=0 andalso x<=7);

                            (* getNodesByDirection' y x accumulator
                            TYPE: int * int * (int list) -> int list
                            PRE:  true
                            POST: a list of positions along that direction
                            *)
                            fun getNodesByDirection' (y: int) (x: int) acc =
                                if(isInbound y x) then
                                    getNodesByDirection' (y+dy) (x+dx) ((8*y+x)::acc)
                                else
                                    acc
                        in
                            if(((y+dy) < 0) orelse ((x+dx) < 0)) then
                                []
                            else if ((dy>0) orelse (dy=0 andalso dx=1)) then
                                List.rev (getNodesByDirection' y x [])
                            else
                                (getNodesByDirection' y x [])
                        end


                    (* getValuesFromNodes listIndexes dy dx
                    TYPE: (int list) * int * int -> (int * int) list
                    PRE:  true
                    POST: a list of tuples, each inclues an index and its value
                    *)
                    fun getValuesFromNodes (listIndexes: int list) (dy: int) (dx: int) = 
                        let
                            fun getVal _ [] _ acc = acc
                                | getVal [] _ _ acc = acc
                                | getVal (x::xs) (y::ys) i acc =           
                                if (i=y) then
                                    getVal xs ys (i+1) ((y, x)::acc)
                                else
                                    getVal xs (y::ys) (i+1) acc           

                            val listValues = getVal board listIndexes 0 []
                        in
                            if (null listIndexes) then
                                []
                            else if ((dy>0) orelse (dy=0 andalso dx=1)) then
                                List.tl (List.rev listValues)
                            else
                                List.tl (listValues)
                        end


                    (* checkValues listValues
                    TYPE: (int * int) list -> (bool * int)
                    PRE:  true
                    POST: whether or not there is a chance that at least a disc is flipped and the furthest position can be flipped
                    *)
                    fun checkValues (listValues: (int * int) list) =
                        let
                            (* checkValues' listValues lastIndex lastValue
                            TYPE: (int * int) list -> int -> int -> (bool * int) list
                            PRE:  true
                            POST: whether or not there is a chance that at least a disc is flipped and the furthest position can be flipped
                            *)
                            fun checkValues' [] (lastIndex: int) (lastValue: int) = (false, ~1)
                                | checkValues' ((index, value)::ls) lastIndex lastValue = 
                                    if (value=0) then 
                                        (false, ~1)
                                    else if (value+plr=0) then
                                        checkValues' ls index value
                                    else if (value=plr andalso (value+lastValue)=0) then
                                        (true, lastIndex)
                                    else
                                        (false, ~1)
                        in
                            checkValues' listValues 0 0
                        end


                    (* checkDirection dy dx
                    TYPE: int -> int -> bool * int
                    PRE:  true
                    POST: whether or not at least a disc is flipped along this direction and the furthest position can be flipped
                    *)
                    fun checkDirection dy dx =
                        let
                            val listNodes = getNodesByDirection idx dy dx
                            val listValues = getValuesFromNodes listNodes dy dx
                            val isValid = checkValues listValues
                        in
                            isValid
                        end


                    (* checkAllDirections directions status listDest
                    TYPE: (int * int) list -> bool -> int list -> (bool * int list)
                    PRE:  true
                    POST: whether or not there is at least a direction along which a disc is flipped and a list of the furthest position can be flipped
                    *)
                    fun checkAllDirections [] status listDest = (status, listDest)
                      | checkAllDirections ((dy, dx)::ds) status listDest = 
                            let
                                    val (result, dest) = checkDirection dy dx
                            in
                                    if(result) then
                                            checkAllDirections ds true (dest::listDest)
                                    else
                                            checkAllDirections ds status listDest
                            end


                    val (result, listDest) = checkAllDirections directions false []
                in
                    if(result) then 
                        (true, listDest)
                    else
                        (false, [])
                end


            (* getAllValidMoves board player
            TYPE: (int list) -> int -> (int * (int list)) list
            PRE:  true
            POST: a list of valid moves accompanying the furthest positions to be flipped for each valid move
            *)
            fun getAllValidMoves (board: int list) (plr: int) = 
                let
                        fun getAllValidMoves' [] i acc = acc
                          | getAllValidMoves' (0::ns) i acc = 
                                let
                                    val (result, dest) = isValidMove board i plr
                                in
                                    if(result) then
                                        getAllValidMoves' ns (i+1) ((i, dest)::acc)
                                    else
                                        getAllValidMoves' ns (i+1) acc
                                end
                          | getAllValidMoves' (n::ns) i acc = getAllValidMoves' ns (i+1) acc
                    
                        val listValidMoves = List.rev (getAllValidMoves' board 0 [])
                in
                    listValidMoves
                end


            (* getHeuristic board player
            TYPE: int list -> int -> real
            PRE:  true
            POST: heuristic value of the current board and the current player
            *)
            fun getHeuristic (board: int list) (plr: int) = 
                let
                    (* count board cellUtility count1 count2 value
                    TYPE: int list -> int list -> int -> int -> int
                    PRE:  true
                    POST: count1 is the number of player's discs, count2 is the number of opponent's discs
                    *)
                    fun count [] _ count1 count2 value = (count1, count2, value)
                      | count _ [] count1 count2 value = (count1, count2, value)
                      | count (x::xs) (v::vs) count1 count2 value = 
                        if(x = plr) then
                                (count xs vs (count1+1) count2 (value+v))
                        else if(x = (0-plr)) then
                                (count xs vs count1 (count2+1) (value-v))
                        else
                                (count xs vs count1 count2 value)

                    val (c1, c2, v) = count board cellUtility 0 0 0
                    val p = (Real.fromInt (c1-c2)) / (Real.fromInt (c1+c2))
                    val h = (Real.fromInt v) + 8.5*p
                in
                    h
                end


            (* getMiddleNodes from to
            TYPE: int -> int -> int list
            PRE:  true
            POST: a list of nodes between node from and node to
            *)
            fun getMiddleNodes (from: int) (to: int) = 
                let
                    val fromy = from div 8
                    val fromx = from mod 8

                    val toy = to div 8
                    val tox = to mod 8

                    val dy = if(fromy = toy) then 0 else if (fromy < toy) then 1 else ~1
                    val dx = if(fromx = tox) then 0 else if (fromx < tox) then 1 else ~1


                    (* get from_y from_x to_y to_x accumulator
                    TYPE: int -> int -> int -> int -> int list
                    PRE:  true
                    POST: a list of nodes between node from and node to
                    *)
                    fun get fy fx ty tx acc = 
                        if(fy=ty andalso fx=tx) then
                            (8*fy+fx)::acc
                        else 
                            get (fy+dy) (fx+dx) ty tx ((8*fy+fx)::acc)

                    val listNodes = get fromy fromx toy tox []
                in
                    if ((dy>0) orelse (dy=0 andalso dx=1)) then
                        List.rev listNodes
                    else
                        listNodes
                end


            (* update board from to value
            TYPE: int list -> int -> int list -> int -> int list
            PRE:  true
            POST: a new board after updating nodes between node from and each in node to with value
            *)
            fun update (board: int list) (from: int) (to: int list) (value: int) = 
                let
                    fun update' [] _ _ acc = List.rev acc
                      | update' l' [] _ acc = (List.rev acc) @ l'
                      | update' (n::ns) (x::xs) i acc = 
                        if(i=x) then 
                                (update' ns xs (i+1) (value::acc)) 
                        else
                                (update' ns (x::xs) (i+1) (n::acc))

                    fun iter (l: int list) [] = l
                      | iter (l: int list) (to::tos) =
                        let
                            val middleNodes = getMiddleNodes from to
                            val l' = update' l middleNodes 0 []
                        in
                            iter l' tos
                        end
                in
                    iter board to
                end


            (* getNextMove board lastMove currentPlayer maxDepth
            TYPE: int list -> int -> int -> int -> int * (int list)
            PRE:  true
            POST: a tuple of the next move and a list of furthest positions to be flipped
            *)
            fun getNextMove (board: int list) (lastMove: int) (crtPlr: int) (maxDepth: int) =
                let
                    val validMoves = getAllValidMoves board crtPlr

                    datatype X = Min | Max

                    fun changeMode (mode: X) = if(mode=Min) then Max else Min


                    (* minimax board index player depth mode a b
                    TYPE: int list -> int -> int -> int -> X -> real -> real -> int * (int list) * real * int
                    PRE:  true
                    POST: next move to make, a list of furthest positions to be flipped, the heuristic value, the player to make move
                    *)
                    fun minimax (board: int list) (idx: int) (plr: int) (depth: int) (mode: X) (a: real) (b: real) = 
                        let
                            fun mnm' 0 _ _ = (idx, [], (getHeuristic board plr), plr)
                              | mnm' (depth: int) (a': real) (b': real) = 
                                let
                                    fun iter [] bestNextIdx bestNextDest a b = if(mode=Max) then (bestNextIdx, bestNextDest, a, plr) else (bestNextIdx, bestNextDest, b, plr)
                                      | iter ((i, d)::ls) bestNextIdx bestNextDest a b = 
                                            let
                                                val (_, _, v, _) = minimax (update board i d plr) i (0-plr) (depth-1) (changeMode mode) a b

                                                val a_ = if((mode=Max andalso v > a)) then v else a

                                                val b_ = if((mode=Min andalso v < b)) then v else b

                                                val bestNextIdx_ = if((mode=Max andalso v > a) orelse (mode=Min andalso v < b)) then i else bestNextIdx

                                                val bestNextDest_ = if((mode=Max andalso v > a) orelse (mode=Min andalso v < b)) then d else bestNextDest

                                            in
                                                if(a_ >= b_) then
                                                        (bestNextIdx_, bestNextDest_, v, plr)
                                                else
                                                        iter ls bestNextIdx_ bestNextDest_ a_ b_
                                            end

                                        val validMoves' = getAllValidMoves board plr
                                in
                                    if(null validMoves') then
                                        minimax board idx (0-plr) (depth-1) (changeMode mode) a' b'
                                    else
                                        iter validMoves' 0 [] a' b'
                                end
                        in
                            mnm' depth a b
                        end
                in
                    if(null validMoves) then
                            (~1, [])
                    else if((List.length validMoves) = 1) then
                            (List.hd validMoves)
                    else
                        let
                            val (candidateMove, candidateDest, _, plr') = minimax board lastMove crtPlr maxDepth Max MIN_REAL MAX_REAL

                            val nextMove = if(lastMove=candidateMove orelse crtPlr<>plr') then ~2 else candidateMove

                            val nextDest = if(lastMove=candidateMove orelse crtPlr<>plr') then [] else candidateDest
                        in
                            (nextMove, nextDest)
                        end
                end


            (* strBoard board player lastMove
            TYPE: int list -> int -> int -> string
            PRE:  true
            POST: a string describing the board
            *)
            fun strBoard (board: int list) (plr: int) (lastMove: int) = 
                let
                    val b = "----1--2--3--4--5--6--7--8\n"
                    fun strBoard' i [] = "\n"
                      | strBoard' i (x::xs) = 
                        let
                                val x' = case x of
                                      0  => "  -"
                                    | 1  => "  X"
                                    | _  => "  o"  

                                val be = if (i mod 8 = 0) then (Int.toString ((i div 8) + 1)) ^ "|" else ""
                                val en = if( (i+1) mod 8 = 0) then "\n" else ""
                        in
                                be ^ x' ^ en ^ (strBoard' (i+1) xs)
                        end

                    val pl = if(plr=1) then "Black 1 " else "White ~1 "
                    val made = "made move -> "
                    val move = Int.toString lastMove
                    val y = Int.toString (lastMove div 8 + 1)
                    val x = Int.toString (lastMove mod 8 + 1)
                in
                    "\n" ^ pl ^ made ^ move ^ " | " ^ y ^ " " ^ x ^ "\n" ^ b ^ (strBoard' 0 board) ^ "\n" ^ "\n\n"
                end

            val prevMove = case lastMove of Pass => ~3 | (Move x) => x

            val updatedBoard = 
                    case lastMove of 
                        Pass => board
                      | (Move x) => 
                            let
                                val (_, listDest) = isValidMove board x (0-plr)
                                val board' = update board x listDest (0-plr)
                            in
                                board'
                            end

            val (candMove, candListDest) = getNextMove updatedBoard prevMove plr MAX_DEPTH

            val newMove = if(candMove < 0) then Pass else (Move candMove)

            val newBoard = if(candMove < 0) then updatedBoard else (update updatedBoard candMove candListDest plr)
        in
            (newMove, (plr, newBoard))
        end

end;
(************************************************************************************************************************************************)