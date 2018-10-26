val listRandom = [4, 5, 4, 5, 5, 4, 5, 5, 5, 2, 3, 1, 2, 5, 2, 5, 1, 5, 3, 5, 5, 5, 4, 5, 4, 5, 5, 4, 1, 4, 3, 2, 3, 5, 2, 5, 5, 5, 5, 5, 1,4, 4, 4, 5, 3, 5, 1, 3, 5, 3, 1, 5, 4, 4, 5, 2, 4, 4, 4, 1, 5, 1, 4, 1, 3]


(*
datatype player = Black | White;
datatype move = Pass | Move of int; (* 0, 1, 2 -> 63*)
*)

(*
1: Black
0: None
-1: White
*)

structure Reversi_AI =
struct
	type T = int * (int list)
    
	val originalBoard = [	0, 0, 0, 0, 0, 0, 0, 0,
							0, 0, 0, 0, 0, 0, 0, 0,
							0, 0, 0, 0, 0, 0, 0, 0,
							0, 0, 0,~1, 1, 0, 0, 0,
							0, 0, 0, 1,~1, 0, 0, 0,
							0, 0, 0, 0, 0, 0, 0, 0,
							0, 0, 0, 0, 0, 0, 0, 0,
							0, 0, 0, 0, 0, 0, 0, 0 ]

    val MAX_REAL =  1000000.0
    val MIN_REAL = ~1000000.0 


    val author = "Sy-Hung Doan"
    val nickname = "bot"
    fun init (p: player) =
            case p of 
              	Black => (1, originalBoard)
			  | White =>  (~1, originalBoard)


	fun think ((p, l), (m: move), (t: Time.time)) = 
		let
			fun isValidMove (l: int list) (i: int) (p: int) = 
				let
					val directions = [(~1, ~1), (~1, 0), (~1, 1), (0, ~1), (0, 1), (1, ~1), (1, 0), (1, 1)]

					fun getNodesByDirection (i: int) (dy: int) (dx: int) =
						let
							val y = i div 8
							val x = i mod 8
						
							fun isInbound (y: int) (x: int) = (y>=0 andalso y<=7) andalso (x>=0 andalso x<=7);

							fun getNodesByDirection' (y: int) (x: int) acc =
								if(isInbound y x) then
									getNodesByDirection' (y+dy) (x+dx) ((8*y+x)::acc)
								else
									acc
								
							(*Chuyen vao if else de tiet kiem thoi gian*)
							(* val listNodes = getNodesByDirection' y x [] *)

							(* val len = List.length listNodes  *)
							(*if len < 3 -> return []*)

						in
							
							if(((y+dy) < 0) orelse ((x+dx) < 0)) then
								[]
							else if ((dy>0) orelse (dy=0 andalso dx=1)) then
								List.rev (getNodesByDirection' y x [])
							else
								(getNodesByDirection' y x [])
							
						end

					fun getValuesFromNodes (ln: int list) (dy: int) (dx: int) = 
						let
							fun getVal _ [] _ acc = acc
							| getVal [] _ _ acc = acc
							| getVal (x::xs) (y::ys) i acc =           
									if (i=y) then
										getVal xs ys (i+1) ((y, x)::acc)
									else
										getVal xs (y::ys) (i+1) acc           

							val listValues = getVal l ln 0 []
						in
							if (null ln) then
								[]
							else if ((dy>0) orelse (dy=0 andalso dx=1)) then
								List.tl (List.rev listValues)
							else
								List.tl (listValues)
						end

					fun checkValues (lv: (int * int) list) =
						let
							fun checkValues' [] (lastIndex: int) (lastValue: int) = (false, ~1)
							| checkValues' ((i, v)::ls) lastIndex lastValue = 
									
									if (v=0) then 
										(false, ~1)
									else if (v+p=0) then
										checkValues' ls i v
									else if (v=p andalso (v+lastValue)=0) then
										(true, lastIndex)
									else
										(false, ~1)
					
						in
							checkValues' lv 0 0
						end

					fun checkDirection dy dx =
						let
							val listNodes = getNodesByDirection i dy dx
							val listValues = getValuesFromNodes listNodes dy dx
							val isValid = checkValues listValues
						in
							isValid
						end


					fun checkAllDirections [] status listDest = (status, listDest)
					| checkAllDirections ((dy, dx)::ds) status listDest = 
							let
								val (r, d) = checkDirection dy dx
							in
								if(r) then
									checkAllDirections ds true (d::listDest)
								else
									checkAllDirections ds status listDest
							end

					val (result, ld) = checkAllDirections directions false []
				in
					if(result) then 
						(true, ld)
					else
						(false, [])
				end
			
			fun getAllValidMoves (l: int list) (p: int) = 
				let
					fun getAllValidMoves' [] i acc = acc
					| getAllValidMoves' (0::ns) i acc = 
							let
								val (r, dest) = isValidMove l i p
							in
								if(r) then
									getAllValidMoves' ns (i+1) ((i, dest)::acc)
								else
									getAllValidMoves' ns (i+1) acc
							end

					| getAllValidMoves' (n::ns) i acc = getAllValidMoves' ns (i+1) acc
							
							
					val listValidMoves = List.rev (getAllValidMoves' l 0 [])
				in
					listValidMoves
				end

			fun getMiddleNodes (from: int) (to: int) = 
				let
					val fromy = from div 8
					val fromx = from mod 8
					val toy = to div 8
					val tox = to mod 8

					val dy = if(fromy = toy) then 0 else if (fromy < toy) then 1 else ~1
					val dx = if(fromx = tox) then 0 else if (fromx < tox) then 1 else ~1

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

			fun update (l: int list) (from: int) (to: int list) (v: int) = 
				let

					fun update' [] _ _ acc = List.rev acc
					| update' l' [] _ acc = (List.rev acc) @ l'
					| update' (n::ns) (x::xs) i acc = 
							if(i=x) then 
								(update' ns xs (i+1) (v::acc)) 
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
					iter l to
				end


			fun getNextMove (l: int list) (i: int) (p: int) (maxd: int) =
                let
                    val validMoves = getAllValidMoves l p
                    val max = List.length validMoves

                    fun get [] _ _ = (~1, [])
                      | get (l::ls) i' idx = if(i'=idx) then l else get ls (i'+1) idx

                    fun getRandomInt (i: int) (m: int) = 
                        let
                            
                            
                            fun get1 [] _ = 0
                            | get1 (l::ls) i' = if(i'=i) then (l mod m) else get1 ls (i'+1)

                        in  
                            get1 listRandom 0
                        end          
                in
                    if(null validMoves) then
                        (~1, [])
                    else if((List.length validMoves) = 1) then
                        (List.hd validMoves)
                    else
                        get validMoves 0 (getRandomInt (i+1) max)
                end


			val oldMove = case m of Pass => ~1 | (Move x) => x
			val updatedBoard = 
					case m of 
					Pass => l 
					| (Move x) => 
						let
							val (_, ld) = isValidMove l x (0-p)
							val nb = update l x ld (0-p)
						in
							nb
						end


			val (move, listDest) = getNextMove updatedBoard oldMove p 6

			val newMove = if(move = ~1) then Pass else (Move move)
			val newBoard = if(move = ~1) then updatedBoard else (update updatedBoard move listDest p)
		in
			(newMove, (p, newBoard))
		end

end;