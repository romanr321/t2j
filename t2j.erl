%%-------------------------------------------------------------------
% @copyright 2018 
% @author Roman Rabinovich <find.roman@gmail.com>
% @doc Tuples to JSON converter 
% takes tuples and tuples of tuples and converts to json format. 
% Tuples can contain atoms, strings, integers and floats.
% @end
%%-------------------------------------------------------------------


-module(t2j).
-compile(export_all).


% @doc t2jp/1 is pretty t2j, uses io:format to improve readability of t2j result.
% Sample tuple: T = {"K",{{key, value},{key2,{{key3, value3}, {key4, value4}, {key5, 5.333}}}}}
% Sample tuple: T2 = {"K",{{key, value},{key2,["a","b","c",4]}}}

% @end

-spec t2jp(tuple()) -> string().

t2jp(T) ->
	io:format("~s~n",[t2j(T)]).


-spec t2j(tuple()) -> string().

t2j(T) ->
	"{"++t2j2(T)++"}".

t2j2(T) ->

 	E = element(1,T),
	case is_tuple(E) of
		true -> J1 = t2j2(E),
				T2 = list_to_tuple(tl(tuple_to_list(T))),
				case size(T2) of
					0 -> J1;
					_ -> J1 ++ "," ++ t2j2(T2)
				end;
		false -> 
			case is_atom(E) of
				true -> J1 = "\"" ++ atom_to_list(E) ++"\":";
				_ -> J1 =  "\"" ++ E ++"\":"
		 	end,
		 	E2 = element(2,T),
		 	case is_tuple(E2) of
		 		true -> J2 = "{"++ t2j2(E2) ++ "}",
		 				T2 = list_to_tuple(tl(tl(tuple_to_list(T)))),
						case size(T2) of
							0 -> J1 ++ J2;
							_ -> J1 ++ J2 ++ "," ++ t2j2(T2)
						end;
		 		false ->
		 			case is_number(E2) of
		 				true -> case is_integer(E2) of
		 							true -> J2 = integer_to_list(E2) ;
		 							false -> J2 = float_to_list(E2,[{decimals, 10}, compact])
		 						end;
		 				false ->
		 					case is_atom(E2) of
								true -> J2 = "\"" ++ atom_to_list(E2) ++"\"";
								_ -> case char_list(E2) of
										true -> J2 = "\"" ++ E2 ++"\"";
										false -> J2 = "[" ++ json_array(E2) ++ "]"
									 end
							end
					end,
					T2 = list_to_tuple(tl(tl(tuple_to_list(T)))),
					case size(T2) of
						0 -> J1++J2;
						_ -> J1 ++ J2 ++ "," ++ t2j2(T2)
					end
		 	end
	end.
	
json_array([H|T]) ->
	case is_number(H) of
		 true -> case is_integer(H) of
		 			true -> S = integer_to_list(H);
		 			false -> S = float_to_list(H,[{decimals, 10}, compact])
		 		end;
		 false ->
		 case is_atom(H) of
				true -> S = "\"" ++ atom_to_list(H) ++"\"";
				_ -> case char_list(H) of
						true -> S = "\"" ++ H ++"\"";
						false -> S = "[" ++ json_array(H) ++ "]"
					end
		end
	end,

	case T of
		[] -> S;
		_ -> S ++ json_array(T)
	end.


	

char_list([C|Cs]) when is_integer(C), C >= 0, C < 16#D800;
       is_integer(C), C > 16#DFFF, C < 16#FFFE;
       is_integer(C), C > 16#FFFF, C =< 16#10FFFF ->
    char_list(Cs);
char_list([]) -> true;
char_list(_) -> false.

	

