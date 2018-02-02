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
% @end

-spec t2jp(tuple()) -> string().

t2jp(T) ->
	io:format("~s~n",[t2j(T)]).


-spec t2j(tuple()) -> string().

t2j(T) ->
 	E = element(1,T),
	case is_tuple(E) of
		true -> J1 = "{" ++ t2j(E) ++ "}",
				T2 = list_to_tuple(tl(tuple_to_list(T))),
				case size(T2) of
					0 -> J1;
					_ -> J1 ++ "," ++ t2j(T2)
				end;
		false -> 
			case is_atom(E) of
				true -> J1 = "{\"" ++ atom_to_list(E) ++"\":";
				_ -> J1 =  "{\"" ++ E ++"\":"
		 	end,
		 	E2 = element(2,T),
		 	case is_tuple(E2) of
		 		true -> J2 = "{" ++ t2j(E2)++ "}",
		 				T2 = list_to_tuple(tl(tl(tuple_to_list(T)))),
						case size(T2) of
							0 -> J1 ++ J2;
							_ -> J1 ++ J2 ++ "," ++ t2j(T2)
						end;
		 		false ->
		 			case is_number(E2) of
		 				true -> case is_integer(E2) of
		 							true -> J2 = integer_to_list(E2) ++ "}";
		 							false -> J2 = float_to_list(E2,[{decimals, 10}, compact]) ++ "}"
		 						end;
		 				false ->
		 					case is_atom(E2) of
								true -> J2 = "\"" ++ atom_to_list(E2) ++"\"}";
								_ -> J2 = "\"" ++ E2 ++"\"}"
							end
					end,
					T2 = list_to_tuple(tl(tl(tuple_to_list(T)))),
					case size(T2) of
						0 -> J1++J2;
						_ -> J1 ++ J2 ++ "," ++ t2j(T2)
					end
		 	end
	end.
	

	

		

	

