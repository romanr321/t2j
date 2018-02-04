# t2j
This simple Erlang module converts Erlang terms to json. 

Just drop the t2j.erl file in your src folder and you're good to go.

supports: atoms, booleans, null, strings, integers, floats, binary and arrays.

atom, string -> "string"

boolean, null -> true|false|null

int, float -> number

binary -> "string"

list -> array

tuple -> object


sample: 
>T = {"K",{{key, value},{key2,[{key3, true}, {key4, value4}, {key5, 5.333}]}}}.

>t2j:t2jp(T).

output
{"K":{"key":"value","key2":["key3":true,"key4":"value4","key5":5.333]}}

>t2j:t2j(T).

output
"{\"K\":{\"key\":\"value\",\"key2\":[\"key3\":true,\"key4\":\"value4\",\"key5\":5.333]}}"

>T2 = {"K",{{key, <<"value">>},{key2,["a",null,"c",4]}}}.

>t2j:t2jp(T2).

output
{"K":{"key":"value","key2":["a",null,"c",4]}}
