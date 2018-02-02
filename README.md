# t2j
Convert tuples to json with ease with this Erlang module. 
Just drop the t2j.erl file in your src folder and you're good to go.

Use tuples of atoms, strings, integers, floats and arrays, and convert them to json. 

sample: 
>T = {"K",{{key, value},{key2,{{key3, value3}, {key4, value4}, {key5, 5.333}}}}}.

>t2j:t2jp(T).

{"K":{"key":"value","key2":{"key3":"value3","key4":"value4","key5":5.333}}}

or

>t2j:t2j(T).

"{\"K\":{\"key\":\"value\",\"key2\":{\"key3\":\"value3\",\"key4\":\"value4\",\"key5\":5.333}}}"

and with arrays

>T2 = {"K",{{key, value},{key2,["a","b","c",4]}}}

>>t2j:t2jp(T2).

{"K":{"key":"value","key2":["a""b""c"4]}}


todo: boolean, null
