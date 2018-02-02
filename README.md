# t2j
Convert tuples to json with ease with this Erlang module. 
Just drop the t2j.erl file in your src folder and you're good to go.

Use tuples of atoms, strings, integers and floats and convert them to json. 

sample: 
>T = {"K",{{key, value},{key2,{{key3, value3}, {key4, value4}, {key5, 5.333}}}}}.

>t2j:t2jp(T).

{"K":{{{"key":"value"}},{{"key2":{{{"key3":"value3"}},{{"key4":"value4"}},{{"key5":5.333}}}}}

or

>t2j:t2j(T).

"{\"K\":{{{\"key\":\"value\"}},{{\"key2\":{{{\"key3\":\"value3\"}},{{\"key4\":\"value4\"}},{{\"key5\":5.333}}}}}"


