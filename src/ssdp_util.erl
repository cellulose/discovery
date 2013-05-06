%% Copyright 2010 Ulf
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%% -------------------------------------------------------------------
%%% Author  : Ulf uaforum1@googlemail.com
%%% Description :
%%% was em_util
%%% Created : 
%%% -------------------------------------------------------------------
-module(ssdp_util).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-import(random).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([v4/0, to_string/1, get_parts/1, v4_as_string/0]).
-export([startsWith/2]).

v4() ->
    v4(random:uniform(trunc(math:pow(2, 48))) - 1, 
       random:uniform(trunc(math:pow(2, 12))) - 1, 
       random:uniform(trunc(math:pow(2, 32))) - 1, 
       random:uniform(trunc(math:pow(2, 30))) - 1).

v4(R1, R2, R3, R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.

to_string(U) ->
    lists:flatten(io_lib:format(
        "~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", 
        get_parts(U))).

v4_as_string() ->
    to_string(v4()).

get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
    [TL, TM, THV, CSR, CSL, N].

startsWith(Original, Substr) ->
    Len = string:len(Substr),
    StartStr = string:substr(Original, 1, Len),
    string:equal(StartStr, Substr).
    
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
startsWith_test() ->
    ?assertEqual(true, startsWith("M-Search sdsadsadsadsad", "M-Search")),
    ?assertEqual(false, startsWith("M-Search sdsadsadsadsad", "Fault")).
-endif.