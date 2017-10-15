%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 十月 2017 21:08
%%%-------------------------------------------------------------------
-module(t_pg_protocol).
-author("simon").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).




out_2_model_test() ->
  PL = [
    {<<"accessType">>, <<"0">>}
    , {<<"bizType">>, <<"000201">>}
    , {<<"certId">>, <<"9">>}
    , {<<"currencyCode">>, <<"156">>}
    , {<<"encoding">>, <<"UTF-8">>}
    , {<<"merId">>, <<"012345678901234">>}
    , {<<"orderId">>, <<"20161111">>}
    , {<<"queryId">>, <<"20162222">>}
    , {<<"reqReserved">>, <<"reqR">>}
    , {<<"respCode">>, <<"00">>}
    , {<<"respMsg">>, <<"success">>}
    , {<<"settleAmt">>, <<"100">>}
    , {<<"settleCurrencyCode">>, <<"156">>}
    , {<<"settleDate">>, <<"20163333">>}
    , {<<"signature">>, <<"sig">>}
    , {<<"signMethod">>, <<"01">>}
    , {<<"traceNo">>, <<"22222">>}
    , {<<"traceTime">>, <<"101010">>}
    , {<<"txnAmt">>, <<"100">>}
    , {<<"txnSubType">>, <<"01">>}
    , {<<"txnTime">>, <<"19991212121212">>}
    , {<<"txnType">>, <<"01">>}
    , {<<"version">>, <<"5.0.0">>}
    , {<<"reserved">>, <<"reserved">>}
    , {<<"accNo">>, <<"95555">>}


    %% extra null fields
    , {<<"Fld1">>, <<"aaa">>}
    , {<<"Fld2">>, <<"aaa">>}
  ],

  ExcludeFields = [<<"Fld1">>, <<"Fld2">>],
  R = [{binary_to_atom(KeyBin, utf8), Value} || {KeyBin, Value} <- PL, not  lists:member(KeyBin, ExcludeFields)],

  M = pg_test_utils:name(protocol),

  ModelUpRespPay = pg_protocol:out_2_in(M, PL),


  ModelRaw = pg_model:new(M, R),


  ModelExpected = pg_model:set(M, ModelRaw, [{txnAmt, 100}, {settleAmt, 100}]),

  ?assertEqual(ModelUpRespPay, ModelExpected),
  ok.
