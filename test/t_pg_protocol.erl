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



-define(TEST_PROTOCOL, pg_test_utils:name(protocol)).

postvals() ->
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
  ].

out_2_in_test() ->
  PL = postvals(),

  ExcludeFields = [<<"Fld1">>, <<"Fld2">>],
  R = [{binary_to_atom(KeyBin, utf8), Value} || {KeyBin, Value} <- PL, not  lists:member(KeyBin, ExcludeFields)],

  M = ?TEST_PROTOCOL,

  ModelUpRespPay = pg_protocol:out_2_in(M, PL),


  ModelRaw = pg_model:new(M, R),


  ModelExpected = pg_model:set(M, ModelRaw, [{txnAmt, 100}, {settleAmt, 100}]),

  ?assertEqual(ModelUpRespPay, ModelExpected),
  ok.

%%--------------------------------------------------------------------

in_2_out_test() ->
  M = ?TEST_PROTOCOL,
  PL = postvals(),

  Protocol = pg_protocol:out_2_in(M, PL),

  PLOut = [
    {<<"version">>, <<"5.0.0">>},
    {<<"encoding">>, <<"UTF-8">>},
    {<<"certId">>, <<"9">>},
    {<<"signature">>, <<"sig">>},
    {<<"signMethod">>, <<"01">>},
    {<<"txnType">>, <<"01">>},
    {<<"txnSubType">>, <<"01">>},
    {<<"bizType">>, <<"000201">>},
    {<<"accessType">>, <<"0">>},
    {<<"merId">>, <<"012345678901234">>},
    {<<"orderId">>, <<"20161111">>},
    {<<"txnTime">>, <<"19991212121212">>},
    {<<"txnAmt">>, <<"100">>},
    {<<"currencyCode">>, <<"156">>},
    {<<"reqReserved">>, <<"reqR">>},
    {<<"reserved">>, <<"reserved">>},
    {<<"accNo">>, <<"95555">>},
    {<<"queryId">>, <<"20162222">>},
    {<<"respCode">>, <<"00">>},
    {<<"respMsg">>, <<"success">>},
    {<<"settleAmt">>, <<"100">>},
    {<<"settleCurrencyCode">>, <<"156">>},
    {<<"settleDate">>, <<"20163333">>},
    {<<"traceNo">>, <<"22222">>},
    {<<"traceTime">>, <<"101010">>},
    {<<"exchangeRate">>, undefined}
  ],

  ?assertEqual(PLOut, pg_protocol:in_2_out(M, Protocol)),

  PLOutString = <<"version=5.0.0&encoding=UTF-8&certId=9&signature=sig&signMethod=01&txnType=01&txnSubType=01&bizType=000201&accessType=0&merId=012345678901234&orderId=20161111&txnTime=19991212121212&txnAmt=100&currencyCode=156&reqReserved=reqR&reserved=reserved&accNo=95555&queryId=20162222&respCode=00&respMsg=success&settleAmt=100&settleCurrencyCode=156&settleDate=20163333&traceNo=22222&traceTime=101010">>,
  ?assertEqual(PLOutString, list_to_binary(pg_protocol:in_2_out(M, Protocol, binary))),
  ?assertEqual(PLOutString, list_to_binary(pg_protocol:in_2_out(M, Protocol, string))),

  ok.


