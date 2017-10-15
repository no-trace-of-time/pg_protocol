-module(pg_protocol).
-include_lib("eunit/include/eunit.hrl").

%% callbacks
-callback in_2_out_map() -> map().
%% API exports
-export([
  out_2_in/2
]).

%%====================================================================
%% API functions
%%====================================================================
-spec out_2_in(M, PL) -> Model when
  M :: atom(),
  PL :: proplists:proplist(),
  Model :: atom().

%% 用途：将外部数据转换为内部格式
%% 实现：M： 可以得到对应的字段列表
%%          然后获取转换配置,model_2_out_map(Type)
%%
out_2_in(M, PL) when is_atom(M), is_list(PL) ->
%%  Fields = get_out_fields(M),
  Fields = pg_model:fields(M),
  Model2OutMap = M:in_2_out_map(),

  F = fun out_2_in_one_field/2,
  {VL, _, _} = lists:foldl(F, {[], Model2OutMap, PL}, Fields),
  pg_model:new(M, VL).


%%====================================================================
%% Internal functions
%%====================================================================
get_out_fields(M) when is_atom(M) ->
  [signature | M: sign_fields()].

out_2_in_one_field(Field, {Acc, Model2OutMap, PL}) when is_atom(Field), is_list(Acc), is_map(Model2OutMap) ->
  Config = maps:get(Field, Model2OutMap),

%%  lager:debug("Config=~p,Field=~p", [Config, Field]),

  Value = do_out_2_in_one_field(Config, PL),
  %% omit undefined key/value , which means not appear in PL
  case Value of
    undefined ->
      {Acc, Model2OutMap, PL};
    Value ->
      AccNew = [{Field, Value} | Acc],
      {AccNew, Model2OutMap, PL}
  end.

do_out_2_in_one_field({KeyInPLList, tuple}, PL) when is_list(KeyInPLList), is_list(PL) ->
  VL = [proplists:get_value(Key, PL) || Key <- KeyInPLList],
  list_to_tuple(VL);
do_out_2_in_one_field({KeyInPL, integer}, PL) when is_binary(KeyInPL), is_list(PL) ->
  Value = proplists:get_value(KeyInPL, PL),
  case Value of
    undefined ->
      undefined;
    Value ->
      binary_to_integer(Value)
  end;
do_out_2_in_one_field(KeyInPL, PL) when is_binary(KeyInPL), is_list(PL) ->
  proplists:get_value(KeyInPL, PL).

%%====================================================================
%% Internal Test functions
%%====================================================================
do_out_2_model_one_field_test() ->
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
  ],
  ?assertEqual(do_out_2_in_one_field(<<"accessType">>, PL), <<"0">>),
  ?assertEqual(do_out_2_in_one_field({<<"accessType">>, integer}, PL), 0),
  ?assertEqual(do_out_2_in_one_field({[<<"accessType">>], tuple}, PL), {<<"0">>}),
  ?assertEqual(do_out_2_in_one_field({[<<"accessType">>, <<"bizType">>], tuple}, PL), {<<"0">>, <<"000201">>}),

  ok.
