-module(pg_protocol).
-include_lib("eunit/include/eunit.hrl").
-behavior(pg_model).

%% callbacks
-callback in_2_out_map() -> map().

%% API exports
-export([
  out_2_in/2
  , in_2_out/2
  , in_2_out/3
]).

-export([

]).

-define(TEST_PROTOCOL, pg_test_utils:name(protocol)).

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

%%----------------------------------------------
-spec in_2_out(M, Model, Format) -> WireFormat when
  M :: atom(),
  Model :: tuple(),
  Format :: proplists | binary |string,
  WireFormat :: proplists:proplist() | binary().

in_2_out(M, Model, proplists) ->
  do_in_2_out(M, Model);
in_2_out(M, Model, binary) ->
  VL = do_in_2_out(M, Model),
  xfutils:post_vals_to_iolist(VL);
in_2_out(M, Model, string) ->
  in_2_out(M, Model, binary).

in_2_out(M, Model) ->
  in_2_out(M, Model, proplists).

do_in_2_out(M, Model) when is_atom(M), is_tuple(Model) ->
  [in_2_out_one_field(M, Model, Field) || Field <- pg_model:fields(M)].


%%====================================================================
%% Internal functions
%%====================================================================
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


%%----------------------------------------------------------
field_config(M, Field) when is_atom(M), is_atom(Field) ->
  ConfigMap = M:in_2_out_map(),
  maps:get(Field, ConfigMap).

in_2_out_one_field(M, Model, Field) when is_atom(M), is_tuple(Model), is_atom(Field) ->
  Config = field_config(M, Field),
  do_in_2_out_one_field(M, Model, {Field, Config}).

do_in_2_out_one_field(M, Model, {Field, FieldInPVBIN}) when is_atom(Field), is_binary(FieldInPVBIN) ->
  Value = pg_model:get(M, Model, Field),
  {FieldInPVBIN, Value};
do_in_2_out_one_field(M, Model, {Field, {FieldInPVBIN, integer}}) when is_atom(Field), is_binary(FieldInPVBIN) ->
  ValueInteger = pg_model:get(M, Model, Field),
  ValueBin = integer_to_binary(ValueInteger),
  {FieldInPVBIN, ValueBin}.

do_out_2_in_one_filed_test() ->
  M = ?TEST_PROTOCOL,
  Protocol = pg_model:new(M, [{txnAmt, 9997}, {settleAmt, 9998}, {txnType, <<"01">>}]),

  ?assertEqual({<<"txnAmt">>, <<"9997">>}, in_2_out_one_field(M, Protocol, txnAmt)),
  ?assertEqual({<<"settleAmt">>, <<"9998">>}, in_2_out_one_field(M, Protocol, settleAmt)),
  ?assertEqual({<<"txnType">>, <<"01">>}, in_2_out_one_field(M, Protocol, txnType)),

  ok.

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
