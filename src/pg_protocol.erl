-module(pg_protocol).
-include_lib("eunit/include/eunit.hrl").
-behavior(pg_model).

-type rule_tag() :: atom().
-type from_model_name() :: atom().
-type result_key() :: atom().
-type from_key() :: atom().
-type convert_action() :: from_key()
| {static, any()}
| {element, non_neg_integer(), from_key()}
| {fun(), []}
| {fun(), [from_key()]}
| {tuple, [from_key()]}.
-type op_step() :: {result_key(), convert_action()}.
-type op_steps() :: [op_step()].
-type convert_op() :: {from_model_name(), op_steps()}.
-type convert_ops() :: [convert_op()].
-type convert_rule() :: {rule_tag(), convert_ops()}.
-type convert_rules() :: [convert_rule()].

-export_type([
  convert_rules/0
]).

%% callbacks
-callback in_2_out_map() -> map().
-callback convert_config() -> convert_rules().

%% API exports
-export([
  out_2_in/2
  , in_2_out/2
  , in_2_out/3
  , convert/2
  , convert/3
]).

-export([
  pr_formatter/1
]).

-define(TEST_PROTOCOL, pg_protocol_t_protocol_up_resp_pay).

%%====================================================================
%% API functions
%%====================================================================
pr_formatter(_) ->
  default.

%%-----------------------------------------------------------------
-spec out_2_in(M, PL) -> Model when
  M :: atom(),
  PL :: proplists:proplist(),
  Model :: pg_model:pg_model().

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


%%----------------------------------------------
-spec convert(MTo, MFrom) -> ModelNew when
  MTo :: atom(),
  MFrom :: pg_model:pg_model() | [pg_model:pg_model()],
  ModelNew :: pg_model:pg_model().
convert(MTo, MFrom) when is_atom(MTo), is_tuple(MFrom) ->
  convert(MTo, [MFrom], default);
convert(MTo, MFromList) when is_atom(MTo), is_list(MFromList) ->
  convert(MTo, MFromList, default).


-spec convert(MTo, MFromList, ConfigItemName) -> ModelNew when
  MTo :: atom(),
  MFromList :: [pg_model:pg_model()],
  ConfigItemName :: atom(),
  ModelNew :: pg_model:pg_model().

convert(MTo, Model, ConfigItemName) when is_atom(MTo), is_tuple(Model), is_atom(ConfigItemName) ->
  convert(MTo, [Model], ConfigItemName);
convert(MTo, ModelList, ConfigItemName) when is_atom(MTo), is_list(ModelList), is_atom(ConfigItemName) ->
  ConvertRuleMap = MTo:convert_config(),
  RuleList = proplists:get_value(ConfigItemName, ConvertRuleMap),

  true = length(ModelList) =:= length(RuleList),

  FConvertOneModel =
    fun(I, Acc) ->
      {MFrom, Rule} = lists:nth(I, RuleList),
      VL = do_convert(MFrom, Rule, lists:nth(I, ModelList)),
      VL ++ Acc
    end,

  VL = lists:foldl(FConvertOneModel, [], lists:seq(1, length(ModelList))),


  pg_model:new(MTo, VL).


%%====================================================================
%% Internal functions
%%====================================================================
out_2_in_one_field(Field, {Acc, Model2OutMap, PL}) when is_atom(Field), is_list(Acc), is_map(Model2OutMap) ->
  Config = maps:get(Field, Model2OutMap, undefined),

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
  proplists:get_value(KeyInPL, PL);
do_out_2_in_one_field(undefined, PL) when is_list(PL) ->
  %% protocol model key is not in in_2_out_map/0
  %% set as undefined
  undefined.


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

%%-------------------------------------------------------------------
do_convert(MFrom, Rule, Model) when is_atom(MFrom), is_list(Rule), is_tuple(Model) ->
  F =
    fun(OpTuple, Acc) ->
      do_convert_one_op(MFrom, Model, OpTuple, Acc)
    end,
  VL = lists:foldl(F, [], Rule),
  VL.

do_convert_test() ->
  Protocol = pg_model:new(?TEST_PROTOCOL, [{encoding, {a, b, c}}]),

  R1 = [
    {t1, version}
    , {t2, {static, 333}}
    , {t3, {element, 3, encoding}}
    , {t4, {fun t1/0, []}}
    , {t5, {fun t3/1, [version]}}
    , {t6, {tuple, [encoding, version]}}
  ],
  ?assertEqual(
    lists:reverse([
      {t1, <<"5.0.0">>}
      , {t2, 333}
      , {t3, c}
      , {t4, <<"test">>}
      , {t5, <<"5.0.0hello">>}
      , {t6, {{a, b, c}, <<"5.0.0">>}}
    ]),
    do_convert(?TEST_PROTOCOL, R1, Protocol)),
  ok.


do_convert_one_op(M, Model, {KeyTo, KeyFrom}, AccIn)
  when is_atom(M), is_atom(KeyTo), is_atom(KeyFrom), is_list(AccIn) ->
  Value = pg_model:get(M, Model, KeyFrom),
  [{KeyTo, Value} | AccIn];
do_convert_one_op(M, _Model, {KeyTo, {static, Value}}, AccIn)
  when is_atom(M), is_atom(KeyTo), is_list(AccIn) ->
  [{KeyTo, Value} | AccIn];
do_convert_one_op(M, Model, {KeyTo, {element, N, KeyFrom}}, AccIn)
  when is_atom(M), is_atom(KeyTo), is_list(AccIn) ->
  Value = pg_model:get(M, Model, KeyFrom),
  [{KeyTo, element(N, Value)} | AccIn];
do_convert_one_op(M, _Model, {KeyTo, {Fun, []}}, AccIn)
  when is_atom(M), is_atom(KeyTo), is_function(Fun), is_list(AccIn) ->
  Value = Fun(),
  [{KeyTo, Value} | AccIn];
do_convert_one_op(M, Model, {KeyTo, {Fun, KeyFromList}}, AccIn)
  when is_atom(M), is_atom(KeyTo), is_function(Fun), is_list(KeyFromList), is_list(AccIn) ->
  VL = [pg_model:get(M, Model, Key) || Key <- KeyFromList],
  Value = apply(Fun, VL),
  [{KeyTo, Value} | AccIn];
do_convert_one_op(M, Model, {KeyTo, {tuple, KeyFromList}}, AccIn)
  when is_atom(M), is_atom(KeyTo), is_list(KeyFromList), is_list(AccIn) ->
  VL = [pg_model:get(M, Model, Key) || Key <- KeyFromList],
  [{KeyTo, list_to_tuple(VL)} | AccIn].

do_convert_one_op_test() ->
  Protocol = pg_model:new(?TEST_PROTOCOL, [{encoding, {a, b, c}}]),
  ?assertEqual([{test, <<"5.0.0">>}, aa],
    do_convert_one_op(?TEST_PROTOCOL, Protocol, {test, version}, [aa])),

  ?assertEqual([{test, 444}, aa],
    do_convert_one_op(?TEST_PROTOCOL, Protocol, {test, {static, 444}}, [aa])),

  ?assertEqual([{test, a}, aa],
    do_convert_one_op(?TEST_PROTOCOL, Protocol, {test, {element, 1, encoding}}, [aa])),
  ?assertEqual([{test, c}, aa],
    do_convert_one_op(?TEST_PROTOCOL, Protocol, {test, {element, 3, encoding}}, [aa])),

  ?assertEqual([{test, <<"test">>}, aa],
    do_convert_one_op(?TEST_PROTOCOL, Protocol, {test, {fun t1/0, []}}, [aa])),

  ?assertEqual([{test, <<"5.0.0hello">>}, aa],
    do_convert_one_op(?TEST_PROTOCOL, Protocol, {test, {fun t3/1, [version]}}, [aa])),

  ?assertEqual([{test, {<<"5.0.0">>, {a, b, c}}}, aa],
    do_convert_one_op(?TEST_PROTOCOL, Protocol, {test, {tuple, [version, encoding]}}, [aa])),
  ok.

t1() ->
  <<"test">>.

t3(V) ->
  <<V/binary, "hello">>.

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
  ?assertEqual(do_out_2_in_one_field(undefined, PL), undefined),
  ?assertEqual(do_out_2_in_one_field({<<"accessType">>, integer}, PL), 0),
  ?assertEqual(do_out_2_in_one_field({[<<"accessType">>], tuple}, PL), {<<"0">>}),
  ?assertEqual(do_out_2_in_one_field({[<<"accessType">>, <<"bizType">>], tuple}, PL), {<<"0">>, <<"000201">>}),

  ok.


