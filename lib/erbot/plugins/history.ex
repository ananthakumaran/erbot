defmodule Erbot.Plugins.History do
  use GenEvent.Behaviour

  defrecord State, client: nil, dbpath: nil
  defrecord Message, timestamp: nil, nick: nil, channel: nil, message: nil

  @table :history
  @max_results 50

  def init([client, [{:dbpath, path}]]) do
    self() <- :start
    {:ok, State.new([client: client, dbpath: path])}
  end

  def handle_event({:private_msg, nick, "!log" <> query}, s) do
    reply = fn(message) -> Erbot.Irc.send_message(s.client, nick, message) end
    safe_search(query, reply)
    {:ok, s}
  end

  def handle_event({:private_msg, nick, message}, s) do
    log(nick, "#private", message)
    {:ok, s}
  end

  def handle_event({:channel_msg, {_nick, channel}, "!log" <> query}, s) do
    reply = fn(message) -> Erbot.Irc.send_message(s.client, channel, message) end
    safe_search(query, reply)
    {:ok, s}
  end

  def handle_event({:channel_msg, {nick, channel}, message}, s) do
    log(nick, channel, message)
    {:ok, s}
  end

  def handle_event(_, s) do
    {:ok, s}
  end

  def handle_info(:start, state) do
    initialize_db(state.dbpath)
    {:ok, state}
  end

  def handle_info(_, state) do
    {:ok, state}
  end

  def initialize_db(path) do
    :history = :lets.new(@table, [:ordered_set, :public, :named_table, {:keypos, 2},{:db, [{:path, path}, :create_if_missing]}])
    :ok
  end


  def string_join([], _seperator) do
    ""
  end
  def string_join([h], _seperator) do
    h
  end
  def string_join([f|[s|r]], seperator) do
    string_join([f <> seperator <> s | r], seperator)
  end


  def fields() do
    [{:timestamp, :'$1'},
    {:nick, :'$2'},
    {:channel, :'$3'},
    {:message, :'$4'}]
  end

  def field_to_var(field) do
    {field, :proplists.get_value(field, fields())}
  end

  def predicate(p) do
    binary_to_atom(p)
  end

  def build_condition(condition) do
    [field | [predicate | value]] = String.split(condition)
    case field_to_var(binary_to_atom(field)) do
      {f, :undefined} -> :erlang.error("unknown field " <> f)
      {:timestamp, var} -> {predicate(predicate), var, Erbot.Utils.triple_to_timestamp(:gate.approxidate(string_join(value, " ")))}
      {_, var} -> {predicate(predicate), var, string_join(value, " ")}
    end
  end

  def build_query(query) do
    try do
      conditions = query
      |> String.split(",")
      |> Enum.filter(&(&1 != ""))
      |> Enum.map(&build_condition/1)
      [{Message.new([timestamp: :'$1', nick: :'$2', channel: :'$3', message: :'$4']), conditions, [:'$_']}]
    catch
      _exception, reason -> {:error, reason}
    end
  end

  def format_time(timestamp) do
      {{y, m, d}, {h, min, _s}} = :calendar.now_to_datetime(Erbot.Utils.timestamp_to_triple(timestamp))
      to_string(:io_lib.format("~2..0b/~2..0b/~4..0b ~2..0b:~2..0b", [d, m, y, h, min]))
  end

  def format_message(Message[timestamp: timestamp, nick: nick, channel: channel, message: message]) do
      to_string(:io_lib.format("~s: ~s in ~s ~s", [format_time(timestamp), nick, channel, message]))
  end

  def render([]), do: "no results"
  def render(results) when length(results) <= 5 do
    string_join(Enum.map(results, &format_message/1), "\n")
  end

  def render(results) do
    integer_to_binary(length(results)) <> " results\n" <>
    Erbot.Utils.gist(string_join(Enum.map(results, &format_message/1), "\n"))
  end

  def search(query, reply) do
    case build_query(query) do
      {:error, reason} -> help(reply, inspect({:error, reason}))
      # TODO validate matchspec. this will throw exception on bad spec
      # don't call it directly. use safe_search
      match_spec ->
        case :lets.select_reverse(@table, match_spec, @max_results) do
          {:error, reason} -> reply.(inspect({:error, reason}))
          :'$end_of_table' -> reply.(render([]))
          {results, _continuation} -> reply.(render(results))
        end
    end
  end

  def safe_search(query, reply) do
    try do
      search(query, reply)
    catch
      _exception, reason  -> reply.(inspect({:error, reason}))
    end
  end


  def log(nick, channel, message) do
    IO.puts("log message #{nick} #{channel} #{message}")
    true = :lets.insert(@table, [Message.new([timestamp: Erbot.Utils.triple_to_timestamp(:erlang.now()), nick: nick, channel: channel, message: message])])
    :ok
  end


  def help(reply) do
    reply.("\neg !log channel == #activesphere\neg !log timestamp >= yesterday, channel == #activesphere\navailable fields timestamp, channel, nick, message. by default will list the last 500 messages in desc order")
  end

  def help(reply, error) do
    reply.(error)
    help(reply)
  end


  # test
  def std(text) do
    IO.puts(text)
  end

  def search(query) do
    search(query, &std/1)
  end
end
