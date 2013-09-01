defmodule Erbot.Irc do
  use GenServer.Behaviour

  @routine_check 1000 * 60 * 1
  @ping_timeout 60 * 5

  defrecord State, socket: nil, nick: nil, publisher: nil, users: [], last_contact: nil

  def start do
    :gen_server.start(__MODULE__, [], [])
  end

  def start_link do
    :gen_server.start_link(__MODULE__, [], [])
  end

  def stop(pid) do
    :gen_server.call(pid, :quit)
  end

  def init([]) do
    :erlang.register(:erbot, self())
    self() <- :start
    {:ok, State.new([users: :orddict.new(), last_contact: :erlang.now()])}
  end

  def handle_call(:quit, _from, s = State[socket: socket]) do
    :ok = :gen_tcp.close(socket)
    log("Going down")
    {:stop, :normal, :ok, s}
  end

  def handle_cast({:send, reply}, s = State[socket: socket]) do
    :gen_tcp.send(socket, reply)
    log(">>>> " <> reply)
    {:noreply, s}
  end

  def handle_cast(:try_new_nick, s = State[nick: nick]) do
    new_nick = nick <> "_"
    register_nick(new_nick)
    {:noreply, s.nick(new_nick)}
  end

  def handle_cast({:add_members, channel, members}, s = State[users: users, publisher: publisher]) do
    new_users = case :orddict.find(channel, users) do
	                {:ok, old_members} -> :orddict.store(channel, add_elements(old_members, members), users)
	                :error -> :orddict.store(channel, :ordsets.from_list(members), users)
	              end
    :gen_event.notify(publisher, {users, new_users})
    {:noreply, s.users(new_users)}
  end

  def handle_cast(:welcome, s) do
    {:ok, channels} = :application.get_env(:channels)
    lc channel inlist channels, do: join(self(), channel)
    {:noreply, s}
  end

  def handle_info(:start, state) do
    {:ok, bot_name} = :application.get_env(:bot_name)
    {:ok, host} = :application.get_env(:host)
    {:ok, port} = :application.get_env(:port)
    {:ok, socket} = :gen_tcp.connect(host, port, [:binary, {:packet, :line}])
    register_nick(bot_name)

    {:ok, publisher} = :gen_event.start_link()
    {:ok, plugins} = :application.get_env(:plugins)
    lc {m, args} inlist plugins, do: :ok = :gen_event.add_handler(publisher, m, [self(), args])
    :erlang.send_after(@routine_check, self(), :routine_check)
    {:noreply, state.socket(socket).nick(bot_name).publisher(publisher)}
  end

  def handle_info({:tcp, _socket, message}, state) do
    log("<<<< " <> message)
    message(Erbot.Protocol.parse(strip_crlf(message)), state)
    {:noreply, state.last_contact(:erlang.now())}
  end

  def handle_info({:tcp_closed, _socket}, state) do
    log("tcp_closed. quitting..")
    {:stop, :tcp_closed, state}
  end

  def handle_info({:tcp_error, _socket, reason}, state) do
    log("tcp_error. quitting..")
    {:stop, reason, state}
  end

  def handle_info(:routine_check, state) do
    diff = (Erbot.Utils.triple_to_timestamp(:erlang.now()) - Erbot.Utils.triple_to_timestamp(state.last_contact)) / 1000000
    case diff < @ping_timeout do
      true ->
        :erlang.send_after(@routine_check, self(), :routine_check)
        {:noreply, state}
      false ->
        log("no message from server in the last #{inspect diff} seconds. quitting..")
        {:stop, @ping_timeout, state}
    end
  end

  def handle_info(unknown, state) do
    log("got unknown message " <> unknown)
    {:noreply, state}
  end

  def strip_crlf(str) do
    String.rstrip(str)
  end

  # irc

  def reply(message) do
    reply(self(), message)
  end

  def reply(pid, message) do
    reply = message <> "\r\n"
    :gen_server.cast(pid, {:send, reply})
  end

  def register_nick(name) do
    reply("NICK " <> name)
    reply("USER " <> name <> " 0 * :" <> name)
  end

  def join(pid, channel) do
    reply(pid, "JOIN " <> channel)
  end

  def members(pid, channel) do
    reply(pid, "NAMES " <> channel)
  end

  def send_message(pid, target, message) do
    lc m inlist String.split(message, "\n"), do: reply(pid, Enum.join(["PRIVMSG", target, ":" <> m], " "))
  end

  def to_us(target, State[nick: nick]) do
    target == nick
  end

  @ignored_commands ["NOTICE", "MODE", "002", "003", "004", "005",
                       "251", "252", "253", "254", "255", "265", "266",
                       "366", "372", "375", "376"]

  def message({"PING", serverName}, _s) do
    reply("PONG " <> serverName)
  end

  def message({from, "PRIVMSG", rest}, s = State[publisher: publisher]) do
    {nick, _name, _host} = Erbot.Protocol.user(from)
    {target, ":" <> message} = Erbot.Protocol.split_space(rest)
    case to_us(target, s) do
	    true ->
        IO.puts("private msg #{nick} #{message}")
	      :gen_event.notify(publisher, {:private_msg, nick, message})
	    false ->
	      :gen_event.notify(publisher, {:channel_msg, {nick, target}, message})
    end
  end

  def message({from, "JOIN", rest}, State[publisher: publisher]) do
    {nick, _name, _host} = Erbot.Protocol.user(from)
    ":" <> channel = rest
    :gen_server.cast(self(), {:add_members, channel, [nick]})
    :gen_event.notify(publisher, {:join, nick, channel})
    :ok
  end

  def message({_prefix, "001", _welcome}, _s) do
    :gen_server.cast(self(), :welcome)
  end

  def message({_prefix, "433", _nickname_in_use}, _s) do
    :gen_server.cast(self(), :try_new_nick)
  end

  def message({_prefix, "353", members_list}, _s) do
    {channel, members} = Erbot.Protocol.members(members_list)
    :gen_server.cast(self(), {:add_members, channel, members})
    :ok
  end

  def message({_prefix, command, params}, _s) do
    case :lists.member(command, @ignored_commands) do
      true -> :ok
      false ->
        IO.puts("got unknown command #{inspect command}  params #{inspect params} ~n")
        :ok
    end
  end


  # Utils
  def add_elements(set, [e | r]) do
    add_elements(:ordsets.add_element(e, set), r)
  end

  def add_elements(set, []) do
    set
  end

  def log(message) do
    IO.puts "#{inspect :erlang.now()} #{inspect message}"
    # IO.puts "#{:strftime.f(:erlang.now(), '%D %T')} #{inspect message}"
  end
end
