defmodule Erbot.Plugins.Fortune do
  use GenEvent.Behaviour

  defrecord State, client: nil, frequency: nil

  def init([client, [{:frequency, frequency}]]) do
    <<a::size(32), b::size(32), c::size(32)>> = :crypto.rand_bytes(12)
    :random.seed(a,b,c)
    {:ok, State.new([client: client, frequency: frequency])}
  end


  def handle_event({:private_msg, nick, "!fortune" <> _rest}, s) do
    send_fortune(s.client, nick)
    {:ok, s}
  end

  def handle_event({:channel_msg, {_nick, channel}, "!fortune" <> _rest}, s) do
    send_fortune(s.client, channel)
    {:ok, s}
  end

  def handle_event({:channel_msg, {_nick, channel}, _}, s) do
    case :random.uniform(s.frequency) do
	    1 -> send_fortune(s.client, channel)
	    _ -> :ok
    end
    {:ok, s}
  end

  def handle_event(_, state) do
    {:ok, state}
  end

  def fortune() do
    System.cmd("fortune")
  end

  def send_fortune(client, to) do
    Erbot.Irc.send_message(client, to, fortune())
  end
end
