defmodule Erbot.Plugins.Echo do
  use GenEvent.Behaviour

  def init([client, []]) do
    {:ok, client}
  end

  def handle_event({:private_msg, nick, "!echo " <> message}, client) do
    Erbot.Irc.send_message(client, nick, "echo: " <> message)
    {:ok, client}
  end

  def handle_event({:channel_msg, {_nick, channel}, "!echo " <> message}, client) do
    Erbot.Irc.send_message(client, channel, "echo: " <> message)
    {:ok, client}
  end

  def handle_event(_, state) do
    {:ok, state}
  end
end
