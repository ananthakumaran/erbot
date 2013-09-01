defmodule Erbot.Plugins.Cowsay do
  use GenEvent.Behaviour

  def init([client, []]) do
    {:ok, client}
  end

  def handle_event({:private_msg, nick, "!cowsay " <> message}, client) do
    cowsay(nick, message, client)
    {:ok, client}
  end

  def handle_event({:channel_msg, {_nick, channel}, "!cowsay " <> message}, client) do
    cowsay(channel, message, client)
    {:ok, client}
  end

  def handle_event(_event, state) do
    {:ok, state}
  end

  def handle_call(_Request, state) do
    {:ok, :ok, state}
  end

  def handle_info(_Info, state) do
    {:ok, state}
  end

  def cowsay(to, message, client) do
    cowsay = System.cmd("cowsay " <> message)
    Erbot.Irc.send_message(client, to, cowsay)
  end
end
