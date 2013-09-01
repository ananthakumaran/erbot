defmodule Erbot.Plugins.AlertMe do
  use GenEvent.Behaviour

  defrecord(State, client: nil, notify: nil)

  def init([client, []]) do
    {:ok, State[client: client, notify: :orddict.new()]}
  end

  def handle_event({:channel_msg, {nick, channel}, "!alertme " <> message}, s) do
    reply = &(Erbot.Irc.send_message(s.client, channel, &1))
    alertme(nick, message, reply, s)
  end

  def handle_event({:join, nick, channel}, s) do
    new_notify = case :orddict.find(nick, s.notify) do
      {:ok, waiters} ->
        alert_waiters(:ordsets.to_list(waiters), s.client, channel, nick)
        :orddict.erase(nick, s.notify)
      :error -> s.notify
    end
    {:ok, s.notify(new_notify)}
  end

  def handle_event(_, state) do
    {:ok, state}
  end

  def alertme(nick, message, reply, s) do
    user = String.strip(message)
    new_notify = case user == "help" do
      true ->
        help(reply)
        s.notify
      false ->
        reply.("Yes sir.")
        add_waiter(s.notify, nick, user)
    end
    {:ok, s.notify(new_notify)}
  end

  def add_waiter(notify, waiter, for) do
    case :orddict.find(for, notify) do
      {:ok, waiters} ->
        :orddict.store(for, :ordsets.add_element(waiter, waiters), notify)
      :error ->
        :orddict.store(for, :ordsets.from_list([waiter]), notify)
    end
  end

  def alert_waiters([w | r], client, channel, nick) do
    Erbot.Irc.send_message(client, w, "hi, " <> nick <> " joined " <> channel)
    alert_waiters(r, client, channel, nick)
  end

  def alert_waiters([], _, _, _), do: :ok

  def help(reply) do
    reply.("alerts you when user 'X' joins the channel\neg !alertme X")
    :ok
  end
end
