defmodule Erbot.Plugins.Message do
  use GenEvent.Behaviour

  defrecord(State, client: nil, notify: nil)

  def init([client, []]) do
    {:ok, State[client: client, notify: :orddict.new()]}
  end

  def handle_event({:channel_msg, {nick, channel}, "!msg " <> message}, state) do
    [to|msg] = String.split(message, " ")
    Erbot.Irc.send_message(state.client, channel, "Yes sir.")
    {:ok, state.notify(save_offline_msg(to, nick, Enum.join(msg, " "), state))}
  end

  def handle_event({:join, nick, channel}, state) do
    {:ok, state.notify(send_offline_msgs(nick, channel, state))}
  end

  def handle_event(_, s) do
    {:ok, s}
  end

  def save_offline_msg(to, from, message, State[notify: notify]) do
    case :orddict.find(to, notify) do
      {:ok, waiters} ->
        :orddict.store(to, :ordsets.add_element({to, from, message}, waiters), notify)
      :error ->
        :orddict.store(to, :ordsets.from_list([{to, from, message}]), notify)
    end
  end

  def send_offline_msgs(to, channel, State[notify: notify, client: client]) do
    case :orddict.find(to, notify) do
      {:ok, waiters} ->
        msg_waiters(:ordsets.to_list(waiters), client, channel)
        :orddict.erase(to, notify)
      :error -> notify
    end
  end

  def msg_waiters([{to, from, message} | r], client, channel) do
    Erbot.Irc.send_message(client, channel, to <> ": " <> from <> " says: " <> message)
    msg_waiters(r, client, channel)
  end

  def msg_waiters([], _, _), do: :ok
end
