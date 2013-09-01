defmodule Erbot.Protocol do
  def parse(m = ":" <> _Rest) do
    parse_prefix(m)
  end

  def parse(message) do
    parse_command(message)
  end

  def split_space(m) do
    case Regex.run(%r/(.+?) (.+)/, m, capture: [1, 2]) do
      [first, rest] ->
        {first, rest}
      nil -> :error
    end
  end

  def user(msg_target) do
    case Regex.run(%r/:(.+)!(.+)@(.+)/, msg_target, capture: [1, 2, 3]) do
      [nick, name, host] ->
        {nick, name, host}
      nil -> :error
    end
  end

  def parse_prefix(m) do
    {prefix, rest} = split_space(m)
    case parse_command(rest) do
      {command, params} ->
        {prefix, command, params}
      :error -> :error
    end
  end

  def parse_command(m) do
    split_space(m)
  end

  def members(m) do
    case Regex.run(%r/.+ [=*@] (.+) :[^ ]* (.+)/, m, capture: [1, 2]) do
      [channel, members] ->
        {channel, String.split(members)}
      nil -> :error
    end
  end
end
