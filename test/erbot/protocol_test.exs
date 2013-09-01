defmodule Erbot.ProtocolTest do
  import Erbot.Protocol
  use ExUnit.Case

  test "#members" do
    {"#channel", []} = members("erbot_ = #channel :@erbot_")
    {"#channel", ["logbot", "another"]} = members("erbot_ = #channel :@erbot_ logbot another")
  end
end
