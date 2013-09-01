defmodule Erbot do
  use Application.Behaviour

  def start(_type, _args) do
    Erbot.Supervisor.start_link
  end
end
