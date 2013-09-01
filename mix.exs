defmodule Erbot.Mixfile do
  use Mix.Project

  def project do
    [ app: :erbot,
      version: "0.0.1",
      elixir: "~> 0.10.2-dev",
      deps: deps ]
  end

  def application do
    [registered: [:erbot],
     applications: [:inets, :sasl, :ssl, :httpotion],
     env: [{:host, 'irc.foonetic.net'},
           {:port, 6667},
           {:channels, ["#activesphere"]},
           {:bot_name, "erbot"},
           {:plugins, [{Erbot.Plugins.Cowsay, []},
                       {Erbot.Plugins.Echo, []},
                       {Erbot.Plugins.Fortune, [{:frequency, 50}]},
          	           {Erbot.Plugins.History, [{:dbpath, "/tmp/erbot_history"}]},
                       {Erbot.Plugins.Message, []},
                       {Erbot.Plugins.AlertMe, []}]}],
     mod: { Erbot, [] }]
  end

  defp deps do
    [{:httpotion, "~> 0.2.0", github: "myfreeweb/httpotion"},
     {:jsex, "~> 0.2", github: "talentdeficit/jsex"},
     {:lets, "v1.1.0", git: "https://github.com/norton/lets.git"},
     {:gate, ">= 0.1", git: "https://github.com/ananthakumaran/gate.git"},
     {:strftimerl, ">= 0.1", git: "https://github.com/kennystone/strftimerl"}]
  end
end
