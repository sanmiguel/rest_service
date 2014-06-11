defmodule RestService.Mixfile do
  use Mix.Project

  def project do
    [ app: :rest_service,
      version: "0.0.1",
      elixir: "~> 0.13",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ registered: [
        :rest_service_sup, 
        :rest_service_store_sup,
        :rest_service_store],
      applications: [:cowboy],
      mod: { :rest_service_app, [] } ]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, git: "https://github.com/elixir-lang/foobar.git", tag: "0.1" }
  #
  # To specify particular versions, regardless of the tag, do:
  # { :barbat, "~> 0.1", github: "elixir-lang/barbat" }
  defp deps do
    [{:cowboy, github: "extend/cowboy", tag: "0.9.0"},
      {:proper, github: "manopapad/proper", tag: "master"},
      {:exrm, "~> 0.8.1"}]
  end
end
