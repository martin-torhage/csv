defmodule Postgrex.MixProject do
  use Mix.Project

  def project() do
    [
      app: :csv,
      version: "2.0.3",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      description: "Erlang CSV parser implemented with NIF",
      package: package(),
      deps: [{:ex_doc, ">= 0.0.0", only: :dev}],
      name: "csve",
      source_url: "https://github.com/martin-torhage/csv"
    ]
  end

  def application() do
    []
  end

  defp package() do
    [
      name: "csve",
      licenses: ["MIT"],
      maintainers: ["Martin Torhage"],
      links: %{"GitHub" => "https://github.com/martin-torhage/csv"},
      files: ["LICENSE",
              "Makefile",
              "README.md",
              "bin",
              "c_src",
              "rebar-nif.config",
              "rebar.config",
              "src",
              "test"]
    ]
  end
end
