defmodule Postgrex.MixProject do
  use Mix.Project

  def project() do
    [
      app: :csv,
      version: "3.0.4",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      description: "Fast Erlang NIF-based CSV parser using libcsv",
      package: package(),
      deps: [],
      source_url: "https://github.com/martin-torhage/csv"
    ]
  end

  def application() do
    []
  end

  defp package() do
    [
      name: "csve",
      licenses: ["MIT", "LGPL-2.1-only"],
      maintainers: ["Martin Torhage"],
      links: %{"GitHub" => "https://github.com/martin-torhage/csv"},
      files: ["LICENSE",
              "Makefile",
              "README.md",
              "c_src",
              "rebar.config",
              "src",
              "test"]
    ]
  end
end
