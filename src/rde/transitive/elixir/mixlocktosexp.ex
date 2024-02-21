#!/usr/bin/env elixir

# Based on https://github.com/ydlr/mix2nix

defmodule MixLockToSexp do
  def process(filename) do
    filename
    |> read
    |> expression_set
  end

  def expression_set(deps) do
    deps
    |> Map.to_list()
    |> Enum.sort(:asc)
    |> Enum.map(fn {k, v} -> s_expression(deps, k, v) end)
    |> Enum.reject(fn x -> x == "" end)
    |> Enum.map(fn x -> "  " <> String.trim(x) end)
    |> Enum.join("\n")
    |> String.trim("\n")
    |> wrap
  end

  defp read(filename) do
    opts = [file: filename, warn_on_unnecessary_quotes: false]

    with {:ok, contents} <- File.read(filename),
         {:ok, quoted} <- Code.string_to_quoted(contents, opts),
         {%{} = lock, _} <- Code.eval_quoted(quoted, opts) do
      lock
    else
      {:error, posix} when is_atom(posix) ->
        :file.format_error(posix) |> to_string() |> IO.puts()
        System.halt(1)

      {:error, {line, error, token}} when is_integer(line) ->
        IO.puts("Error on line #{line}: #{error} (" <> inspect(token) <> ")")
        System.halt(1)
    end
  end

  def is_required(allpkgs, hex: name, repo: _, optional: optional) do
    Map.has_key?(allpkgs, name) or !optional
  end

  def dep_string(allpkgs, deps) do
    depString =
      deps
      |> Enum.filter(fn x -> is_required(allpkgs, elem(x, 2)) end)
      |> Enum.map(fn x -> "\"" <> Atom.to_string(elem(x, 0)) <> "\"" end)
      |> Enum.join(" ")

    if String.length(depString) > 0 do
      "#(" <> depString <> ")"
    else
      "#()"
    end
  end

  def specific_workaround(pkg) do
    case pkg do
      "cowboy" -> "rebar"
      "ssl_verify_fun" -> "rebar"
      "jose" -> "mix"
      _ -> false
    end
  end

  def get_build_env(builders, pkgname) do
    cond do
      specific_workaround(pkgname) ->
        specific_workaround(pkgname)

      Enum.member?(builders, :mix) ->
        "mix"

      Enum.member?(builders, :rebar3) or Enum.member?(builders, :rebar) ->
        "rebar"

      Enum.member?(builders, :make) ->
        "gnu"

      true ->
        "other"
    end
  end

  def get_hash(name, version) do
    url = "https://repo.hex.pm/tarballs/#{name}-#{version}.tar"
    {result, status} = System.cmd("get-nix32-hash", [url])

    case status do
      0 ->
        String.trim(result)

      _ ->
        IO.puts("Use of get-nix32-hash failed.")
        System.halt(1)
    end
  end

  def s_expression(
        allpkgs,
        name,
        {:hex, hex_name, version, _hash, builders, deps, "hexpm", hash2}
      ),
      do: get_hexpm_expression(allpkgs, name, hex_name, version, builders, deps, hash2)

  def s_expression(
        allpkgs,
        name,
        {:hex, hex_name, version, _hash, builders, deps, "hexpm"}
      ),
      do: get_hexpm_expression(allpkgs, name, hex_name, version, builders, deps)

  def s_expression(
        allpkgs,
        name,
        {:git, url, commit, deps}
      ),
    do: get_git_expression(allpkgs, name, url, commit, deps)

  def s_expression(_allpkgs, name, _pkg) do
    "#{name}"
  end

  defp get_git_expression(allpkgs, name, url, commit, deps) do
    name = Atom.to_string(name)
    deps = dep_string(allpkgs, deps)

    """
    ("#{name}" . #("git" "#{url}" "#{commit}" #{deps}))
    """
  end

  defp get_hexpm_expression(allpkgs, name, hex_name, version, builders, deps, sha256 \\ nil) do
    name = Atom.to_string(name)
    hex_name = Atom.to_string(hex_name)
    buildEnv = get_build_env(builders, name)
    sha256 = sha256 || get_hash(hex_name, version)
    deps = dep_string(allpkgs, deps)
    """
    ("#{name}" . #("#{buildEnv}" "#{hex_name}" "#{version}" "#{sha256}" #{deps}))
    """
  end

  defp wrap(pkgs) do
    """
    (
    #{pkgs}
    )
    """
  end

end

# Read first command line argument
[mixlock] = System.argv()
mixlock
|> MixLockToSexp.process()
|> IO.puts()
