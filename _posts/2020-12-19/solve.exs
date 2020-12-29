#!/usr/bin/env elixir

{:ok, input} = File.read("./example_input")

[raw_rules, targets | _] =
  input
  |> String.split("\n\n")
  |> Enum.map(&String.split(&1, "\n"))

defmodule Solve do
  defmodule Or, do: defstruct(values: [])
  defmodule And, do: defstruct(values: [])

  def count_for_zero(raw_rules) do
    rules = parse_rules(raw_rules)
    IO.inspect(rules)

    #tree = treeify(rules, %And{ values: [ %And{ values: ["0"] } ] })
    #IO.inspect(tree)

    # IO.inspect(Map.get(rules, to_string(n)))
    # IO.inspect(rules)
  end

  def treeify(_, leaf) when is_bitstring(leaf), do: leaf

  def treeify(rules, branches) do
    branches.values
    |> Enum.map(fn branch ->
      Enum.map(branch.values, &treeify(rules, Map.get(rules, &1)))
    end)
  end

  def parse_rules(rules), do: parse_rules(rules, %{})
  def parse_rules([], acc), do: acc

  def parse_rules([rule | rules], acc) do
    [key, raw_value] = String.split(rule, ": ")

    values =
      case Regex.run(~r/"(.)"/, raw_value) do
        [_, s] ->
          s

        _ ->%And{values: 
              raw_value
              |> String.split(" | ")
              |> Enum.map(&String.split(&1, " "))
            }
      end

    parse_rules(rules, Map.put(acc, key, values))
  end
end

Solve.count_for_zero(raw_rules)
