#! /usr/bin/env elixir

{:ok, input} = File.read("./input")

[raw_rules, targets | _] =
  input
  |> String.split("\n\n")
  |> Enum.map(&String.split(&1, "\n"))

defmodule Solve do
  def count_for_zero(raw_rules, targets) do
    rules = parse_rules(raw_rules)
    rule_tree = treeify_and_shake(rules, "0")
    Enum.count(targets, &tree_contains?(rule_tree, &1))
  end

  def tree_contains?(rules, target) do
    # tree_search could return a suffix, so explicitly require true
    true == tree_search(rules, target)
  end

  def tree_search(_, false), do: false
  def tree_search(_, ""), do: false
  def tree_search(target, target), do: true

  def tree_search(prefix, target) when is_bitstring(prefix) do
    String.starts_with?(target, prefix) && String.replace_prefix(target, prefix, "")
  end

  def tree_search({_, [rule]}, target), do: tree_search(rule, target)

  def tree_search({:or, [rule | rules]}, target) do
    tree_search(rule, target) || tree_search({:or, rules}, target)
  end

  def tree_search({:and, [rule | rules]}, target) do
    tree_search({:and, rules}, tree_search(rule, target))
  end

  def treeify_and_shake(rules, starting_key) do
    {:or, [{:and, [starting_key]}]}
    |> treeify(rules)
    |> shake_tree
  end

  def shake_tree(leaf) when is_bitstring(leaf), do: leaf
  def shake_tree(leaf) when is_bitstring(leaf), do: leaf
  def shake_tree({:and, leaf}) when is_bitstring(leaf), do: leaf
  def shake_tree({:or, leaf}) when is_bitstring(leaf), do: leaf
  def shake_tree({:and, [branch]}), do: shake_tree(branch)
  def shake_tree({:or, [branch]}), do: shake_tree(branch)

  def shake_tree({:and, branches}) do
    if Enum.all?(branches, &is_bitstring(&1)) do
      Enum.join(branches)
    else
      {:and, Enum.map(branches, &shake_tree(&1))}
    end
  end

  def shake_tree({:or, branches}), do: {:or, Enum.map(branches, &shake_tree(&1))}

  def treeify(leaf, _) when is_bitstring(leaf), do: leaf

  def treeify({:or, ors}, rules) do
    {
      :or,
      Enum.map(ors, fn {:and, ands} ->
        {
          :and,
          Enum.map(ands, fn value ->
            treeify(Map.get(rules, value), rules)
          end)
        }
      end)
    }
  end

  def parse_rules(rules), do: parse_rules(rules, %{})
  def parse_rules([], acc), do: acc

  def parse_rules([rule | rules], acc) do
    [key, raw_value] = String.split(rule, ": ")

    values =
      case Regex.run(~r/"(.)"/, raw_value) do
        [_, s] -> s
        _ -> {:or, parse_rule(raw_value)}
      end

    parse_rules(rules, Map.put(acc, key, values))
  end

  def parse_rule(raw_value) do
    raw_value
    |> String.split(" | ")
    |> Enum.map(&String.split(&1, " "))
    |> Enum.map(fn values -> {:and, values} end)
  end
end

IO.write(Solve.count_for_zero(raw_rules, targets))
IO.puts(" messages match the 0 rule.")
