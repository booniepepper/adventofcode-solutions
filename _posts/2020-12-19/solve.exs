#! /usr/bin/env elixir

defmodule Solve do
  def count_for_zero(rulebook, targets) do
    rule_tree = treeify({:start, "0"}, rulebook)
    Enum.count(targets, &tree_contains?(rule_tree, &1))
  end

  def tree_contains?(rules, target) do
    # tree_search could return a suffix, so explicitly require true
    true == tree_search(rules, target)
  end

  def tree_search(_, false), do: false
  def tree_search(_, ""), do: false
  def tree_search(target, target), do: true
  def tree_search({_, [rule]}, target), do: tree_search(rule, target)

  def tree_search(prefix, target) when is_bitstring(prefix) do
    String.starts_with?(target, prefix) && String.replace_prefix(target, prefix, "")
  end

  def tree_search({:or, [rule | rules]}, target) do
    tree_search(rule, target) || tree_search({:or, rules}, target)
  end

  def tree_search({:and, [rule | rules]}, target) do
    tree_search({:and, rules}, tree_search(rule, target))
  end

  def treeify({:start, value}, rules), do: treeify(Map.get(rules, value), rules)
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

{:ok, input} = File.read("./input")

[rule_text, targets | _] =
  input
  |> String.split("\n\n")
  |> Enum.map(&String.split(&1, "\n"))

rulebook = Solve.parse_rules(rule_text)

IO.write(Solve.count_for_zero(rulebook, targets))
IO.puts(" messages are valid.")

_forbidden_arts = %{
  "8" => {:slurp, ["42", :self]},
  "11" => {:slurp, ["42", :self, "31"]}
}

#IO.write(Solve.count_for_zero(Map.merge(rulebook, forbidden_arts), targets))
#IO.puts(" messages are valid (with danger).")
