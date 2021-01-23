#! /usr/bin/env elixir

# This is the messiest solution yet. I struggled with this one,
# partially due to a 1 month time gap (personal things) between
# part 1 and 2, and partially just exhausted and want to finish
# the project more than I want to have each solution be excellent.
#
# tl;dr: Elixir is a better language than my code here suggests.

defmodule Solve do
  def count_for_zero(rulebook, targets) do
    rule_tree = treeify({:start, "0"}, rulebook)
    Enum.count(targets, &tree_contains?(rule_tree, &1))
  end

  def tree_contains?(rules, target) do
    # tree_search could return a suffix, so explicitly require true
    result = tree_search(rules, target)
    true == result
  end

  def tree_search(_, false), do: false
  def tree_search(_, true), do: false
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

  def tree_search({:slurp, [left, :self, right]}, target) do
    1..String.length(target)
    |> Enum.scan({0, target}, fn _, {n, acc} ->
      {n + 1, tree_search(left, acc)}
    end)
    |> Enum.filter(fn {_, acc} -> !is_boolean(acc) end)
    |> Enum.map(fn {n_left, target_rem} ->
      1..n_left
      |> Enum.scan({0, target_rem}, fn _, {n, acc} ->
        {n + 1, tree_search(right, acc)}
      end)
      |> Enum.any?(fn {_, acc} -> true == acc end)
    end)
    |> Enum.any?(fn b -> b end)
  end

  def treeify({:start, value}, rules), do: treeify(Map.get(rules, value), rules)
  def treeify(leaf, _) when is_bitstring(leaf), do: leaf

  def treeify({:slurp, [a, :self, b]}, rules) do
    {:slurp, [treeify(Map.get(rules, a), rules), :self, treeify(Map.get(rules, b), rules)]}
  end

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

forbidden_arts = %{
  "0" => {:slurp, ["42", :self, "31"]}
}

IO.write(Solve.count_for_zero(Map.merge(rulebook, forbidden_arts), targets))
IO.puts(" messages are valid (with danger).")
