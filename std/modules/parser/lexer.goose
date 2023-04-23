import "std/modules/array.goose"
import "std/modules/regex.goose"

module Lexer 
  type Rule = { type_: string, regex: string, ignore: bool }
  type Token = { type_: string, value: string }
  def tokenize[a](rules: [Lexer::Rule], input: string): [Lexer::Token] do
    if Array::length(input) == 0 then return []
    for rule in rules do
      def res = Regex::get(rule.regex, input)
      if Array::length(res) > 0 then do
        if Array::has(rule, "ignore") then
          if rule.ignore then return Lexer::tokenize(rules, Array::slice(input, Array::length(res)))
          else return [{ type_: rule.type_, value: res }] + Lexer::tokenize(rules, Array::slice(input, Array::length(res)))
        else 
          return 
            [{ type_: rule.type_, value: res }] +
            Lexer::tokenize(rules, Array::slice(input, Array::length(res)))
      end
    end
    return []
  end
end