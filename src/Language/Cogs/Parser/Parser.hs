{-# LANGUAGE OverloadedStrings #-}
module Cogs.Parse where


ops, types, names :: [String]
ops   = ["+","*","-","^", "**", ":",".", "<~","==", "=", "_", "<|>"]
types = ["->"]
names = ["def", "fn", "if", "else", "∞", "expect", "observe",
         "return", "match", "integrate", "summate", "product",
         "data", "import"]
