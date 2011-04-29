
module Repl where


import Parser
import Evaluator

import Text.ParserCombinators.Parsec


t1 = parse program "" "a=1; b=2; c = let q = 41 in q+a+b"

