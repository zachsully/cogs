module Data.Number.Transcendental where


-- Transcendatl Number

class Transcendental a where
  approx :: Num b => a -> b
