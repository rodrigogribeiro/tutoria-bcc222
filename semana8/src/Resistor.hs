module Resistor where


-- color codes for resistors

data Color
  = Black | Brown
  | Red | Orange
  | Yellow | Green
  | Blue | Violet
  | Gray | Gold | Silver
  deriving (Eq, Ord)


-- Resistence table

type Table = [(Color, [Maybe Float])]

table :: Table
table = [(Black,  [Just 0.0, Just 0.0, Just 0.0]),
         (Brown,  [Just 1.0, Just 1.0, Just 10.0]),
         (Red,    [Just 2.0, Just 2.0, Just 100.0]),
         (Orange, [Just 3.0, Just 3.0, Just 1000.0]),
         (Yellow, [Just 4.0, Just 4.0, Just 10000.0]),
         (Green,  [Just 5.0, Just 5.0, Just 100000.0]),
         (Blue,   [Just 6.0, Just 6.0, Just 1000000.0]),
         (Violet, [Just 7.0, Just 7.0, Just 100000000.0]),
         (Gray,   [Just 8.0, Just 8.0, Just 1000000000.0]),
         (Gold,   [Nothing,  Nothing, Just 0.1]),
         (Silver, [Nothing,  Nothing, Just 0.01])]

-- exercise 1.

value :: Color -> Int -> Maybe Float
value = undefined

newtype Resistor
  = Resistor [Color]
  deriving (Eq, Ord)

-- exercise 2.

valid :: Resistor -> Bool
valid = undefined

-- exercise 3.

resistence :: Resistor -> Float
resistence = undefined


data Circuit = Parallel Circuit Circuit
             | Series   Circuit Circuit
             | Single Resistor
             deriving (Eq, Ord)


validCircuit :: Circuit -> Bool
validCircuit = undefined

circuitResistence :: Circuit -> Float
circuitResistence = undefined
