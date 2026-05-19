# Допълнителни задачи за дървета

# Двоични дървета

```hs
tree :: BinTree Int
tree =
  Node
    1
    ( Node
        4
        Empty
        (Node 6 Empty Empty)
    )
    ( Node
        3
        (Node 7 Empty Empty)
        ( Node
            5
            Empty
            (Node (-1) Empty Empty)
        )
    )
```

## Задача 00

Да се дефинира функция, която приема две двоични дървета
и връща дали първото е поддърво на второто.

```hs
subtree :: BinTree Int
subtree =
   Node
     4
     Empty
     (Node 6 Empty Empty)
```

```hs
ghci> subtree `isSubtreeOf` tree
True
```

## Задача 01

Да се дефинира функция, която приема две стойности `x` и `y`
и намира най-близкия общ родител на двата възела със стойности
`x` и `y`.

```hs
ghci> lca 4 6 tree
Just 1
ghci> lca (-1) 7 tree
Just 3
ghci> lca (-1) 5 tree
Nothing
```

# Дървета с произволна разклоненост

## Задача 02

Да се дефинира тип за дърво с произволна разклоненост,
чиито елементи могат да бъдат от произволен тип.

## Задача 03

Да се дефинират следните функции, работещи върху
дървета с произволна разклоненост:

- `height :: Tree a -> Int`;
- `sum :: Num a => Tree a -> a`;
- `mapTree :: (a -> b) -> Tree a -> Tree b`;
- `contains :: Eq a => a -> Tree a -> Bool`;
- `preorder :: Tree a -> [a]`;
- `levels :: Tree a -> [[a]]`.

# Дървета, моделиращи изрази

## Задача 04

Нека е даден следният алгебричен тип данни,
представящ математически израз:

```hs
data MathExpr
  = Literal Double
  | Minus MathExpr
  | MathExpr :+ MathExpr
  | MathExpr :- MathExpr
  | MathExpr :* MathExpr
  | MathExpr :/ MathExpr
  | MathExpr :^ MathExpr  -- повдигане на степен

infixr 9 :^
infixl 8 :*
infixl 8 :/
infixl 7 :+
infixl 7 :-
```

Така, например, изразът `5.15 + 4 * 10 ^ (2+1)` се представя чрез израза

```hs
expr :: MathExpr
expr =
  Literal 5.15
    :+ ( Literal 4
           :* ( Literal 10
                  :^ (Literal 2 :+ Literal 1)
              )
       )
```

или записано без скоби (възползваме се от горните дефиниции
на приоритет и асоциативност на конструкторите):

```hs
expr :: MathExpr
expr =
  Literal 5.15
    :+ Literal 4
    :* Literal 10
    :^ (Literal 2 :+ Literal 1)
```

Да се дефинират следните функции:

- `eval :: MathExpr -> Double`;
- `toString :: MathExpr -> String`;
- `distributiveLaws :: MathExpr -> MathExpr`,
  която прилага дистрибутивните закони за умножение и деление
  спрямо събиране и изваждане.

## Задача 05

Нека е даден следният алгебричен тип данни,
представящ регулярен израз над азбука с елементи от тип `a`:

*Забележка*: Азбука наричаме крайно множество, чиито елементи
ще наричаме букви. Използвайки това множество, ще можем да
изграждаме произволно сложни регулярни изрази.

```hs
data Regex a
  = Empty
  | Epsilon
  | Symbol a
  | KStar (Regex a)
  | Regex a `Union` Regex a
  | Regex a `Intersect` Regex a

infixl 9 `Intersect`
infixl 8 `Union`
```

Например, за азбука `{0,1}` регулярният израз `(00)*1` представя низ,
завършващ на `1` и съдържащ четен брой нули в началото,
а `(0 + 1)(1 + 0)*`, представя низ, който съдържа поне едно `0` или едно `1`.

Горните два регулярни израза имат следните представяния чрез горния тип данни:

```hs
data A01 = Zero | One
  deriving Show

zero :: Regex A01
zero = Symbol Zero

one :: Regex A01
one = Symbol One

re1 :: Regex A01
re1 = KStar (zero `Intersect` zero) `Intersect` one

re2 :: Regex A01
re2 = (zero `Union` one) `Intersect` KStar (one `Union` zero)
```

Да се дефинират следните функции, работещи с регулярни изрази:

- `toString :: Show a => Regex a -> String`;
- `recognises :: Eq a => Regex a -> [a] -> Bool`,
  която проверява дали даден регулярен израз разпознава низ
  над азбука с елементи от тип `a`.
