# Входно-изходни операции в Haskell

Входно-изходните операции в Haskell се извършват посредством т.нар. `IO` монада
(монада за входно-изходни операции).

## Основни функции при работа с `IO`

- операции със стандартен вход/изход:

  - `putStr :: String -> IO ()` - извежда низ на стандартния изход;
  - `putStrLn :: String -> IO ()` - извежда низ на стандартния изход, като добавя нов ред;
  - `print :: Show a => a -> IO ()` - композиция на `show` с `putStrLn`;
  - `getChar :: IO Char` - чете един символ от стандартния вход;
  - `getLine :: IO String` - чете низ от стандартния вход, до достигане на символ за нов ред.

- писане/четене от файлове

  - `writeFile :: FilePath -> String -> IO ()`
  - `readFile :: FilePath -> IO String`

  *Забележка*: `FilePath` е просто типов синоним за `String`.

- трансформации/композиция на входно-изходни ефекти

  - `fmap :: (a -> b) -> IO a -> IO b` - прилага функция над стойността в `IO`;
  - `(>>=) :: IO a -> (a -> IO b) -> IO b` - композиция на входно-изходни ефекти;
  - `(>>) :: IO a -> IO b -> IO b` - като `>>=`, но резултатът от първото изчисление
      не се подава на второто такова; `ioA >> ioB` е същото като `ioA >>= _ -> ioB`;
  - `return :: a -> IO a` / `pure :: a -> IO a` - "опакова" стойност в `IO`.

## Do-нотация

Нека е даден следният тип данни:

```hs
data Date = Date {day :: Int, month :: Int, year :: Int}
```

Бихме искали да напишем програма, която да чете данните
за рождена дата на някакъв потребител и да ги запази
в стойност от горния тип данни:

```
Enter your birthday:
>day
2
>month
7
>year
2004
```

Един начин да реализираме горното е долният,
който по очевидни причини не е нито лесно четим,
нито лесен за писане:

```hs
bdayPrompt :: IO Date
bdayPrompt =
  putStrLn "Enter your birthday:"
    >> putStrLn ">day"
    >> fmap read getLine
    >>= ( \day ->
            putStrLn ">month"
              >> fmap read getLine
              >>= ( \month ->
                      putStrLn ">year"
                        >> fmap read getLine
                        >>= (\year -> pure $ Date day month year)
                  )
        )
```

За да си спестим горното главоболие, можем да използваме
т.нар. do-нотация, като тя е просто синтактична захар
за оператора `>>=`:

```hs
bdayPrompt :: IO Date
bdayPrompt = do
  _ <- putStrLn "Enter your birthday:"

  _ <- putStrLn ">day"
  day <- fmap read getLine

  _ <- putStrLn ">month"
  month <- fmap read getLine

  _ <- putStrLn ">year"
  year <- fmap read getLine

  pure $ Date day month year
```
