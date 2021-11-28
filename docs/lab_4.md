# Лабораторна робота 4

Тема: Програмування списків мовами функціонального програмування

**Варіант 1**

## Мета

Опанувати теоретичні основи використання списків функціональними мовами та розробити програми обробки списків

## Умова задачі

### Задача 1
Створити список натуральних чисел (натуральні числа >= 1), кратних 3, задавши їх кількість. Вивести створений список. Виконати такі операції: 
a)	додати елементи в список на задану позицію в списку;
b)	підрахувати кількість парних елементів в списку;
c)	замінити усі парні значення списку на середнє арифметичне елементів списку.  

### Задача 2
Написати код, що моделює роботу сортувального вузла на залізниці в процесі формування складу потягу. На сортувальному вузлі формуються потяги. Нехай існує два типи вагонів. На кожний напрямок потяг складається з вагонів одного типу. Відомі такі часові характеристики для кожного потягу: інтервали між потягами, кількість вагонів у потягу, тривалість причеплення вагону до потягу, тривалість огляду сформованого потягу, загальна тривалість простою потягу до його відправлення. Вивести на екран склад кожного потягу та сценарій виконання дій з урахування часових характеристик.

## Обгрунтування вибору середовища та мови програмування

Середовище програмування: ***VS Code***

Обгрунтування:

- Легкий та зручний редактор коду, який можна використовувати майже для будь-якої мови програмування.
- Має всі необхідні функції для програмування і працює набагато швидше в порівнянні з іншими IDE.

Мова функціонального програмування: ***Haskell***

Обгрунтування:

- Це одна з самих популярних функціональних мов програмування.
- Зручна та обширна документація.
- Звичний синтаксис.

## Код програми

### Задача 1
```
import Data.List
import System.IO
import Distribution.Compat.CharParsing (integral)

createList :: Int -> [Int]
createList a = [x * 3 | x <- [1..a]]

insertIntoList :: [Int] -> Int -> [Int] -> [Int]
insertIntoList el index list = left ++ el ++ right
  where (left, right) = splitAt index list

countEven :: [Int] -> Int
countEven list = length [x | x <- list, even x]

getMean :: [Int] -> Int
getMean list = ceiling (fromIntegral (sum list) / fromIntegral (length list))

replaceEvenWithMean:: [Int] -> [Int]
replaceEvenWithMean list = [ if even x then getMean list else x | x <- list]

firstTask = do
    putStrLn "Input length of list"
    input <- getLine
    let length = read input :: Int
    let list = createList length
    putStrLn "Result"
    print list
    
    putStrLn "List after inserting elements 1, 2, 3 index 4"
    print (insertIntoList [1, 2, 3] 4 list)

    putStrLn "Even numbers quantity in list:"
    print (countEven list)

    putStrLn "After replace all even elements by mean:"
    print (replaceEvenWithMean list)
```

### Задача 2
```

showIdle :: Show a => a -> [Char]
showIdle idle = "The idle is " ++ show idle ++ " hours"

showContent :: (Num t, Enum t, Show a) => a -> t -> String
showContent carType qty = "Train: " ++ intercalate "-" [show carType | x <- [0..qty]]
showIntervals :: Show a => a -> String
showIntervals int = "The interval is " ++ show int ++ " hours"
showCouplingDuration :: Int -> Int -> String
showCouplingDuration carType qty = "Train coupling duration: " ++ show (qty * (dur !! carType)) ++ " hours"
  where dur = [1, 2]
showInspectionDuration :: Int -> Int -> String
showInspectionDuration carType qty = 
  "Train coupling duration: " ++ show (qty * (dur !! carType)) ++ " hours"
  where dur = [2, 1]

showTrain :: [Int] -> String
showTrain train = unlines [
    showContent (head train) (train !! 1),
    showIntervals (train !! 2),
    showCouplingDuration (head train) (train !! 1),
    showInspectionDuration (head train) (train !! 1),
    showIdle (train !! 3),
    "-------------"
  ]

secondTask = do
  -- (car type, cars quantity, interval, idle time)
  let trains = [[1, 7, 24, 3], [0, 8, 36, 2]]
  putStrLn (unlines [showTrain x | x <- trains])

```

## Результати виконання
![Lab 4 Results](img/lab_4/lab_4_task_1_results.png)
![Lab 4 Results](img/lab_4/lab_4_task_2_results.png)

## Висновки

У ході виконання цієї лабораторної роботи було опановано теоретичні основи використання списків функціональними мовами та розробити програми обробки списків.