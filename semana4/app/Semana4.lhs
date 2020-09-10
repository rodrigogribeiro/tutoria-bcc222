---
author: Programação Funcional
title: Exercícios para tutoria. Semana 4.
date: Prof. Rodrigo Ribeiro
---

Introdução
==========

Setup inicial
-------------

Inicialmente, vamos importar bibliotecas para construção de
testes de programas Haskell. Utilizaremos funções destas
bibliotecas para construção de testes para os exercícios
deste material.

> import           Control.Monad
> import           Data.Char
> import           Data.List                             hiding (lines)
> import           Prelude                               hiding (words, lines, Word)
> import qualified Prelude                as P (words) 
> import           Test.Tasty
> import           Test.Tasty.HUnit
> import qualified Test.Tasty.QuickCheck  as QC

A seguinte função `main` é usada apenas para
execução dos testes para as funções deste material.

> main :: IO ()
> main = defaultMain tests

Ao contrário da primeira semana, em que você realizou
testes usando apenas o interpretador, nestes exercícios
você deverá conferir seus resultados utilizando a
bateria de testes fornecida. Para execução dos testes,
você deverá utilizar os seguintes comandos:

```
$> stack build
$> stack exec semana4-exe
```

O primeiro é responsável por compilar o projeto e o
segundo de executá-lo.

Assim como no material da semana anterior, você deve substituir as
chamadas para a função

> tODO :: a
> tODO = undefined

que interompe a execução do programa com uma
mensagem de erro, por código que implementa
as funcionalidades requeridas por cada exercício.


Descrição do material
---------------------

Esse material consiste em exercícios sobre o conteúdo de listas e funções
de ordem superior.

Antes de resolver os exercícios contidos nesse material, recomendo que você
faça todos os exercícios presentes nos slides das aulas:

- Recursão sobre listas
- Tipos em Haskell
- Funções de ordem superior


Criação de um índice
====================

Introdução
----------

O objetivo desse exercício é a criação de funções de processamento de texto para
criação de um índice remissivo. É muito comum que livros possuam esse tipo de
índice em que palavras ou termos de interesse são associados a página onde
ocorrem. Como não lidaremos com textos dividos em páginas fixas, nosso índice
indicará em que _linha_ do texto um certo termo ocorre. Como exemplo, considere
o texto:

     Uma rosa é \n uma rosa \n bem rosa.

usando o programa a ser construído sobre o texto devemos obter
o seguinte índice:

      uma  1, 2
      rosa 1, 2, 3
      bem  3

Modelando o problema
--------------------

Para modelar esse problema, devemos inicialmente definir os tipos envolvidos
nos parâmetros de entrada e resultados desse programa. Sabemos que a entrada
corresponde a um texto. Para fins de legibilidade do código, usaremos um
sinônimo de tipo:

> type Text = String

Adicionalmente, devemos produzir um índice composto por palavras e a linha em que
essas ocorrem. Inicialmente, vamos acrescentar sinônimos para linhas e palavras
contidas no texto.

> type Line = String
> type Word = String

Finalmente, representaremos o índice resultante como uma lista formada por
pares contendo a lista de linhas em que uma palavra ocorre e a palavra em si.

> type Index = [([Int], Word)]

A construção `type` de Haskell permite a definição de um _sinônimo de tipo_, isto é,
um novo nome para um tipo existente.

Para solucionar esse problema, vamos adotar uma abordagem _data-driven_, isto é,
desenvolveremos esse algoritmo criando funções que mostram como a entrada é
transformada, passo a passo, no resultado desejado. Os exercícios seguintes
apresentam a solução passo a passo deste problema.


1. Implemente a função `lines` que divide o texto de entrada em uma lista
das linhas que o formam.

> lines :: Text -> [Line]
> lines = undefined

Os seguintes casos de teste devem ser satisfeitos por sua implementação de `lines`.

> lineTests :: TestTree
> lineTests
>    = testGroup "Tests for lines"
>                [
>                  testCase "lines empty"   $ lines ""              @?= []
>                , testCase "lines single"  $ lines "abc"           @?= ["abc"]
>                , testCase "lines many 1"  $ lines "a\nbc\ncd"     @?= ["a","bc","cd"]
>                , testCase "lines many 2"  $ lines "ab\ncd ef\ngh" @?= ["ab", "cd ef", "gh"]
>                ] 


2. Implemente a função `numberLines` que associa a cada linha o seu respectivo
número.

> numberLines :: [Line] -> [(Int, Line)]
> numberLines = tODO

Os seguintes casos de teste devem ser satisfeitos por sua implementação de `numberLines`.

> numberLinesTests :: TestTree
> numberLinesTests
>   = testGroup "Tests for numberLines"
>               [
>                  testCase "number lines empty"   $ numberLines []                    @?= []
>                , testCase "number lines single"  $ numberLines ["abc"]               @?= [(1,"abc")]
>                , testCase "number lines many 1"  $ numberLines ["a", "bc", "cd"]     @?= [(1,"a"),(2,"bc"),(3,"cd")]
>                , testCase "number lines many 2"  $ numberLines ["ab", "cd ef", "gh"] @?= [(1,"ab"), (2,"cd ef"), (3,"gh")] 
>                ] 


3. Desenvolva a função

> numberWords :: (Int,Line) -> [(Int, Word)]
> numberWords = tODO

que divide uma linhas nas palavras que a formam adicionando a cada palavra o número da
linha em que essa está contida. Os seguintes casos de teste devem ser satisfeitos por
sua implementação de `numberWords`. 

> numberWordsTests :: TestTree
> numberWordsTests
>   = testGroup "Tests for numberWords"
>               [
>                  testCase "number words single"  $ numberWords (1,"abc")     @?= [(1,"abc")]
>                , testCase "number lines many"  $ numberWords (2, "cd ef gh") @?= [(2,"cd"),(2,"ef"),(2,"gh")] 
>               ] 


4. Utilizando sua função `numberWorlds`, implemente

> numberAll :: [(Int, Line)] -> [(Int, Word)]
> numberAll = tODO

que associa a todos palavras de um texto o seu respectivo número de linha.

Sua implementação de `numberAll` deve satisfazer os seguintes casos de teste.

> numberAllTests :: TestTree
> numberAllTests
>   = testGroup "Tests for numberAll"
>               [
>                 testCase "numberAll empty"  $ numberAll []                            @?= []
>               , testCase "numberAll single" $ numberAll [(1, "ab cd ef")]             @?= [(1, "ab"), (1, "cd"), (1,"ef")] 
>               , testCase "numberAll many"   $ numberAll [(1, "ab cd"),  (2, "ef gh")] @?= [(1, "ab"), (1,"cd"), (2, "ef"), (2, "gh")]
>               ]


5. Desenvolva a função

> sortEntries :: [(Int,Word)] -> [(Int, Word)]
> sortEntries = tODO

que ordena uma lista de palavras e números de linhas utilizando a seguinte
função de comparação:

> (.<.) :: (Int, Word) -> (Int, Word) -> Bool
> (l1, w1) .<. (l2, w2)
>    = w1 < w2 || (w1 == w2 && l1 < l2)

Sua função de ordenação deve satisfazer os seguintes testes.

> sortEntriesTests :: TestTree
> sortEntriesTests
>   = testGroup "Tests for sortEntries"
>               [
>                  testCase "sort entries empty"   $ sortEntries []                                       @?= []
>                , testCase "sort entries single"  $ sortEntries [(1,"abc")]                              @?= [(1,"abc")]
>                , testCase "sort entries many 1"  $ sortEntries [(1,"a"),(2,"bc"),(3,"cd")]              @?= [(1,"a"),(2,"bc"),(3,"cd")]
>                , testCase "sort entries many 2"  $ sortEntries [(1,"ab"), (2,"cd"), (2,"af"), (3,"gh")] @?= [(1,"ab"), (2,"af"), (2,"cd"), (3,"gh")] 
>               ] 

6. Implemente a função

> combine :: [(Int, Word)] -> Index
> combine = tODO

que a partir de uma lista contendo pares de palavras e suas respectivas linhas,
combina os números de linha de uma certa palavra em uma lista de números para
formar o índice.

Sua implementação de `combine` deve obedecer os seguintes casos de teste.


> combineTests :: TestTree
> combineTests
>   = testGroup "Tests for combine"
>               [
>                  testCase "combine empty"  $ combine []                                        @?= []
>                , testCase "combine single" $ combine [(1,"a"),(2,"bc"),(3,"cd")]               @?= [([1],"a"),([2],"bc"),([3],"cd")]
>                , testCase "combine many"   $ combine [(1,"ab"), (2,"af"), (3,"af"), (3,"gh")]  @?= [([1],"ab"), ([2,3],"af"),([3],"gh")] 
>               ]


7. Finalmente, de posse de todas as funções anteriores, implemente a função

> makeIndex :: Text -> Index
> makeIndex = tODO

que a partir de um texto retorna o seu índice remissivo. Sua função deve ser
codificada como a composição de todos os exercícios desenvolvidos anteriormente.

Funções auxiliares
------------------

> sortedProperty :: QC.Property
> sortedProperty
>   = QC.forAll genText
>            (\ t -> sorted (.<.) (f t))
>     where
>       g = numberLines . lines
>       f = sortEntries . concatMap numberWords . g

> indexProperty :: QC.Property
> indexProperty
>   = QC.forAll genText
>               (\ t -> let ws = map P.words $ lines t
>                           ixs = makeIndex t
>                           isAt is w = all (\i -> w `elem` (ws !! i)) is
>                       in all (uncurry isAt) ixs)


> sorted :: (a -> a -> Bool) -> [a] -> Bool
> sorted p [] = True
> sorted p [_] = True
> sorted p (x : x' : xs)
>   = p x x' && sorted p (x' : xs)


> genText :: QC.Gen Text
> genText
>   = do
>        -- maximum number of lines
>        n <- QC.choose (1,5)
>        -- maximum number of words per line
>        m <- QC.choose (1,5)
>        createText n m

> createText :: Int -> Int -> QC.Gen Text
> createText l w
>   = (concat . intersperse "\n") <$> replicateM l (createLine w)

> createLine :: Int -> QC.Gen Line
> createLine w
>   = (concat . intersperse " ") <$> replicateM w createWord

> createWord :: QC.Gen Word
> createWord
>   = do
>       -- número de letras
>       n <- QC.choose (1,6)
>       QC.vectorOf n genLetter

> genLetter :: QC.Gen Char
> genLetter
>   = chr <$> QC.choose (97,122) 

> tests :: TestTree
> tests
>    = testGroup "Semana 4 tests"
>                 [
>                   lineTests,
>                   numberLinesTests,
>                   numberWordsTests,
>                   numberAllTests,
>                   sortEntriesTests,
>                   combineTests,
>                   QC.testProperty "Sorted property" sortedProperty,
>                   QC.testProperty "makeIndex correct" indexProperty
>                 ]
