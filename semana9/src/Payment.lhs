
---
author: Programação Funcional
title: Exercícios para tutoria. Semana 9.
date: Prof. Rodrigo Ribeiro
---

\begin{code}
module Payment where
\end{code}

Modelando uma folha de pagamento
================================

O objetivo destes exercícios é a implementação de parte
de um sistema para cálculo da folha de pagamentos de
uma empresa. Uma empresa, representada pelo tipo `Company`,
é formada por uma lista de departamentos (tipo `Dept`).

\begin{code}
data Company
  = Company [Dept]
    deriving(Eq, Ord, Show)
\end{code}

Por sua vez, um departamento é formado por um nome, tipo `Name`,
um gerente, de tipo `Manager`, e uma lista de empregados.

\begin{code}
data Dept
  = Dept Name Manager [Employee]
  deriving(Eq, Ord, Show)
\end{code}

Cada empregado é representado por seu nome, endereço e seu respectivo salário.

\begin{code}
data Employee
  = Employee Person Salary
    deriving(Eq, Ord, Show)
data Person
  = Person Name Address
    deriving(Eq, Ord, Show)
data Salary
  = Salary Float
    deriving(Eq, Ord, Show)
\end{code}

Finalmente, os tipos `Manager`, `Name` e `Address`
são apenas sinônimos para `Employee`, `String` e
`String`, respectivamente.

\begin{code}
type Manager = Employee
type Name = String
type Address = String
\end{code}

A folha de pagamento de uma empresa consiste
de uma listagem contendo o nome e salário de
cada um de seus funcionários.
Para codificar essa funcionalidade,
utilizaremos uma classe de tipos

\begin{code}
class SalaryList a where
  salaryList :: a -> [(Name, Salary)]
\end{code}

que possui uma única função, `salaryList`, que
retorna a lista de salários
presente em um determinado valor de tipo `a`.
Com base nos tipos apresentados, desenvolva o que se pede.

Exercícios
----------


1. Apresente uma instância de `SalaryList` para o tipo `Employee`.

\begin{code}
instance SalaryList Employee where
  salaryList = undefined
\end{code}

2. Apresente uma instância de `SalaryList` para o tipo `Dept`.

\begin{code}
instance SalaryList Dept where
  salaryList = undefined
\end{code}


3. Apresente uma instância de `SalaryList` para o tipo `Company`.

\begin{code}
instance SalaryList Company where
  salaryList = undefined
\end{code}


4. Implemente a função

\begin{code}
bill :: Company -> Float
bill = undefined
\end{code}

que calcula a soma dos salários de todos os funcionários da empresa
fornecida como argumento.

4. Outra funcionalidade importante é a de modificar o salário de
funcionários. Podemos modelar alterações salariais como funções de
tipo `Salary -> Salary`. Dessa forma, para alterar os salários
de todos os funcionários, basta percorrer a estrutura recursiva
do tipo `Company` realizando as
atualizações necessárias. Para implementar essa operação, utilizaremos
a classe de tipos `Modify`:
\begin{code}
class Modify a where
  modify :: (Salary -> Salary) -> a -> a
\end{code}
que define a interface de tipos que suportam a operação de
modificação de salários. Sua tarefa
é implementar essas operações para os diferentes
tipos do sistema de folha de pagamento.

5. Implemente uma instância de `Modify` para o tipo `Employee`.

\begin{code}
instance Modify Employee where
  modify = undefined
\end{code}


6. Implemente uma instância de `Modify` para o tipo `Dept`.

\begin{code}
instance Modify Dept where
  modify = undefined
\end{code}

7. Implemente uma instância de `Modify` para o tipo `Company`.

\begin{code}
instance Modify Company where
  modify = undefined
\end{code}

A seguir, alguns dados de teste para seu código

\begin{code}
company :: Company
company
  = Company [ Dept "Research" ralf [joost, marlow]
            , Dept "Strategy" blair []]

ralf :: Employee
ralf = Employee (Person "Ralf" "USA") (Salary 1000.0)

joost :: Employee
joost = Employee (Person "Joost" "Norway") (Salary 200.0)

marlow :: Employee
marlow = Employee (Person "Marlow" "Brazil") (Salary 500.0)

blair :: Employee
blair = Employee (Person "Blair" "France") (Salary 600.0)
\end{code}
