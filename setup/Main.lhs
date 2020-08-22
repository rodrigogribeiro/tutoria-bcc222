\documentclass[a4paper]{article}

\usepackage[portuguese]{babel}
\usepackage[utf8]{inputenc}
\usepackage{graphicx,hyperref}
\usepackage{float}
\usepackage{proof,tikz}
\usepackage{amssymb,amsthm,stmaryrd}


\usepackage[edges]{forest}

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include polycode.fmt

\DeclareMathAlphabet{\mathkw}{OT1}{cmss}{bx}{n}
%subst keyword a = "\mathkw{" a "}"
%subst conid a = "\V{" a "}"
%subst varid a = "\V{" a "}"
%subst numeral a = "\C{" a "}"
%subst string a = "\orange{\mathsf{``" a "\char34}}"


\newtheorem{Lemma}{Lemma}
\newtheorem{Theorem}{Theorem}
\theoremstyle{definition}
\newtheorem{Example}{Example}

\usepackage{xcolor}
\newcommand{\redFG}[1]{\textcolor[rgb]{0.6,0,0}{#1}}
\newcommand{\greenFG}[1]{\textcolor[rgb]{0,0.4,0}{#1}}
\newcommand{\blueFG}[1]{\textcolor[rgb]{0,0,0.8}{#1}}
\newcommand{\orangeFG}[1]{\textcolor[rgb]{0.8,0.4,0}{#1}}
\newcommand{\purpleFG}[1]{\textcolor[rgb]{0.4,0,0.4}{#1}}
\newcommand{\yellowFG}[1]{\textcolor{yellow}{#1}}
\newcommand{\brownFG}[1]{\textcolor[rgb]{0.5,0.2,0.2}{#1}}
\newcommand{\blackFG}[1]{\textcolor[rgb]{0,0,0}{#1}}
\newcommand{\whiteFG}[1]{\textcolor[rgb]{1,1,1}{#1}}
\newcommand{\yellowBG}[1]{\colorbox[rgb]{1,1,0.2}{#1}}
\newcommand{\brownBG}[1]{\colorbox[rgb]{1.0,0.7,0.4}{#1}}

\newcommand{\ColourStuff}{
  \newcommand{\red}{\redFG}
  \newcommand{\green}{\greenFG}
  \newcommand{\blue}{\blueFG}
  \newcommand{\orange}{\orangeFG}
  \newcommand{\purple}{\purpleFG}
  \newcommand{\yellow}{\yellowFG}
  \newcommand{\brown}{\brownFG}
  \newcommand{\black}{\blackFG}
  \newcommand{\white}{\whiteFG}
}

\newcommand{\MonochromeStuff}{
  \newcommand{\red}{\blackFG}
  \newcommand{\green}{\blackFG}
  \newcommand{\blue}{\blackFG}
  \newcommand{\orange}{\blackFG}
  \newcommand{\purple}{\blackFG}
  \newcommand{\yellow}{\blackFG}
  \newcommand{\brown}{\blackFG}
  \newcommand{\black}{\blackFG}
  \newcommand{\white}{\blackFG}
}

\ColourStuff

\newcommand{\D}[1]{\blue{\mathsf{#1}}}
\newcommand{\C}[1]{\red{\mathsf{#1}}}
\newcommand{\F}[1]{\green{\mathsf{#1}}}
\newcommand{\V}[1]{\black{\mathsf{#1}}}
\newcommand{\TC}[1]{\purple{\mathsf{#1}}}

%subst comment a = "\orange{\texttt{--" a "}}"

\usepackage{fancyhdr}

\begin{document}

  \title{Configurando o ambiente para desenvolvimento Haskell}
  \author{Rodrigo Ribeiro}

  \maketitle

  \pagestyle{fancy}
  \fancyhf{}
  \lhead{Programa\c{c}\~ao Funcional}
  \rhead{Prof. Rodrigo Ribeiro}
  \rfoot{\thepage}
  \pagestyle{fancy}

  \section{Instalação}

  Para um bom desempenho na disciplina de Programação Funcional, é recomendado que
  você instale um ambiente para desenvolvimento Haskell. O que chamo de ``ambiente
  de desenvolvimento'' consiste de: 1) uma ferramenta para gerenciar bibliotecas,
  compiladores e projetos em Haskell e; 2) um editor de texto de sua preferência.

  Recomendo a instalação da ferramenta Haskell Stack, disponível gratuitamente em:
  
  \begin{center}
     \url{https://haskellstack.org}
  \end{center}

  \paragraph{Editores de texto} Existem vários editores de texto com suporte
  para Haskell. Eu recomendo:
  \begin{enumerate}
     \item Atom (com o pacote \texttt{language-haskell}). Disponível em:
     \begin{center}
         \url{https://atom.io}
     \end{center}
     \item Emacs: Esse editor é quase um sistema operacional...
           Caso deseje usar o Emacs, recomendo instalar o Haskell-mode. Como uso o emacs,
           fique a vontade para me perguntar sobre esse editor. O emacs pode ser baixado em:
           \begin{center}
             \url{https://www.gnu.org/software/emacs/}
           \end{center}
  \end{enumerate}

  \section{Meu primeiro projeto usando o Stack}

  No terminal de seu sistema operacional, execute o seguinte comando:

  \begin{verbatim}
  stack new hello-world
  \end{verbatim}

  que irá produzir a seguinte estrutura de arquivos:

\begin{forest}
  for tree={%
    folder,
    grow'=0,
    fit=band,
  }
  [.
    [LICENSE
    ]
    [Setup.hs
    ]
    [app
      [Main.hs]
    ]
    [src
      [Lib.hs]
    ]
    [stack.yaml]
    [test
      [Spec.hs]
    ]
  ]
\end{forest}

Após a criação desta estrutura de projeto, você pode executar o comando \verb|stack setup|
que irá baixar o compilador Haskell (GHC), caso necessário.

\section{Hello World!}

Use seu editor de texto favorito e digite o seguinte programa:

%format main = "\F{main}"
%format Main = "\D{Main}"
%format putStrLn = "\F{putStrLn}"

\begin{spec}
module Main where

main = putStrLn "Hello world!"
\end{spec}

Após digitar o seguinte programa, sobreescreva o arquivo \verb|app/Main.hs| do projeto recém-criado.

Note que arquivos Haskell possuem a extensão \verb|.hs| ou \verb|.lhs|. Assim como em outras linguagens,
é importante que usemos o mesmo nome de arquivo para o módulo nele definido, caso contrário o compilador irá
apresentar uma mensagem de erro ao tentarmos importar esse módulo a partir de outro.

\subsection{Compilando e executando seu projeto}

O Haskell stack automatiza tarefas de gerenciamento de bibliotecas e compilação de programas. Por enquanto,
não vamos utilizar nenhuma biblioteca além das previamente incluídas no compilador GHC. Para executar a
aplicação contida em um projeto, basta executar os seguintes comandos em ordem:

\begin{verbatim}
stack build
stack exec hello-world-exe
\end{verbatim}

O comando \verb|stack build| irá compilar o programa atual e instalar bibliotecas por ele requeridas. Finalmente,
o comando \verb|stack exec hello-world-exe| irá executar o programa de nome \verb|hello-world-exe|. O nome
|hello-world-exe| é o nome do executável de seu projeto. Como executáveis possuem nomes, é possível ter mais de
um executável em um mesmo projeto. 

Além de gerenciar a compilação e execução de projetos, o stack permite usarmos o intepretador de Haskell, ghci, para
execução interativa do projeto. Para invocar o interpretador, basta usar o comando \verb|stack ghci|, que irá apresentar
o seguinte \emph{prompt}:
\begin{verbatim}
*Main>
\end{verbatim}
Ao digitarmos o nome da função |main|, obtemos o resultado esperado.
\begin{verbatim}
Hello world!
\end{verbatim}

\end{document}
