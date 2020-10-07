\documentclass{report}

\usepackage[brazilian]{babel}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{booktabs} % For formal tables
\usepackage{graphicx,hyperref}
\usepackage{float}
\usepackage{proof,tikz}
\usepackage{amssymb,amsthm,stmaryrd}


%include lhs2TeX.fmt
%include lhs2TeX.sty
%include polycode.fmt

\DeclareMathAlphabet{\mathkw}{OT1}{cmss}{bx}{n}
%subst keyword a = "\mathkw{" a "}"
%subst conid a = "\V{" a "}"
%subst varid a = "\V{" a "}"
%subst numeral a = "\C{" a "}"
%subst string a = "\TC{``"a "''''}"

\newtheorem{Lemma}{Lemma}
\newtheorem{Theorem}{Theorem}
\theoremstyle{definition}
\newtheorem{Example}{Example}

\newcommand{\sembrackets}[1]{\ensuremath{\llbracket #1 \rrbracket}}

\usepackage{color}
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

\newcommand{\conf}[1]{\ensuremath{\langle #1 \rangle}}

\begin{document}


%format Char = "\D{Char}"
%format String = "\D{String}"
%format Int = "\D{Int}"

%if False
\begin{code}
module Semana10 where

import Parser
\end{code}   
%endif

\section*{Processando pacotes UDP}

O User Datagram Protocol (UDP) é um protocolo simples utilizado em redes de computadores.
A transmissão de dados de acordo com o protocolo UDP utiliza a noção de datagrama, que
pode ser ilustrada visualmente como:

\begin{figure}[h]
  \includegraphics[scale=0.5]{UDP-Header.jpg}
  \centering
\end{figure}

De maneira intuitiva, um datagrama pode ser entendido como um registro contendo os seguintes campos
representados visualmente na figura anterior:

\begin{itemize}
   \item Porta de origem (source port): número de 16 bits que representa o endereço que enviou o
         datagrama.
   \item Porta de destino (destination port): número de 16 bits que representa o endereço que receberá
         o datagrama.
   \item Tamanho (length): número de 16 bits que determina o número de bits que forma o datagrama.
   \item Checksum: número de 16 bits utilizado para validar a consistência dos dados no datagrama.
   \item Data: Sequências de 32 bits que representam a informação transmitida usando o protocolo UDP.
\end{itemize}

O objetivo dessa prova é a implementação de um parser de pacotes UDP. Para isso, primeiramente vamos
representar um bit, usando o tipo a seguir.

%format Bit = "\D{Bit}"
%format O = "\C{O}"
%format I = "\C{I}"
\begin{code}
data Bit = O | I deriving (Eq, Ord)
\end{code}

%format Field = "\D{Field}"
%format size = "\C{size}"
%format content = "\C{content}"

Os diferentes campos do datagrama UDP podem ser representados pelo tipo |Field| a seguir.
%format Field = "\D{Field}"
%format size = "\C{size}"
%format content = "\C{content}"
\begin{code}
data Field
  = Field {
      size :: Int
    , content :: [Bit]
    } deriving (Eq, Ord)
\end{code}
|Field| é representado como um registro que armazena o seu valor em bits (representado como |[Bit]|) e
um inteiro que representa número de bits armazenado nessa lista. Como exemplo, considere o seguinte
campo de dois bits:
\begin{spec}
ex :: Field
ex = Field 2 [O,I]
\end{spec}
A utilidade do tipo |Field| é representar as restrições de tamanho presentes na especificação de
datagramas UDP. Representaremos um datagrama pelo seguinte tipo de dados:
%format UDP = "\D{UDP}"
%format source = "\C{source}"
%format destination = "\C{destination}"
%format plength = "\C{plength}"
%format pdata = "\C{pdata}"
%format checksum = "\C{checksum}"
\begin{code}
data UDP
  = UDP {
      source :: Field       --- 16 bits
    , destination :: Field  --- 16 bits
    , plength :: Field      --- 16 bits
    , checksum :: Field     --- 16 bits
    , pdata :: [Field]      --- list of 32 bit fields
    } deriving (Eq, Ord)
\end{code}
Cada campo do tipo |UDP| representa um componente do datagrama, utilizando o tipo de dados |Field|
que representa sequências de bits de um certo tamanho. Como exemplo, o campo |source| é representado
por um valor de tipo |Field| contendo uma lista de 16 bits.

Com base no apresentado, faça o que se pede.

%format Parser = "\D{Parser}"
%format bitParser = "\F{bitParser}"

\begin{enumerate}
   \item Implemente um parser para o tipo |Bit|, de forma que o dígito 1 seja representado pelo construtor
         |I| e o dígito 0 por |O|.

\begin{spec}
bitParser :: Parser Char Bit
bitParser = undefined
\end{spec}

%format bitList = "\F{bitList}"

   \item Usando o parser para bits, podemos construir um parser que processa uma sequência de n bits.
         Implemente o parser:
\begin{spec}
bitList :: Int -> Parser Char [Bit]
bitList n = undefined
\end{spec}
        Um parser |bitList n| processa uma sequência de n
        bits retornando-os como resultado.

\item Usando parser |bitList| implemente um parser para o tipo |Field|.
%format fieldParser = "\F{fieldParser}"
\begin{spec}
fieldParser :: Int -> Parser Char Field
fieldParser n = undefined
\end{spec}
Note que |fieldParser n| retorna um valor de tipo |Field| contendo uma
lista de |n| bits e o inteiro n como campos deste registro.
%format UDP = "\D{UDP}"
%format udpParser = "\F{udpParser}"

\item Finalmente, usando os tamanhos de sequências de bits de cada campo de um datagrama UDP, construa
um parser para o tipo |UDP|.
\begin{spec}
udpParser :: Parser Char UDP
udpParser = undefined
\end{spec}
\end{enumerate}
\end{document}
