---
author: Programação Funcional
title: Exercícios para tutoria. Semana 6.
date: Prof. Rodrigo Ribeiro
---


Um utilitário para editoras de livros
=====================================

Introdução
----------

Um problema muito relevante para editoras é
determinar como remunerar autores sobre o
material por eles escrito. A editora WeFail
pretende automatizar esse processo usando
uma ferramenta que produz estatísticas
sobre o texto entregue por um autor.

A seguir, apresentaremos a definição das
estatísticas consideradas pela ferramenta.

Estatísticas
------------

É importante ressaltar que a ferramenta deve
desconsiderar palavras de tamanho menor ou igual
a 3. A maioria dessas palavras consiste de proposições
ou artigos que não são contabilizados para fim de
construir as estatísticas.

1. Total de palavras: Medida do número total de palavras
presente no texto.

2. Frequência de palavras: A frequência de palavras consiste
em uma tabela formada pela palavra e o número total de
ocorrências desta palavra no texto.

3. Lista de palavras pobres: Considera-se como palavra pobre
palavras com os seguintes sufixos: ão, eza, or, dor, ando, ado
oso, e ar. Para cada sufixo, você deve listar as palavras que o
contém e sua respectiva frequência no texto.

4. Lista de nomes próprios: Consideramos que uma pa
