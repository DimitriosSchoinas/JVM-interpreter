# LAP 2023-2024 -- Segundo Trabalho

## Instruções de submissão

Dentro da pasta `lib` encontrará o seguintes ficheiro:

  - `runner.ml`

O ficheiro `runner.ml` deverá conter a sua solução do segundo trabalho de LAP.

Realizar um *commit* significa realizar uma nova entrega do trabalho. Assim,
apenas o último *commit* será considerado como a entrega de versão final do seu
trabalho.

### Implementação

No ficheiro `runner.ml` encontrará a declaração da função, `run`. O corpo desta função apresenta apenas a expressão

```ocaml
  assert false (* COMPLETAR AQUI *)
```

Deverá apagar esta expressão e substituir pela sua resolução. Mais uma vez
alertamos que poderá utilizar *todas* as funções auxiliares que achar
conveniente.

## Testes

Para executar a bateria de testes definida pela equipa docente deverá, na raiz
do projeto e num terminal, executar o seguinte comando:

```console
  dune runtest
```

Se todos os testes forem bem sucedidos, deverá obter a seguinte mensagem:

```console
...............
Ran: 15 tests in: XXX seconds.
OK
```

indicando o número de testes executados, assim como o tempo total de execução.

## Erros

Se existir pelo menos um teste que não é bem sucedido, então a execução do
comando `dune runtest` irá abortar a sua execução. Os erros que farão este
comando falhar poderão ser tão diversos como:

- erros puros de OCaml (*e.g.*, erros de sintaxe, argumentos de função não
  utilizados, erros de tipificação, etc.);
- erro na *string* utilizada para detetar uma situação anómala do interpretador.
  Por exemplo, quando ocorre uma instrução inválida não utilizar a *string*
  "Invalid Instruction: iiiiii";
- erro num teste OUnit, em que alguma das funções devolve um inteiro diferente
  daquele que é esperado.
