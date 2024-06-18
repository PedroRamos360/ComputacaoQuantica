```haskell
pauliX :: Qubit -> Qubit
pauliX (a, b) = (b, a)

pauliY :: Qubit -> Qubit
pauliY (a, b) = (0 :+ (-realPart a), imagPart b :+ 0)

pauliZ :: Qubit -> Qubit
pauliZ (a, b) = (a, -b)
```

Essas são as operações necessárias para recuperar o qbit de A em C referenciados em
[Conditional operations](conditional_operations.png).

- pauliX: Representa o bit flip, ou state flip (operação X)
- pauliZ: Representa o phase flip (operação Z)
- pauliY: Representa o bit flip combinado com o phase flip (operação Y ou XZ)

```haskell
hadamard :: Qubit -> Qubit
hadamard (a, b) = ((a + b) / sqrt 2, (a - b) / sqrt 2)
```

Define a operação de hadamard que cria a superposição de estados A e B (? não tenho certeza)

```haskell
cnot (a1, b1) (a2, b2) =
    if magnitude b1 == 0 then ((a1, b1), (a2, b2))
    else ((a1, b1), pauliX (a2, b2))
```

Define o gate CNOT que flipa o segundo qbit caso o primeiro seja |1>
Exemplo:
primeiro qbit |1>
cnot (0, 1) (1, 0) => (0, 1) (0, 1)
primeiro qbit |0>
cnot (1, 0) (1, 0) => (0, 1) (1, 0)
Lembrando que a notação |> representa uma matriz vertical em que o primeiro
elemento representa o 0 e o segundo 1 caso tenha 1 no primeiro elemento (o elemento
que representa o 0) então ela fica |0> caso esteja no segundo elemento fica |1>
